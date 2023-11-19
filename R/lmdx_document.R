#' Chunk a pdf_data long format into a list of wide format per segment.
#' 
#' 
#' @param pdf_data_df a data frame of the flatten output of  `pdftools::pdf_data()`
#' @param segment layout information granularity. either "word", "font" or "line", the text
#'  segment to assemble for each layout information.
#' @param max_lines the text segment max chunk size in lines
#'
#' @return the list of text formatted in wide format chunk-ed into no more than `max_lines`. 
#' @export
#'
#' @importFrom dplyr mutate group_by summarize lag lead %>%
#' @importFrom tidyr replace_na
pivot_to_short_segment <- function(pdf_data_df, segment, max_lines) {
  pivot_lts <- pivot_longer_to_segment(pdf_data_df, segment)
  n_lines <- stringr::str_count(pivot_lts, "\\n")
  n_words <- stringr::str_count(pivot_lts, "(\\s+|\\n)")
  if (n_lines > max_lines) {
    group <- cut(seq(nrow(pdf_data_df)), 1 + n_lines %/% max_lines) %>% as.numeric()
    # group <- group[1:(length(group) - 1)]
    map_chr(seq(max(group)), ~pivot_longer_to_segment(pdf_data_df[(group == .x), ], segment) )
  } else {
    list(pivot_lts)
  }
}
  
#' Turn a pdf_data long format into wide format per segment
#'
#' @param pdf_data a data frame output of `pdftools::pdf_data()`
#' @param segment layout information granularity. either "word", "font" or "line", the text
#'  segment to assemble for each layout information.
#'
#' @return the text segmented in wide format, trailed with the segment layout values in the format "x|y".
#' @export
#'
#' @importFrom dplyr mutate group_by summarize lag lead %>%
#' @importFrom tidyr replace_na
pivot_longer_to_segment <- function(pdf_data, segment) {
  has_needed_font_col <- segment != "font" | "font_name" %in% names(pdf_data)
  stopifnot(
    "`font` type segments requires `pdf_data()` extraction with option `font_info = TRUE`" = has_needed_font_col
  )
  pdf_data <- pdf_data %>% mutate(
      x_center = trunc(x + width / 2),
      y_center = trunc(y + height / 2)
    )
  pivoted_data <- switch(
    segment,
    "word" = pdf_data,
    "line" = pdf_data %>%
      mutate(
        sentence_id = lag(cumsum(!space)) %>% tidyr::replace_na(0)
      ) %>%
      group_by(sentence_id) %>%
      summarize(
        text = paste0(text, collapse = " "),
        x_center = trunc(mean(x_center)),
        y_center = trunc(mean(y_center))
      ),
    "font" = pdf_data %>%
      mutate(
        sentence_id = lag(cumsum(!space)) %>% replace_na(0),
        font_id = lag(cumsum(dplyr::lead(font_name) != font_name)) %>% replace_na(0)
      ) %>%
      group_by(sentence_id + font_id) %>%
      summarize(
        text = paste0(text, collapse = " "),
        x_center = trunc(mean(x_center)),
        y_center = trunc(mean(y_center))
      )
  )
  paste0(pivoted_data$text, " ", pivoted_data$x_center, "|", pivoted_data$y_center, collapse = "\n")
  
}

#' Turn a pdf_data into LMDX prompt <document> part
#'
#' @param pdf_data a data frame output of `pdftools::pdf_data()`.
#' @param segment layout information granularity. either "word", "font" or "line", the text
#'  segment to assemble for each layout information.
#' @param chunk either "page" or "sequence", the text segment to assemble.
#' @param max_lines the text segment chunk size in lines (for chunk = "sequence")
#'
#' @return the list of text formatted in wide format chunk-ed into no more than `max_lines`. 
#'
#' @return a character vector being the document part of the prompt. The size depends on 
#'   `chunk` strategy.
#' @export
#' @importFrom purrr map flatten_dfc
lmdx_document <- function(pdf_data, segment, chunk = "page", max_lines = 100L) {
  stopifnot("segment is not supported" = segment %in% c("word", "font", "line"))
  stopifnot("chunk is not supported" = chunk %in% c("page", "sequence"))
  if (chunk == "sequence") {
    stopifnot("`chunk mode='sequence'` expect a value for `max_line`" =  !is.null(max_lines))
    stopifnot("`max_line` shall be an integer" = is.integer(max_lines))
  }
  txt_segment <- switch(chunk,
        "page" = map(pdf_data, pivot_longer_to_segment, segment),
        "sequence" = pivot_to_short_segment(flatten_dfc(pdf_data), segment, max_lines)
  )
  document <- map(txt_segment, ~glue::glue("<Document>\n{.x}\n</Document>\n"))
  document
}

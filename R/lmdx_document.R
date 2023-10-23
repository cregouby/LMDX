#' Turn a pdf_data long format into wide format per segment
#'
#' @param pdf_data a data frame output of `pdftools::pdf_data()`
#' @param segment either "word" , "font", or "line", the text segment to assemble
#'
#' @return the text formated in wide wide 
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
#' @param segment either "word", "font" or "line", the text segment to assemble.
#'
#' @return a character vector being the document part of the prompt 
#' @export
#' @importFrom purrr map
lmdx_document <- function(pdf_data, segment) {
  stopifnot("segment is not supported" = segment %in% c("word", "font", "line"))
  is_multi_page <- rlang::is_list(pdf_data)
  if (is_multi_page) {
    txt_segment <- map(pdf_data, pivot_longer_to_segment, segment)
    document <- map(txt_segment, ~glue::glue("<Document>\n{.x}\n</Document>\n"))
  } else {
    txt_segment <- pivot_longer_to_segment(pdf_data, segment)
    document <- glue::glue("<Document>\n{txt_segment}\n</Document>\n")
  }
  document
}

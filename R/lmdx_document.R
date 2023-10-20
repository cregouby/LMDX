#' Turn a pdf_data long format into wide format per segment
#'
#' @param pdf_data a data frame output of `pdftools::pdf_data()`
#' @param segment either "word" or "sentence", the text segment to assemble
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr mutate group_by summarize %>%
#' @importFrom tidyr replace_na
pivot_longer_to_segment <- function(pdf_data, segment = "word") {
  pdf_data <- pdf_data %>% 
    mutate(x_center = trunc(x + width/2),
           y_center = trunc(y + height/2))
  sent_data <- pdf_data %>% 
    mutate( sentence_id = lag(cumsum(!space)) %>% tidyr::replace_na(0)) %>% 
    group_by(sentence_id) %>% 
    summarize(text = paste0(text, collapse = " "), x_center = trunc(mean(x_center)), y_center = trunc(mean(y_center)))
  
  switch (segment,
    "word" = paste0(pdf_data$text, " ", pdf_data$x_center, "|", pdf_data$y_center, collapse = "\n"),
    "sentence" = paste0(sent_data$text, " ", sent_data$x_center, "|", sent_data$y_center, collapse = "\n")
                        
  )
  
}

#' Turn a pdf_data into LMDX prompt <document> part
#'
#' @param pdf_data a data frame output of `pdftools::pdf_data()`
#' @param segment either "word" or "sentence", the text segment to assemble
#'
#' @return a character vector being the document part of the prompt 
#' @export
#' @importFrom purrr map
lmdx_document <- function(pdf_data, segment = "word", chunk = "page") {
  stopifnot("segment is not supported" = segment %in% c("word", "sentence"))
  stopifnot("only `page` chunk is supported" = chunk == "page")
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

#' Turn a pdf document into LMDX prompt 
#'
#' @param document a pdf document
#' @param entities an entity taxonomy to extract from the document 
#'
#' @return the prompt to pass to the LLM
#' @export
#' @importFrom pdftools pdf_data
lmdx_prompt <- function(document, entities, ...) {
  stopifnot("only pdf document is supported" = fs::path_ext(document) == "pdf")
  stopifnot("cannot find the document" = fs::file_exists(document))
  pdf_data <- pdftools::pdf_data(document)
  lm_document <- lmdx_document(pdf_data, ...)
}
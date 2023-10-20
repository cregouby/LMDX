# turn a pdf_data into LMDX input format
#
#' Title
#'
#' @param pdf_data 
#' @param segment 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr mutate group_by summarize %>%
pivot_longer_to_segment <- function(pdf_data, segment = "word") {
  pdf_data <- pdf_data %>% 
    mutate(x_center = trunc(x + width/2),
           y_center = trunc(y + height/2))
  sent_data <- pdf_data %>% 
    mutate( sentence_id = lag(cumsum(!space)) %>% replace_na(0)) %>% 
    group_by(sentence_id) %>% 
    summarise(text = paste0(text, collapse = " "), x_center = trunc(mean(x_center)), y_center = trunc(mean(y_center)))
  
  switch (segment,
    "word" = paste0(pdf_data$text, " ", pdf_data$x_center, "|", pdf_data$y_center, collapse = "\n"),
    "sentence" = paste0(sent_data$text, " ", sent_data$x_center, "|", sent_data$y_center, collapse = "\n")
                        
  )
  
}


# turn a pdf_data into LMDX input format
#
#' Title
#'
#' @param pdf_data 
#' @param segment 
#'
#' @return
#' @export
#' @importFrom purrr map
lmdx_document <- function(pdf_data, segment = "word", chunk = "page") {
  stopifnot("segment is not supported" = segment %in% c("word", "sentence"))
  stopifnot("only `page` chunk is supported" = chunk == "page")
  txt_segment <- map(pdf_data, pivot_longer_to_segment, segment)
  document <- map(txt_segment, ~glue::glue("<Document>\n{.x}\n</Document>\n"))
  document
}


lmdx_prompt <- function(document, entities, ...) {
  stopifnot("only pdf document is supported" = fs::path_ext(document) == "pdf")
  stopifnot("cannot find the document" = fs::file_exists(document))
  pdf_data <- pdftools::pdf_data(document)
  lm_document <- lmdx_document(pdf_data, ...)
}
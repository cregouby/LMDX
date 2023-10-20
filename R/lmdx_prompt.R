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
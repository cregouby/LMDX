#' Turn a pdf document into LMDX prompt 
#'
#' @param document a pdf document
#' @param taxonomy an entity taxonomy to extract from the document.
#' @param ... used to pass parameter to the underlying `lmdx_document()` function.
#'
#' @return the prompt to pass to the LLM
#' @export
#' @importFrom pdftools pdf_data
#' @importFrom glue glue
lmdx_prompt <- function(document, taxonomy, ...) {
  stopifnot("only pdf document is supported" = fs::path_ext(document) == "pdf")
  stopifnot("cannot find the document" = fs::file_exists(document))
  pdf_data <- pdftools::pdf_data(document)
  glue::glue(lmdx_document(pdf_data, ...), lmdx_task(taxonomy))
}
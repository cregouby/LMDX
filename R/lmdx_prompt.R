#' Turn a pdf document into LMDX prompt 
#'
#' @param document a pdf document
#' @param taxonomy an entity taxonomy to extract from the document.
#' @param segment used to pass parameter to the underlying `lmdx_document()` function.
#'
#' @return the prompt to pass to the LLM
#' @export
#' @importFrom pdftools pdf_data
#' @importFrom glue glue
lmdx_prompt <- function(document, taxonomy, segment = "word") {
  stopifnot("only pdf document is supported" = fs::path_ext(document) == "pdf")
  stopifnot("cannot find the document" = fs::file_exists(document))
  if (segment == "font") {
    pdf_data <- pdftools::pdf_data(document, font_info = TRUE)
  } else {
    pdf_data <- pdftools::pdf_data(document)
  }
  glue::glue("{lmdx_document(pdf_data, segment)}{lmdx_task(taxonomy)}")
}
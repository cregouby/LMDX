#' Turn a pdf document into LMDX prompt 
#'
#' @param document a pdf document
#' @param taxonomy an entity taxonomy to extract from the document.
#' @param segment used to pass parameter to the underlying `lmdx_document()` function.
#' @param chunk either "page" or "sequence", the chunking of the text. If `page`, the default,
#' the original page chunk is used, with the risk of out of sequence tokens if the page 
#' contains a lot of words. if `sequence`, the text is chunked into lines to fit in the LLM
#' max-sequence length.
#' @param max_lines length of the chunk in lines in case of chunk = "sequence".
#'
#' @return the prompt to pass to the LLM
#' @export
#' @importFrom pdftools pdf_data
#' @importFrom glue glue
lmdx_prompt <- function(document, taxonomy, segment = "word", chunk = "page", max_lines) {
  stopifnot("only pdf document is supported" = fs::path_ext(document) == "pdf")
  stopifnot("cannot find the document" = fs::file_exists(document))
  if (segment == "font") {
    pdf_data <- pdftools::pdf_data(document, font_info = TRUE)
  } else {
    pdf_data <- pdftools::pdf_data(document)
  }
  map(lmdx_document(pdf_data, segment, chunk, max_lines), ~glue::glue("{.x}{lmdx_task(taxonomy)}"))
}
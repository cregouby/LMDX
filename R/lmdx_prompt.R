#' Turn a pdf document into LMDX prompt 
#'
#' @details
#' `lmdx_prompt()` turns the content of a pdf file into a text with layout encoded according
#'  to LMDX paper. This is the prompt to pass into the LLM model for extraction of the entities
#'  present in the provided taxonomy.
#'  
#' @section Chunking and segmentation strategies:
#'  In order to fit the maximum input token limitation of the model you use, multiple `chunk`
#'   strategy are available : 
#'   `page`: (default) the pdf is chunked in it's original page. This is recommended if
#'    the page content is not too large.
#'   `sequence` : chunk every a sequence of `max_lines` lines. This may prevent the model
#'    to reconstruct the hierarchy 
#'   in the ontology as you may chunk in the middle of a paragraph.
#'   
#'  In order for the model to consider text layout, you will segment the text associated to
#'   a layout according to your need : 
#'   `word` : each word gets it's own layout data. This is very verbose and quickly
#'    fills in the prompt. This is recommended for table extraction.
#'   `line` : each line is associated with a layout data. This is much less verbose and 
#'   fits the need for paragraph recognition, reading order, ...
#'   `font` : an intermediate solution where a change in the font of the text triggers 
#'   a new layout segment.
#'   
#' @section Other usefull options:
#'
#' @param document a pdf document filename.
#' @param taxonomy an entity taxonomy to extract from the document.
#' @param segment layout information granularity, either `word`, `font` or `line`.
#'   Parameter passed to `lmdx_document()` function.
#' @param chunk either "page" or "sequence", the chunking of the text. If `page`, the default,
#' the original page chunk is used, with the risk of input tokens exceeding the model capacity
#' if the page contains a lot of words. if `sequence`, the text is chunked into lines to fit in the LLM
#' max-sequence length.
#' @param max_lines length of the chunk in lines in case of chunk = "sequence".
#'
#' @return the prompt to pass to the LLM
#' @export
#' @importFrom pdftools pdf_data
#' @importFrom glue glue
lmdx_prompt <- function(document, taxonomy, segment = "word", chunk = "page", max_lines = NULL) {
  stopifnot("only pdf document is supported" = fs::path_ext(document) == "pdf")
  stopifnot("cannot find the document" = fs::file_exists(document))
  stopifnot("`chunk mode='sequence'` expect a value for `max_line`" = (chunk = "sequence") == "pdf")
  if (segment == "font") {
    pdf_data <- pdftools::pdf_data(document, font_info = TRUE)
  } else {
    pdf_data <- pdftools::pdf_data(document)
  }
  map(lmdx_document(pdf_data, segment, chunk, max_lines), ~glue::glue("{.x}{lmdx_task(taxonomy)}"))
}
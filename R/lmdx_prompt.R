#' Turn a pdf document into LMDX prompt 
#'
#' @details
#' `lmdx_prompt()` turns the content of a pdf file into a text with layout encoded according
#'  to LMDX paper. This is the prompt to pass into the LLM model for extraction of the entities
#'  present in the provided taxonomy.
#'  
#' @section Chunking and segmentation strategies:
#'  At the time of chunking the text out of the document, you should take care of two limitations of the model :
#'  1. The entire prompt ( document and taxonomy) shall fit in the maximum input tokens that the model can
#'   support per query.
#'  2. All the feature extracted in the answer, encoded in the json schema you provide shall fit in the 
#'   model maximum output tokens.
#'  
#'  In order to conform to the maximum input token limitation of the model you use, multiple `chunk`
#'   strategy are available : 
#'   - `page`: (default) the pdf is chunked in it's original page. This is recommended if
#'    the page content is not too large.
#'   - `sequence` : chunk every a sequence of `max_lines` lines. This may prevent the model
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
#' @param chunk either `page` or `sequence` for sequence of lines, the chunking of the text. If `page`, the default,
#' the document is chunked per page, with the risk of input tokens exceeding the model capacity
#' if the page contains a lot of words. If `sequence`, the text is chunked into `max_lines` lines to fit in the LLM
#' max-sequence length.
#' @param max_lines length of the chunk in lines in case of chunk = "sequence".
#' @param into_yaml shall we include the taxonomy in json format (`FALSE`), the default or turn it into YAML encoding 
#'  in the prompt (`TRUE`).
#'
#' @return the prompt to pass to the LLM  and a console message about its length.
#' @export
#' @importFrom pdftools pdf_data
#' @importFrom purrr map
#' @importFrom glue glue
#' @importFrom rlang warn inform
lmdx_prompt <- function(document, taxonomy, segment = "word", chunk = "page", max_lines = NULL, into_yaml = FALSE) {
  stopifnot("only pdf document is supported" = fs::path_ext(document) == "pdf")
  stopifnot("cannot find the document" = fs::file_exists(document))
  if (segment == "font") {
    pdf_data <- pdf_data(document, font_info = TRUE)
  } else {
    pdf_data <- pdf_data(document)
  }
  prompt <- map(lmdx_document(pdf_data, segment, chunk, max_lines), ~glue::glue("{.x}\n{lmdx_task(taxonomy, into_yaml)}"))
  prompt_length <- max(stringr::str_length(prompt))
  if (prompt_length > 2500) {
    warn(glue("Max prompt length is {prompt_length} characters, It is recommended to change your chunking strategy."))
  } else {
    inform(glue("Max prompt length is {prompt_length} characters."))
  }
  prompt
}
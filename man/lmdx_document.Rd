% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmdx_document.R
\name{lmdx_document}
\alias{lmdx_document}
\title{Turn a pdf_data into LMDX prompt <document> part}
\usage{
lmdx_document(pdf_data, segment, chunk = "page", max_lines = 100L)
}
\arguments{
\item{pdf_data}{a data frame output of `pdftools::pdf_data()`.}

\item{segment}{layout information granularity. either "word", "font" or "line", the text
segment to assemble for each layout information.}

\item{chunk}{either "page" or "sequence", the text segment to assemble.}

\item{max_lines}{the text segment chunk size in lines (for chunk = "sequence")}
}
\value{
the list of text formatted in wide format chunk-ed into no more than `max_lines`.

a character vector being the document part of the prompt. The size depends on 
  `chunk` strategy.
}
\description{
Turn a pdf_data into LMDX prompt <document> part
}

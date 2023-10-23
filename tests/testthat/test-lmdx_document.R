pdf_data <- pdftools::pdf_data(pdf = system.file("extdata", "Short-refcard.pdf", package = "LMDX"))
pdf_n_page <- pdftools::pdf_length(input = system.file("extdata", "Short-refcard.pdf", package = "LMDX"))
pdf_single <- pdftools::pdf_data(pdf = system.file("extdata", "Short-refcard_1.pdf", package = "LMDX"))
  
test_that("pivot_longer_to_segment works for word", {
  expect_error(pivoted_data <- pivot_longer_to_segment(pdf_data[[1]], segment = "word"), 
               NA)
  expect_type(pivoted_data, "character")
  expect_length(pivoted_data, 1L)
})

test_that("pivot_longer_to_segment works for line", {
  expect_error(pivoted_data <- pivot_longer_to_segment(pdf_data[[2]], segment = "line"), 
               NA)
  expect_type(pivoted_data, "character")
  expect_length(pivoted_data, 1L)
})

test_that("pivot_longer_to_segment works for font", {
  # CI pipeline relies on a debian version with older libpoppler that do not allow option `font_info = TRUE` 
  skip_on_os("linux")
  pdf_font <- pdftools::pdf_data(pdf = system.file("extdata", "Short-refcard_1.pdf", package = "LMDX"), font_info = TRUE)
  expect_error(pivoted_data <- pivot_longer_to_segment(pdf_font[[1]], segment = "font"), 
               NA)
  expect_type(pivoted_data, "character")
  expect_length(pivoted_data, 1L)
})

test_that("pivot_longer_to_segment provides an explicit error if font extraction is missing", {
  expect_error(pivoted_data <- pivot_longer_to_segment(pdf_data[[2]], segment = "font"), 
               "extraction with option `font_info")
})

test_that("lmdx_document works for word", {
  expect_error(lmdx_doc <- lmdx_document(pdf_data, segment = "word"), 
               NA)
  expect_type(lmdx_doc, "list")
  expect_length(lmdx_doc, pdf_n_page)
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "^<Document>")))
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "</Document>$")))
})

test_that("lmdx_document works for line", {
  expect_error(lmdx_doc <- lmdx_document(pdf_data, segment = "line"), 
               NA)
  expect_type(lmdx_doc, "list")
  expect_length(lmdx_doc, pdf_n_page)
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "^<Document>")))
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "</Document>$")))
})

test_that("lmdx_document works for font", {
  # CI pipeline relies on a debian version with older libpoppler that do not allow option `font_info = TRUE` 
  skip_on_os("linux")
  pdf_data <- pdftools::pdf_data(pdf = system.file("extdata", "Short-refcard.pdf", package = "LMDX"), font_info = TRUE)
  expect_error(lmdx_doc <- lmdx_document(pdf_data, segment = "font"), 
               NA)
  expect_type(lmdx_doc, "list")
  expect_length(lmdx_doc, pdf_n_page)
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "^<Document>")))
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "</Document>$")))
})

test_that("lmdx_document  provides an explicit error if font extraction is missing", {
  # CI pipeline relies on a debian version with older libpoppler that do not allow option `font_info = TRUE` 
  expect_error(lmdx_document(pdf_data, segment = "font"), 
               "extraction with option `font_info")
})

test_that("lmdx_document works for word for a single page document", {
  expect_error(lmdx_doc <- lmdx_document(pdf_single, segment = "word"), 
               NA)
  expect_type(lmdx_doc, "list")
  expect_length(lmdx_doc, 1L)
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "^<Document>")))
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "</Document>$")))
})

test_that("lmdx_document works for sentence", {
  expect_error(lmdx_doc <- lmdx_document(pdf_single, segment = "line"), 
               NA)
  expect_type(lmdx_doc, "list")
  expect_length(lmdx_doc, 1L)
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "^<Document>")))
  expect_true(all(purrr::map_lgl(lmdx_doc, stringr::str_detect, "</Document>$")))
})

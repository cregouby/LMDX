pdf_data <- pdftools::pdf_data(pdf = system.file("extdata", "Short-refcard.pdf", package = "LMDX"))
pdf_single <- pdftools::pdf_data(pdf = system.file("extdata", "Short-refcard_1.pdf", package = "LMDX"))
  
test_that("pivot_longer_to_segment works for word", {
  expect_error(pivot_longer_to_segment(pdf_data[[1]], segment = "word"), 
               NA)
})

test_that("pivot_longer_to_segment works for sentence", {
  expect_error(pivot_longer_to_segment(pdf_data[[2]], segment = "sentence"), 
               NA)
})

test_that("lmdx_document works for word", {
  expect_error(lmdx_document(pdf_data, segment = "word"), 
               NA)
lmdx_document})

test_that("lmdx_document works for sentence", {
  expect_error(lmdx_document(pdf_data, segment = "line"), 
               NA)
})

test_that("lmdx_document works for word for a single page document", {
  expect_error(lmdx_document(pdf_single, segment = "word"), 
               NA)
lmdx_document})

test_that("lmdx_document works for sentence", {
  expect_error(lmdx_document(pdf_single, segment = "line"), 
               NA)
})

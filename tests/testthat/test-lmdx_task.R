test_that("lmdx_task works with VRDU_Ad_Buy taxonomy", {
  expect_error(lmdx_task("VRDU_Ad_Buy"),
               NA
  )
})

test_that("lmdx_task works with VRDU_Registration taxonomy", {
  expect_error(lmdx_task("VRDU_Registration"),
               NA
  )
})

test_that("lmdx_task works with CORD taxonomy", {
  expect_error(lmdx_task("CORD"),
               NA
  )
})

test_that("lmdx_task works with json taxonomy", {
  expect_error(lmdx_task(jsonlite::toJSON(mtcars)),
               NA
  )
})

test_that("lmdx_task works with VRDU_Ad_Buy taxonomy in yaml", {
  expect_error(lmdx_task("VRDU_Ad_Buy", into_yaml = TRUE),
               NA
  )
})

test_that("lmdx_task works with VRDU_Registration taxonomy into yaml", {
  expect_error(lmdx_task("VRDU_Registration", into_yaml = TRUE),
               NA
  )
})

test_that("lmdx_task works with CORD taxonomyinto yaml", {
  expect_error(lmdx_task("CORD", into_yaml = TRUE),
               NA
  )
})

test_that("lmdx_task works with json taxonomy into yaml", {
  expect_error(lmdx_task(jsonlite::toJSON(mtcars), into_yaml = TRUE),
               NA
  )
})


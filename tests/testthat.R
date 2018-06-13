library(testthat)
library(FARS)
library(mapdata)

test_that('This map function works!', {
  fars_mapping <- fars_map_state(13, 2013)
  expect_that(fars_mapping, is_null())
}
)
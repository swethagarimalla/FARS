library(testthat)
library(FARS)
library(mapdata)

test_that('This map function works!', {
  fn <- make_filename(2014) 
  expect_that(fn, equals("accident_2014.csv.bz2"))
  fars_mapping <- fars_map_state(13, 2013)
  expect_that(fars_mapping, is_null())
}
)
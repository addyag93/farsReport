# test make_filename
test_that("make_filename generates file name", {
  expect_equal(make_filename(1992), "accident_1992.csv.bz2")
})

# test fars_read_years
test_that("fars_read_years return NULL if the file is missing", {
  expect_equal(fars_read_years(2014), list(NULL))
})




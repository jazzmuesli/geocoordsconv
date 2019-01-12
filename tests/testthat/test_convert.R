context("convert")
library(geocoordsconv)

test_that("DMS1", {
  expect_equal(convert_coordinate("43\u00b037'83N"), 43.63972, tolerance=1e-6)
  expect_equal(convert_coordinate("43\u00ba37'83N"), 43.63972, tolerance=1e-6)
})

test_that("DGS1", {
  expect_equal(convert_coordinate("41\u00b059.00'N"), 41.9833333333333, tolerance=1e-6)
  expect_equal(convert_coordinate("42\u00b0 29.60´N"), 42.49333, tolerance=1e-6)
  expect_equal(convert_coordinate("43\u00b037.67N"), 43.62783, tolerance=1e-6)
  expect_equal(convert_coordinate("38\u00b033.5’N"), 38.55833, tolerance=1e-6)
  expect_equal(convert_coordinate("35\u00b009′N"), 35.15, tolerance=1e-6)
})

test_that("DMS2", {
  expect_equal(convert_coordinate("43\u00b042'93''N"), 43.72583, tolerance=1e-6)
  expect_equal(convert_coordinate("28\u00b009'19'N"), 28.15528, tolerance=1e-6)
})

test_that("DGS2", {
  expect_equal(convert_coordinate("42\u00b021,12´N"), 42.352, tolerance=1e-6)
})

test_that("decimal", {
  expect_equal(convert_coordinate("-27.95"), -27.95, tolerance=1e-6)
})

test_that("two coordinates", {
  expect_equal(convert_coordinate("12°02.75'S/12°02.43'S"), -12.04317, tolerance=1e-6)
})



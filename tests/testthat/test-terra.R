context("clhs-terra")

test_that("terra SpatRaster and SpatVector methods work", {
  
  skip_if_not_installed("terra")
  
  suppressWarnings(RNGversion("3.5.0"))
  
  x <- terra::rast(system.file("ex", "elev.tif", package = "terra"))
  
  # without cpp
  set.seed(1)
  res1 <- clhs(x, size = 100, iter = 100, use.cpp = FALSE)
  
  set.seed(1)
  res2 <- clhs(terra::as.points(x), size = 100, iter = 100, use.cpp = FALSE)
  
  # with cpp
  set.seed(1)
  res3 <- clhs(x, size = 100, simple = FALSE)
  
  set.seed(1)
  res4 <- clhs(terra::as.points(x), size = 100)
  
  expect_equal(lengths(list(res1, res2, res3$index_samples, res4)), rep(100, 4))
  
  expect_equal(res1, res2)
  expect_equal(res3$index_samples, res4)

})

context("clhs-sp")

test_that("clhs on a SpatialPointsDataFrame works", {
  
  skip_if_not_installed("sp")
  
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(1)
  
  df <- data.frame(
    a = runif(1000), 
    b = rnorm(1000), 
    c = c(rnorm(n = 500, mean = 20, sd = 3), rnorm(n = 500, mean = -20, sd = 3)),
    x = c(rnorm(n = 500, mean = 20, sd = 3), rnorm(n = 500, mean = -20, sd = 3)),
    y = c(rnorm(n = 500, mean = 20, sd = 3), rnorm(n = 500, mean = -20, sd = 3))
  )
  
  spdf <- sp::SpatialPointsDataFrame(
    coords = df[, c("x", "y")],
    data = df[, c("a", "b", "c")],
    proj4string = sp::CRS("EPSG:4326")
  )
  
  res1 <- clhs(spdf, size = 5, iter = 100, progress = FALSE, simple = TRUE)
  res2 <- clhs(spdf, size = 5, iter = 100, progress = FALSE, simple = TRUE, use.coords = TRUE)
  
  expect_equal(res1, c(573, 127, 939, 848, 171))
  expect_equal(res2, c(398, 475, 826, 4, 650))
})

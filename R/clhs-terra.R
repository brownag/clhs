#' @include clhs-data.frame.R
#' @rdname clhs
#' @export
#' @noRd
clhs.SpatRaster <- function(
    x, # data
    ...,
    use.coords = FALSE
){
  
  if (!requireNamespace("terra")) {
    stop("package 'terra' is required to convert SpatRaster objects to data.frame")
  }
  
  if (use.coords) {
    df <- terra::as.data.frame(x, xy = TRUE)
  } else {
    df <- terra::as.data.frame(x)
  }
  
  spl <- clhs.data.frame(x = df, ...)
  
  if (is(spl, "cLHS_result")) {
    spl$initial_object <- x # replacing the data.frame by the Spat* object
    spl$sampled_data <- x[spl$index_samples, ]
  }
  
  spl
}

#' @include clhs-data.frame.R
#' @rdname clhs
#' @export
#' @noRd
clhs.SpatVector <- function(
    x, # data
    ...,
    use.coords = FALSE
){
  
  if (!requireNamespace("terra")) {
    stop("package 'terra' is required to convert SpatVector objects to data.frame")
  }
  
  if (use.coords) {
    if (!terra::is.points(x)) {
      stop("When `use.coords` is set to TRUE, only POINT geometries are supported",
           call. = FALSE)
    }
    df <- terra::as.data.frame(x, geom = "XY")
  } else {
    
    df <- terra::as.data.frame(x)
  }
  
  spl <- clhs.data.frame(x = df, ...)
  
  if (is(spl, "cLHS_result")) {
    spl$initial_object <- x # replacing the data.frame by the Spat* object
    spl$sampled_data <- x[spl$index_samples, ]
  }
  
  spl
}


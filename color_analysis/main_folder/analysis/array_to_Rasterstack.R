#' Convert from an array to a raster stack
#'
#' Convert from an image array to a raster stack, optionally using the alpha
#' channel as a mask.
#'
#' @param img_array An RGB array.
#' @param type Type of Raster* object to return. One of either "stack"
#'   ([raster::stack]) or "brick" ([raster::brick]).
#' @param alpha_mask Logical. Use the alpha channel as a background mask?
#' @param return_alpha Logical. Return the alpha channel as a layer?
#'
#' @return A Raster* object, either `RasterStack` or `RasterBrick` depending
#' on the `type` argument.
array_to_RasterStack <- function(img_array,
                                 type = "stack",
                                 alpha_mask = TRUE,
                                 return_alpha = FALSE) {
  
  requireNamespace("raster")
  
  type <- match.arg(type, c("stack", "brick"))
  
  r <- apply(img_array * 255, 3, raster::raster)
  
  if (type == "stack") {
    r2 <- raster::stack(r)
  } else {
    r2 <- raster::brick(r)
  }
  
  output <- r2
  
  if (alpha_mask) {
    
    if (dim(r2)[3] != 4) {
      warning("No alpha channel included; not masking output")
    } else {
      r3 <- raster::mask(raster::subset(r2, 1:3),
                         raster::subset(r2, 4),
                         maskvalue = 0)
      output <- r3
    }
  }
  
  raster::crs(output) <- "+proj=longlat"
  
  return(output)
  
}
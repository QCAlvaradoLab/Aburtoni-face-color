list <-  file.path(choose.files())
aligned <- readRDS('rds/aligned_new_blue_faces.rds')
{for (i in 1:length(aligned)) {
  img <- brick_to_array(aligned[[i]])
  blur <- blurImage(img, blur_function = "blur_anisotropic",
                         amplitude = 20, sharpness = 0.05, plotting = T)
  aligned [[i]] <- array_to_RasterStack(blur, type = "brick", alpha_mask = T, return_alpha = T
  )}
}

devtools::install_github('hiweller/recolorize', force=T)

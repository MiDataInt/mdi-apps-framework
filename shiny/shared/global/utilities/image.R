#----------------------------------------------------------------------
# image array manipulation functions; requires imager
#----------------------------------------------------------------------

# vertically expand an imager image array to multiple output pixels per input pixel
# NO interpolation, just interleaved repetition
expandImgV <- function(img, n){
  imgD <- dim(img)
  lrg <- imager::as.cimg(array(0, dim = c(imgD[1], imgD[2] * n, 1, 3)))
  for(x in 0:(n - 1)) lrg[, 1:imgD[2] * n - x, , ] <- img[, , , ]    
  lrg
}

expandImg <- function(img, h = 1, v = 1){
  if(h > 1){
    imgD <- dim(img)
    lrg <- imager::as.cimg(array(0, dim = c(imgD[1] * h, imgD[2], 1, 3)))   
    for(x in 0:(h - 1)) lrg[1:imgD[1] * h - x, , , ] <- img[, , , ]
    img <- lrg
  }
  if(v > 1){
    imgD <- dim(img)
    lrg <- imager::as.cimg(array(0, dim = c(imgD[1], imgD[2] * v, 1, 3)))
    for(x in 0:(v - 1)) lrg[, 1:imgD[2] * v - x, , ] <- img[, , , ]
    img <- lrg
  }
  img
}

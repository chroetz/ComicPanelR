formatImage <- function(img) {
  image_convert(
    img,
    format = "tiff",
    type = "ColorSeparationAlpha",
    colorspace = "cmyk",
    depth = 8,
    antialias = TRUE,
    matte = TRUE)
}


writeMagickImage <- function(img, fileName, dpi=NULL) {
  image_write(
    formatImage(img),
    path = fileName,
    format = "tiff",
    compression = "Zip",
    density = dpi)
  rm(img);gc()
  return(invisible())
}


readMagickImage <- function(fileName) {
  image_read(
    fileName,
    depth = 8
  ) |>
    formatImage()
}


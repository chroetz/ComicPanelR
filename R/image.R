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


setImageMeta <- function(fileName, dpi) {
  sprintf(
    'magick "%s" -strip -profile %s -density %d %s "%s"',
    fileName,
    getColorProfilePath(),
    dpi,
    .formatString,
    fileName
  ) |>
    system()
}

getNumberOfLayers <- function(fileName) {
  res <-
    paste0('magick identify -format "%n\n" "', fileName, '"') |>
    system(intern=TRUE)
  return(length(res))
}


getColorSpace <- function(fileName) {
  res <-
    paste0('magick identify -format "%r\n" "', fileName, '"') |>
    system(intern=TRUE)
  return(res[1])
}

getColorProfilePath <- function() {
  system.file("colorProfiles", "ISOcoated_v2_300_eci.icc", package="ComicPanelR")
}

checkColorProfile <- function(fileName) {
  tmpFileName <- tempfile(fileext = ".icc")
  profileCmdResult <-
    sprintf('magick "%s" "%s"', fileName, tmpFileName) |>
    system(intern = TRUE) |>
    suppressWarnings()
  if (length(profileCmdResult) > 0) {
    if (startsWith(profileCmdResult, "magick.exe: no color profile is available"))
      return("No color profile.")
  }
  if (!file.exists(tmpFileName)) {
    pattern <- paste0(
        "*",
        stringr::str_sub(basename(tmpFileName), end=-5),
        "-[0-9]+",
        "\\.icc")
    profiles <- dir(
      dirname(tmpFileName),
      pattern = pattern)
    return(sprintf("Detected %d profiles.", length(profiles)))
  }
  comparison <-
    sprintf(
      'fc.exe /b "%s" "%s"',
      tmpFileName,
      getColorProfilePath()
    ) |>
    system(intern = TRUE) |>
    suppressWarnings()
  if (length(comparison) >= 2) {
    if (comparison[2] == "FC: no differences encountered") return(TRUE)
    first <- strsplit(comparison[2], ":")[[1]][1]
    if (as.integer(first) == 43) return(TRUE)
  }
  return(comparison)
}

checkAll <- function(path = ".") {
  fileNames <- dir(
    path = path,
    pattern="(\\.tif$)|(\\.tiff$)",
    full.names = TRUE)
  for (fn in fileNames) {
    cat(sprintf("Checking %s:", fn))
    check <- checkColorProfile(fn)
    cat("\n\tcolor profile: ")
    if (isTRUE(check)) {
      cat("ok\n")
    } else {
      cat(paste(check[1:pmin(3, length(check))], collapse="; "), ";...\n")
    }
    cat("\tnumber of layers:", getNumberOfLayers(fn))
    cat("\n\tcolor space:", getColorSpace(fn))
    cat("\n")
  }
}

magickResize <- function(pathIn, pathOut, w, h) {
  pathIn <- normalizePath(pathIn, mustWork=TRUE)
  sprintf(
    'magick "%s" -resize %dx%d %s "%s"',
    pathIn, w, h, .formatString, pathOut
  ) |>
    system()
}

magickFormat <- function(pathIn, pathOut) {
  pathIn <- normalizePath(pathIn, mustWork=TRUE)
  sprintf(
    'magick "%s" %s "%s"',
    pathIn, .formatString, pathOut
  ) |>
    system()
}

createCutout <- function(positive, image, outPath) {
  positive <- normalizePath(positive, mustWork=TRUE)
  image <- normalizePath(image, mustWork=TRUE)
  sprintf(
    'magick composite -compose copy-opacity "%s" "%s" "%s"',
    positive,
    image,
    outPath
  ) |>
    system()
}

composeOver <- function(top, bottom, out) {
  top <- normalizePath(top, mustWork=TRUE)
  bottom <- normalizePath(bottom, mustWork=TRUE)
  sprintf(
    'magick composite -compose over "%s" "%s" "%s"',
    top,
    bottom,
    out
  ) |>
    system()
}

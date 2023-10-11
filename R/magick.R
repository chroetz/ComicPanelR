magickResize <- function(pathIn, pathOut, w, h) {
  sprintf(
    'magick "%s" -resize %dx%d %s "%s"',
    pathIn, w, h, .formatString, pathOut
  ) |>
    system()
}

magickFormat <- function(pathIn, pathOut) {
  sprintf(
    'magick "%s" %s "%s"',
    pathIn, .formatString, pathOut
  ) |>
    system()
}

createCutout <- function(positive, image, outPath) {
  sprintf(
    'magick composite -compose copy-opacity "%s" "%s" "%s"',
    positive,
    image,
    outPath
  ) |>
    system()
}

composeOver <- function(top, bottom, out) {
  sprintf(
    'magick composite -compose over "%s" "%s" "%s"',
    top,
    bottom,
    out
  ) |>
    system()
}

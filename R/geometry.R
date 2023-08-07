.cmPerInch <- 2.54

convertCmToPx <- function(len, dpi) {
  round(len * dpi / .cmPerInch)
}

getDataWidthInCm <- function(geometry) {
  geometry$size$w + 2*geometry$bleed
}

getDataHeightInCm <- function(geometry) {
  geometry$size$h + 2*geometry$bleed
}

getFinalWidthInCm <- function(geometry) {
  geometry$size$w
}

getFinalHeightInCm <- function(geometry) {
  geometry$size$h
}

getDataWidthInPx <- function(geometry, dpi) {
  convertCmToPx(getDataWidthInCm(geometry), dpi)
}

getDataHeightInPx <- function(geometry, dpi) {
  convertCmToPx(getDataHeightInCm(geometry), dpi)
}

getFinalWidthInPx <- function(geometry, dpi) {
  convertCmToPx(getFinalWidthInCm(geometry), dpi)
}

getFinalHeightInPx <- function(geometry, dpi) {
  convertCmToPx(getFinalHeightInCm(geometry), dpi)
}

getDataBoxInCm <- function(geometry) {
  makeBox(
    -geometry$bleed,
    -geometry$bleed,
    geometry$size$w+2*geometry$bleed,
    geometry$size$h+2*geometry$bleed)
}

getFinalBoxInCm <- function(geometry) {
  makeBox(
    0,
    0,
    geometry$size$w,
    geometry$size$h)
}

getPanelBoxInCm <- function(geometry) {
  makeBox(
    geometry$sideMargin,
    geometry$sideMargin,
    geometry$size$w-2*geometry$sideMargin,
    geometry$size$h-2*geometry$sideMargin)
}



makeBox <- function(x, y, w, h) {
  box <- list(
    x = x,
    y = y,
    w = w,
    h = h,
    left = x,
    right = x + w,
    top = y,
    bottom = y + h,
    midX = x + w/2,
    midY = y + h/2)
  box$xlim <- c(box$left, box$right)
  box$ylim <- c(box$bottom, box$top)
  return(box)
}


drawBox <- function(box, ...) {
  graphics::rect(
    box$left, box$bottom, box$right, box$top,
    ...)
}

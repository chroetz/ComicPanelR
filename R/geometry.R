.cmPerInch <- 2.54

.sideToIndex <- c(right = 1, top = 2, left = 3, bottom = 4)
.sideLetterToIndex <- c(r = 1, t = 2, l = 3, b = 4)
.indexToSide <- c("right", "top", "left", "bottom")
.indexToSideLetter <- c("r", "t", "l", "b")

convertCmToPx <- function(len, dpi) {
  round(len * dpi / .cmPerInch)
}

getDataWidthInCm <- function(geometry) {
  geometry$size$width + geometry$bleed$left + geometry$bleed$right
}

getDataHeightInCm <- function(geometry) {
  geometry$size$height + geometry$bleed$top + geometry$bleed$bottom
}

getFinalWidthInCm <- function(geometry) {
  geometry$size$width
}

getFinalHeightInCm <- function(geometry) {
  geometry$size$height
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
    -geometry$bleed$left,
    -geometry$bleed$top,
    geometry$size$width + geometry$bleed$left + geometry$bleed$right,
    geometry$size$height + geometry$bleed$top + geometry$bleed$bottom)
}

getFinalBoxInCm <- function(geometry) {
  makeBox(
    0,
    0,
    geometry$size$width,
    geometry$size$height)
}

getPanelBoxInCm <- function(geometry) {
  makeBox(
    geometry$margin$left,
    geometry$margin$top,
    geometry$size$width - (geometry$margin$left + geometry$margin$right),
    geometry$size$height - (geometry$margin$top + geometry$margin$bottom))
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


extendSegment <- function(side, geometry) {
  eps <- .Machine$double.eps |> sqrt()
  if (nrow(side) < 1) return(side)
  r <- 1 + geometry$size$width + geometry$size$height # HACKy: large enough number, could also be calculated exactly...
  v <- side[1,]-side[2,]
  vNorm <- sqrt(sum(v^2))
  if (vNorm >= eps) {
    side <- rbind(side[1,] + v*r, side)
  }
  n <- nrow(side)
  v <- side[n,]-side[n-1,]
  vNorm <- sqrt(sum(v^2))
  if (vNorm >= eps) {
    side <- rbind(side, side[n,]+ r*v)
  }
  return(side)
}

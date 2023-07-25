#' Convert a Layout Specification File into a Coordinates File
#' @param fileName The base file name (preferably without file extension).
#'   ".layout.json" will be added to obtain the input file. The json file should
#'   contain an object of class ` ["Comic", "Opts"]`.
#' @export
layout2coor <- function(fileName) {

  fileBaseName <- sub("\\.layout\\.json$", "", fileName, ignore.case=TRUE)
  fileIn <- paste0(fileBaseName, ".layout.json")

  comic <- ConfigOpts::readOpts(fileIn, optsClass="Comic")

  polygonLists <- lapply(comic$pages$list, page2coor, geometry = comic$geometry)

  lapply(
    seq_along(polygonLists),
    \(i) ConfigOpts::writeOpts(polygonLists[[i]], sprintf("%s_page%02d.coor.json", fileBaseName, i)))

  return(invisible())
}


rect <- function(x,y,w,h) {
    coor <- matrix(
      c(
        c(x, y),
        c(x, y+h),
        c(x+w, y+h),
        c(x+w, y)),
      ncol = 2,
      byrow = TRUE)
    return(coor)
  }


page2coor <- function(page, geometry) {

  rows <- page$layout$elements$list
  firstPanel <- rows[[1]]
  lastPanel <- rows[[length(rows)]]

  polygonList <- list()

  topSpacer <- if (firstPanel$sideMargin) geometry$sideMargin else 0
  bottomSpacer <- if (lastPanel$sideMargin) geometry$sideMargin else 0
  panelAreaH <- geometry$size$h - topSpacer - bottomSpacer - (length(rows)-1)*geometry$panelSpacing

  x <- 0
  y <- topSpacer

  for (rowIdx in seq_along(rows)) {
    r <- rows[[rowIdx]]
    polygonRow <- list()
    if (r$sideMargin) {
      leftSpacer <- geometry$sideMargin
      rightSpacer <- geometry$sideMargin
    } else {
      leftSpacer <- 0
      rightSpacer <- 0
    }
    h <- panelAreaH * page$layout$weights[rowIdx] / sum(page$layout$weights)
    panelAreaW <- geometry$size$w - leftSpacer - rightSpacer - (length(r$weights)-1)*geometry$panelSpacing
    x <- leftSpacer

    for (i in seq_along(r$weights)) {
      w <- panelAreaW * r$weights[i] / sum(r$weights)
      polygon <- rect(x, y, w, h)
      polygonRow <- c(polygonRow, list(polygon))
      x <- x + w + geometry$panelSpacing
    }

    r$tilt <- r$tilt[seq_len(length(r$weights)-1)]
    r$tilt[is.na(r$tilt)] <- 0

    for (j in seq_along(r$tilt)) {
      tilt <- r$tilt[j]
      if (tilt >= 0) {
        len <- polygonRow[[j+1]][4,1] - polygonRow[[j+1]][1,1]
      } else if (tilt < 0) {
        len <- polygonRow[[j]][4,1] - polygonRow[[j]][1,1]
      }
      polygonRow[[j]][4,1] <- polygonRow[[j]][4,1] + tilt * len
      polygonRow[[j]][3,1] <- polygonRow[[j]][3,1] - tilt * len
      polygonRow[[j+1]][1,1] <- polygonRow[[j+1]][1,1] + tilt * len
      polygonRow[[j+1]][2,1] <- polygonRow[[j+1]][2,1] - tilt * len
    }

    polygonList <- c(
      polygonList,
      lapply(
        seq_along(polygonRow),
        \(k)  ConfigOpts::makeOpts(
          "Polygon",
          points = polygonRow[[k]],
          frame = r$frame)
      ))

    y <- y + h + geometry$panelSpacing
  }

  results <-  ConfigOpts::makeOpts(
    c("Polygon", "List"),
    size = geometry$size,
    bleed = geometry$bleed,
    list = polygonList
  )

  return(results)
}


writeCoorPages <- function(polygonList) {

}


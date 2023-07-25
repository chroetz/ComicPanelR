#' Convert a Layout Specification File into a Coordinates File
#' @param fileName The base file name (preferably without file extension).
#'   ".layout.json" will be added to obtain the input file. The json file should
#'   contain an object of class ` ["Comic", "Opts"]`.
#' @export
layout2coor <- function(fileName) {

  fileBaseName <- sub("\\.layout\\.json$", "", fileName, ignore.case=TRUE)
  fileIn <- paste0(fileBaseName, ".layout.json")
  fileOut <- paste0(fileBaseName, ".coor.json")

  comic <- ConfigOpts::readOpts(fileIn, optsClass="Comic")
  page <- comic$pages$list[[1]]
  rows <- page$layout$elements$list
  firstPanel <- rows[[1]]
  lastPanel <- rows[[length(rows)]]

  polygonList <- list()

  topSpacer <- if (firstPanel$sideMargin) comic$sideMargin else 0
  bottomSpacer <- if (lastPanel$sideMargin) comic$sideMargin else 0
  panelAreaH <- comic$size$h - topSpacer - bottomSpacer - (length(rows)-1)*comic$panelSpacing

  x <- 0
  y <- topSpacer

  for (rowIdx in seq_along(rows)) {
    r <- rows[[rowIdx]]
    polygonRow <- list()
    if (r$sideMargin) {
      leftSpacer <- comic$sideMargin
      rightSpacer <- comic$sideMargin
    } else {
      leftSpacer <- 0
      rightSpacer <- 0
    }
    h <- panelAreaH * page$layout$weights[rowIdx] / sum(page$layout$weights)
    panelAreaW <- comic$size$w - leftSpacer - rightSpacer - (length(r$weights)-1)*comic$panelSpacing
    x <- leftSpacer

    for (i in seq_along(r$weights)) {
      w <- panelAreaW * r$weights[i] / sum(r$weights)
      polygon <- rect(x, y, w, h)
      polygonRow <- c(polygonRow, list(polygon))
      x <- x + w + comic$panelSpacing
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

    polygonList <- c(polygonList, polygonRow)

    y <- y + h + comic$panelSpacing
  }

  results <-  ConfigOpts::makeOpts(
    c("Polygon", "List"),
    size = comic$size,
    bleed = comic$bleed,
    list = lapply(
      polygonList,
      \(p)  ConfigOpts::makeOpts(
        "Polygon",
        points = p)
    )
  )
  ConfigOpts::writeOpts(results, fileOut)
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


rectPanel <- function(x, y, w, h) {
  matrix(
    c(x, y,
      x, y + h,
      x + w, y + h,
      x + w, y),
    byrow=TRUE,
    ncol = 2)
}

makeBox <- function(x, y, w, h) {
  list(
    x = x,
    y = y,
    w = w,
    h = h)
}

getColumnPanels <- function(layout, box) {
  layout <- ConfigOpts::asOpts(layout, c("Column", "Layout"))
  x <- box$x
  w <- box$w
  panelLists <- lapply(
    seq_along(layout$weights),
    \(i) {
      h <- box$h * layout$weights[i] / sum(layout$weights)
      y <- box$y - h + box$h * cumsum(layout$weights)[i] / sum(layout$weights)
      getLayoutPanels(layout$elements$list[[i]], makeBox(x, y, w, h))
    }
  )
  unlist(panelLists, recursive=FALSE)
}

getRowPanels <- function(layout, box) {
  layout <- ConfigOpts::asOpts(layout, c("Row", "Layout"))
  y <- box$y
  h <- box$h
  panelLists <- lapply(
    seq_along(layout$weights),
    \(i) {
      w <- box$w * layout$weights[i] / sum(layout$weights)
      x <- box$x - w + box$w * cumsum(layout$weights)[i] / sum(layout$weights)
      getLayoutPanels(layout$elements$list[[i]], makeBox(x, y, w, h))
    }
  )
  unlist(panelLists, recursive=FALSE)
}

getLayoutPanels <- function(layout, box) {
  className <- ConfigOpts::getClassAt(layout, 2)
  panels <- switch(
    className,
    "Column" = getColumnPanels(layout, box),
    "Row" = getRowPanels(layout, box),
    "Panel" = list(do.call(rectPanel, box))
  )
  return(panels)
}


coorPanels2Id <- function(panels) {
  coors <- do.call(rbind, panels)
  n <- nrow(coors)
  isSame <- matrix(
    rowSums(abs(coors[rep(1:n, each=n),] - coors[rep(1:n, times=n),])) < sqrt(.Machine$double.eps),
    nrow = n)
  representativeIdx <- apply(isSame, 1, which.max)
  sel <- representativeIdx == 1:n
  newId <- representativeIdx[sel]
  uniqueCoors <- coors[sel, ]
  ids <- sapply(representativeIdx, \(idx) which(newId == idx))
  panelsCoorIds <- matrix(ids, byrow = TRUE, ncol = 4)

  panelCoorsCheck <- apply(panelsCoorIds, 1, \(row) uniqueCoors[row, ], simplify=FALSE)
  for (i in seq_along(panelCoorsCheck)) {
    stopifnot(mean(abs(panelCoorsCheck[[i]] - panels[[i]])) < sqrt(.Machine$double.eps))
  }

  list(
    coors = uniqueCoors,
    panels = panelsCoorIds)
}

drawIdPanelsRects <- function(pan, box) {

  colors <- sample(rainbow(nrow(pan$panels), alpha=0.3))
  for (i in seq_len(nrow(pan$panels))) {
    coors <- pan$coors[pan$panels[i, ], ]
    # TODO: if any side of the quadrilateral has a deco, apply it
    polygon(coors[,1], coors[,2], border="#000000", lwd=2, col=colors[i])
    center <- colMeans(coors)
    text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
  }

  for (i in seq_len(nrow(pan$coors))) {
    drawNode(pan$coors[i,], paste0("C", i))
  }
}

drawNode <- function(pos, txt, r = 0.5, fill="#EEEEEE", draw="#000000", textColor = "#000000") {
  theta <- seq(0, 2*pi, length.out = 100)
  polygon(pos[1]+r*cos(theta), pos[2]+r*sin(theta), border=draw, lwd=2, col=fill)
  text(pos[1], pos[2], txt, adj = c(0.5, 0.5), col = textColor)
}

pan2image <- function(pan, box, geometry, fileOut) {
  cmPerInch <- 2.54
  pxPerInch <- 300 # dpi
  pxPerCm <- pxPerInch/cmPerInch
  png(
    filename = fileOut,
    width = round(geometry$size$w * pxPerCm),
    height = round(geometry$size$h * pxPerCm),
    res = 300)
  par(mar = c(0,0,0,0), xaxs = "i", xaxt="n", yaxs = "i", yaxt="n", bg="#FF0000")
  plot.new()
  brdr <- geometry$sideMargin + geometry$bleed
  plot.window(xlim = c(box$x-brdr, box$x+box$w+brdr), ylim = c(box$y+box$h+brdr, box$y-brdr))
  graphics::rect(
    box$x-geometry$sideMargin, box$y-geometry$sideMargin, box$x+box$w+geometry$sideMargin, box$y+box$h+geometry$sideMargin,
    col = "#FFFFFF", border=NA)
  drawIdPanelsRects(pan)
  dev.off()
}

# ConfigOpts::addPackageToPathDefaults("inst/defaultOpts/")

fileOutJson <- "_pan.json"
fileOutPng <- "_pan.png"
fileIn <- "_page.json"

page <- ConfigOpts::readOpts(fileIn, "Page")
box <- makeBox(0, 0, page$geometry$size$w, page$geometry$size$h)
layout <- page$panelArea
coorPanels <- getLayoutPanels(layout, box)
pan <- coorPanels2Id(coorPanels)
pan2image(pan, box, page$geometry, fileOutPng)

jsonlite::write_json(pan, fileOutJson, auto_unbox = FALSE, digits = NA, pretty = TRUE)


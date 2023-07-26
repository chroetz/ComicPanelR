getPanelSpaceBorderPath <- function(tiling) {
  borders <- lapply(seq_len(nrow(tiling$panels)), \(i) {
    segIds <- tiling$panels$segments[[i]]
    segments <- tiling$segments$segment[segIds,]
    ordering <- getSegmentOrdering(segments)
    segIds <- segIds[ordering$rowIdxs]
    paths <- lapply(
      seq_len(nrow(ordering)),
      \(j) {
        p <- tiling$segments$path[[segIds[j]]]
        if (ordering$reverse[j]) p <- p[rev(seq_len(nrow(p))),]
        return(p)
      })
    res <- tibble::tibble(segId = segIds, path = paths)
    return(res)
  })
  return(borders)
}




makeInner <- function(border, center, panelStyle) {
  clipCondition <-
    border |>
    left_join(panelStyle, by="segId")
  border |>
    left_join(panelStyle, by="segId") |>
    rowwise() |>
    #mutate(inner = list(moveNormal(path, center, margin))) |>
    mutate(movedPath = list(moveNormal(path, center, margin))) |>
    mutate(inner = list(clipPath(movedPath, clipCondition, center, segId))) |>
    select(segId, inner) |>
    ungroup()
}

clipPath <- function(path, conditions, target, excludeSegId) {
  for (i in seq_len(nrow(conditions))) {
    if (conditions$segId[i] == excludeSegId) next
    path <- clipPathOne(path, conditions$path[[i]], target, conditions$margin[[i]])
  }
  return(path)
}

clipPathOne <- function(path, other, target, threshold) {
  dim(target) <- c(1, 2)
  eps <- sqrt(.Machine$double.eps)
  n <- nrow(path)
  m <- nrow(other)
  if (n == 0 || m == 0) return(path)
  dstMat <- matrix(sqrt(rowSums((path[rep(1:n, each=m),]-other[rep(1:m, times=n),])^2)), nrow=m)
  dsts <- apply(dstMat, 2, min)
  sel <- dsts > threshold-eps
  if (sum(sel) <= 1) return(path[sel, , drop=FALSE])
  middleIdx <- which(sel)[round(sum(sel)/2)]
  topN <- which(1:n > middleIdx & !sel)[1] - 1
  if (is.na(topN)) topN <- n
  bottomN <- rev(which(1:n < middleIdx & !sel))[1] + 1
  if (is.na(bottomN)) bottomN <- 1
  return(path[bottomN:topN, , drop=FALSE])
}

moveNormal <- function(path, target, distance) {
  nrms <- getNormalVectors(path)
  sgn <- sign(sum(rowSums((rep(target, each = nrow(path)) - path) * nrms)))
  nrms <- sgn*nrms
  movedPath <- path + nrms * distance
  return(movedPath)
}




tiling2image2 <- function(tiling, box, geometry, fileOut) {
  cmPerInch <- 2.54
  pxPerInch <- 300 # dpi
  pxPerCm <- pxPerInch/cmPerInch
  grDevices::png(
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
    col = "#00FF00", border=NA)
  graphics::rect(
    box$x, box$y, box$x+box$w, box$y+box$h,
    col = "#FFFFFF", border=NA)
  drawTiling2(tiling)
  dev.off()
}

drawTiling2 <- function(tiling) {

  colors <- sample(grDevices::rainbow(nrow(tiling$panels), alpha=0.3))
  for (i in seq_len(nrow(tiling$panels))) {
    path <- do.call(rbind, tiling$panels$inner[[i]]$inner)
    graphics::polygon(path[,1], path[,2], border="#000000", lwd=2, col=colors[i])
    center <- colMeans(path)
    graphics::text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
  }

  for (i in seq_len(nrow(tiling$panels))) {
    path <- do.call(rbind, tiling$panels$border[[i]]$path)
    graphics::polygon(path[,1], path[,2], border="#0000FF", lwd=1)
  }

  for (i in seq_len(nrow(tiling$segments))) {
    path <- tiling$segments$path[[i]]
    center <- colMeans(path)
    drawNode(center, paste0("S", i), fill = "#555555", draw = "#FFFFFF", textColor= "#FFFFFF")
  }
}

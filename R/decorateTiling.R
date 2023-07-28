decorate <- function(tiling, deco) {
  className <- ConfigOpts::getClassAt(deco, 2)
  path <- tiling$segments$path[[deco$borderId]]
  tiling$segments$path[[deco$borderId]] <- switch(
    className,
    "Sine" = decorateSine(path, deco),
    stop(paste0("Unknown Deco ", className))
  )
  return(tiling)
}

makeStraightPath <- function(seg, vertices, n = 2) {
  A <- vertices$coordinates[seg[1],]
  B <- vertices$coordinates[seg[2],]
  v <- B - A
  x <- seq(0, 1, length.out = n)
  path <- rep(A, each=n) + outer(x, v)
  path[1, ] <- A
  path[n, ] <- B
  return(path)
}

getNormalVectors <- function(path, target = NULL, sameSign = TRUE) {
  dff <- diff(path)
  rotated <- cbind(dff[,2], -dff[,1])
  normed <- rotated / sqrt(rowSums(rotated^2))
  normed <- normed[c(1, 1:nrow(normed), nrow(normed)),]
  normed <- (normed[1:nrow(path),]+normed[2:nrow(normed),])/2
  if (length(target) > 0) {
    if (sameSign) {
      sgn <- sign(sum(rowSums((rep(target, each = nrow(path)) - path) * normed)))
      normed <- sgn*normed
    } else {
      sgn <- sign(rowSums((rep(target, each = nrow(path)) - path) * normed))
      normed <- sgn * normed
    }
  }
  return(normed)
}

interpolate <- function(path, n) {
  time <- seq(0, 1, length.out = nrow(path))
  targetTimes <- seq(0, 1, length.out = n)
  do.call(
    cbind,
    lapply(seq_len(ncol(path)), \(j) stats::approx(time, path[,j], targetTimes)$y))
}

decorateSine <- function(path, deco) {
  sinifyPath(path, time=deco$time, amplitude=deco$amplitude, n=deco$n)
}

sinifyPath <- function(path, time, amplitude, n) {
  path <- interpolate(path, n)
  normalVecs <- getNormalVectors(path)
  y <- sin(seq(0, 2*pi*time, length.out = n)) * amplitude
  return(path + normalVecs * y)
}

tiling2image <- function(tiling, box, geometry, fileOut) {
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
    col = "#FFFFFF", border=NA)
  drawTiling(tiling)
  dev.off()
}

getSegmentOrdering <- function(segments) {
  rowIdxs <- 1
  colIdxs <- 1
  current <- segments[1, 2]
  for (i in seq_len(nrow(segments)-1)) {
    candidates <- which(segments == current, arr.ind=TRUE)
    sel <- !(candidates[, "row"] %in% rowIdxs)
    nextRowIdx <- candidates[sel, "row"]
    colIdxs <- c(colIdxs, candidates[sel, "col"])
    rowIdxs <- c(rowIdxs, nextRowIdx)
    current <- setdiff(segments[nextRowIdx, ], current)
  }
  return(tibble(rowIdxs = unname(rowIdxs), reverse = unname(colIdxs == 2)))
}


drawTiling <- function(tiling) {

  colors <- sample(grDevices::rainbow(nrow(tiling$panels), alpha=0.3))
  for (i in seq_len(nrow(tiling$panels))) {
    vertices <- tiling$vertices$coordinates[tiling$panels$vertices[[i]], ]
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
    path <- do.call(rbind, paths)
    graphics::polygon(path[,1], path[,2], border="#000000", lwd=2, col=colors[i])
    center <- colMeans(vertices)
    graphics::text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
  }

  for (i in seq_len(nrow(tiling$segments))) {
    path <- tiling$segments$path[[i]]
    center <- colMeans(path)
    drawNode(center, paste0("S", i), fill = "#444444", draw = "#AAAAAA", textColor= "#FFFFFF", type = "rect")
  }
}


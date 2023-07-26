makeStraightPath <- function(seg, vertices) {
  A <- vertices$coordinates[seg[1],]
  B <- vertices$coordinates[seg[2],]
  v <- B - A
  n <- 1e3
  x <- seq(0, 1, length.out = n)
  path <- rep(A, each=n) + outer(x, v)
  return(path)
}

getNormalVectors <- function(path, target = NULL) {
  dff <- diff(path)
  rotated <- cbind(dff[,2], -dff[,1])
  normed <- rotated / sqrt(rowSums(rotated^2))
  normed <- normed[c(1, 1:nrow(normed), nrow(normed)),]
  normed <- (normed[1:nrow(path),]+normed[2:nrow(normed),])/2
  if (length(target) > 0) {
    normed <- sign(rowSums((rep(target, each = nrow(path)) - path) * normed)) * normed
  }
  return(normed)
}

sinifyPath <- function(path, time, amplitude) {
  normalVecs <- getNormalVectors(path)
  n <- length(path)
  y <- sin(seq(0, 2*pi*time, length.out = n)) * amplitude
  return(path + normalVecs * y)
}

tiling2image <- function(tiling, box, geometry, fileOut) {
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
  return(tibble::tibble(rowIdxs = unname(rowIdxs), reverse = unname(colIdxs == 2)))
}


drawTiling <- function(tiling) {

  colors <- sample(rainbow(nrow(tiling$panels), alpha=0.3))
  for (i in seq_len(nrow(tiling$panels))) {
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
    polygon(path[,1], path[,2], border="#000000", lwd=2, col=colors[i])
    center <- colMeans(path)
    text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
  }

  segNr <- 0
  for (i in seq_len(nrow(tiling$panels))) {
    segIds <- tiling$panels$segments[[i]]
    segments <- tiling$segments$segment[segIds,]
    paths <- tiling$segments$path[segIds]
    for (k in seq_along(paths)) {
      segNr <- segNr + 1
      center <- colMeans(paths[[k]])
      drawNode(center, paste0("S", segNr), fill = "#555555", draw = "#FFFFFF", textColor= "#FFFFFF")
    }
  }
}

tiling$segments <-
  tiling$segments |>
  rowwise() |>
  mutate(
    path = list(makeStraightPath(segment, tiling$vertices)))
tiling$segments$path[[5]] <- sinifyPath(tiling$segments$path[[5]], 10, 1)

tiling2image(tiling, box, page$geometry, "_tiling.png")

saveRDS(tiling, "_tiling.RDS")
#' @export
createDecoratedPan <- function() {
  files <- getFilePairs(
    "store_02_parti-tr", "RDS",
    "opt_03_deco", "json")

  for (i in seq_len(nrow(files))) {
    createDecoratedPanOne(
      fileInStore = files$file1[i],
      fileInOpts = files$file2[i],
      fileOutPng = paste0("preview_03_pan", files$suffix[i], ".png"),
      fileOutRds = paste0("store_03_pan", files$suffix[i], ".RDS"))
  }
}


createDecoratedPanOne <- function(fileInStore, fileInOpts, fileOutPng, fileOutRds) {
  parti <- readRDS(fileInStore)
  decoOpts <- ConfigOpts::readOpts(fileInOpts, c("Deco", "List"))
  pan <- parti2pan(parti)
  for (deco in decoOpts$list) {
    pan$borders <- decorate(pan$borders, deco)
  }
  renderPan(pan, fileOutPng)
  saveRDS(pan, fileOutRds)
}


decorate <- function(borders, deco) {
  className <- ConfigOpts::getClassAt(deco, 2)
  for (segId in deco$segmentIds) {
    path <- borders$coorSegments[[segId]]
    borders$coorSegments[[segId]] <- switch(
      className,
      "Sine" = decorateSine(path, deco),
      stop(paste0("Unknown Deco ", className))
    )
  }
  return(borders)
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

interpolate <- function(path, nPoints) {
  dsts <- sqrt(rowSums((path[-1,,drop=FALSE] - path[-nrow(path),,drop=FALSE])^2))
  sel <- dsts > sqrt(.Machine$double.eps)
  time <- c(0, cumsum(dsts[sel])) / sum(dsts[sel])
  path <- path[c(TRUE, sel), ]
  targetTimes <- seq(0, 1, length.out = nPoints)
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


renderPan <- function(pan, fileOut, dpi = 300) {
  geo <- pan$geometry
  finalBox <- getFinalBoxInCm(geo)
  plotPageWithBleed(geo, dpi, fileOut)
  drawPan(pan)
  graphics::text(
    finalBox$midX,
    finalBox$y+geo$margin$top/2,
    pan$name,
    adj = c(0.5, 0.5),
    cex=2, col="black")
  grDevices::dev.off()
}


drawPan <- function(pan) {

  n <- length(pan$idPanels)

  colors <- getPanelColors(n)
  for (i in seq_len(n)) {
    vertexIds <- pan$idPanels[[i]]
    idSegments <- cbind(vertexIds, c(vertexIds[-1], vertexIds[1]))
    paths <- apply(idSegments, 1, getBorder, pan = pan, simplify=FALSE)
    path <- do.call(rbind, paths)
    vertices <- pan$vertices[vertexIds, ]
    graphics::polygon(path[,1], path[,2], border="#000000", lwd=2, col=colors[i])
    center <- colMeans(vertices)
    graphics::text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
  }

  for (i in seq_len(nrow(pan$borders))) {
    path <- pan$borders$coorSegments[[i]]
    center <- colMeans(path)
    drawNode(center, paste0("S", i), fill = "#444444", draw = "#AAAAAA", textColor= "#FFFFFF", type = "rect")
  }
}

getBorder <- function(idSeg, pan) {
  idSegSort <- sort(idSeg)
  idSegments <- pan$idSegments[pan$borders$segmentId, ]
  idx <- which(idSegments[, 1] == idSegSort[1] & idSegments[, 2] == idSegSort[2])
  b <- pan$borders$coorSegments[[idx]]
  if (!all(idSeg == idSegSort)) b <- b[rev(seq_len(nrow(b))),]
  return(b)
}

getSegmentId <- function(idSeg, pan) {
  idSegSort <- sort(idSeg)
  idSegments <- pan$idSegments[pan$borders$segmentId, ]
  idx <- which(idSegments[, 1] == idSegSort[1] & idSegments[, 2] == idSegSort[2])
  return(idx)
}

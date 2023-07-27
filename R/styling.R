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
    res <- tibble(segId = segIds, path = paths)
    return(res)
  })
  return(borders)
}



makeInner <- function(border, panelStyle) {
  eps <- sqrt(.Machine$double.eps)
  p <-
    border |>
    left_join(panelStyle, by="segId")

  path <- do.call(rbind, p$path)
  polyTmp <- geos_make_polygon(path[,1], path[,2], ring_id = 1)
  polyBorder <- geos_make_valid(polyTmp)
  polyInner <- polyBorder

  for (k in seq_len(nrow(p))) {
    line <- geos_make_linestring(p$path[[k]][,1], p$path[[k]][,2])
    segBuff <- geos_buffer(line, distance = p$margin[[k]])
    polyInner <- geos_difference(polyInner, segBuff)
  }

  ng <- geos_num_geometries(polyInner)
  if (ng > 1) {
    areas <- sapply(1:ng, \(i) geos_area(geos_geometry_n(polyInner, i)))
    polyInner <- geos_geometry_n(polyInner, which.max(areas))
  }

  coords <- wk::wk_coords(geos_unique_points(polyInner))
  points <- geos_make_point(coords$x, coords$y)
  dsts <- sapply(p$path, \(seg) {
    segment <- geos_make_linestring(seg[,1], seg[,2])
    geos_distance(segment, points)
  })
  excessDists <- dsts - rep(p$margin, each=nrow(dsts))
  idxs <- apply(excessDists < eps, 2, which, simplify=FALSE)
  idxs <- lapply(idxs, \(idx) {
    if (length(idx) == 0) return(idx)
    k <- which(diff(idx) != 1)
    if (length(k) > 1) browser()
    stopifnot(length(k) < 2)
    if (length(k) == 0) return(idx)
    idx[c((k+1):length(idx), 1:k)]
  })
  for (i in seq_len(length(idxs)-1)) {
    if (length(idxs[[i]]) + length(idxs[[i+1]]) >= 3) {
      if (last(idxs[[i]]) != first(idxs[[i+1]])) {
        idxs <- lapply(idxs, rev)
      }
      break
    }
  }

  inner <- lapply(idxs, \(idx) coords[idx, c("x", "y")] |> as.matrix())

  p$inner <- inner

  return(p)
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
    vertices <- tiling$vertices$coordinates[tiling$panels$vertices[[i]], ]
    center <- colMeans(vertices)
    graphics::text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
  }

  for (i in seq_len(nrow(tiling$panels))) {
    path <- do.call(rbind, tiling$panels$border[[i]]$path)
    graphics::polygon(path[,1], path[,2], border="#0000FF", lwd=1)
  }

  for (i in seq_len(nrow(tiling$segments))) {
    path <- tiling$segments$path[[i]]
    center <- colMeans(path)
    drawNode(center, paste0("S", i), fill = "#444444", draw = "#AAAAAA", textColor= "#FFFFFF", type = "rect")
  }
}

endDirection <- function(path) {
  n <- nrow(path)
  if (n < 2) return(c(0, 0))
  v <- path[n,]-path[n-1,]
  v / sqrt(sum(v^2))
}
startDirection <- function(path) {
  n <- nrow(path)
  if (n < 2) return(c(0, 0))
  v <- path[1,]-path[2,]
  v / sqrt(sum(v^2))
}

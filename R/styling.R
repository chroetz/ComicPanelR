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




makeInner <- function(border, panelStyle) {
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

  coords <- wk::wk_coords(geos_unique_points(polyInner))
  points <- geos_make_point(coords$x, coords$y)
  dsts <- sapply(p$path, \(seg) {
    segment <- geos_make_linestring(seg[,1], seg[,2])
    geos_distance(segment, points)
  })
  excessDists <- dsts - rep(p$margin, each=nrow(dsts))
  # TODO: do not allow to collect non-conscutive points, except at end
  inner <- apply(excessDists < eps, 2, \(sel) coords[sel,c("x", "y")] |> as.matrix(), simplify=FALSE)

  # order the paths
  for (i in seq_along(inner)) {
    i1 <- i
    i2 <- if (i+1 > length(inner)) 1 else i+1
    if (nrow(inner[[i1]]) == 0 || nrow(inner[[i2]]) == 0) next
    if (any(tail(inner[[i1]], 1) != head(inner[[i2]], 1))) {
      inner[[i1]] <- inner[[i1]][rev(seq_len(nrow(inner[[i1]]))),]
    }
    if (any(tail(inner[[i1]], 1) != head(inner[[i2]], 1))) {
      browser() # TODO bug...
    }
    force(1)
  }

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

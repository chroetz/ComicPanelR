#' @export
createPanels <- function(fileInPan, fileInMargin, fileOutPng, fileOutRds) {
  pan <- readRDS(fileInPan)
  marginOpts <- ConfigOpts::readOpts(fileInMargin)
  panels <- makePanels(pan, marginOpts)
  renderPanels(panels, pan, fileOutPng)
  saveRDS(panels, fileOutRds)
}

makePanels <- function(pan, marginOpts) {

  panels <- pan2panels(pan)
  margins <- getMargins(pan, marginOpts)

  panels <-
    panels |>
    left_join(margins, by = c("panelId", "segmentId")) |>
    nest(data = c(segmentId, side, margin), .by=panelId) |>
    rowwise() |>
    mutate(inner = list(makeInner(data$side, data$margin))) |>
    ungroup() |>
    unnest(c(data, inner)) |>
    rowwise() |>
    filter(nrow(inner) > 0) |>
    ungroup() |>
    left_join(pan$idGraph, by = join_by(panelId, segmentId)) |>
    select(panelId, sideId, segmentId, side, inner)

  return(panels)
}

pan2panels <- function(pan) {
  n <- length(pan$idPanels)
  panels <- lapply(seq_len(n), \(i) {
    vertexIds <- pan$idPanels[[i]]
    idSegments <- cbind(vertexIds, c(vertexIds[-1], vertexIds[1]))
    segmentIds <- apply(idSegments, 1, getSegmentId, pan = pan)
    paths <- apply(idSegments, 1, getBorder, pan = pan, simplify=FALSE)
    tibble(panelId = i, segmentId = segmentIds, side = paths)
  })
  return(bind_rows(panels))
}



getMargins <- function(pan, opts) {
  opts <- ConfigOpts::asOpts(opts, "Margin")
  panelMargin <-
    tibble(id = names(opts$panels), panelMargin = unlist(opts$panels)) |>
    mutate(panelId = as.integer(str_extract(id, "[0-9]+", ))) |>
    select(panelId, panelMargin)
  sideMargin <-
    tibble(id = names(opts$sides), sideMargin = unlist(opts$sides)) |>
    mutate(
      panelId = as.integer(str_extract(id, "(?<=[pP])[0-9]+")),
      segmentId = as.integer(str_extract(id, "(?<=[sS])[0-9]+"))) |>
    select(panelId, segmentId, sideMargin)
  margins <-
    pan$idGraph |>
    select(panelId, segmentId) |>
    mutate(margin = opts$default) |>
    left_join(panelMargin, by="panelId") |>
    mutate(margin = ifelse(is.na(panelMargin), margin, panelMargin)) |>
    left_join(sideMargin, by=c("panelId", "segmentId")) |>
    mutate(margin = ifelse(is.na(sideMargin), margin, sideMargin)) |>
    select(panelId, segmentId, margin)
  return(margins)
}



makeInner <- function(sides, margin) {
  eps <- sqrt(.Machine$double.eps)
  n <- length(sides)
  path <- do.call(rbind, sides)
  polyTmp <- geos_make_polygon(path[,1], path[,2], ring_id = 1)
  polyBorder <- geos_make_valid(polyTmp)
  polyInner <- polyBorder

  for (k in seq_len(n)) {
    line <- geos_make_linestring(sides[[k]][,1], sides[[k]][,2])
    segBuff <- geos_buffer(line, distance = margin[[k]])
    polyInner <- geos_difference(polyInner, segBuff)
  }

  ng <- geos_num_geometries(polyInner)
  if (ng > 1) {
    areas <- sapply(1:ng, \(i) geos_area(geos_geometry_n(polyInner, i)))
    polyInner <- geos_geometry_n(polyInner, which.max(areas))
  }

  coords <- wk::wk_coords(geos_unique_points(polyInner))
  points <- geos_make_point(coords$x, coords$y)
  dsts <- sapply(sides, \(seg) {
    segment <- geos_make_linestring(seg[,1], seg[,2])
    geos_distance(segment, points)
  })
  excessDists <- dsts - rep(margin, each=nrow(dsts))
  idxs <- apply(excessDists < eps, 2, which, simplify=FALSE)
  idxs <- lapply(idxs, \(idx) {
    if (length(idx) == 0) return(idx)
    k <- which(diff(idx) != 1)
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

  return(inner)
}




renderPanels <- function(panels, pan, fileOut, dpi=300, drawBorder=TRUE, drawSegText=TRUE, drawPanelText=TRUE) {
  geometry <- pan$geometry
  box <- makeBox(0, 0, geometry$size$w, geometry$size$h)
  cmPerInch <- 2.54
  pxPerInch <- dpi
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
  drawPanels(panels, pan, drawBorder, drawSegText, drawPanelText)
  text(box$x + box$w/2, box$y-geometry$sideMargin/2, pan$name, adj = c(0.5, 0.5), cex=2, col="black")
  dev.off()
}

drawPanels <- function(panels, pan, drawBorder, drawSegText, drawPanelText) {

  panels <-
    panels |>
    arrange(panelId, sideId)
  p <- nest(panels, data = c(segmentId, side, inner, sideId))
  colors <- getPanelColors(nrow(p))
  for (i in seq_len(nrow(p))) {
    d <- p$data[[i]]
    inner <- do.call(rbind, d$inner)
    graphics::polygon(inner[,1], inner[,2], border="#000000", lwd=2, col=colors[i])
    if (drawBorder) {
      border <- do.call(rbind, d$side)
      graphics::polygon(border[,1], border[,2], border="#0000FF", lwd=1)
    }
    if (drawPanelText) {
      vertices <- pan$vertices[pan$idPanels[[i]], ]
      center <- colMeans(vertices)
      graphics::text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
    }
  }

  if (drawSegText) {
    for (i in seq_len(nrow(pan$borders))) {
      path <- pan$borders$coorSegments[[i]]
      center <- colMeans(path)
      drawNode(center, paste0("S", i), fill = "#444444", draw = "#AAAAAA", textColor= "#FFFFFF", type = "rect")
    }
  }
}

#' @export
createPanels <- function() {

  files <- getFilePairs(
    "store_03_pan", "RDS",
    "opt_04_margin", "json")

  for (i in seq_len(nrow(files))) {
    createPanelsOne(
      fileInStore = files$file1[i],
      fileInOpts = files$file2[i],
      fileOutPng = paste0("preview_04_panels", files$suffix[i], ".png"),
      fileOutRds = paste0("store_04_panels", files$suffix[i], ".RDS"))
  }
}

createPanelsOne <- function(fileInStore, fileInOpts, fileOutPng, fileOutRds) {
  pan <- readRDS(fileInStore)
  marginOpts <- ConfigOpts::readOpts(fileInOpts, "Margins")
  panels <- makePanels(pan, marginOpts)
  renderPanels(panels, pan, fileOutPng)
  saveRDS(list(panels = panels, pan = pan), fileOutRds)
}

makePanels <- function(pan, marginOpts) {
  panels <- pan2panels(pan)
  margins <- getMargins(pan, marginOpts)

  if (nrow(panels) == 0) return(tibble(
    panelId = integer(),
    sideId = integer(),
    segmentId = integer(),
    side = list(),
    inner = list()))
  panels <-
    panels |>
    left_join(margins, by = c("panelId", "segmentId")) |>
    nest(data = c(segmentId, side, margin), .by=panelId) |>
    rowwise() |>
    mutate(inner = list(makeInner(data$side, data$margin, pan$geometry))) |>
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
  if (n == 0) return(tibble(panelId = integer(), segmentId = integer(), side = list()))
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
  opts <- ConfigOpts::asOpts(opts, "Margins")
  margins <-
    pan$idGraph |>
    select(panelId, segmentId) |>
    rowwise() |>
    mutate(margin = list(ConfigOpts::asOpts(opts$default, "Margin"))) |>
    ungroup()
  if (length(opts$panels) > 0) {
    panelMargin <-
      tibble(
        id = names(opts$panels),
        panelMargin = lapply(opts$panels, ConfigOpts::asOpts, optsClass = "Margin")) |>
      mutate(panelId = as.integer(str_extract(id, "[0-9]+", ))) |>
      select(panelId, panelMargin)
    margins <-
      margins |>
      left_join(panelMargin, by="panelId") |>
      rowwise() |>
      mutate(margin = list(if (is.null(panelMargin)) margin else panelMargin)) |>
      ungroup()
  }
  if (length(opts$sides) > 0) {
    sideMargin <-
      tibble(
        id = names(opts$sides),
        sideMargin = lapply(opts$sides, ConfigOpts::asOpts, optsClass = "Margin")) |>
      mutate(
        panelId = as.integer(str_extract(id, "(?<=[pP])[0-9]+")),
        segmentId = as.integer(str_extract(id, "(?<=[sS])[0-9]+"))) |>
      select(panelId, segmentId, sideMargin)
    margins <-
      margins |>
      left_join(sideMargin, by=c("panelId", "segmentId")) |>
      rowwise() |>
      mutate(margin = list(if (is.null(sideMargin)) margin else sideMargin)) |>
      ungroup()
  }
  margins <-
    margins |>
    select(panelId, segmentId, margin)
  return(margins)
}



makeInner <- function(sides, margin, geometry) {
  eps <- sqrt(.Machine$double.eps)
  n <- length(sides)
  path <- do.call(rbind, sides)
  polyTmp <- geos_make_polygon(path[,1], path[,2], ring_id = 1)
  polyBorder <- geos_make_valid(polyTmp)
  polyInner <- polyBorder

  for (k in seq_len(n)) {
    s <- sides[[k]]
    if (margin[[k]]$extend) {
      s <- extendSegment(s, geometry)
    }
    line <- geos_make_linestring(s[,1], s[,2])
    segBuff <- geos_buffer(line, distance = margin[[k]]$size)
    polyInner <- geos_difference(polyInner, segBuff)
  }

  ng <- geos_num_geometries(polyInner)
  if (ng > 1) {
    areas <- sapply(1:ng, \(i) geos_area(geos_geometry_n(polyInner, i)))
    polyInner <- geos_geometry_n(polyInner, which.max(areas))
  }

  coords <- wk::wk_coords(geos_unique_points(polyInner))
  points <- geos_make_point(coords$x, coords$y)
  dsts <- sapply(seq_len(n), \(k) {
    s <- sides[[k]]
    if (margin[[k]]$extend) {
      s <- extendSegment(s, geometry)
    }
    segment <- geos_make_linestring(s[,1], s[,2])
    geos_distance(segment, points)
  })

  marginSizes <- sapply(margin, \(m) m$size)
  excessDists <- dsts - rep(marginSizes, each=nrow(dsts))
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
  geo <- pan$geometry
  finalBox <- getFinalBoxInCm(geo)
  plotPageWithBleed(geo, dpi, fileOut)
  drawPanels(panels, pan, drawBorder, drawSegText, drawPanelText)
  graphics::text(
    finalBox$midX,
    finalBox$y+geo$sideMargin/2,
    pan$name,
    adj = c(0.5, 0.5),
    cex=2, col="black")
  grDevices::dev.off()
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

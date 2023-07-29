#' @export
createStecil <- function(
    fileInPan,
    fileInPanels,
    fileInRender,
    fileOutPng
) {
  pan <- readRDS(fileInPan)
  panels <- readRDS(fileInPanels)
  renderOpts <- ConfigOpts::readOpts(fileInRender, "Render")

  frameStyles <- getFrameStyles(renderOpts, pan)

  stencil <- panel2stencil(panels, pan, frameStyles, dpi = renderOpts$dpi)
  magick::image_write(stencil, path = fileOutPng, format = "png")
}

# Convert Coordinates File to Image
panel2stencil <- function(panels, pan, frameStyles, dpi) {
  geometry <- pan$geometry
  box <- makeBox(0, 0, geometry$size$w, geometry$size$h)
  cmPerInch <- 2.54
  pxPerInch <- dpi
  pxPerCm <- pxPerInch/cmPerInch

  img <- magick::image_graph(
    width = round(geometry$size$w * pxPerCm),
    height = round(geometry$size$h * pxPerCm),
    res = pxPerInch)

  par(mar = c(0,0,0,0), xaxs = "i", xaxt="n", yaxs = "i", yaxt="n", bg="#FF0000")
  plot.new()
  brdr <- geometry$sideMargin + geometry$bleed
  plot.window(xlim = c(box$x-brdr, box$x+box$w+brdr), ylim = c(box$y+box$h+brdr, box$y-brdr))

  panels <-
    panels |>
    left_join(frameStyles, join_by(panelId, segmentId)) |>
    arrange(panelId, sideId)
  p <- nest(panels, data = c(segmentId, side, inner, sideId, frameOpts))
  for (i in seq_len(nrow(p))) {
    d <- p$data[[i]]
    inner <- do.call(rbind, d$inner)
    graphics::polygon(inner[,1], inner[,2], col = "#0000FF", border = NA)
    for (j in seq_len(nrow(d))) {
      opts <- ConfigOpts::asOpts(d$frameOpts[[j]], "Frame")
      if (!opts$draw) next
      lines(d$inner[[j]][,1], d$inner[[j]][,2], col = "#00FF00", lwd = opts$width, lty = opts$linetype)
    }
  }
  channels <- magick::image_separate(img)
  rgb <- magick::image_combine(c(channels[1], channels[1], channels[1]))
  alpha <- magick::image_negate(channels[3])
  res <- magick::image_composite(rgb, alpha, operator='copy-opacity')
  return(res)
}


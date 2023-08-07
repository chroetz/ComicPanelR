plotPageWithBleed <- function(geo, dpi, fileOut) {
  grDevices::png(
    filename = fileOut,
    width = getDataWidthInPx(geo, dpi),
    height = getDataHeightInPx(geo, dpi),
    res = dpi)
  graphics::par(
    mar = c(0,0,0,0),
    xaxs = "i", xaxt="n", yaxs = "i", yaxt="n",
    bg="#FF0000")
  graphics::plot.new()
  dataBox <- getDataBoxInCm(geo)
  finalBox <- getFinalBoxInCm(geo)
  graphics::plot.window(xlim = dataBox$xlim, ylim = dataBox$ylim)
  drawBox(finalBox, col = "#FFFFFF", border=NA)
}

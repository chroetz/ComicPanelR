#' Convert Coordinates File to Image
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


#' Convert Coordinates One Stencil Image per Panel
#'
#' @param fileName The base file name (preferably without file extension).
#'   ".coor.json" will be added to obtain the input file. The json file should
#'   contain an object of class ` ["Polygon", "List", "Opts"]`.
#' @importFrom grDevices dev.off
#' @importFrom graphics par plot.new plot.window polygon
#' @export
coor2panelStencils <- function(fileName) {

  fileBaseName <- sub("\\.coor\\.json$", "", fileName, ignore.case=TRUE)
  fileIn <- paste0(fileBaseName, ".coor.json")

  coor <- ConfigOpts::readOpts(fileIn, c("Polygon", "List"))

  cmPerInch <- 2.54
  pxPerInch <- 300 # dpi
  pxPerCm <- pxPerInch/cmPerInch

  for (i in seq_along(coor$list)) {

    img <- magick::image_graph(
      width = round(coor$size$w * pxPerCm),
      height = round(coor$size$h * pxPerCm),
      res = pxPerInch)
    par(mar = c(0,0,0,0), xaxs = "i", xaxt="n", yaxs = "i", yaxt="n", bg="#FF0000")
    plot.new()
    plot.window(xlim = c(0, coor$size$w), ylim = c(coor$size$h, 0))

    polyg <- coor$list[[i]]
    p <- polyg$points
    if (polyg$frame) {
      polygon(p[,1], p[,2], lwd=4, col = "#0000FF", border = "#000000")
    } else {
      polygon(p[,1], p[,2], col = "#0000FF", border = NA)
    }

    dev.off()

    channels <- magick::image_separate(img)
    rgb <- magick::image_combine(c(channels[1], channels[1], channels[1]))
    alpha <- magick::image_negate(channels[3])
    res <- magick::image_composite(rgb, alpha, operator='copy-opacity')

    magick::image_write(
      res,
      path = sprintf("%s_panel%02d.png", fileBaseName, i),
      format = "png")

  }

}

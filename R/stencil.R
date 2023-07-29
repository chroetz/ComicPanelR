#' @export
createStencils <- function(
    fileInPanelsAndPan = "store_05_panels-effect.RDS",
    fileInRender = "opt_06_render.json"
) {
  panelsAndPan <- readRDS(fileInPanelsAndPan)
  pan <- panelsAndPan$pan
  panels <- panelsAndPan$panels
  renderOpts <- ConfigOpts::readOpts(fileInRender, "Render")

  frameStyles <- getFrameStyles(renderOpts, pan)
  panel2stencil(panels, pan, frameStyles, dpi = renderOpts$dpi)
  gc()

  return(invisible())
}

# Convert Coordinates File to Image
panel2stencil <- function(panels, pan, frameStyles, dpi) {

  panelsAndFrames <-
    panels |>
    left_join(frameStyles, join_by(panelId, segmentId)) |>
    arrange(panelId, sideId)
  p <- nest(panelsAndFrames, data = c(segmentId, side, inner, sideId, frameOpts))
  n <- nrow(p)

  for (i in seq_len(n)) {
    filePrefix <- sprintf("%s_%03d_", pan$name, i)
    cat(filePrefix, "...", sep="")
    createSinglePanelStencils(p$data[[i]], pan, dpi, filePrefix)
    gc()
  }

  filePrefix <- sprintf("%s_", pan$name)
  cat(filePrefix, "...", sep="")
  createPageStencils(p, pan, dpi, filePrefix)
  gc()

  createBlank("#FFFFFFFF", pan, dpi, sprintf("%s_gutter_image.png", pan$name))
  createBlank("#000000FF", pan, dpi, sprintf("%s_frame_image.png", pan$name))
  createBlank("#00000000", pan, dpi, sprintf("%s_abovegutter_image.png", pan$name))
  createBlank("#00000000", pan, dpi, sprintf("%s_belowgutter_image.png", pan$name))
  colors <- getPanelColors(n, alpha=1)
  for (i in seq_len(n)) {
    createBlank(colors[i], pan, dpi, sprintf("%s_%03d_image.png", pan$name, i))
  }

  return(invisible())
}

createPageStencils <- function(p, pan, dpi, filePrefix) {

  img <- setupDevice(pan, dpi)
  grDevices::dev.off()
  channels <- image_separate(img)
  stencilEmpty <- channels[1]
  rm(img);rm(channels);gc()

  img <- setupDevice(pan, dpi)
  for (i in seq_len(nrow(p))) {
    d <- p$data[[i]]
    path <- do.call(rbind, d$inner)
    graphics::polygon(path[,1], path[,2], col = "#FFFFFF", border = NA)
  }
  grDevices::dev.off()
  channels <- image_separate(img)
  stencilPanel <- channels[1]
  rm(img);rm(channels);gc()

  img <- setupDevice(pan, dpi)
  for (i in seq_len(nrow(p))) {
    d <- p$data[[i]]
    for (j in seq_len(nrow(d))) {
      opts <- ConfigOpts::asOpts(d$frameOpts[[j]], "Frame")
      if (!opts$draw) next
      graphics::lines(d$inner[[j]][,1], d$inner[[j]][,2], col = "#FFFFFF", lwd = opts$width, lty = opts$linetype)
    }
  }
  grDevices::dev.off()
  channels <- image_separate(img)
  stencilFrame <- channels[1]
  rm(img);rm(channels);gc()

  # stencilUnframeedPanel <-
  #   image_composite(
  #     stencilPanel,
  #     image_negate(stencilFrame),
  #     operator = "Darken")
  #
  # stencilNegative <-
  #   image_composite(
  #     image_composite(
  #       image_negate(stencilEmpty),
  #       image_negate(stencilFrame),
  #       operator = "Darken"),
  #     image_negate(stencilUnframeedPanel),
  #     operator='copy-opacity'
  #   )
  # rm(stencilUnframeedPanel);gc()
  #
  # image_write(
  #   stencilNegative,
  #   path = sprintf("%sall_negative.png", filePrefix),
  #   format = "png")
  # rm(stencilNegative);gc()
  #
  #
  # stencilPositive <-
  #   image_composite(
  #     image_negate(stencilEmpty),
  #     stencilPanel,
  #     operator='copy-opacity')
  #
  # image_write(
  #   stencilPositive,
  #   path = sprintf("%sall_positive.png", filePrefix),
  #   format = "png")
  # rm(stencilPositive);gc()


  stencilGutterPos <-
    image_composite(
      image_negate(stencilEmpty),
      image_negate(stencilPanel),
      operator='copy-opacity')

  image_write(
    stencilGutterPos,
    path = sprintf("%sgutter_positive.png", filePrefix),
    format = "png")
  rm(stencilGutterPos);gc()

  stencilFrameedPanel <-
    image_composite(
      stencilPanel,
      stencilFrame,
      operator = "Lighten")

  stencilGutterNeg <-
    image_composite(
      image_composite(
        image_negate(stencilEmpty),
        image_negate(stencilFrame),
        operator = "Darken"),
      stencilFrameedPanel,
      operator='copy-opacity'
    )
  rm(stencilFrameedPanel);gc()

  image_write(
    stencilGutterNeg,
    path = sprintf("%sgutter_negative.png", filePrefix),
    format = "png")
  rm(stencilGutterNeg);gc()

  stencilFrameNeg <-
    image_composite(
      image_negate(stencilEmpty),
      image_negate(stencilFrame),
      operator='copy-opacity')

  image_write(
    stencilFrameNeg,
    path = sprintf("%sframe_negative.png", filePrefix),
    format = "png")
  rm(stencilFrameNeg);gc()

  stencilFramePos <-
    image_composite(
      image_negate(stencilEmpty),
      stencilFrame,
      operator='copy-opacity')

  image_write(
    stencilFramePos,
    path = sprintf("%sframe_positive.png", filePrefix),
    format = "png")
  rm(stencilFramePos);gc()

  rm(list = ls());gc()
  return(invisible())
}

createSinglePanelStencils <- function(d, pan, dpi, filePrefix) {

  imgEmpty <- setupDevice(pan, dpi)
  grDevices::dev.off()
  channels <- image_separate(imgEmpty)
  stencilEmpty <- channels[1]

  img <- setupDevice(pan, dpi)
  path <- do.call(rbind, d$inner)
  graphics::polygon(path[,1], path[,2], col = "#FFFFFF", border = NA)
  grDevices::dev.off()
  channels <- image_separate(img)
  stencilPanel <- channels[1]
  rm(img);rm(channels);gc()

  img <- setupDevice(pan, dpi)
  for (j in seq_len(nrow(d))) {
    opts <- ConfigOpts::asOpts(d$frameOpts[[j]], "Frame")
    if (!opts$draw) next
    graphics::lines(d$inner[[j]][,1], d$inner[[j]][,2], col = "#FFFFFF", lwd = opts$width, lty = opts$linetype)
  }
  grDevices::dev.off()
  channels <- image_separate(img)
  stencilFrame <- channels[1]
  rm(img);rm(channels);gc()

  stencilUnframeedPanel <-
    image_composite(
      stencilPanel,
      image_negate(stencilFrame),
      operator = "Darken")

  stencilNegative <-
    image_composite(
      image_composite(
        image_negate(stencilEmpty),
        image_negate(stencilFrame),
        operator = "Darken"),
      image_negate(stencilUnframeedPanel),
      operator='copy-opacity'
    )
  rm(stencilUnframeedPanel);gc()

  image_write(
    stencilNegative,
    path = sprintf("%snegative.png", filePrefix),
    format = "png")
  rm(stencilNegative);gc()

  stencilPositive <-
    image_composite(
      image_negate(stencilEmpty),
      stencilPanel,
      operator='copy-opacity')

  image_write(
    stencilPositive,
    path = sprintf("%spositive.png", filePrefix),
    format = "png")
  rm(stencilPositive);gc()

  rm(list = ls());gc()
  return(invisible())
}

setupDevice <- function(pan, dpi) {
  geometry <- pan$geometry
  box <- makeBox(0, 0, geometry$size$w, geometry$size$h)
  cmPerInch <- 2.54
  pxPerInch <- dpi
  pxPerCm <- pxPerInch/cmPerInch

  img <- image_graph(
    width = round(geometry$size$w * pxPerCm),
    height = round(geometry$size$h * pxPerCm),
    res = pxPerInch,
    bg = "#000000")

  graphics::par(mar = c(0,0,0,0), xaxs = "i", xaxt="n", yaxs = "i", yaxt="n", bg="#000000")
  graphics::plot.new()
  brdr <- geometry$sideMargin + geometry$bleed
  graphics::plot.window(xlim = c(box$x-brdr, box$x+box$w+brdr), ylim = c(box$y+box$h+brdr, box$y-brdr))

  return(img)
}

createBlank <- function(color, pan, dpi, fileName, overwrite=FALSE) {
  if (file.exists(fileName)) {
    if (!overwrite) {
      cat(" - not overwriting", fileName, "- ")
      return()
    }
  }

  geometry <- pan$geometry
  cmPerInch <- 2.54
  pxPerInch <- dpi
  pxPerCm <- pxPerInch/cmPerInch

  grDevices::png(
    fileName,
    width = round(geometry$size$w * pxPerCm),
    height = round(geometry$size$h * pxPerCm),
    bg = color,
    res = dpi)

  graphics::plot.new()

  grDevices::dev.off()
}

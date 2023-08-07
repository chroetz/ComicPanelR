#' @export
createStencils <- function() {

  files <- getFilePairs(
    "store_05_panels-effect", "RDS",
    "opt_06_render", "json")

  for (i in seq_len(nrow(files))) {
    createStencilsOne(
      fileInStore = files$file1[i],
      fileInOpts = files$file2[i])
  }

  return(invisible())
}

createStencilsOne <- function(fileInStore, fileInOpts) {
  panelsAndPan <- readRDS(fileInStore)
  pan <- panelsAndPan$pan
  panels <- panelsAndPan$panels
  renderOpts <- ConfigOpts::readOpts(fileInOpts, "Render")

  frameStyles <- getFrameStyles(renderOpts, pan)
  panel2stencil(panels, pan, frameStyles, dpi = renderOpts$dpi)
  gc()
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

  createBlank("#FFFFFFFF", pan, dpi, sprintf("%s_gutter_image.tiff", pan$name))
  createBlank("#000000FF", pan, dpi, sprintf("%s_frame_image.tiff", pan$name))
  createBlank("#00000000", pan, dpi, sprintf("%s_abovegutter_image.tiff", pan$name))
  createBlank("#00000000", pan, dpi, sprintf("%s_belowgutter_image.tiff", pan$name))
  colors <- getPanelColors(n, alpha=1)
  for (i in seq_len(n)) {
    createBlank(colors[i], pan, dpi, sprintf("%s_%03d_image.tiff", pan$name, i))
  }

  createMergeInfo(panels, pan, dpi)

  return(invisible())
}

createMergeInfo <- function(panels, pan, dpi) {
  geometry <- pan$geometry
  cmPerInch <- 2.54
  pxPerInch <- dpi
  pxPerCm <- pxPerInch/cmPerInch
  n <- panels$panelId |> unique() |> length()

  info <- list(
    name = pan$name,
    dpi = dpi,
    width = round(geometry$size$w * pxPerCm),
    height = round(geometry$size$h * pxPerCm),
    panels = lapply(
      1:n,
      \(i) {
        list(
          image = sprintf("%s_%03d_image.tiff", pan$name, i),
          positive = sprintf("%s_%03d_positive.tiff", pan$name, i),
          negative = sprintf("%s_%03d_negative.tiff", pan$name, i)
        )
      }),
    aboveGutter = sprintf("%s_abovegutter_image.tiff", pan$name),
    belowGutter = sprintf("%s_belowgutter_image.tiff", pan$name),
    gutter = list(
      image = sprintf("%s_gutter_image.tiff", pan$name),
      positive = sprintf("%s_gutter_positive.tiff", pan$name),
      negative = sprintf("%s_gutter_negative.tiff", pan$name)),
    frame = list(
      image = sprintf("%s_frame_image.tiff", pan$name),
      positive = sprintf("%s_frame_positive.tiff", pan$name),
      negative = sprintf("%s_frame_negative.tiff", pan$name)),
    out = sprintf("%s.tiff", pan$name)
  )

  jsonlite::write_json(
    info,
    "mergeinfo.json",
    auto_unbox = TRUE,
    pretty = TRUE)

  return(invisible())
}

createPageStencils <- function(p, pan, dpi, filePrefix) {

  img <- setupDevice(pan, dpi)
  grDevices::dev.off()
  channels <- image_separate(img) # TODO: this seems unnecessary
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

  writeMagickImage(
    stencilNegative,
    sprintf("%sall_negative.tiff", filePrefix))
  rm(stencilNegative);gc()


  stencilPositive <-
    image_composite(
      image_negate(stencilEmpty),
      stencilPanel,
      operator='copy-opacity')

  writeMagickImage(
    stencilPositive,
    sprintf("%sall_positive.tiff", filePrefix))
  rm(stencilPositive);gc()


  stencilGutterPos <-
    image_composite(
      image_negate(stencilEmpty),
      image_negate(stencilPanel),
      operator='copy-opacity')

  writeMagickImage(
    stencilGutterPos,
    sprintf("%sgutter_positive.tiff", filePrefix))
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

  writeMagickImage(
    stencilGutterNeg,
    sprintf("%sgutter_negative.tiff", filePrefix))
  rm(stencilGutterNeg);gc()

  stencilFrameNeg <-
    image_composite(
      image_negate(stencilEmpty),
      image_negate(stencilFrame),
      operator='copy-opacity')

  writeMagickImage(
    stencilFrameNeg,
    sprintf("%sframe_negative.tiff", filePrefix))
  rm(stencilFrameNeg);gc()

  stencilFramePos <-
    image_composite(
      image_negate(stencilEmpty),
      stencilFrame,
      operator='copy-opacity')

  writeMagickImage(
    stencilFramePos,
    sprintf("%sframe_positive.tiff", filePrefix))
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

  writeMagickImage(
    stencilNegative,
    sprintf("%snegative.tiff", filePrefix))
  rm(stencilNegative);gc()

  stencilPositive <-
    image_composite(
      image_negate(stencilEmpty),
      stencilPanel,
      operator='copy-opacity')

  writeMagickImage(
    stencilPositive,
    sprintf("%spositive.tiff", filePrefix))
  rm(stencilPositive);gc()

  rm(list = ls());gc()
  return(invisible())
}

setupDevice <- function(pan, dpi) {
  geo <- pan$geometry

  img <- image_graph(
    width = getDataWidthInPx(geo, dpi),
    height = getDataHeightInPx(geo, dpi),
    res = dpi,
    bg = "#000000")

  graphics::par(
    mar = c(0,0,0,0),
    xaxs = "i", xaxt="n", yaxs = "i", yaxt="n",
    bg="#000000")
  graphics::plot.new()
  dataBox <- getDataBoxInCm(geo)
  graphics::plot.window(xlim = dataBox$xlim, ylim = dataBox$ylim)

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

  img <- image_blank(
    width = round(geometry$size$w * pxPerCm),
    height = round(geometry$size$h * pxPerCm),
    color = color)

  writeMagickImage(img, fileName)
  rm(img);gc()
}

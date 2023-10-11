#' @export
runResize <- function(nr = NULL) {

  opts <- jsonlite::read_json("resize.json")
  panAndPanels <- readRDS("store_05_panels-effect.RDS")
  render <- ConfigOpts::readOpts("opt_06_render.json", c("Render"))

  for (resizeOpts in opts$panels) {

    if (!is.null(nr)) if (resizeOpts$panelNr != nr) next

    prefix <- sprintf("Ch%dPage%02d_%03d", opts$chapterNr, opts$pageNr, resizeOpts$panelNr)
    innerBorder <- panAndPanels$panels |> filter(panelId == resizeOpts$panelNr) |> pull(inner)

    cat(prefix, "... ")

    resizeImage(
      pathDrawn = resizeOpts$path,
      pathNegative = paste0(prefix, "_negative.tiff"),
      pathMasked = paste0(prefix, "_masked.tiff"),
      pathOut = paste0(prefix, "_image.tiff"),
      scale = resizeOpts$scale,
      xOff = resizeOpts$xOff,
      yOff = resizeOpts$yOff,
      reformat = resizeOpts$reformat,
      dpi = render$dpi,
      geo = panAndPanels$pan$geometry,
      innerBorder = innerBorder)

    cat("ok \n")
  }

  if (!is.null(opts$gutter) && (is.null(nr) || "gutter" == nr)) {

    cat("gutter ... ")

    resizeOpts <- opts$gutter
    prefix <- sprintf("Ch%dPage%02d_gutter", opts$chapterNr, opts$pageNr)
    resizeImage(
      pathDrawn = resizeOpts$path,
      pathNegative = paste0(prefix, "_negative.tiff"),
      pathMasked = paste0(prefix, "_masked.tiff"),
      pathOut = paste0(prefix, "_image.tiff"),
      scale = 1,
      xOff = 0,
      yOff = 0,
      reformat = resizeOpts$reformat,
      dpi = render$dpi,
      geo = panAndPanels$pan$geometry,
      innerBorder = NULL,
      fitExactly = TRUE)

    cat("ok \n")
  }

  cat("Done. \n")
}


resizeImage <- function(
    pathDrawn,
    pathNegative,
    pathMasked,
    pathOut,
    scale = 1,
    xOff = 0,
    yOff = 0,
    reformat = FALSE,
    dpi,
    geo,
    innerBorder,
    fitExactly = FALSE
    ) {
  if (!dir.exists("tmp")) dir.create("tmp")
  if (reformat) {
    sprintf(
      "magick %s -strip tmp/drawn_stripped.tiff",
      pathDrawn
    ) |>
      system()
    sprintf(
      "magick tmp/drawn_stripped.tiff -profile %s %s tmp/drawn.tiff",
      getColorProfilePath(),
      .formatString
    ) |>
      system()
    pathDrawnReady <- "tmp/drawn.tiff"
  } else {
    pathDrawnReady <- pathDrawn
  }
  infoDrawn <- tiff::readTIFF(pathDrawnReady, payload = FALSE)

  createBlank(
    "white",
    getDataWidthInPx(geo, dpi),
    getDataHeightInPx(geo, dpi),
    dpi,
    fileName = "tmp/canvas.tiff",
    overwrite = TRUE)

  if (!is.null(innerBorder)) {
    v <- do.call(rbind, innerBorder)
    boxInCm <- makeBox(
      x = min(v[,1]),
      y = min(v[,2]),
      w = diff(range(v[,1])),
      h = diff(range(v[,2])))
  } else {
    boxInCm <- getDataBoxInCm(geo)
  }
  boxInPx <- convertBoxCmToDataPx(boxInCm, geo, dpi, bleedSide = FALSE)
  yOffInPx <- convertCmToPx(yOff, dpi)
  xOffInPx <- convertCmToPx(xOff, dpi)

  if (fitExactly) {
    sprintf(
      'magick %s -resize %d\\!x%d\\! %s tmp/drawnResized.tiff',
      pathDrawnReady,
      boxInPx$w,
      boxInPx$h,
      .formatString
    ) |>
      shell()
  } else {
    wRatio <- boxInPx$w / infoDrawn$width
    hRatio <- boxInPx$h / infoDrawn$length
    if (wRatio > hRatio) {
      sprintf(
        'magick %s -resize %d %s tmp/drawnResized.tiff',
        pathDrawnReady,
        round(scale*boxInPx$w),
        .formatString
      ) |>
        shell()
    } else {
      sprintf(
        'magick %s -resize x%d %s tmp/drawnResized.tiff',
        pathDrawnReady,
        round(scale*boxInPx$h),
        .formatString
      ) |>
        shell()
    }
  }
  sprintf(
    'magick composite tmp/drawnResized.tiff -geometry %+d%+d tmp/canvas.tiff %s %s',
    boxInPx$x + xOffInPx,
    boxInPx$y + yOffInPx,
    .formatString,
    pathOut
  ) |>
    shell()
  sprintf(
    'magick composite %s %s %s %s',
    pathNegative,
    pathOut,
    .formatString,
    pathMasked
  ) |>
    shell()
}


preparePositioning <- function(
    pathDrawn,
    pathNegative,
    scale = 1,
    reformat = FALSE,
    dpi,
    geo,
    innerBorder,
    fitExactly = FALSE
    ) {
  if (!dir.exists("tmp")) dir.create("tmp")
  if (reformat) {
    shell(paste0('magick "', pathDrawn, '" ', .formatString, ' tmp/drawn.tiff'))
    pathDrawnReady <- "tmp/drawn.tiff"
  } else {
    pathDrawnReady <- pathDrawn
  }
  infoDrawn <- tiff::readTIFF(pathDrawnReady, payload = FALSE)
  sprintf(
    'magick -size %dx%d canvas:white %s tmp/canvas.tiff',
    getDataWidthInPx(geo, dpi),
    getDataHeightInPx(geo, dpi),
    .formatString
  ) |>
    shell()
  sprintf(
    'magick %s -resize %dx%d %s tmp/negative.tiff',
    pathNegative,
    getDataWidthInPx(geo, dpi),
    getDataHeightInPx(geo, dpi),
    .formatString
  ) |>
    shell()
  if (!is.null(innerBorder)) {
    v <- do.call(rbind, innerBorder)
    boxInCm <- makeBox(
      x = min(v[,1]),
      y = min(v[,2]),
      w = diff(range(v[,1])),
      h = diff(range(v[,2])))
  } else {
    boxInCm <- getDataBoxInCm(geo)
  }
  boxInPx <- convertBoxCmToDataPx(boxInCm, geo, dpi, bleedSide = FALSE)

  if (fitExactly) {
    sprintf(
      'magick %s -resize %dx%d %s tmp/drawnResized.tiff',
      pathDrawnReady,
      boxInPx$w,
      boxInPx$h,
      .formatString
    ) |>
      shell()
  } else {
    wRatio <- boxInPx$w / infoDrawn$width
    hRatio <- boxInPx$h / infoDrawn$length
    if (wRatio > hRatio) {
      sprintf(
        'magick %s -resize %d %s tmp/drawnResized.tiff',
        pathDrawnReady,
        round(scale*boxInPx$w),
        .formatString
      ) |>
        shell()
    } else {
      sprintf(
        'magick %s -resize x%d %s tmp/drawnResized.tiff',
        pathDrawnReady,
        round(scale*boxInPx$h),
        .formatString
      ) |>
        shell()
    }
  }
}

positionPrepared <- function(
    xOff = 0,
    yOff = 0,
    innerBorder,
    geo,
    dpi
    ) {
  if (is.null(innerBorder)) {
    boxInCm <- getDataBoxInCm(geo)
  } else {
    v <- do.call(rbind, innerBorder)
    boxInCm <- makeBox(
      x = min(v[,1]),
      y = min(v[,2]),
      w = diff(range(v[,1])),
      h = diff(range(v[,2])))
  }
  boxInPx <- convertBoxCmToDataPx(boxInCm, geo, dpi, bleedSide = FALSE)
  yOffInPx <- convertCmToPx(yOff, dpi)
  xOffInPx <- convertCmToPx(xOff, dpi)
  sprintf(
    'magick composite tmp/drawnResized.tiff -geometry %+d%+d tmp/canvas.tiff %s tmp/out.tiff',
    boxInPx$x + xOffInPx,
    boxInPx$y + yOffInPx,
    .formatString
  ) |>
    shell()
  sprintf(
    'magick composite tmp/negative.tiff tmp/out.tiff %s tmp/masked.tiff',
    .formatString
  ) |>
    shell()
}
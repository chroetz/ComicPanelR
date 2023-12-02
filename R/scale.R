#' @export
runResize <- function(nr = NULL) {

  opts <- jsonlite::read_json("opt_07_resize.json")
  panAndPanels <- readRDS(opts$panAndPanel)
  render <- ConfigOpts::readOpts(opts$renderOpts, "Render")


  for (resizeOpts in opts$panels) {

    if (!is.null(nr)) if (resizeOpts$panelNr != nr) next

    prefix <- sprintf("%s_%03d", panAndPanels$pan$name, resizeOpts$panelNr)
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
    prefix <- sprintf("%s_gutter", panAndPanels$pan$name)
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
  pathDrawn <- normalizePath(pathDrawn, mustWork=TRUE)
  if (is.null(reformat)) reformat <- FALSE
  if (reformat) {
    sprintf(
      'magick "%s" -strip tmp/drawn_stripped.tiff',
      pathDrawn
    ) |>
      system()
    sprintf(
      'magick tmp/drawn_stripped.tiff -profile "%s" %s tmp/drawn.tiff',
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
    color = NULL,
    colorFromImg = pathDrawnReady,
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
      'magick "%s" -resize %d\\!x%d\\! %s tmp/drawnResized.tiff',
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
        'magick "%s" -resize %d %s tmp/drawnResized.tiff',
        pathDrawnReady,
        round(scale*boxInPx$w),
        .formatString
      ) |>
        shell()
    } else {
      sprintf(
        'magick "%s" -resize x%d %s tmp/drawnResized.tiff',
        pathDrawnReady,
        round(scale*boxInPx$h),
        .formatString
      ) |>
        shell()
    }
  }
  sprintf(
    'magick composite tmp/drawnResized.tiff -geometry %+d%+d tmp/canvas.tiff %s "%s"',
    boxInPx$x + xOffInPx,
    boxInPx$y + yOffInPx,
    .formatString,
    pathOut
  ) |>
    shell()
  sprintf(
    'magick composite "%s" "%s" %s "%s"',
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
  pathDrawn <- normalizePath(pathDrawn, mustWork=TRUE)
  if (is.null(reformat)) reformat <- FALSE
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
    'magick "%s" -resize %dx%d %s tmp/negative.tiff',
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
      'magick "%s" -resize %dx%d %s tmp/drawnResized.tiff',
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
        'magick "%s" -resize %d %s tmp/drawnResized.tiff',
        pathDrawnReady,
        round(scale*boxInPx$w),
        .formatString
      ) |>
        shell()
    } else {
      sprintf(
        'magick "%s" -resize x%d %s tmp/drawnResized.tiff',
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


#' @export
createResizeOpts <- function(
    path = ".",
    assetOptsPath = file.path(path, "opt_07_assets.json"),
    outPath = file.path(path, "opt_07_resizeAbsolute.json")
) {

  outPath <- normalizePath(outPath, mustWork=FALSE)

  if (file.exists(outPath)) {
    warning(outPath, " exists. Not overwriting.")
    return(invisible())
  }

  assetOptsPath <- normalizePath(assetOptsPath, mustWork=TRUE)

  wd <- getwd()
  on.exit(setwd(wd))
  setwd(path)

  assets <- ConfigOpts::readOptsBare(assetOptsPath)

  panelResizeParms <- lapply(assets$panels, \(panel) c(
    panel[c("panelNr", "path")],
    extractResizeParms(
      panel$pathTemplate,
      panel$panelNr,
      panAndPanelsPath = assets$panAndPanel,
      renderOptsPath = assets$renderOpts)))

  resizeInfo <- assets
  resizeInfo$panels <- panelResizeParms

  jsonlite::write_json(
    resizeInfo,
    outPath,
    pretty = TRUE,
    auto_unbox = TRUE)
}



extractResizeParms <- function(
    imgPath,
    panelNr,
    panAndPanelsPath = "store_05_panels-effect.RDS",
    renderOptsPath = "opt_06_render.json"
) {

  img <- tiff::readTIFF(imgPath)
  panAndPanels <- readRDS(panAndPanelsPath)
  render <- ConfigOpts::readOpts(renderOptsPath, "Render")

  inner <- panAndPanels$panels |> filter(panelId == panelNr) |> pull(inner)
  verti <- do.call(rbind, inner)
  targetCm <- makeBox(
    x = min(verti[,1]),
    y = min(verti[,2]),
    w = max(verti[,1]) - min(verti[,1]),
    h = max(verti[,2]) - min(verti[,2]))
  target <- convertBoxCmToDataPx(
    targetCm,
    geometry = panAndPanels$pan$geometry,
    dpi = render$dpi,
    bleedSide = FALSE)

  res2 <- rle(img[nrow(img)/2,,4] > 1-3/255)
  res1 <- rle(img[,ncol(img)/2,4] > 1-3/255)

  top <- res1$lengths[1] + res1$lengths[2]/2
  n <- length(res1$lengths)
  bottom <- res1$lengths[n] + res1$lengths[n-1]/2

  left <- res2$lengths[1] + res2$lengths[2]/2
  m <- length(res2$lengths)
  right <- res2$lengths[m] + res2$lengths[m-1]/2

  wImg <- ncol(img)
  hImg <- nrow(img)

  position <- makeBox(
    x = left,
    y = top,
    w = wImg - (right + left),
    h = hImg - (top + bottom))

  scale <- sqrt((target$w / position$w) * (target$h / position$h))

  xOffPx <- - scale * position$left
  yOffPx <- - scale * position$top

  xOffCm <- convertPxToCm(xOffPx, render$dpi)
  yOffCm <- convertPxToCm(yOffPx, render$dpi)

  return(dplyr::lst(scale, xOffCm, yOffCm))
}


#' @export
runResizeAbsolute <- function(
    path = ".",
    resizeOptsPath = "opt_07_resizeAbsolute.json",
    nr = NULL) {

  resizeOptsPath <- normalizePath(resizeOptsPath, mustWork=TRUE)

  wd <- getwd()
  on.exit(setwd(wd))
  setwd(path)

  resizeOpts <- ConfigOpts::readOptsBare(resizeOptsPath)

  panAndPanels <- readRDS(resizeOpts$panAndPanel)
  render <- ConfigOpts::readOpts(resizeOpts$renderOpts, "Render")

  for (resizeOpt in resizeOpts$panels) {

    if (!is.null(nr)) if (resizeOpt$panelNr != nr) next

    prefix <- sprintf("%s_%03d", panAndPanels$pan$name, resizeOpt$panelNr)
    innerBorder <- panAndPanels$panels |> filter(panelId == resizeOpt$panelNr) |> pull(inner)

    cat(prefix, "... ")

    resizeImageAbsolute(
      pathDrawn = resizeOpt$path,
      pathNegative = paste0(prefix, "_negative.tiff"),
      pathMasked = paste0(prefix, "_masked.tiff"),
      pathOut = paste0(prefix, "_image.tiff"),
      scale = resizeOpt$scale,
      xOff = resizeOpt$xOffCm,
      yOff = resizeOpt$yOffCm,
      reformat = resizeOpt$reformat,
      dpi = render$dpi,
      geo = panAndPanels$pan$geometry,
      innerBorder = innerBorder)

    cat("ok \n")
  }

  if (!is.null(resizeOpts$gutter) && (is.null(nr) || "gutter" == nr)) {

    cat("gutter ... ")

    resizeOpt <- resizeOpts$gutter
    prefix <- sprintf("%s_gutter", panAndPanels$pan$name)
    resizeImage(
      pathDrawn = resizeOpt$path,
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

  if (!is.null(resizeOpts$aboveGutter) && (is.null(nr) || "aboveGutter" == nr)) {

    cat("aboveGutter ... ")

    prefix <- sprintf("%s_abovegutter", panAndPanels$pan$name)
    pathOut <- paste0(prefix, "_image.tiff")

    dpi <- render$dpi
    geo <- panAndPanels$pan$geometry

    createBlank(
      color = "#00000000",
      getDataWidthInPx(geo, dpi),
      getDataHeightInPx(geo, dpi),
      dpi,
      fileName = "tmp/canvas.tiff",
      overwrite = TRUE)

    for (resizeOpt in resizeOpts$aboveGutter) {

      filePath <- normalizePath(resizeOpt$path)
      color <- system(sprintf('magick "%s" -format "%%[pixel:p{0,0}]" info:-', filePath), intern = TRUE)

      # change fuzz value to be more or less aggressive in removing color
      sprintf(
        'magick "%s" -alpha off -bordercolor %s -border 1 ( +clone -fuzz 16%% -fill none -floodfill +0+0 %s -alpha extract -geometry 200%% -blur 0x0.5 -morphology erode square:1 -geometry 50%% ) -compose CopyOpacity -composite -shave 1 -profile "%s" %s "tmp/drawn.tiff"',
        filePath,
        color,
        color,
        getColorProfilePath(),
        .formatString
      ) |>
        system()

      sprintf(
        'magick "tmp/drawn.tiff" -resize %f%% %s tmp/drawnResized.tiff',
        resizeOpt$scale*100,
        .formatString
      ) |>
        shell()

      if (!is.null(resizeOpt$coorPx)) {
        xInPx <- resizeOpt$coorPx[1]
        yInPx <- resizeOpt$coorPx[2]
      } else if (!is.null(resizeOpt$coor)) {
        xInPx <- convertCmToPx(resizeOpt$coor[1], dpi)
        yInPx <- convertCmToPx(resizeOpt$coor[2], dpi)
      } else {
        stop("need to sepcify coor or coorPx for aboveGutter")
      }

      sprintf(
        'magick composite tmp/drawnResized.tiff -geometry %+d%+d tmp/canvas.tiff -profile "%s" %s "tmp/canvas.tiff"',
        xInPx,
        yInPx,
        getColorProfilePath(),
        .formatString
      ) |>
        shell()
    }

    file.rename("tmp/canvas.tiff", pathOut)

    cat("ok \n")
  }

  cat("Done. \n")

}


resizeImageAbsolute <- function(
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
  pathDrawn <- normalizePath(pathDrawn, mustWork=TRUE)
  if (is.null(reformat)) reformat <- FALSE
  if (reformat) {
    sprintf(
      'magick "%s" -strip tmp/drawn_stripped.tiff',
      pathDrawn
    ) |>
      system()
    sprintf(
      'magick tmp/drawn_stripped.tiff -profile "%s" %s tmp/drawn.tiff',
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
    color = NULL,
    colorFromImg = pathDrawnReady,
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
      'magick "%s" -resize %d\\!x%d\\! %s tmp/drawnResized.tiff',
      pathDrawnReady,
      boxInPx$w,
      boxInPx$h,
      .formatString
    ) |>
      shell()
  } else {
    sprintf(
      'magick "%s" -resize %f%% %s tmp/drawnResized.tiff',
      pathDrawnReady,
      scale*100,
      .formatString
    ) |>
      shell()
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
    'magick composite "%s" "%s" %s "%s"',
    pathNegative,
    pathOut,
    .formatString,
    pathMasked
  ) |>
    shell()
}

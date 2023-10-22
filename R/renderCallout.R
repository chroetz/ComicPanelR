getPanelBoxFromPanels <- function(panels, panelNr) {
  innerBorder <- panels |>
    filter(.data$panelId == .env$panelNr) |>
    pull(inner)
  v <- do.call(rbind, innerBorder)
  panelBox <- makeBox(
    x = min(v[,1]),
    y = min(v[,2]),
    w = diff(range(v[,1])),
    h = diff(range(v[,2])))
  return(panelBox)
}


getTikzPanelPath <- function(panels, panelNr) {
  innerBorder <- panels |>
    filter(.data$panelId == .env$panelNr) |>
    pull(inner)
  v <- do.call(rbind, innerBorder)
  tikz <- coordsToTikzPath(removeDuplicates(v))
  return(tikz)
}


removeDuplicates <- function(x) {
  if (!is.matrix(x)) return(x)
  n <- nrow(x)
  if (n <= 1) return(x)
  delta <- sqrt(rowSums((x-x[c(2:n,1),])^2))
  x[delta >= 10e-12,]
}


coordsToTikzPath <- function(points) {
  paste0(
    paste0("(", points[,1], ",", points[,2], ")", collapse="--"),
    "--cycle;"
  )
}


readTextOpt <- function(filePath) {
  jsonlite::read_json(
    filePath,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE)
}


#' @export
renderPageCallout <- function() {
  opts <- readTextOpt("opt_09_text.json")
  panAndPanels <- readRDS(opts$panAndPanel)
  render <- ConfigOpts::readOpts(opts$renderOpts, c("Render"))
  info <- jsonlite::read_json("opt_08_merge.json")

  geo <- panAndPanels$pan$geometry
  dataBox <- getDataBoxInCm(geo)
  finalBox <- getFinalBoxInCm(geo)
  bleed <- geo$bleed$right
  stopifnot(bleed == geo$bleed |> unlist())

  if (!dir.exists("tmp")) dir.create("tmp")

  textRenderData <- sapply(
    opts$texts,
    getTextRenderData,
    panels = panAndPanels$panels)

  panelIds <- panAndPanels$panels$panelId |> unique()

  panelTikzPath <- sapply(panelIds, getTikzPanelPath, panels = panAndPanels$panels)
  stopifnot(panelIds == seq_along(panelIds))

  content <- ""
  for (panelId in panelIds) {
    content <- paste0(content, "\\begin{scope}\n\\clip ", panelTikzPath[panelId], "\n")
    for (i in seq_along(opts$texts)) {
      textOpt <- opts$texts[[i]]
      if (textOpt$panelNr != panelId) next
      content <- paste0(content, textRenderData[i], "\n")
    }
    content <- paste0(content, "\\end{scope}\n")
  }

  wd <- getwd()
  on.exit(setwd(wd))
  setwd("tmp")

  vars <- list(
    wTot = dataBox$w,
    hTot = dataBox$h,
    bleed = bleed,
    wInner = finalBox$w,
    hInner = finalBox$h,
    imageBelow = sprintf("../%s_merged_below.pdf", info$name),
    imageAbove = sprintf("../%s_merged_above.pdf", info$name),
    main = content)
  writeTemplate(vars, "page.tex", getTikzCalloutPagePath())

  runLualatex("page.tex", 2)

  file.rename("page.pdf", sprintf("../%s.pdf", opts$name))
}


getTextRenderData <- function(textOpt, panels) {

  panelBox <- getPanelBoxFromPanels(panels,  textOpt$panelNr)

  content <- createTikzContent(textOpt, panelBox)

  return(content)
}


renderCalloutOne <- function(nr) {
  opts <- readTextOpt("opt_09_text.json")
  panAndPanels <- readRDS(opts$panAndPanel)
  render <- ConfigOpts::readOpts(opts$renderOpts, c("Render"))

  textOpt <- opts$texts[[nr]]

  panelBox <- getPanelBoxFromPanels(panAndPanels$panels,  textOpt$panelNr)

  pathPanelImage <-
    sprintf("%s_%03d_image.tiff", opts$name, textOpt$panelNr) |>
    normalizePath(mustWork = TRUE)
  pathPanelNegative <-
    sprintf("%s_%03d_negative.tiff", opts$name, textOpt$panelNr) |>
    normalizePath(mustWork = TRUE)

  wd <- getwd()
  on.exit(setwd(wd))
  if (!dir.exists("tmp")) dir.create("tmp")
  setwd("tmp")

  .renderCalloutOne(
    pathPanelImage = pathPanelImage,
    pathPanelNegative = pathPanelNegative,
    geo = panAndPanels$pan$geometry,
    dpi = render$dpi/5,
    panelBox = panelBox,
    textOpt = textOpt
  )
}


.renderCalloutOne <- function(
    pathPanelImage,
    pathPanelNegative,
    geo,
    dpi,
    panelBox,
    textOpt
) {

  dataBox <- getDataBoxInCm(geo)
  finalBox <- getFinalBoxInCm(geo)

  bleed <- geo$bleed$right
  stopifnot(bleed == geo$bleed |> unlist())

  magickResize(
    pathPanelImage,
    "image.pdf",
    getDataWidthInPx(geo, dpi),
    getDataHeightInPx(geo, dpi))
  magickResize(
    pathPanelNegative,
    "negative.pdf",
    getDataWidthInPx(geo, dpi),
    getDataHeightInPx(geo, dpi))

  content <- createTikzContent(textOpt, panelBox)

  vars <- list(
    wTot = dataBox$w,
    hTot = dataBox$h,
    bleed = bleed,
    wInner = finalBox$w,
    hInner = finalBox$h,
    imageBelow = "image.pdf",
    imageAbove = "negative.pdf",
    main = content)
  writeTemplate(vars, "panelCalloutOne.tex", getTikzCalloutOnePath())

  runLualatex("panelCalloutOne.tex", 2)
}


createTikzContent <- function(textOpt, panelBox) {
  content <- switch(
    textOpt$kind,
    narration = ,
    computer = createTextBox(textOpt, panelBox),
    speech = ,
    whisper = ,
    thought = createCallout(textOpt, panelBox)
  )
  return(content)
}


getFromPostionString <- function(posStr, panelBox) {
  stopifnot(is.character(posStr))
  nodeOpt <- list()
  switch(
    posStr,
    "topright" = {
      nodeOpt$anchor <- "north east"
      nodeOpt$align <- "right"
      coordinate <- c(panelBox$right, panelBox$top)
    },
    "top" = {
      nodeOpt$anchor <- "north"
      nodeOpt$align <- "center"
      coordinate <- c(panelBox$midX, panelBox$top)
    },
    "topleft" = {
      nodeOpt$anchor <- "north west"
      nodeOpt$align <- "left"
      coordinate <- c(panelBox$left, panelBox$top)
    },
    "bottomleft" = {
      nodeOpt$anchor <- "south west"
      nodeOpt$align <- "left"
      coordinate <- c(panelBox$left, panelBox$bottom)
    },
    "bottom" = {
      nodeOpt$anchor <- "south"
      nodeOpt$align <- "center"
      coordinate <- c(panelBox$midX, panelBox$bottom)
    },
    "bottomright" = {
      nodeOpt$anchor <- "south east"
      nodeOpt$align <- "right"
      coordinate <- c(panelBox$right, panelBox$bottom)
    },
    "right" = {
      nodeOpt$anchor <- "east"
      nodeOpt$align <- "right"
      coordinate <- c(panelBox$right, panelBox$midY)
    },
    "left" = {
      nodeOpt$anchor <- "west"
      nodeOpt$align <- "left"
      coordinate <- c(panelBox$left, panelBox$midY)
    },
    stop("Unknown postition ", posStr)
  )
  return(list(nodeOpt = nodeOpt, coor = coordinate))
}


createTextBox <- function(opt, panelBox) {
  nodeOpt <- list()
  if (is.character(opt$position)) {
    posInfo <- getFromPostionString(opt$position, panelBox)
    nodeOpt <- posInfo$nodeOpt
  } else {
    stop()
  }
  if (!is.null(opt$width)) nodeOpt[["text width"]] <- paste0(opt$width, "cm")
  if (!is.null(opt$height)) nodeOpt[["text height"]] <- paste0(opt$height, "cm")
  tikz <- paste0(
    r"(\node[)",
    opt$kind, ", ",
    paste0(names(nodeOpt), "=", unlist(nodeOpt),collapse=", "),
    str_glue(r"(] at ({posInfo$coor[1]},{posInfo$coor[2]}) {{{opt$text}}};)"))
  return(tikz)
}


createCallout <- function(opt, panelBox, innerSep=0.1) {
  textLines <- str_split_1(opt$text, "\n")
  textLength <- getTextLengths(textLines)
  textWidth <- textLength |> filter(kind == "width") |> pull(length)
  lineHeight <- 9 * .cmPerPt # TODO magic number!!
  panel <- geos_create_rectangle(0, 0, panelBox$w, panelBox$h)
  nodeOpt <- list()
  switch(
    opt$position,
    "topleft" = {
      rects <- geos_create_rectangle(
        0,
        (seq_along(textWidth)-1)*lineHeight,
        textWidth,
        seq_along(textWidth)*lineHeight)
      nodeOpt$anchor <- "north west"
      nodeOpt$align <- "left"
      pos <- c(0, 0)
      borderIndicator <- c(1, 1)
    },
    "top" = {
      rects <- geos_create_rectangle(
        panelBox$w/2-textWidth/2,
        (seq_along(textWidth)-1)*lineHeight,
        panelBox$w/2+textWidth/2,
        seq_along(textWidth)*lineHeight)
      nodeOpt$anchor <- "north"
      nodeOpt$align <- "center"
      pos <- c(panelBox$w/2, 0)
      borderIndicator <- c(0, 1)
    },
    "topright" = {
      rects <- geos_create_rectangle(
        panelBox$w-textWidth,
        (seq_along(textWidth)-1)*lineHeight,
        panelBox$w,
        seq_along(textWidth)*lineHeight)
      nodeOpt$anchor <- "north east"
      nodeOpt$align <- "right"
      pos <- c(panelBox$w, 0)
      borderIndicator <- c(-1, 1)
    },
    "bottomleft" = {
      rects <- geos_create_rectangle(
        0,
        panelBox$h+(-length(textWidth)+seq_along(textWidth)-1)*lineHeight,
        textWidth,
        panelBox$h+(-length(textWidth)+seq_along(textWidth))*lineHeight)
      nodeOpt$anchor <- "south west"
      nodeOpt$align <- "left"
      pos <- c(0, panelBox$h)
      borderIndicator <- c(1, -1)
    },
    "bottom" = {
      rects <- geos_create_rectangle(
        panelBox$w/2-textWidth/2,
        panelBox$h+(-length(textWidth)+seq_along(textWidth)-1)*lineHeight,
        panelBox$w/2+textWidth/2,
        panelBox$h+(-length(textWidth)+seq_along(textWidth))*lineHeight)
      nodeOpt$anchor <- "south"
      nodeOpt$align <- "center"
      pos <- c(panelBox$w/2, panelBox$h)
      borderIndicator <- c(0, -1)
    },
    "bottomright" = {
      rects <- geos_create_rectangle(
        panelBox$w-textWidth,
        panelBox$h+(-length(textWidth)+seq_along(textWidth)-1)*lineHeight,
        panelBox$w,
        panelBox$h+(-length(textWidth)+seq_along(textWidth))*lineHeight)
      nodeOpt$anchor <- "south east"
      nodeOpt$align <- "right"
      pos <- c(panelBox$w, panelBox$h)
      borderIndicator <- c(-1, -1)
    },
    "left" = {
      rects <- geos_create_rectangle(
        0,
        panelBox$h/2+(-length(textWidth)/2+seq_along(textWidth)-1)*lineHeight,
        textWidth,
        panelBox$h/2+(-length(textWidth)/2+seq_along(textWidth))*lineHeight)
      nodeOpt$anchor <- "west"
      nodeOpt$align <- "left"
      pos <- c(0, panelBox$h/2)
      borderIndicator <- c(1, 0)
    },
    "right" = {
      rects <- geos_create_rectangle(
        panelBox$w-textWidth,
        panelBox$h/2+(-length(textWidth)/2+seq_along(textWidth)-1)*lineHeight,
        panelBox$w,
        panelBox$h/2+(-length(textWidth)/2+seq_along(textWidth))*lineHeight)
      nodeOpt$anchor <- "east"
      nodeOpt$align <- "right"
      pos <- c(panelBox$w, panelBox$h/2)
      borderIndicator <- c(-1, 0)
    },
    "center" = {
      rects <- geos_create_rectangle(
        panelBox$w/2-textWidth/2,
        panelBox$h/2+(-length(textWidth)/2+seq_along(textWidth)-1)*lineHeight,
        panelBox$w/2+textWidth/2,
        panelBox$h/2+(-length(textWidth)/2+seq_along(textWidth))*lineHeight)
      nodeOpt$anchor <- "center"
      nodeOpt$align <- "center"
      pos <- c(panelBox$w/2, panelBox$h/2)
      borderIndicator <- c(0, 0)
    },
    stop("Unknown position ", opt$position)
  )
  pos <- pos + innerSep * borderIndicator
  rects <- geos_transform_xy(rects, wk::wk_affine_translate(innerSep * borderIndicator[1], innerSep * borderIndicator[2]))
  if (!is.null(opt$shift)) {
    pos <- pos + opt$shift
    rects <- geos_transform_xy(rects, wk::wk_affine_translate(opt$shift[1],opt$shift[2]))
  }
  textPolygon <- geos_unary_union(geos_make_collection(rects))
  par <- optimizeEllipse(panel, textPolygon)
  bubble <- makeEllipse(par[1], par[2], par[3], par[4])
  bubbleBuffed <- geos_buffer(bubble, distance = innerSep)
  indiFrom <- c(
    par[1] + cos(opt$indicator$fromAngle/180*pi)*par[3],
    par[2] + sin(opt$indicator$fromAngle/180*pi)*par[4])
  indiTo <- c(
    indiFrom[1] + cos(opt$indicator$toAngle/180*pi)*opt$indicator$length,
    indiFrom[2] + sin(opt$indicator$toAngle/180*pi)*opt$indicator$length)
  along <- indiTo - indiFrom
  along <- along/sqrt(sum(along^2))
  perp <- c(along[2], -along[1])
  indiPoints <- rbind(
    indiFrom - along*opt$indicator$width + opt$indicator$width/2*perp,
    indiFrom - along*opt$indicator$width - opt$indicator$width/2*perp,
    indiTo)
  indiPoly <- geos_make_polygon(indiPoints[,1], indiPoints[,2], ring_id = 1)
  bubbleIndi <- geos_union(bubbleBuffed, indiPoly)
  bubbleIndiCoords <- wk::wk_coords(geos_unique_points(bubbleIndi))
  finalCoords <- cbind(
    bubbleIndiCoords$x + panelBox$x,
    bubbleIndiCoords$y + panelBox$y)
  tikzElli <- paste0(
    sprintf(r"(\draw[%sEllipse] )", opt$kind),
    coordsToTikzPath(finalCoords)
  )
  textBoxCoords <- wk::wk_coords(geos_unique_points(textPolygon))
  finalCoords <- cbind(
    textBoxCoords$x + panelBox$x,
    textBoxCoords$y + panelBox$y)
  tikzTextArea <- paste0(
    r"(\draw[textArea] )",
    coordsToTikzPath(finalCoords)
  )
  text <- str_replace_all(opt$text, "\\n", r"(\\\\)")
  tikzText <- str_glue(
    sprintf(r"(\node[%sText,)", opt$kind),
    paste0(names(nodeOpt), "=", unlist(nodeOpt),collapse=", "),
    r"(] at ({pos[1]+panelBox$x},{pos[2]+panelBox$y}) {{{text}}};)")
  return(paste(
    tikzElli,
    tikzTextArea,
    tikzText, sep="\n"))
}


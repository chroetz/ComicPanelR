getPanelBoxFromPanels <- function(panels, panelNr, geo) {
  if (panelNr == "gutter") {
    return(getFinalBoxInCm(geo))
  }
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

getPanelPolygon <- function(panels, panelNr, geo) {
  if (panelNr == "gutter") {
    finalBox <- getFinalBoxInCm(geo)
    poly <- geos_make_polygon(
      c(finalBox$left,finalBox$left, finalBox$right,finalBox$right),
      c(finalBox$top, finalBox$bottom, finalBox$bottom, finalBox$top))
    return(poly)
  }
  innerBorder <- panels |>
    filter(.data$panelId == .env$panelNr) |>
    pull(inner)
  v <- do.call(rbind, innerBorder)
  poly <-
    geos_make_polygon(v[,1], v[,2]) |>
    geos_remove_repeated_points(tolerance = .Machine$double.eps)
  return(poly)
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
  #render <- ConfigOpts::readOpts(opts$renderOpts, c("Render"))
  info <- jsonlite::read_json("opt_08_merge.json")

  fontOpts <- opts[c("font", "fontSizePt", "lineHeightPt", "innerSep")]
  geo <- panAndPanels$pan$geometry
  dataBox <- getDataBoxInCm(geo)
  finalBox <- getFinalBoxInCm(geo)
  bleed <- geo$bleed$right
  stopifnot(bleed == geo$bleed |> unlist())

  if (!dir.exists("tmp")) dir.create("tmp")

  textRenderData <- sapply(
    opts$texts,
    getTextRenderData,
    panels = panAndPanels$panels,
    fontOpts = fontOpts,
    geo = geo)

  panelIds <- panAndPanels$panels$panelId |> unique()

  panelTikzPath <- sapply(panelIds, getTikzPanelPath, panels = panAndPanels$panels)

  names(panelTikzPath) <- panelIds

  content <- ""
  for (panelId in panelIds) {
    content <- paste0(content, "\\begin{scope}\n\\clip ", panelTikzPath[as.character(panelId)], "\n")
    for (i in seq_along(opts$texts)) {
      textOpt <- opts$texts[[i]]
      if (textOpt$panelNr != panelId) next
      content <- paste0(content, textRenderData[i], "\n")
    }
    content <- paste0(content, "\\end{scope}\n")
  }

  aboveGutterContent <- ""
  for (i in seq_along(opts$texts)) {
    textOpt <- opts$texts[[i]]
    if (textOpt$panelNr == "gutter")
      aboveGutterContent <- paste0(aboveGutterContent, textRenderData[i], "\n")
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
    font = opts$font,
    fontSizePt = opts$fontSizePt,
    lineHeightPt = opts$lineHeightPt,
    main = content,
    aboveGutter = aboveGutterContent)
  writeTemplate(vars, "page.tex", getTikzCalloutPagePath())

  runLualatex("page.tex", 2)

  file.rename("page.pdf", sprintf("../%s.pdf", panAndPanels$pan$name))
}


getTextRenderData <- function(textOpt, panels, fontOpts, geo, devel = FALSE) {

  panelPoly <- getPanelPolygon(panels, textOpt$panelNr, geo)

  content <- createTikzContent(textOpt, panelPoly, fontOpts, geo, devel)

  return(content)
}


renderCalloutOne <- function(panelNr) {
  opts <- readTextOpt("opt_09_text.json")
  panAndPanels <- readRDS(opts$panAndPanel)
  render <- ConfigOpts::readOpts(opts$renderOpts, c("Render"))

  panelPoly <- getPanelPolygon(panAndPanels$panels, panelNr, panAndPanels$pan$geometry)

  pathImageAbove <-
    sprintf("%s_merged_above.pdf", panAndPanels$pan$name) |>
    normalizePath(mustWork = TRUE, winslash="/")
  pathImageBelow <-
    sprintf("%s_merged_below.pdf", panAndPanels$pan$name) |>
    normalizePath(mustWork = TRUE, winslash="/")

  wd <- getwd()
  on.exit(setwd(wd))
  if (!dir.exists("tmp")) dir.create("tmp")
  setwd("tmp")

  .renderCalloutOne(
    pathImageAbove = pathImageAbove,
    pathImageBelow = pathImageBelow,
    geo = panAndPanels$pan$geometry,
    dpi = render$dpi/5,
    panelPoly = panelPoly,
    textOpt = opts$texts[sapply(opts$texts, \(txt) txt$panelNr == panelNr)],
    fontOpts = opts[c("font", "fontSizePt", "lineHeightPt", "innerSep")]
  )
}


.renderCalloutOne <- function(
    pathImageAbove,
    pathImageBelow,
    geo,
    dpi,
    panelPoly,
    textOpts,
    fontOpts
) {

  dataBox <- getDataBoxInCm(geo)
  finalBox <- getFinalBoxInCm(geo)

  bleed <- geo$bleed$right
  stopifnot(bleed == geo$bleed |> unlist())

  contents <- sapply(textOpts, createTikzContent, panelPoly, fontOpts, geo, devel=TRUE)

  vars <- list(
    wTot = dataBox$w,
    hTot = dataBox$h,
    bleed = bleed,
    wInner = finalBox$w,
    hInner = finalBox$h,
    imageBelow = pathImageBelow,
    imageAbove = pathImageAbove,
    font = fontOpts$font,
    fontSizePt = fontOpts$fontSizePt,
    lineHeightPt = fontOpts$lineHeightPt,
    main = paste(contents, collapse="\n"))

  writeTemplate(vars, "panelCalloutOne.tex", getTikzCalloutOnePath())

  runLualatex("panelCalloutOne.tex", 2)
}


createTikzContent <- function(textOpt, panelPoly, fontOpts, geo, devel) {
  content <- switch(
    textOpt$kind,
    narration = ,
    computer = createTextBox(textOpt, panelPoly, fontOpts, geo, devel),
    speech = ,
    telepathy = ,
    whisper = ,
    thought = createCallout(textOpt, panelPoly, fontOpts, geo, devel)
  )
  return(content)
}


getFromPostionString <- function(posStr, panelPoly) {
  stopifnot(is.character(posStr))
  nodeOpt <- list()
  panelExtent <- panelPoly |> geos_extent()
  panelBox <- makeBox(
    panelExtent$xmin,
    panelExtent$ymin,
    panelExtent$xmax-panelExtent$xmin,
    panelExtent$ymax-panelExtent$ymin)
  switch(
    posStr,
    "center" = {
      nodeOpt$anchor <- "center"
      nodeOpt$align <- "center"
      coordinate <- c(panelBox$midX, panelBox$midY)
    },
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


createTextBox <- function(opt, panelPoly, fontOpts, geo, devel) {
  nodeOpt <- list()
  if (is.character(opt$position)) {
    posInfo <- getFromPostionString(opt$position, panelPoly)
    nodeOpt <- posInfo$nodeOpt
  } else {
    stop()
  }
  if (!is.null(opt$coor)) { # coor overwrites posInfo$coor; transform to panel coor sys
    newPos <- dataCm2FinalCm(opt$coor, geo)
    posInfo$coor <- newPos
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


createCallout <- function(textOpt, panelPoly, fontOpts, geo, devel) {
  textLines <- str_split_1(textOpt$text, "\n")
  textLength <- getTextLengths(textLines, textOpt, fontOpts)
  textWidth <- textLength |> filter(kind == "width") |> pull(length)
  lineHeight <- fontOpts$lineHeightPt * .cmPerPt
  panelExtent <- panelPoly |> geos_extent()
  panelBox <- makeBox(
    panelExtent$xmin,
    panelExtent$ymin,
    panelExtent$xmax-panelExtent$xmin,
    panelExtent$ymax-panelExtent$ymin)
  panel <- geos_transform_xy(
    panelPoly,
    wk::wk_affine_translate(-panelBox$x, -panelBox$y))
  nodeOpt <- list()
  switch(
    textOpt$position,
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
    stop("Unknown position ", textOpt$position)
  )
  if (!is.null(textOpt$coor)) { # coor overwrites pos; transform to panel coor sys
    newPos <- dataCm2PanelCm(textOpt$coor, panelBox, geo)
    del <- newPos - pos
    pos <- newPos
    rects <- geos_transform_xy(
      rects,
      wk::wk_affine_translate(del[1], del[2]))
  }
  pos <- pos + fontOpts$innerSep * borderIndicator
  rects <- geos_transform_xy(
    rects,
    wk::wk_affine_translate(
      fontOpts$innerSep * borderIndicator[1],
      fontOpts$innerSep * borderIndicator[2]))
  if (!is.null(textOpt$shift)) {
    pos <- pos + textOpt$shift
    rects <- geos_transform_xy(
      rects, wk::wk_affine_translate(textOpt$shift[1],textOpt$shift[2]))
  }
  textPolygon <- geos_unary_union(geos_make_collection(rects))
  par <- optimizeEllipse(panel, textPolygon)
  bubble <- makeEllipse(par[1], par[2], par[3], par[4])
  bubbleBuffed <- geos_buffer(bubble, distance = fontOpts$innerSep)
  indicators <- textOpt$indicators
  if (is.null(indicators) && !is.null(textOpt$indicator)) {
    indicators <- list(textOpt$indicator)
  }
  bubble <- bubbleBuffed
  centroid <- wk::wk_coords(geos_centroid(geos_intersection(bubble, panel)))
  boundary <- geos_boundary(bubble)
  for (indicator in indicators) {
    bubble <- addIndicator(
      bubble, indicator, par[1], par[2], par[3], par[4], centroid, boundary, panelBox, geo)
  }
  bubbleFinalCoords <- wk::wk_coords(geos_unique_points(bubble))
  finalCoords <- cbind(
    bubbleFinalCoords$x + panelBox$x,
    bubbleFinalCoords$y + panelBox$y)
  tikzElli <- paste0(
    sprintf(r"(\draw[%sEllipse] )", textOpt$kind),
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

  if (devel) {
    if (!is.null(textOpt$indicator) && !is.null(textOpt$indicator$targetCoor)) {
      x <- dataCm2FinalCm(textOpt$indicator$targetCoor, geo)
      tikzTextArea <- paste0(tikzTextArea, "\n", sprintf("\\draw[red] (%f, %f) circle (0.05cm);", x[1], x[2]))
    }
    x <- c(panelBox$x + centroid$x, panelBox$y + centroid$y)
    tikzTextArea <- paste0(tikzTextArea, "\n", sprintf("\\fill[green] (%f, %f) circle (0.1cm);", x[1], x[2]))
  }

  text <- str_replace_all(textOpt$text, "\\n", r"(\\\\)")
  tikzText <- str_glue(
    sprintf(r"(\node[%sText,)", textOpt$kind),
    paste0(names(nodeOpt), "=", unlist(nodeOpt),collapse=", "),
    r"(] at ({pos[1]+panelBox$x},{pos[2]+panelBox$y}) {{{text}}};)")
  return(paste(
    tikzElli,
    tikzTextArea,
    tikzText, sep="\n"))
}

addIndicator <- function(bubble, indicator, x, y, rx, ry, centroid, boundary, panelBox, geo) {

  if (!is.null(indicator$targetCoor)) {
    targetCoor <- dataCm2PanelCm(indicator$targetCoor, panelBox, geo)
    indiTo <- targetCoor
    if (is.null(indicator$fromAngle)) {
      wkCoord <- wk::wk_coords(geos_intersection(
        geos_make_linestring(
          c(centroid$x, targetCoor[1]),
          c(centroid$y, targetCoor[2])),
        boundary))
      indiFrom <- c(wkCoord$x, wkCoord$y)
    } else {
      fromAngle <- indicator$fromAngle/180*pi
      indiFrom <- c(
        x + cos(fromAngle)*rx,
        y + sin(fromAngle)*ry)
    }
  } else {
    fromAngle <- indicator$fromAngle/180*pi
    toAngle <- indicator$toAngle/180*pi
    indiFrom <- c(
      x + cos(fromAngle)*rx,
      y + sin(fromAngle)*ry)
    indiTo <- c(
      indiFrom[1] + cos(toAngle)*indicator$length,
      indiFrom[2] + sin(toAngle)*indicator$length)
  }
  along <- indiTo - indiFrom
  along <- along/sqrt(sum(along^2))
  perp <- c(along[2], -along[1])
  indiPoints <- rbind(
    indiFrom - along*indicator$width + indicator$width/2*perp,
    indiFrom - along*indicator$width - indicator$width/2*perp,
    indiFrom + indicator$length*along)
  indiPoly <- geos_make_polygon(indiPoints[,1], indiPoints[,2], ring_id = 1)
  bubble <- geos_union(bubble, indiPoly)
}

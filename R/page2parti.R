#' @export
createBaseParti <- function() {
  fileInPage <- dir(pattern="^opt_01_page.*\\.json$")
  suffix <- str_match(fileInPage, "^opt_01_page(.*)\\.json$")[,2]
  fileOutPng <- paste0("preview_01_parti", suffix, ".png")
  fileOutRds <- paste0("store_01_parti", suffix, ".RDS")
  for (i in seq_along(fileInPage)) {
    page <- ConfigOpts::readOpts(fileInPage[i], "Page")
    parti <- page2parti(page)
    renderParti(parti, fileOutPng[i])
    saveRDS(parti, fileOutRds[i])
  }
}


page2parti <- function(page) {
  page <- ConfigOpts::asOpts(page, "Page")
  className <- ConfigOpts::getClassAt(page, 2)
  parti <- switch(
    className,
    "Rowwise" = partiFromRowwisePage(page),
    "Layout" = partiFromLayoutPage(page),
    stop(paste0("Unknown Page Class ", className))
  )
  return(parti)
}

partiFromRowwisePage <- function(page) {
  page <- ConfigOpts::asOpts(page, c("Rowwise", "Page"))
  rows <- page$rows
  if (is.matrix(rows)) rows <- apply(rows, 1, force, simplify=FALSE)
  layoutPage <- ConfigOpts::makeOpts(
    c("Layout", "Page"),
    name = page$name,
    geometry = page$geometry,
    panelArea = ConfigOpts::makeOpts(
      c("Column", "Layout"),
      weights = page$rowWeights,
      elements = ConfigOpts::makeOpts(
        c("Layout", "List"),
        list = lapply(
          rows,
          \(wei) {
            ConfigOpts::makeOpts(
              c("Row", "Layout"),
              weights = wei,
              elements = ConfigOpts::makeOpts(
                c("Layout", "List"),
                list = lapply(
                  wei,
                  \(w) {
                    ConfigOpts::makeOpts(c("Panel", "Layout"))
                  }
                )
              )
            )
          }
        )
      )
    )
  )

  parti <- partiFromLayoutPage(layoutPage)
  return(parti)
}

partiFromLayoutPage <- function(page) {
  page <- ConfigOpts::asOpts(page, c("Layout", "Page"))
  box <- makeBox(0, 0, page$geometry$size$w, page$geometry$size$h)
  layout <- page$panelArea
  coorPanels <- getLayoutPanels(layout, box)
  idPanelsAndVertices <- coorPanels2Id(coorPanels)
  vertices <- idPanelsAndVertices$vertices
  rectIdPanels <- idPanelsAndVertices$idPanels
  panelGraph <- idPanels2panelGraph(rectIdPanels, vertices)
  polygonIdPanels <- idPanelsFromPanelGraph(panelGraph$idGraph, panelGraph$idSegments)
  names(polygonIdPanels) <- paste0("P", seq_along(polygonIdPanels))
  parti <- ConfigOpts::makeOpts(
    "Parti",
    name = page$name,
    geometry = page$geometry,
    vertices = vertices,
    idPanels = polygonIdPanels)
  return(parti)
}


rectPanel <- function(x, y, w, h) {
  matrix(
    c(x, y,
      x, y + h,
      x + w, y + h,
      x + w, y),
    byrow=TRUE,
    ncol = 2)
}

makeBox <- function(x, y, w, h) {
  list(
    x = x,
    y = y,
    w = w,
    h = h)
}

getColumnPanels <- function(layout, box) {
  layout <- ConfigOpts::asOpts(layout, c("Column", "Layout"))
  x <- box$x
  w <- box$w
  panelLists <- lapply(
    seq_along(layout$weights),
    \(i) {
      h <- box$h * layout$weights[i] / sum(layout$weights)
      y <- box$y - h + box$h * cumsum(layout$weights)[i] / sum(layout$weights)
      getLayoutPanels(layout$elements$list[[i]], makeBox(x, y, w, h))
    }
  )
  unlist(panelLists, recursive=FALSE)
}

getRowPanels <- function(layout, box) {
  layout <- ConfigOpts::asOpts(layout, c("Row", "Layout"))
  y <- box$y
  h <- box$h
  panelLists <- lapply(
    seq_along(layout$weights),
    \(i) {
      w <- box$w * layout$weights[i] / sum(layout$weights)
      x <- box$x - w + box$w * cumsum(layout$weights)[i] / sum(layout$weights)
      getLayoutPanels(layout$elements$list[[i]], makeBox(x, y, w, h))
    }
  )
  unlist(panelLists, recursive=FALSE)
}

getLayoutPanels <- function(layout, box) {
  className <- ConfigOpts::getClassAt(layout, 2)
  panels <- switch(
    className,
    "Column" = getColumnPanels(layout, box),
    "Row" = getRowPanels(layout, box),
    "Panel" = list(do.call(rectPanel, box))
  )
  return(panels)
}


coorPanels2Id <- function(panels) {
  eps <- sqrt(.Machine$double.eps)
  coors <- do.call(rbind, panels)
  n <- nrow(coors)
  isSame <- matrix(
    rowSums(abs(coors[rep(1:n, each=n),] - coors[rep(1:n, times=n),])) < eps,
    nrow = n)
  representativeIdx <- apply(isSame, 1, which.max)
  sel <- representativeIdx == 1:n
  newId <- representativeIdx[sel]
  uniqueCoors <- coors[sel, ]
  ids <- sapply(representativeIdx, \(idx) which(newId == idx))
  panelsCoorIds <- matrix(ids, byrow = TRUE, ncol = 4)

  panelCoorsCheck <- apply(panelsCoorIds, 1, \(row) uniqueCoors[row, ], simplify=FALSE)
  for (i in seq_along(panelCoorsCheck)) {
    stopifnot(mean(abs(panelCoorsCheck[[i]] - panels[[i]])) < eps)
  }

  panels <- apply(panelsCoorIds, 1, force, simplify=FALSE)

  list(
    vertices = uniqueCoors,
    idPanels = panels)
}

drawPartiPolygons <- function(parti, box) {

  colors <- getPanelColors(length(parti$idPanels))
  for (i in seq_along(parti$idPanels)) {
    p <- parti$idPanels[[i]]
    coors <- parti$vertices[p, ]
    graphics::polygon(coors[,1], coors[,2], border="#000000", lwd=2, col=colors[i])
    center <- colMeans(coors)
    graphics::text(center[1], center[2], paste0("P", i), adj = c(0.5, 0.5))
  }

  for (i in seq_len(nrow(parti$vertices))) {
    drawNode(parti$vertices[i,], paste0("V", i))
  }
}

drawNode <- function(pos, txt, r = 0.5, fill="#EEEEEE", draw="#000000", textColor = "#000000", type = "circle") {
  if (type == "circle") {
    theta <- seq(0, 2*pi, length.out = 100)
    graphics::polygon(pos[1]+r*cos(theta), pos[2]+r*sin(theta), border=draw, lwd=2, col=fill)
  } else {
    graphics::rect(pos[1]-r, pos[2]-r/2, pos[1]+r, pos[2]+r/2, border=draw, lwd=2, col=fill)
  }
  graphics::text(pos[1], pos[2], txt, adj = c(0.5, 0.5), col = textColor)
}

renderParti <- function(parti, fileOut, dpi = 300) {
  geometry <- parti$geometry
  box <- makeBox(0, 0, geometry$size$w, geometry$size$h)
  cmPerInch <- 2.54
  pxPerInch <- dpi
  pxPerCm <- pxPerInch/cmPerInch
  grDevices::png(
    filename = fileOut,
    width = round(geometry$size$w * pxPerCm),
    height = round(geometry$size$h * pxPerCm),
    res = 300)
  graphics::par(mar = c(0,0,0,0), xaxs = "i", xaxt="n", yaxs = "i", yaxt="n", bg="#FF0000")
  graphics::plot.new()
  brdr <- geometry$sideMargin + geometry$bleed
  graphics::plot.window(xlim = c(box$x-brdr, box$x+box$w+brdr), ylim = c(box$y+box$h+brdr, box$y-brdr))
  graphics::rect(
    box$x-geometry$sideMargin, box$y-geometry$sideMargin, box$x+box$w+geometry$sideMargin, box$y+box$h+geometry$sideMargin,
    col = "#FFFFFF", border=NA)
  drawPartiPolygons(parti)
  graphics::text(box$x + box$w/2, box$y-geometry$sideMargin/2, parti$name, adj = c(0.5, 0.5), cex=2, col="black")
  grDevices::dev.off()
}

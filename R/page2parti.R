#' @export
createBaseParti <- function() {
  files <- getFiles("opt_01_page", "json")
  for (i in seq_len(nrow(files))) {
    createBasePartiOne(
      fileInOpts = files$file[i],
      fileOutPng = paste0("preview_01_parti", files$suffix[i], ".png"),
      fileOutRds = paste0("store_01_parti", files$suffix[i], ".RDS"))
  }
}

createBasePartiOne <- function(fileInOpts, fileOutPng, fileOutRds) {
  page <- ConfigOpts::readOpts(fileInOpts, "Page")
  parti <- page2parti(page)
  renderParti(parti, fileOutPng)
  saveRDS(parti, fileOutRds)
}

page2parti <- function(page) {
  page <- ConfigOpts::asOpts(page, "Page")
  className <- ConfigOpts::getClassAt(page, 2)
  parti <- switch(
    className,
    "Free" = partiFromFreePage(page),
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
  panelBox <- getPanelBoxInCm(page$geometry)
  coorPanels <- getLayoutPanels(page$panelArea, panelBox)
  parti <- partiFromCoords(page$name, page$geometry, coorPanels)
  return(parti)
}

partiFromCoords <- function(name, geometry, coorPanels) {
  idPanelsAndVertices <- coorPanels2Id(coorPanels)
  vertices <- idPanelsAndVertices$vertices
  rectIdPanels <- idPanelsAndVertices$idPanels
  panelGraph <- idPanels2panelGraph(rectIdPanels, vertices)
  polygonIdPanels <- idPanelsFromPanelGraph(panelGraph$idGraph, panelGraph$idSegments)
  names(polygonIdPanels) <- paste0("P", seq_along(polygonIdPanels))
  parti <- ConfigOpts::makeOpts(
    "Parti",
    name = name,
    geometry = geometry,
    vertices = vertices,
    idPanels = polygonIdPanels)
  return(parti)
}

partiFromFreePage <- function(page) {
  page <- ConfigOpts::asOpts(page, c("Free", "Page"))
  parti <- partiFromCoords(page$name, page$geometry, page$panels)
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

rectPanelFromBox <- function(box) {
  matrix(
    c(box$left, box$top,
      box$left, box$bottom,
      box$right, box$bottom,
      box$right, box$top),
    byrow=TRUE,
    ncol = 2)
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
    "Panel" = list(rectPanelFromBox(box))
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

  panelVertexCount <- sapply(panels, nrow)
  panelIdxEnd <- cumsum(panelVertexCount)
  panelIdxesStart <- panelIdxEnd - panelVertexCount + 1
  panelIdxes <- lapply(seq_along(panels), \(i) panelIdxesStart[i]:panelIdxEnd[i])

  idPanels <- lapply(seq_along(panels), \(i) ids[panelIdxes[[i]]])

  panelCoorsCheck <- lapply(idPanels, \(ids) uniqueCoors[ids, , drop=FALSE])
  for (i in seq_along(panelCoorsCheck)) {
    stopifnot(mean(abs(panelCoorsCheck[[i]] - panels[[i]])) < eps)
  }

  list(
    vertices = uniqueCoors,
    idPanels = idPanels)
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
  geo <- parti$geometry
  finalBox <- getFinalBoxInCm(geo)
  plotPageWithBleed(geo, dpi, fileOut)
  drawPartiPolygons(parti)
  graphics::text(
    finalBox$midX,
    finalBox$y+geo$margin$top/2,
    parti$name,
    adj = c(0.5, 0.5),
    cex=2, col="black")
  grDevices::dev.off()
}

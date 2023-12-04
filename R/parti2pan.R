

parti2pan <- function(parti) {

  panelGraph <- getPanelGraph(parti$idPanels)
  coorSegments <- apply(panelGraph$idSegments, 1, \(row) parti$vertices[row, ], simplify=FALSE)
  borders <- tibble(
    segmentId = seq_along(coorSegments),
    coorSegments = coorSegments)

  pan <- c(unclass(parti), panelGraph, list(borders = borders))

  return(pan)
}


getPanelGraph <- function(idPanels) {
  directedSides <- lapply(
    idPanels,
    \(p) if (length(p) < 2) NULL else cbind(p, c(p[-1], p[1]))) |>
    do.call(rbind, args=_)
  panelIds <- unlist(lapply(
    seq_along(idPanels),
    \(i) rep(i, length(idPanels[[i]]))))
  sideIds <- unlist(lapply(
    seq_along(idPanels),
    \(i) seq_along(idPanels[[i]])))
  sortedSides <- cbind(
    pmin(directedSides[,1], directedSides[,2]),
    pmax(directedSides[,1], directedSides[,2]))
  idSegments <- uniqueRows(sortedSides)
  segmentId <- apply(sortedSides, 1, whichIdSegment, idSegments = idSegments)

  if (length(panelIds) == 0) panelIds <- integer(0)
  if (length(sideIds) == 0) sideIds <- integer(0)
  if (length(segmentId) == 0) segmentId <- integer(0)
  idGraph <-
    tibble(
      panelId = panelIds,
      sideId = sideIds,
      segmentId = segmentId
    )
  return(list(idSegments = idSegments, idGraph = idGraph))
}


whichIdSegment <- function(x, idSegments) {
  x <- sort(x)
  left <- pmin(idSegments[,1], idSegments[,2])
  right <- pmax(idSegments[,1], idSegments[,2])
  which(x[1] == left & x[2] == right)
}


makeEllipse <- function(x, y, rx, ry, n=100) {
  angle <- seq(0, 2*pi, length.out = n+1)[-(n+1)]
  path <- cbind(x + rx * cos(angle), y + ry * sin(angle))
  polyTmp <- geos_make_polygon(path[,1], path[,2], ring_id = 1)
  return(geos_make_valid(polyTmp))
}

getArea <- function(par, poly, panel) {
  ellipse <- makeEllipse(par[1], par[2], par[3], par[4])
  if (!geos_contains(ellipse, poly)) return(Inf)
  ellipseInPanel <- geos_intersection(ellipse, panel)
  whiteSpace <- geos_difference(ellipseInPanel, poly)
  geos_area(whiteSpace)
}

getArea2 <- function(par, poly, panel, penalty) {
  ellipse <- makeEllipse(par[1], par[2], par[3], par[4])
  ellipseInPanel <- geos_intersection(ellipse, panel)
  whiteSpace <- geos_difference(ellipseInPanel, poly)
  redSpace <- geos_difference(poly, ellipse)
  geos_area(whiteSpace) + geos_area(redSpace) * penalty
}

optimizeEllipse <- function(panel, poly, penalty = 10) {

  extent <- geos_extent(poly)
  panelExtent <- geos_extent(panel)
  lower <- c(
    extent$xmin,
    extent$ymin,
    (extent$xmax - extent$xmin)/2,
    (extent$ymax - extent$ymin)/2)
  upper <- c(
    extent$xmax,
    extent$ymax,
    panelExtent$xmax - panelExtent$xmin,
    panelExtent$ymax - panelExtent$ymin)
  startPar <- c(
    ((lower + upper)/2)[1:2],
    sqrt((upper*lower)[3:4]))

  res <- stats::optim(
    startPar,
    getArea2,
    method = "L-BFGS-B",
    lower = lower, upper = upper,
    poly = poly, panel = panel, penalty = penalty)

  return(res$par)
}



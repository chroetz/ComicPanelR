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

optimizeEllipse <- function(ranges, panel, poly, penalty = 10, retrys = NULL) {
  stopifnot(length(retrys) %in% c(0, 1))
  bestPar <- NULL
  bestValue <- Inf
  lower <- sapply(ranges, `[`, 1)
  upper <- sapply(ranges, `[`, 2)
  if (length(retrys) == 0) {
    startPar <- sapply(ranges, mean)
    repLen <- 1
  } else {
    repLen <- retrys
  }
  for (k in seq_len(repLen)){
    if (length(retrys) == 1) {
      startPar <- lower + stats::runif(length(ranges)) * sapply(ranges, diff)
    }
    for (p in penalty) {
      res <- stats::optim(
        startPar,
        getArea2,
        method = "L-BFGS-B",
        lower = lower, upper = upper,
        poly = poly, panel = panel, penalty = p)
      startPar <- res$par
    }
    value <- getArea2(startPar, poly, panel, penalty = last(penalty))
    if (value < bestValue) {
      bestValue <- value
      bestPar <- startPar
    }
  }
  return(bestPar)
}



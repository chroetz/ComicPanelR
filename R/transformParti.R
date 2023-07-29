#' @export
createTransformedParti <- function(
    fileInParti = "store_01_parti.RDS",
    fileInTransform = "opt_02_transform.json",
    fileOutPng = "preview_02_parti-tr.png",
    fileOutRds = "store_02_parti-tr.RDS"
) {
  parti <- readRDS(fileInParti)
  transformOpts <- ConfigOpts::readOpts(fileInTransform, c("Transform", "List"))
  parti <- transformParti(parti, transformOpts$list)
  renderParti(parti, fileOutPng)
  saveRDS(parti, fileOutRds)
}


transform <- function(parti, transform) {
  className <- ConfigOpts::getClassAt(transform, 2)
  parti <- switch(
    className,
    "Rotate" = rotatePath(parti, transform),
    "Adjust" = adjustCoor(parti, transform),
    stop(paste0("Unknown Transform ", className))
  )
  return(parti)
}

getRotationMatrix <- function(x) {
  rbind(
    c(cos(x), -sin(x)),
    c(sin(x), cos(x))
  )
}

`%-%` <- function(x, y) {
  if (is.matrix(x) && (is.vector(y) || nrow(y)==1)) return(x - rep(y, each=nrow(x)))
  if (is.matrix(y) && (is.vector(x) || nrow(x)==1)) return(rep(x, each=nrow(y)) - y)
  return(x - y)
}
`%+%` <- function(x, y) {
  if (is.matrix(x) && (is.vector(y) || nrow(y)==1)) return(x + rep(y, each=nrow(x)))
  if (is.matrix(y) && (is.vector(x) || nrow(x)==1)) return(rep(x, each=nrow(y)) + y)
  return(x + y)
}

rotatePath <- function(parti, opts) {
  opts <- ConfigOpts::asOpts(opts, c("Rotate", "Transform"))
  firstId <- opts$vertexIds[1]
  lastId <-  opts$vertexIds[length(opts$vertexIds)]
  center <- colMeans(parti$vertices[c(firstId, lastId),])
  x <- opts$angle / 360 * 2*pi
  parti$vertices[opts$vertexIds,] <- center %+% ((
    (parti$vertices[opts$vertexIds,] %-% center) %*% getRotationMatrix(x)) / cos(x))
  return(parti)
}

adjustCoor <- function(parti, opts) {
  opts <- ConfigOpts::asOpts(opts, c("Adjust", "Transform"))
  parti$vertices[opts$vertexIds,] <- parti$vertices[opts$vertexIds,] %+% opts$adjustment
  return(parti)
}

transformParti <- function(parti, transformList) {
  for (trnsfrm in transformList) {
    parti <- transform(parti, trnsfrm)
  }
  return(parti)
}

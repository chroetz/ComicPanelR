#' @export
createTransformedParti <- function() {
  files <- getFilePairs(
    "store_01_parti", "RDS",
    "opt_02_transform", "json")
  for (i in seq_len(nrow(files))) {
    createTransformedPartiOne(
      fileInStore = files$file1[i],
      fileInOpts = files$file2[i],
      fileOutPng = paste0("preview_02_parti-tr", files$suffix[i], ".png"),
      fileOutRds = paste0("store_02_parti-tr", files$suffix[i], ".RDS"))
  }
}

createTransformedPartiOne <- function(fileInStore, fileInOpts, fileOutPng, fileOutRds) {
  parti <- readRDS(fileInStore)
  transformOpts <- ConfigOpts::readOpts(fileInOpts, c("Transform", "List"))
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

# TODO: from pan object create sides which are ~1000 points. save as binary. Add for each side on the path, explicit coordinates (many points if smooth); need list of all shortest lines and list of all sides of quadrilaterals

transformPan <- function(pan, transform) {
  className <- ConfigOpts::getClassAt(transform, 2)
  pan <- switch(
    className,
    "Rotate" = rotatePath(pan, transform),
    "Adjust" = adjustCoor(pan, transform),
    stop(paste0("Unknown Transform ", className))
  )
  return(pan)
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

rotatePath <- function(pan, rotation) {
  rotation <- ConfigOpts::asOpts(rotation, c("Rotate", "Transform"))
  firstId <- rotation$path[1]
  lastId <-  rotation$path[length(rotation$path)]
  center <- colMeans(pan$coors[c(firstId, lastId),])
  x <- rotation$angle / 360 * 2*pi
  pan$coors[rotation$path,] <- center %+% ((
    (pan$coors[rotation$path,] %-% center) %*% getRotationMatrix(x)) / cos(x))
  return(pan)
}

# TODO: would only work if a allow arbitrary polygons instead of quadrilaterals.
# But then I do not have top right left bottom anymore for styling.
adjustCoor <- function(pan, transform) {
  transform <- ConfigOpts::asOpts(transform, c("Adjust", "Transform"))
  pan$coors[transform$coor,] <- pan$coors[transform$coor,] %+% transform$adjustment
  return(pan)
}

applyTransforms <- function(pan, transformList) {
  for (trnsfrm in transformList) {
    pan <- transformPan(pan, trnsfrm)
  }
  return(pan)
}

getPanelSpaceBorderPath <- function(tiling) {
  borders <- lapply(seq_len(nrow(tiling$panels)), \(i) {
    segIds <- tiling$panels$segments[[i]]
    segments <- tiling$segments$segment[segIds,]
    ordering <- getSegmentOrdering(segments)
    segIds <- segIds[ordering$rowIdxs]
    paths <- lapply(
      seq_len(nrow(ordering)),
      \(j) {
        p <- tiling$segments$path[[segIds[j]]]
        if (ordering$reverse[j]) p <- p[rev(seq_len(nrow(p))),]
        return(p)
      })
    path <- do.call(rbind, paths)
    return(path)
  })
  return(borders)
}

tiling$panels$border <- getPanelSpaceBorderPath(tiling)
tiling$panels <-
  tiling$panels |>
  rowwise() |>
  mutate(center = matrix(colMeans(border), nrow=1))


i <- 1
innerSep <- 0.5
eps <- .Machine$double.eps
border <- tiling$panels$border[[i]]
nrms <- getNormalVectors(border, target = tiling$panels$center[i])
plot(NA, xlim=range(border[,1]), ylim = range(border[,2]))
polygon(border[,1], border[,2])
inner <- border + nrms * innerSep
n <- nrow(border)
dstMat <- matrix(sqrt(rowSums((inner[rep(1:n, each=n),]-border[rep(1:n, times=n),])^2)), nrow=n)
dsts <- apply(dstMat, 2, min)
inner <- inner[dsts>innerSep-eps, ]
inner <- inner[apply(inner, 1, \(x) !any(is.na(x))), ]
polygon(inner[,1], inner[,2], border="red", col="blue")

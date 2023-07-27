uniqueRows <- function(mat) {
  n <- nrow(mat)
  isSame <- matrix(
    rowSums(abs(mat[rep(1:n, each=n),] - mat[rep(1:n, times=n),])) < sqrt(.Machine$double.eps),
    nrow = n)
  representativeIdx <- apply(isSame, 1, which.max)
  sel <- representativeIdx == 1:n
  newId <- representativeIdx[sel]
  uniqueRows <- mat[sel, ]

  return(uniqueRows)
}

overlapSegments <- function(s1, s2, pan) {
  eps <- sqrt(.Machine$double.eps)
  S11 <- pan$coors[s1[1], ]
  S12 <- pan$coors[s1[2], ]
  S21 <- pan$coors[s2[1], ]
  S22 <- pan$coors[s2[2], ]
  mat <- rbind(S12-S11, S21-S11, S22-S11)
  if (all(colSums(abs(mat)) > eps)) {
    rel <- mat[,1] / mat[,2]
    rel <- rel[!is.na(rel)]
    if (length(rel) >= 2) {
      if (any(!is.finite(rel))) return(NULL)
      if (stats::var(rel) > eps) return(NULL)
    }
  }
  t11 <- 0
  t12 <- 1
  t21 <- mean(mat[2,] / mat[1,], na.rm = TRUE)
  t22 <- mean(mat[3,] / mat[1,], na.rm = TRUE)
  if (max(c(t21, t22))-eps < t11 || min(c(t21, t22))+eps > t12) return(NULL)
  rdr <- order(c(t11, t12, t21, t22))
  s <- unique(c(s1, s2)[rdr])
  if (length(s) < 2) return(NULL)
  segs <- cbind(s[1:(length(s)-1)], s[2:length(s)])
  return(segs)
}

addNewSegments <- function(old, candidates) {
  candidates <- cbind(pmin(candidates[,1], candidates[,2]), pmax(candidates[,1], candidates[,2]))
  found <- apply(candidates, 1, \(seg) any(rowSums(old != rep(seg, each=nrow(old))) == 0))
  new <- candidates[!found,]
  res <- rbind(old, new)
  return(res)
}

getTiling <- function(pan) {

  directedSides <- rbind(
    pan$panels[,1:2],
    pan$panels[,2:3],
    pan$panels[,3:4],
    pan$panels[,c(1,4)])
  panelIds <- rep(seq_len(nrow(pan$panels)), times = 4)
  sideDirection <- rep(c("left", "bottom", "right", "top"), each = nrow(pan$panels))

  sortedSides <- cbind(pmin(directedSides[,1], directedSides[,2]), pmax(directedSides[,1], directedSides[,2]))
  uniqueSide <- uniqueRows(sortedSides)

  segments <- uniqueSide

  while(TRUE) {
    found <- FALSE
    for (i in seq_len(nrow(segments)-1)) {
      if (i >= nrow(segments)) break
      s1 <- segments[i,]
      for (j in (i+1):nrow(segments)) {
        s2 <- segments[j,]
        newSegments <- overlapSegments(s1, s2, pan)
        if (length(newSegments) > 0) {
          segments <- addNewSegments(segments[-c(i,j),], newSegments)
          found <- TRUE
          break
        }
      }
      if (found) break
    }
    if (!found) break
  }
  dimnames(segments) <- NULL
  segmentIdsInSide <- lapply(
    seq_len(nrow(sortedSides)),
    \(i) which(segmentContains(sortedSides[i,], segments, pan)))

  vertices <-
    tibble(coordinates = pan$coors) |>
    rowid_to_column("coorId")

  segments <-
    tibble(segment = segments) |>
    rowid_to_column("segId") |>
    rowwise() |>
    mutate(path = list(makeStraightPath(.data$segment, vertices)))

  sides <-
    tibble(
      directed = directedSides,
      sorted = sortedSides,
      panel = panelIds,
      direction = sideDirection,
      segments = segmentIdsInSide
    ) |>
    rowid_to_column("sideId")

  panels <- tibble(
    panelId = seq_len(nrow(pan$panels)),
    vertices = lapply(seq_len(nrow(pan$panels)), \(i) pan$panels[i,]),
  ) |>
    rowwise() |>
    mutate(sides = list(.env$sides |> filter(.data$panel == panelId) |> pull(sideId))) |>
    mutate(segments = list(unlist(.env$sides |> filter(.data$panel == panelId) |> pull(segments)))) |>
    ungroup()

  return(list(
    segments = segments,
    sides = sides,
    panels = panels,
    vertices = vertices
    )
  )
}

segmentContains <- function(seg, others, pan) {
  sapply(seq_len(nrow(others)), \(i) segmentContainsOne(seg, others[i,], pan))
}

segmentContainsOne <- function(s1, s2, pan) {
  if (all(s1 == s2)) return(TRUE)
  eps <- sqrt(.Machine$double.eps)
  S11 <- pan$coors[s1[1], ]
  S12 <- pan$coors[s1[2], ]
  S21 <- pan$coors[s2[1], ]
  S22 <- pan$coors[s2[2], ]
  mat <- rbind(S12-S11, S21-S11, S22-S11)
  if (all(colSums(abs(mat)) > eps)) {
    rel <- mat[,1] / mat[,2]
    rel <- rel[!is.na(rel)]
    if (length(rel) >= 2) {
      if (any(!is.finite(rel))) return(FALSE)
      if (stats::var(rel) > eps) return(FALSE)
    }
  }
  t11 <- 0
  t12 <- 1
  t21 <- mean(mat[2,] / mat[1,], na.rm = TRUE)
  t22 <- mean(mat[3,] / mat[1,], na.rm = TRUE)
  if (max(c(t21, t22))-eps < t11 || min(c(t21, t22))+eps > t12) return(FALSE)
  return(TRUE)
}

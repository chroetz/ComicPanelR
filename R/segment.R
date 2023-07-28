idPanelsFromPanelGraph <- function(idGraph, idSegments) {
  panels <-
    idGraph |>
    arrange(panelId, sideId) |>
    mutate(segment = idSegments[segmentId, ]) |>
    select(panelId, segment) |>
    nest(segments = segment, .by = panelId) |>
    rowwise() |>
    mutate(vertexIds = list(getVertexSequenceFromSegments(segments |> as.matrix()))) |>
    ungroup()
  return(panels$vertexIds)
}


orderSegments <- function(segments) {
  ordering <- getSegmentOrdering(segments)
  segmentsOrdered <- lapply(
    seq_len(nrow(ordering)),
    \(i) {
      if (ordering$reverse[i]) {
        segments[ordering$rowIdxs[i], 2:1]
      } else {
        segments[ordering$rowIdxs[i], 1:2]
      }
    }
  )
  matrix(unlist(segmentsOrdered), byrow=TRUE, ncol=2)
}


getVertexSequenceFromSegments <- function(segments) {
  segments |>
    orderSegments() |>
    as.vector() |>
    unique()
}

getSegmentOrdering <- function(segments) {
  rowIdxs <- 1
  colIdxs <- 1
  current <- segments[1, 2]
  for (i in seq_len(nrow(segments)-1)) {
    candidates <- which(segments == current, arr.ind=TRUE)
    sel <- !(candidates[, "row"] %in% rowIdxs)
    nextRowIdx <- candidates[sel, "row"]
    colIdxs <- c(colIdxs, candidates[sel, "col"])
    rowIdxs <- c(rowIdxs, nextRowIdx)
    current <- setdiff(segments[nextRowIdx, ], current)
  }
  return(tibble(rowIdxs = unname(rowIdxs), reverse = unname(colIdxs == 2)))
}


idPanels2panelGraph <- function(idPanels, vertices) {

  rectGraph <- getPanelGraph(idPanels)

  segments <- rectGraph$idSegments

  while(TRUE) {
    found <- FALSE
    for (i in seq_len(nrow(segments)-1)) {
      s1 <- segments[i,]
      for (j in (i+1):nrow(segments)) {
        s2 <- segments[j,]
        newSegments <- overlapSegments(s1, s2, idPanels, vertices)
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

  sortedSides <- rectGraph$idSegments[rectGraph$idGraph$segmentId, ]

  segmentIdxInSide <- lapply(
    seq_len(nrow(sortedSides)),
    \(i) which(segmentContains(sortedSides[i,], segments, idPanels, vertices)))

  idGraph <-
    rectGraph$idGraph |>
    mutate(segmentId = segmentIdxInSide) |>
    unnest_longer(segmentId) |>
    arrange(panelId, sideId)

  return(list(idGraph = idGraph, idSegments = segments))
}


uniqueRows <- function(mat) {
  eps <- sqrt(.Machine$double.eps)
  n <- nrow(mat)
  isSame <- matrix(
    rowSums(abs(mat[rep(1:n, each=n),] - mat[rep(1:n, times=n),])) < eps,
    nrow = n)
  representativeIdx <- apply(isSame, 1, which.max)
  sel <- representativeIdx == 1:n
  uniqueRows <- mat[sel, ]
  return(uniqueRows)
}


overlapSegments <- function(s1, s2, idPanels, vertices) {
  eps <- sqrt(.Machine$double.eps)
  S11 <- vertices[s1[1], ]
  S12 <- vertices[s1[2], ]
  S21 <- vertices[s2[1], ]
  S22 <- vertices[s2[2], ]
  mat <- rbind(S12-S11, S21-S11, S22-S11)
  mat[abs(mat) < eps] <- 0
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


segmentContains <- function(seg, others, idPanels, vertices) {
  sapply(seq_len(nrow(others)), \(i) segmentContainsOne(seg, others[i,], idPanels, vertices))
}

segmentContainsOne <- function(s1, s2, idPanels, vertices) {
  if (all(s1 == s2)) return(TRUE)
  eps <- sqrt(.Machine$double.eps)
  S11 <- vertices[s1[1], ]
  S12 <- vertices[s1[2], ]
  S21 <- vertices[s2[1], ]
  S22 <- vertices[s2[2], ]
  mat <- rbind(S12-S11, S21-S11, S22-S11)
  mat[abs(mat) < eps] <- 0
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


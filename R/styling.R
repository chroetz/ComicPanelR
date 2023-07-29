#' @export
createPanelsWithEffects <- function(fileInPan, fileInPanels, fileInEffects, fileOutPng, fileOutRds) {
  pan <- readRDS(fileInPan)
  panels <- readRDS(fileInPanels)

  effectList <- ConfigOpts::readOpts(fileInEffects, c("Effect", "List"))
  decoratedPanels <- applyEffectsToPanels(panels, effectList, pan)

  renderPanels(
    decoratedPanels,
    pan,
    fileOutPng,
    drawBorder=FALSE,
    drawSegText=FALSE,
    drawPanelText=FALSE)

  saveRDS(decoratedPanels, fileOutRds)
}


applyEffectsToPanels <- function(panels, effects, pan) {
  browser()
  for (effect in effects) {
    panels <- applyEffectToPanels(panels, effect, pan)
  }
  return(panels)
}

applyEffectToPanels <- function(panels, effect, pan) {
  panel <-
    panels |>
    filter(panelId == effect$panelId)
  otherPanels <-
    panels |>
    anti_join(panel, by = join_by(panelId, segmentId))
  panel <- applyEffectToPanel(panel, effect, pan)
  outPanels <-
    otherPanels |>
    bind_rows(panel) |>
    arrange(panelId, sideId)
  return(outPanels)
}

applyEffectToPanel <- function(panel, effect, pan) {
  className <- ConfigOpts::getClassAt(effect, 2)
  panel <- switch(
    className,
    "ToPageEnd" = effectToPageEnd(panel, effect, pan),
    "Adjust" = effectAdjust(panel, effect, pan),
    "Deco" = effectDeco(panel, effect, pan),
    "Remove" = NULL,
    stop(paste0("Unknown Effect ", className))
  )
  return(panel)
}

effectToPageEnd <- function(panel, effect, pan) {
  effect <- ConfigOpts::asOpts(effect, c("ToPageEnd", "Effect"))
  n <- nrow(panel)
  for (segId in effect$sideIds) {
    i <- which(panel$segmentId == segId)
    iPrev <- i-1
    iNext <- i+1
    if (iPrev <= 0) iPrev <- n
    if (iNext > n) iNext <- 1
    panel$inner[[i]] <- moveToPagedEnd(panel$inner[[i]], pan)
    panel$inner[[iPrev]] <- rbind(panel$inner[[iPrev]], panel$inner[[i]][1,])
    panel$inner[[iNext]] <- rbind(panel$inner[[i]], panel$inner[[iNext]])
  }
  return(panel)
}


effectAdjust <- function(panel, effect, pan) {
  effect <- ConfigOpts::asOpts(effect, c("Adjust", "Effect"))
  path <- do.call(rbind, panel$inner)
  polyTmp <- geos_make_polygon(path[,1], path[,2], ring_id = 1)
  poly <- geos_make_valid(polyTmp)
  center <- unlist(wk::wk_coords(geos_centroid(poly))[,c("x", "y")])
  a <- effect$rotate / 360 * 2 * pi
  rotationMat <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), nrow=2)
  for (i in seq_len(nrow(panel))) {
    panel$inner[[i]] <- (effect$scale * (panel$inner[[i]] %-% center) %*% rotationMat) %+% center %+% effect$translate
  }
  return(panel)
}



effectDeco <- function(panel, effect, pan) {
  effect <- ConfigOpts::asOpts(effect, c("Deco", "Effect"))
  path <- do.call(rbind, panel$inner)
  path <- sinifyPath(path, effect$frequency, effect$amplitude, 1e3)
  panel$inner <- splitPathToSegment(path, panel)
  return(panel)
}

splitPathToSegment <- function(path, panel) {
  n <- nrow(panel)
  points <- geos_make_point(path[,1], path[,2])
  dsts <- sapply(panel$side, \(seg) {
    segment <- geos_make_linestring(seg[,1], seg[,2])
    geos_distance(segment, points)
  })
  minIdx <- apply(dsts, 1, which.min)
  idxs <- minIdx
  runs <- rle(c(minIdx, minIdx))
  for (k in 1:length(runs$values)) {
    runs <- rle(idxs)
    runs$lengths <- runs$lengths[!is.na(runs$values)]
    runs$values <- runs$values[!is.na(runs$values)]
    if (length(unique(runs$values)) == length(runs$values)) break
    if (length(unique(runs$values)) == length(runs$values) - 1 && runs$values[1] == runs$values[length(runs$values)]) break
    maxLen <- sapply(seq_len(n), \(i) max(runs$lengths[runs$values == i]))
    relLen <- runs$lengths / maxLen[runs$values]
    i <- whichRle(runs, which.min(relLen))
    nextI <- max(i)+1
    if (nextI > length(idxs)) nextI <- 1
    idxs[i] <- idxs[nextI]
  }
  pathIdx <- lapply(seq_len(n), \(i) which(idxs==i))
  pathIdx <- lapply(pathIdx, \(idx) {
    if (length(idx) == 0) return(idx)
    k <- which(diff(idx) != 1)
    stopifnot(length(k) < 2)
    if (length(k) == 0) return(idx)
    idx[c((k+1):length(idx), 1:k)]
  })
  segs <- lapply(pathIdx, \(idx) path[idx,])
  return(segs)
}

whichRle <- function(runs, i) {
  runs$values <- seq_along(runs$lengths)
  which(inverse.rle(runs) == i)
}

moveToPagedEnd <- function(path, pan) {
  r <- pan$geometry$bleed + pan$geometry$sideMargin
  n <- nrow(path)
  ends <- path[c(1,n), ]
  del <- abs(path[n,]-path[1,])
  center <- colMeans(ends)
  if (del[1] > del[2]) {
    if (center[2] < pan$geometry$size$h / 2) {
      # move to top
      ends[,2] <- -r
    } else {
      # move to bottom
      ends[,2] <- pan$geometry$size$h+r
    }
  } else {
    if (center[1] < pan$geometry$size$w / 2) {
      # move to left
      ends[,1] <- -r
    } else {
      # move to right
      ends[,1] <- pan$geometry$size$w+r
    }
  }
  return(ends)
}

endDirection <- function(path) {
  n <- nrow(path)
  if (n < 2) return(c(0, 0))
  v <- path[n,]-path[n-1,]
  v / sqrt(sum(v^2))
}

startDirection <- function(path) {
  n <- nrow(path)
  if (n < 2) return(c(0, 0))
  v <- path[1,]-path[2,]
  v / sqrt(sum(v^2))
}


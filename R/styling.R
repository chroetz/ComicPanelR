applyEffectsToPanels <- function(panels, effects, pan) {
  for (effect in effects) {
    panels <- applyEffectToPanels(panels, effect, pan)
  }
  return(panels)
}

applyEffectToPanels <- function(panels, effect, pan) {
  effected <- getEffected(effect, pan)
  newSides <-
    panels |>
    semi_join(effected, by = join_by(panelId, segmentId)) |>
    rowwise() |>
    mutate(inner = list(applyEffectToPath(inner, effect, pan)))
  panels <-
    panels |>
    anti_join(effected, by = join_by(panelId, segmentId)) |>
    bind_rows(newSides)
  return(panels)
}

applyEffectToPath <- function(path, effect, pan) {
  className <- ConfigOpts::getClassAt(effect, 2)
  path <- switch(
    className,
    "ToPageEnd" = effectToPageEnd(path, effect, pan),
    stop(paste0("Unknown Effect ", className))
  )
  return(path)
}

effectToPageEnd <- function(path, effect, pan) {
  r <- pan$geometry$sideMargin + pan$geometry$bleed
  switch(
    effect$side,
    "top" = {path[,2] <- -r},
    "bottom" = {path[,2] <- pan$geometry$size$h+r},
    "left" = {path[,1] <- -r},
    "right" = {path[,1] <- pan$geometry$size$w+r})
  return(path)
}

getEffected <- function(effect, pan) {
  if (length(effect$sideIds) == 1 && effect$sideIds == 0) {
    effected <-
      pan$idGraph |>
      filter(panelId %in% effect$panelIds) |>
      select(panelId, segmentId)
  } else {
    effected <-
      pan$idGraph |>
      filter(panelId %in% effect$panelIds & segmentId %in% effect$sideIds) |>
      select(panelId, segmentId)
  }
  return(effected)
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

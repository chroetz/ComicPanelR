getFrameStyles <- function(opts, pan) {
  opts <- ConfigOpts::asOpts(opts, "Render")
  frames <-
    pan$idGraph |>
    select(panelId, segmentId) |>
    mutate(frameOpts = list(opts$default))
  if (length(opts$panels) > 0) {
    panelFrames <-
      tibble(
        id = names(opts$panels),
        panelFrameOpts = opts$panels
      ) |>
      mutate(panelId = as.integer(str_extract(id, "[0-9]+", ))) |>
      select(panelId, panelFrameOpts)
    frames <-
      frames |>
      left_join(panelFrames, by="panelId") |>
      mutate(frameOpts = ifelse(sapply(panelFrameOpts, length) == 0, frameOpts, panelFrameOpts))
  }
  if (length(opts$sides) > 0) {
    sideFrames <-
      tibble(id = names(opts$sides), sideFrameOpts = opts$sides) |>
      mutate(
        panelId = as.integer(str_extract(id, "(?<=[pP])[0-9]+")),
        segmentId = as.integer(str_extract(id, "(?<=[sS])[0-9]+"))) |>
      select(panelId, segmentId, sideFrameOpts)
    frames <-
      frames |>
      left_join(sideFrames, by=c("panelId", "segmentId")) |>
      mutate(frameOpts = ifelse(sapply(sideFrameOpts, length) == 0, frameOpts, sideFrameOpts))
  }
  frames <-
    frames |>
    select(panelId, segmentId, frameOpts)
  return(frames)
}



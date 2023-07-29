#' @export
createMerged <- function(path = ".") {
  suffix <- "_gutter_image.png"
  name <-
    dir(
      path = path,
      pattern = paste0(suffix, "$")
    ) |>
    str_sub(end = -(nchar(suffix)+1))
  stopifnot(length(name) == 1)
  merge(name)
}

merge <- function(name) {
  panelImages <- dir(pattern=paste0(name,"_[0-9]+_image\\.png$")) |> sort()
  panelPositives <- dir(pattern=paste0(name,"_[0-9]+_positive\\.png$")) |> sort()
  stopifnot(length(panelImages) == length(panelPositives))
  n <- length(panelImages)

  page <- image_read(paste0(name,"_gutter_image.png"))

  for (i in seq_len(n)) {
    positive <- image_read(panelPositives[i])
    image <- image_read(panelImages[i])
    panel <- image_composite(
      image,
      positive,
      operator = "CopyOpacity")
    page <- image_composite(
        page,
        panel,
        operator="over")
  }

  page <- image_composite(
    page,
    image_read(paste0(name,"_belowgutter_image.png")),
    operator="over")

  positive <- image_read(paste0(name,"_gutter_positive.png"))
  image <- image_read(paste0(name,"_gutter_image.png"))
  gutter <- image_composite(
    image,
    positive,
    operator = "CopyOpacity")

  page <- image_composite(
    page,
    gutter,
    operator="over")

  positive <- image_read(paste0(name,"_frame_positive.png"))
  image <- image_read(paste0(name,"_frame_image.png"))
  frame <- image_composite(
    image,
    positive,
    operator = "CopyOpacity")
  page <- image_composite(
    page,
    frame,
    operator="over")

  page <- image_composite(
    page,
    image_read(paste0(name,"_abovegutter_image.png")),
    operator="over")

  image_write(page, paste0(name,".png"), format="png")
}

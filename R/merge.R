#' @export
createMerged <- function() {
  files <- getFiles("mergeinfo", "json")
  for (i in seq_len(nrow(files))) {
    createMergedOne(
      fileInMerge = files$file[i])
  }
}

createMergedOne <- function(fileInMerge) {
  mergeInfo <- jsonlite::read_json(fileInMerge)
  merge(mergeInfo)
}

merge <- function(info) {

  page <- image_graph(
    width = info$width,
    height = info$height,
    res = info$dpi,
    bg = "#FFFFFF")
  graphics::par(mar = c(0,0,0,0), xaxs = "i", xaxt="n", yaxs = "i", yaxt="n", bg="#FFFFFF")
  graphics::plot.new()
  grDevices::dev.off()
  page <- image_convert(
    page,
    format = "tiff",
    type = "ColorSeparationAlpha",
    colorspace = "cmyk",
    depth = 8,
    antialias = TRUE,
    matte = TRUE)

  for (nf in info$panels) {
    positive <- readMagickImage(nf$positive)
    image <- readMagickImage(nf$image)
    panel <- image_composite(
      image,
      positive,
      operator = "CopyOpacity")
    rm(positive,image);gc()
    page <- image_composite(
        page,
        panel,
        operator="over")
    rm(panel);gc()
  }

  page <- image_composite(
    page,
    readMagickImage(info$belowGutter),
    operator="over")
  gc()

  positive <- readMagickImage(info$gutter$positive)
  image <- readMagickImage(info$gutter$image)
  gutter <- image_composite(
    image,
    positive,
    operator = "CopyOpacity")
  rm(positive,image);gc()

  page <- image_composite(
    page,
    gutter,
    operator="over")
  rm(gutter);gc()

  positive <- readMagickImage(info$frame$positive)
  image <- readMagickImage(info$frame$image)
  frame <- image_composite(
    image,
    positive,
    operator = "CopyOpacity")
  rm(positive,image);gc()
  page <- image_composite(
    page,
    frame,
    operator="over")
  rm(frame);gc()

  page <- image_composite(
    page,
    readMagickImage(info$aboveGutter),
    operator="over")
  gc()

  writeMagickImage(page, info$out)
  rm(page);gc()

  return(invisible())
}

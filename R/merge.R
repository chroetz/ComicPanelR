#' @export
createMerged <- function() {
  mergeInfo <- jsonlite::read_json("mergeinfo.json")
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
    positive <- image_read(nf$positive)
    image <- image_read(nf$image)
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
    image_read(info$belowGutter),
    operator="over")
  gc()

  positive <- image_read(info$gutter$positive)
  image <- image_read(info$gutter$image)
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

  positive <- image_read(info$frame$positive)
  image <- image_read(info$frame$image)
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
    image_read(info$aboveGutter),
    operator="over")
  gc()

  writeMagickImage(page, info$out)
  rm(page);gc()

  return(invisible())
}

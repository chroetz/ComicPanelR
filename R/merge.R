#' @export
createMerged <- function() {
  files <- getFiles("opt_08_merge", "json")
  for (i in seq_len(nrow(files))) {
    createMergedOne(
      fileInMerge = files$file[i])
  }
}

createMergedOne <- function(fileInMerge) {
  mergeInfo <- jsonlite::read_json(fileInMerge)
  merge(mergeInfo)
}

merge <- function(info, outPath = NULL) {
  belowPath <- createBelowGutter(info, NULL)
  abovePath <- createGutterAndAbove(info, NULL)
  if (is.null(outPath)) {
    outPath <- paste0(info$name, "_merged", ".tiff")
  }
  composeOver(abovePath, belowPath, outPath)
}


createBelowGutter <- function(info, outPath = NULL) {

  tmpPagePath <- tempfile(fileext = ".tiff")

  cat("creating blank canvas ...\n")
  createBlank(
    "white",
    info$width,
    info$height,
    dpi = info$dpi,
    fileName = tmpPagePath,
    overwrite = TRUE)

  for (nf in info$panels) {
    cat("adding", nf$image, "...\n")
    tmpPanelPath <- tempfile(fileext = ".tiff")
    createCutout(nf$positive, nf$image, tmpPanelPath)
    composeOver(tmpPanelPath, tmpPagePath, tmpPagePath)
  }

  cat("adding", info$belowGutter, "...\n")
  composeOver(info$belowGutter, tmpPagePath, tmpPagePath)

  finalizeMerge(tmpPagePath, info, outPath, suffix = "_merged_below")
  return(tmpPagePath)
}


createGutterAndAbove <- function(info, outPath = NULL) {

  tmpPagePath <- tempfile(fileext = ".tiff")

  cat("creating blank canvas ...\n")
  createBlank(
    '"#00000000"',
    info$width,
    info$height,
    dpi = info$dpi,
    fileName = tmpPagePath,
    overwrite = TRUE)

  cat("adding", info$gutter$image, "...\n")
  tmpGutterPath <- tempfile(fileext = ".tiff")
  createCutout(info$gutter$positive, info$gutter$image, tmpGutterPath)
  composeOver(tmpGutterPath, tmpPagePath, tmpPagePath)

  cat("adding", info$frame$image, "...\n")
  tmpFramePath <- tempfile(fileext = ".tiff")
  createCutout(info$frame$positive, info$frame$image, tmpFramePath)
  composeOver(tmpFramePath, tmpPagePath, tmpPagePath)

  cat("adding", info$aboveGutter, "...\n")
  composeOver(info$aboveGutter, tmpPagePath, tmpPagePath)

  finalizeMerge(tmpPagePath, info, outPath, suffix = "_merged_above")
  return(tmpPagePath)
}


finalizeMerge <- function(tmpPagePath, info, outPath=NULL, suffix = "") {
  setImageMeta(tmpPagePath, dpi=info$dpi)
  if (is.null(outPath)) {
    outPath <- paste0(info$name, suffix, ".pdf")
  }
  cat("creating output file", outPath, "...\n")
  magickFormat(tmpPagePath, outPath)
  cat("Done.\n")
  return(invisible())
}

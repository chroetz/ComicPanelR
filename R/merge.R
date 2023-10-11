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

merge <- function(info, until = NULL, outPath = NULL) {

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

  if (identical(until, "belowGutter")) {
    finalizeMerge(tmpPagePath, info, outPath)
    return(invisible())
  }

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

  finalizeMerge(tmpPagePath, info, outPath)
  return(invisible())
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

  finalizeMerge(tmpPagePath, info, outPath)
  return(invisible())
}


finalizeMerge <- function(tmpPagePath, info, outPath=NULL) {
  setImageMeta(tmpPagePath, dpi=info$dpi)
  if (!is.null(outPath)) {
    cat("creating output file", outPath, "...\n")
    file.rename(tmpPagePath, outPath)
  } else {
    cat("creating output file", info$out, "...\n")
    file.rename(tmpPagePath, info$out)
  }

  cat("Done.\n")
  return(invisible())
}

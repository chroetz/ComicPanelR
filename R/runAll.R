runAll <- function(path = ".") {
  wd <- getwd()
  on.exit(setwd(wd))
  folders <- list.dirs(path) |> normalizePath(winslash="/")
  for (folder in folders) {
    print(folder)
    setwd(folder)
    removeHere()
    createBaseParti()
    createTransformedParti()
    createDecoratedPan()
    createPanels()
    createPanelsWithEffects()
    createStencils()
    createMerged()
  }
}

removeHere <- function() {
  tiffs <- c(
    "_[0-9]{3}_image",
    "_[0-9]{3}_positive",
    "_[0-9]{3}_negative",
    "_abovegutter_image",
    "_all_negative",
    "_all_positive",
    "_belowgutter_image",
    "_frame_image",
    "_frame_negative",
    "_frame_positive",
    "_gutter_image",
    "_gutter_negative",
    "_gutter_positive")
  stores <- "^store_[0-9]{2}_.*\\.RDS$"
  preview <- "^preview_[0-9]{2}_.*\\.png$"
  merge <- "mergeinfo.*\\.json"
  pattern <- paste0("(" , c(paste0(tiffs, "\\.tiff$"), stores, preview, merge), ")", collapse="|")
  oldFiles <- dir(pattern = pattern)
  file.remove(oldFiles)
}

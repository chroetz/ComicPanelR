annotate <- function(filePath, panAndPanelFileName = "store_05_panels-effect.RDS") {
  panAndPanel <- readRDS(panAndPanelFileName)
  filePath <- normalizePath(filePath, mustWork=TRUE)
  fileName <- basename(filePath)
  path <- dirname(filePath)
  fileNameWoEnding <- stringr::str_remove(fileName, "\\.[^.]*$")
  fileNameEnding <- stringr::str_sub(fileName, start=nchar(fileNameWoEnding)-nchar(fileName))
  # TODO
  sprintf('magick %s -stroke "#00ff00" -draw "line 53,0 53,4713 line 3030,0 3030,4713 line 0,53 3083,53 line 0,4660 3083,4660" -stroke "#0000ff" -draw "line 106,0 106,4713 line 2977,0 2977,4713 line 0,106 3083,106 line 0,4607 3083,4607" %s %s',
    filePath,
    .formatString,
    file.path(path, paste0(fileNameWoEnding, "_annot", fileNameEnding))
  ) |>
    system()
}

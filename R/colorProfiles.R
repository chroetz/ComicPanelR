getColorProfilePath <- function() {
  system.file("colorProfiles", "ISOcoated_v2_300_eci.icc", package="ComicPanelR")
}

checkColorProfile <- function(fileName) {
  tmpFileName <- tempfile(fileext = ".icc")
  profileCmdResult <-
    sprintf('magick "%s" "%s"', fileName, tmpFileName) |>
    system(intern = TRUE) |>
    suppressWarnings()
  if (length(profileCmdResult) > 0) {
    if (startsWith(profileCmdResult, "magick.exe: no color profile is available"))
      return("No color profile.")
  }
  if (!file.exists(tmpFileName)) {
    pattern <- paste0(
        "*",
        stringr::str_sub(basename(tmpFileName), end=-5),
        "-[0-9]+",
        "\\.icc")
    profiles <- dir(
      dirname(tmpFileName),
      pattern = pattern)
    return(sprintf("Detected %d profiles.", length(profiles)))
  }
  comparison <-
    sprintf(
      'fc.exe /b "%s" "%s"',
      tmpFileName,
      getColorProfilePath()
    ) |>
    system(intern = TRUE) |>
    suppressWarnings()
  if (length(comparison) >= 2) {
    if (comparison[2] == "FC: no differences encountered") return(TRUE)
    first <- strsplit(comparison[2], ":")[[1]][1]
    if (as.integer(first) == 43) return(TRUE)
  }
  return(comparison)
}

checkAllColorProfiles <- function(path = ".") {
  fileNames <- dir(
    path = path,
    pattern="(\\.tif$)|(\\.tiff$)",
    full.names = TRUE)
  for (fn in fileNames) {
    cat(sprintf("Checking %s... ", fn))
    check <- checkColorProfile(fn)
    if (isTRUE(check)) {
      cat("ok\n")
      next
    }
    cat("\n", paste(check, collapse="\n"), "\n")
  }
}

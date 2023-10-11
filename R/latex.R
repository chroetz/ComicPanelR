getTikzCalloutOnePath <- function() {
  system.file("tex", "tikzCalloutOne.tex", package="ComicPanelR")
}

getTikzCalloutPagePath <- function() {
  system.file("tex", "tikzCalloutPage.tex", package="ComicPanelR")
}

getTextLengthTexPath <- function() {
  system.file("tex", "textLength.tex", package="ComicPanelR")
}

runLualatex <- function(filePath, times=1) {
  for (i in seq_len(times)) {
    sprintf('lualatex.exe -synctex=1 -interaction=nonstopmode "%s"', filePath) |>
      system()
  }
}

getPatterns <- function(templatePath) {
  template <- readLines(templatePath)
  patterns <-
    template |>
    str_extract_all("~[^~]+~") |>
    unlist() |>
    unique() |>
    str_sub(2, -2)
  return(patterns)
}

writeTemplate <- function(vars, outPath, templatePath) {
  template <- readLines(templatePath)
  for (nm in names(vars)) {
    template <- str_replace_all(template, fixed(sprintf("~%s~", nm)), vars[[nm]])
  }
  writeLines(template, outPath)
}

getTextWidth <- function(text) {
  tmpPath <- tempdir()
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(tmpPath)
  content <- paste0(
    r"(\settowidth{\textwidthlength}{)",
    text,
    "}\n",
    r"(\immediate\write\myfile{width=\the\textwidthlength})",
    collapse="\n")
  writeTemplate(
    list(content=content, fileName="textLengthResult.txt"),
    "textLengthInstance.tex",
    getTextLengthTexPath())
  runLualatex("textLengthInstance.tex")
  res <- readLines("textLengthResult.txt")
  textWidthCm <-
    res |>
    str_extract("\\d+(\\.\\d+)?") |>
    as.numeric() |>
    (`*`)(.cmPerPt)
  return(textWidthCm)
}

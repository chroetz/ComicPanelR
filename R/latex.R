getTikzCalloutOnePath <- function() {
  system.file("tex", "tikzTextPage.tex", package="ComicPanelR")
}

getTikzCalloutPagePath <- function() {
  system.file("tex", "tikzTextPage.tex", package="ComicPanelR")
}

getTextLengthTexPath <- function() {
  system.file("tex", "textLength.tex", package="ComicPanelR")
}

runLualatex <- function(filePath, times=1, doNotRender=FALSE) {
  for (i in seq_len(times)) {
    if(doNotRender) {
      sprintf('lualatex.exe --synctex=0 --draftmode --interaction=nonstopmode "%s"', filePath) |>
        system(ignore.stdout = TRUE)
    } else {
      sprintf('lualatex.exe --synctex=1 --interaction=nonstopmode "%s"', filePath) |>
        system(ignore.stdout = TRUE)
    }
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

getTextLengths <- function(text, textOpts, fontOpts) {
  tmpPath <- tempdir()
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(tmpPath)
  texBase <- r"(\setto%s{\textlength}{%s}\immediate\write\myfile{%s(%s)=\the\textlength})"
  txtData <- expand.grid(
    kind = c("width", "height", "depth"),
    text = text,
    stringsAsFactors=FALSE)
  content <- paste0(
    sprintf(texBase, txtData$kind, txtData$text, txtData$kind, txtData$text),
    collapse="\n")
  writeTemplate(
    list(
      content = content,
      fileName = "textLengthResult.txt",
      font = fontOpts$font,
      fontSizePt = fontOpts$fontSizePt,
      lineHeightPt = fontOpts$lineHeightPt),
    "textLengthInstance.tex",
    getTextLengthTexPath())
  runLualatex("textLengthInstance.tex", doNotRender = TRUE)
  res <- readLines("textLengthResult.txt")
  mat <- str_match(res, r"(^(\w+)\((.*)\)=(\d+\.?\d*)pt$)")[,-1]
  colnames(mat) <- c("kind", "text", "length")
  lenInCmTable <-
    mat |>
    tibble::as_tibble() |>
    mutate(length = as.numeric(length) * .cmPerPt)
  return(lenInCmTable)
}

getPanelColors <- function(n, alpha=0.3) {
   goodColors <- c(
    "#e6194B", "#3cb44b", "#4363d8", "#f58231", "#911eb4", "#469990",
    "#9A6324", "#800000", "#808000", "#000075", "#f032e6", "#ffd610",
    "#404040", "#42d4f4", "#bfef45", "#B0B0B0", "#dcbeff", "#aaffc3",
    "#fabed4")
  if (n > length(goodColors)) {
    delta <- n - length(goodColors)
    oldRandomSeed <- get(
      ".Random.seed",
      globalenv(),
      mode = "integer",
      inherits = FALSE)
    set.seed(0)
    rgbValues <- matrix(stats::runif(3 * delta), nrow = 3)
    assign(".Random.seed", oldRandomSeed, globalenv())
    moreColors <- grDevices::rgb(
      red = rgbValues[1, ],
      green = rgbValues[2, ],
      blue = rgbValues[3, ])
  } else {
    moreColors <- character(0)
  }
  colors <- c(goodColors, moreColors)[seq_len(n)]
  colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
  return(colors)
}

getFilePairs <- function(prefix1, ending1, prefix2, ending2, path = ".") {
  files1 <- dir(path = path, pattern = paste0("^", prefix1, ".*\\.", ending1, "$"))
  files2 <- dir(path = path, pattern = paste0("^", prefix2, ".*\\.", ending2, "$"))
  suffix1 <- str_match(files1, paste0("^", prefix1, "(.*)\\.", ending1, "$"))[,2]
  suffix2 <- str_match(files2, paste0("^", prefix2, "(.*)\\.", ending2, "$"))[,2]
  commonSuffix <- intersect(suffix1, suffix2)
  out <- tibble(
    suffix = commonSuffix,
    file1 = file.path(path, paste0(prefix1, commonSuffix, ".", ending1)),
    file2 = file.path(path, paste0(prefix2, commonSuffix, ".", ending2)))
  if ("" %in% suffix1) {
    noMatchSuffix2 <- setdiff(suffix2, commonSuffix)
    out <- bind_rows(out, tibble(
      suffix = noMatchSuffix2,
      file1 = file.path(path, paste0(prefix1, ".", ending1)),
      file2 = file.path(path, paste0(prefix2, noMatchSuffix2, ".", ending2))))
  }
  if ("" %in% suffix2) {
    noMatchSuffix1 <- setdiff(suffix1, commonSuffix)
    out <- bind_rows(out, tibble(
      suffix = noMatchSuffix1,
      file1 = file.path(path, paste0(prefix1, noMatchSuffix1, ".", ending1)),
      file2 = file.path(path, paste0(prefix2, ".", ending2))))
  }

  # check
  for (i in seq_len(nrow(out))) {
    stopifnot(file.exists(out$file1[i]))
    stopifnot(file.exists(out$file2[i]))
  }

  return(out)
}

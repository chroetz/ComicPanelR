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
  for (s1 in setdiff(suffix1, commonSuffix)) {
    s2 <- findMatch(s1, suffix2)
    if (length(s2) == 0) next
    out <- bind_rows(out, tibble(
      suffix = s1,
      file1 = file.path(path, paste0(prefix1, s1, ".", ending1)),
      file2 = file.path(path, paste0(prefix2, s2, ".", ending2))))
  }
  for (s2 in setdiff(suffix2, commonSuffix)) {
    s1 <- findMatch(s2, suffix1)
    if (length(s1) == 0) next
    out <- bind_rows(out, tibble(
      suffix = s2,
      file1 = file.path(path, paste0(prefix1, s1, ".", ending1)),
      file2 = file.path(path, paste0(prefix2, s2, ".", ending2))))
  }

  # check
  for (i in seq_len(nrow(out))) {
    stopifnot(file.exists(out$file1[i]))
    stopifnot(file.exists(out$file2[i]))
  }

  return(out)
}

# The match is the longest prefix of `query` that is contained in `candidates`.
findMatch <- function(query, candiates) {
  sel <- startsWith(query, candiates)
  x <- candiates[sel]
  if (length(x) == 0) return(NULL)
  return(x[nchar(x) == max(nchar(x))])
}


getFiles <- function(prefix, ending, path = ".") {
  files <- dir(path = path, pattern = paste0("^", prefix, ".*\\.", ending, "$"))
  suffix <- str_match(files, paste0("^", prefix, "(.*)\\.", ending, "$"))[,2]
  out <- tibble(
    suffix = suffix,
    file = file.path(path, paste0(prefix, suffix, ".", ending)))
  return(out)
}

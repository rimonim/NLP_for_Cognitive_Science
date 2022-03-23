# debugging the cosine similarity function
my_textstat_proxy <- function (x, y = NULL, margin = c("documents", "features"), 
                               method = c("cosine", "correlation", "jaccard", "ejaccard", "dice", "edice", 
                                          "hamman", "simple matching", "euclidean", "chisquared", "hamming", 
                                          "kullback", "manhattan", "maximum", "canberra", "minkowski"), 
                               p = 2, min_proxy = NULL, rank = 100, use_na = FALSE) 
{
  x <- as.dfm(x)
  if (is.null(y)) {
    y <- x
  }
  else {
    if (!is.dfm(y)) 
      stop("y must be a dfm")
    y <- as.dfm(y)
  }
  margin <- match.arg(margin)
  method <- match.arg(method)
  if (margin == "documents") {
    f <- union(featnames(x), featnames(y))
    x <- t(pad_dfm(x, f))
    y <- t(pad_dfm(y, f))
  }
  else {
    if (!identical(docnames(x), docnames(y))) 
      stop("x and y must contain the same documents")
  }
  if (method %in% c("cosine", "correlation", "jaccard", "ejaccard", 
                    "dice", "edice", "hamman", "simple matching", "faith")) {
    if (identical(x, y)) {
      result <- simil(x, NULL, 2, method, min_simil = min_proxy, 
                      rank = rank)
    }
    else {
      result <- simil(x, y, 2, method, min_simil = min_proxy, 
                      rank = rank)
    }
  }
  else {
    if (identical(x, y)) {
      result <- dist(x, NULL, 2, method, p = p)
    }
    else {
      result <- dist(x, y, 2, method, p = p)
    }
  }
  dimnames(result) <- list(colnames(x), colnames(y))
  if (use_na) {
    if (method == "correlation") {
      na1 <- colSds(x) == 0
      na2 <- colSds(y) == 0
    }
    else {
      na1 <- colZeros(x) == nrow(x)
      na2 <- colZeros(y) == nrow(y)
    }
    if (any(na1) || any(na2)) 
      result <- result + make_na_matrix(dim(result), which(na1), 
                                        which(na2))
  }
  return(result)
}

# cosine similarity
tfidfsim <- function(dfmsource, x) {
  tfidfsim <- quanteda::dfm_subset(dfmsource, conversation == x)
  tfidfsim <- quanteda.textstats::textstat_simil(tfidfsim, margin = "documents", method = "cosine")
  tfidfsim <- as.matrix(tfidfsim)
}

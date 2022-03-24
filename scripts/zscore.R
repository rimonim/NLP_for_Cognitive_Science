# For my convenience

zscore <- function(x){
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
}
unzscore <- function(z, x){
  z*sd(x, na.rm = T) + mean(x, na.rm = T)
}

collapseturns <- function(convdf) {
  trashrows <- c()
  for(row in 2:nrow(convdf)){
    if(convdf[row, "participant"] == convdf[row - 1L, "participant"] & 
       convdf[row, "conversation"] == convdf[row - 1L, "conversation"]){
      convdf[row, "text"] <- paste(convdf[row - 1L, "text"], convdf[row, "text"])
      trashrows <- append(trashrows, row - 1L)
    }
  }
  convdf <- convdf[!(1:nrow(convdf) %in% trashrows), ]
  convdf
}

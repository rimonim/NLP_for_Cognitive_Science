## FUNCTION: combine adjacent turns by the same participant
  #> convdf: A dataframe including columns named "participant", "conversation", and "text"
  #> aggregate: A character vector of the names of additional columns to aggregate
  #> method: A character vector specifying the method of aggregation for each column in `aggregate`. 
     # Options: "sum", "mean", "any"
     # Anything else will default to keeping the value of the second turn.
  #> dropcols: Should the rest of the columns in `convdf` be dropped?

collapseturns <- function(convdf, aggregate, method, dropcols = TRUE) {
  trashrows <- c()
  for(row in 2:nrow(convdf)){
    if(convdf[row, "participant"] == convdf[row - 1L, "participant"] & 
       convdf[row, "conversation"] == convdf[row - 1L, "conversation"]){
      convdf[row, "text"] <- paste(convdf[row - 1L, "text"], convdf[row, "text"])
      for(col in 1:length(aggregate)){
        if(method[col] == "sum"){
          convdf[row, aggregate[col]] <- convdf[row-1L, aggregate[col]] + convdf[row, aggregate[col]]
        }
        if(method[col] == "mean"){
          convdf[row, aggregate[col]] <- mean(c(convdf[row-1L, aggregate[col]], convdf[row, aggregate[col]]), na.rm = T)
        }
        if(method[col] == "any"){
          convdf[row, aggregate[col]] <- any(convdf[row-1L, aggregate[col]], convdf[row, aggregate[col]])
        }
      }
      trashrows <- append(trashrows, row - 1L)
    }
  }
  convdf <- convdf[!(1:nrow(convdf) %in% trashrows), ]
  if(dropcols){
    convdf[, c("participant", "conversation", "text", aggregate)]
  }else{
    convdf
  }
}

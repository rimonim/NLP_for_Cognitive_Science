# For use with ggdag
# Make sure to include `aes(x = xstart, y = ystart)` in `geom_dag_edges()`

shorten_dag_arrows <- function(tidy_dag, shorten_distance){
  # Update underlying ggdag object
  tidy_dag$data <- dplyr::mutate(tidy_dag$data, 
                                 proportion = shorten_distance/sqrt((xend - x)^2 + (yend - y)^2),
                                 xend = (1-proportion/2)*(xend - x) + x, 
                                 yend = (1-proportion/2)*(yend - y) + y,
                                 xstart = (1-proportion/2)*(x - xend) + xend,
                                 ystart = (1-proportion/2)*(y-yend) + yend) %>% select(!proportion)
  return(tidy_dag)
}

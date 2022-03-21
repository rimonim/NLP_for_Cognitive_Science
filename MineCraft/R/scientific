### Scientific Model
library(ggdag)
# Length -> TF-IDF Sum -> Probability of Repair -> Question Mark at End
# Turns since Last Repair -> Probability of Repair -> Question Mark at End
dag_coords <-
  tibble(name = c("L", "T", "W", "R", "Q"),
         x    = c(1, 2, 2, 3, 4),
         y    = c(3, 3, 1, 2, 2))

dagify(T ~ L,
       R ~ W + T,
       Q ~ R,
       coords = dag_coords) %>%
  dag_label(labels = c("L" = str_wrap("Length of Previous Turn", 10), 
                       "T" = str_wrap("TF-IDF Sum of Previous Turn", 8), 
                       "W" = str_wrap("Turns since Repair", 15), 
                       "R" = "Repair", "Q" = str_wrap("Question Mark", 10))) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = name == "R"),
                   alpha = 1/2, size = 20, show.legend = F) +
    geom_dag_text(aes(label = label), color = "black", size = 2.5) +
    scale_x_continuous(NULL, breaks = NULL, expand = c(.1, .1)) +
    scale_y_continuous(NULL, breaks = NULL, expand = c(.1, .1)) +
    geom_dag_edges() +
    theme_dag()

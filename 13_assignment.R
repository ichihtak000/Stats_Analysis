library(tibble)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

data <-starwars %>%
  group_split(homeworld) %>%
  map(mutate, height_diff = height -mean(height)) %>%
  map(filter, str_detect(films,pattern="Return")) %>%
  map_dfr(select,name,height_diff,films)
  
class(data$height_diff)
data$films <- vapply(data$films, paste, collapse = ", ", character(1L))


write.csv(data, "71975110_ICHIHASHI_13.csv",
          quote=FALSE, row.names=FALSE) 

mean <- starwars %>%
  group_by(homeworld) %>%
  summarise(mean = mean(height))

dplyr:: starwars %>%
  left_join(mean,)
  

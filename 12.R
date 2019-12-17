library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)

dplyr::glimpse(iris)


iris %>% 
  dplyr::select(Sepal.Length)

iris %>% 
  select(1, 3)

iris %>% 
  select(1:3)

iris %>% 
  select(Sepal.Length:Petal.Length)

iris %>% 
  select(-Species)

iris %>% 
  select(-c(Sepal.Length, Petal.Length))

iris %>% 
  select(starts_with("petal"))

iris %>% 
  select(ends_with("Length", ignore.case = FALSE))

iris %>% 
  select(contains("al"))

iris %>%
  select(matches("i"))

var_list <- c("Sepal.Length", "Petal.Width")
iris %>% 
  select(one_of(var_list))

iris %>%
  select(var = Species)

iris %>%
  select(var = contains("al"))

dat <- iris %>% 
  select(var = contains("al"))
dat %>% 
  select(num_range(prefix = "var", 
                   range = 1:3, 
                   width = 1))


iris %>%
  dplyr::filter(Sepal.Length > 5.84)

iris %>% 
  filter(Species != "versicolor")

iris %>% 
  filter(between(Sepal.Length, 5, 6))

iris %>%
  filter(Species != "setosa" | Sepal.Length > 5.5)

iris %>% 
  filter(Species != "setosa" & Sepal.Length > 5.5)

iris %>%
  filter(xor(Sepal.Length > 5.84, Petal.Length >=3.76))

iris %>% 
  filter(Species %in% c("setosa", "virginica"))

iris %>% 
  dplyr::arrange(Sepal.Length)

iris %>%
  arrange(desc(Sepal.Length))

iris %>%
  arrange(Species, desc(Sepal.Length))

iris %>%
  dplyr::distinct(Species)

iris %>%
  distinct(Species, .keep_all = TRUE)

iris %>%
  sample_n(5, replace =TRUE)

iris %>%
  sample_frac(0.1, replace = TRUE)

iris %>%
  slice(10:15)

iris %>%
  top_n(6,Sepal.Width)


iris %>% 
  dplyr::mutate(twice = Sepal.Length * 2,
                square = Sepal.Length ^2)

iris %>% 
  dplyr::mutate(Sepal.Length = Sepal.Length * 2)

iris %>% 
  mutate(TF = if_else(condition = Sepal.Length > mean(iris$Sepal.Length),
                      true = TRUE, false = FALSE))

iris %>% 
  mutate(SpeciesNo = case_when(
    Species == "setosa" ~ "A",
    Species == "versicolor" ~ "B",
    Species == "virginica" ~ "C"
  ))

iris %>% 
  dplyr::mutate(row_number = row_number(Sepal.Length))

iris %>% 
  dplyr::mutate(min_rank = min_rank(Sepal.Length))

iris %>% 
  dplyr::mutate(percent_rank = percent_rank(desc(Sepal.Length)))

iris %>% 
  dplyr::mutate(cume_dist = cume_dist(desc(Sepal.Length)))

iris %>% 
  dplyr::mutate(ntile = ntile(Sepal.Length, 5))


iris %>% 
  tidyr::gather(key = variable, value = value, -Species)


dplyr::full_join(band_members, band_instruments, by = "name")


dplyr::left_join(band_members, band_instruments2,
                 by = c("name" = "artist"))

inner_join(band_members, band_instruments, by = "name")

semi_join(band_members, band_instruments, by = "name")

anti_join(band_members, band_instruments, by = "name")

bind_cols(band_instruments, band_instruments2)

bind_rows(band_instruments, band_instruments2)

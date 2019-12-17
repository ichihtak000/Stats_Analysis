
dplyr::glimpse(starwars)
starwars %>%
  filter(species == "Human" & (homeworld == "Tatooine" | homeworld == "Alderaan") & height > 160)
data <- starwars %>%
  filter(species == "Human" & (homeworld == "Tatooine" | homeworld == "Alderaan") & height > 160) %>% 
  mutate(BMI = (mass / height / height) * 10000) %>%
  select(name, contains("color"), BMI) %>%
  filter(BMI != "NA") %>%
  arrange(BMI)

write.csv(data, "71975110_ICHIHASHI.csv",
                 quote=FALSE, row.names=FALSE)  

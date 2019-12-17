library(datasets)

warpbreaks <- datasets::warpbreaks


library(dplyr)
glimpse(warpbreaks)

summary(warpbreaks)


warpbreaks %>% 
  summarise(mean = mean(breaks),
            var = var(breaks),
            ratio = var / mean)


library(ggplot2)

ggplot(warpbreaks, aes(breaks)) +
  geom_histogram()


ggplot(warpbreaks, aes(breaks, fill = wool)) +
  geom_histogram(position = "identity", alpha = 0.5)


ggplot(warpbreaks, aes(breaks, fill = tension)) +
  geom_histogram(position = "identity", alpha = 0.5)



wb.poisson <- glm(breaks~wool+tension, data=warpbreaks, 
                  family = poisson(link = "log"))


library(stargazer)
stargazer(wb.poisson, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)


predict(wb.poisson, newdata=data.frame(wool="A", tension="L"), type="response")


warpbreaks %>% 
  mutate(predict = predict(wb.poisson, type = "response", newdata = .)) %>% 
  ggplot(aes(breaks, predict, colour = tension)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

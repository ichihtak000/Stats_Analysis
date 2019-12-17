install.packages("ggplot2")
install.packages("GGally")
install.packages("betareg")
install.packages("Zelig")
install.packages("lme4")
install.packages("gamlss")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stargazer")

library(ggplot2)
library(GGally)
library(betareg)
library(Zelig)
library(lme4)
library(gamlss)
library(dplyr)
library(tidyr)
library(stargazer)

data("GasolineYield", package = "betareg")

summary(GasolineYield)

ggplot(GasolineYield, aes(yield)) +
  geom_histogram()

ggplot(GasolineYield, aes(temp)) +
  geom_histogram()

GasolineYield %>% 
  dplyr::select(yield, temp, batch) %>% 
  ggpairs()

gs.beta <- betareg(yield ~ batch + temp, data = GasolineYield)

stargazer(gs.beta, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

GasolineYield %>% 
  mutate(predict = predict(gs.beta, type = "response", newdata = .)) %>%
  ggplot(aes(yield, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")



data(coalition, package = "Zelig")

summary(coalition)

ggplot(coalition, aes(duration, ..density..)) + 
  geom_histogram() +
  geom_density(fill = "lightblue", alpha = 0.5)

ggplot(coalition, aes(fract, ..density..)) + 
  geom_histogram() +
  geom_density(fill = "lightblue", alpha = 0.5)

coalition %>% 
  dplyr::select(duration, fract, numst2) %>% 
  mutate_at(vars(numst2), as.factor) %>% 
  ggpairs()

coa.gam <- glm(duration ~ fract + numst2,
               data=coalition,
               family=Gamma)

stargazer(coa.gam, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

coalition %>% 
  mutate(predict = predict(coa.gam, type = "response", newdata = .)) %>% 
  ggplot(aes(duration, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")



df <- data.frame(
  norm    = rnorm(1000, mean = 10, sd = 1),
  exp     = rexp(1000,  rate = 0.5),
  exGauss = rexGAUS(2000, mu = 10, sigma = 1, nu = 1/0.5)) %>% 
  mutate(norm_exp = norm + exp) %>%
  gather() %>% 
  transform(key = factor(key,
                         levels = c("norm","exp","norm_exp","exGauss")))

ggplot(df, aes(value, fill = key)) +
  geom_histogram() +
  facet_wrap(~key)

data(sleepstudy, package = "lme4")

summary(sleepstudy)

ggplot(sleepstudy, aes(Reaction, ..density..)) + 
  geom_histogram() +
  geom_density(fill = "lightblue", alpha = 0.5)

ggplot(sleepstudy, aes(Days, Reaction)) + 
  geom_point()

sleep.model <- gamlss(Reaction ~ Days,
                      data=sleepstudy,
                      family=exGAUS, 
                      mu.fix=FALSE,
                      sigma.fix = FALSE,
                      nu.fix = FALSE)

summary(sleep.model)

sleepstudy %>% 
  mutate(predict = predict(sleep.model, type = "response", newdata = .)) %>% 
  ggplot(aes(Reaction, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

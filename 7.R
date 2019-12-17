install.packages("ggplot2")
install.packages("AER")
install.packages("MASS")
install.packages("pscl")
install.packages("GGally")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stargazer")

library(ggplot2)
library(AER)
library(MASS)
library(pscl)
library(GGally)
library(tidyr)
library(dplyr)
library(stargazer)

data("NMES1988")

summary(NMES1988)

NMES1988 %>% 
  summarise(mean = mean(visits), var = var(visits), ratio = var(visits) / mean(visits))

ggplot(NMES1988, aes(visits)) +
  geom_histogram()

NMES1988 %>% 
  dplyr::select(visits, hospital, health, age, income) %>% 
  ggpairs()


formula <- formula(visits ~ hospital + health + chronic + adl + 
                     region + age + afam + gender + married + 
                     school + income + insurance)
nmes.poi <- glm(formula, 
                data=NMES1988, 
                family = poisson(link = "log"))

stargazer(nmes.poi, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

NMES1988 %>% 
  mutate(predict = predict(nmes.poi, type = "response", newdata = .)) %>% 
  ggplot(aes(visits, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


c(obs=sum(NMES1988$visits==0), poi=sum(dpois(0,exp(predict(nmes.poi)))))

nmes.hpoi <- hurdle(formula, 
                    data=NMES1988,
                    dist="poisson", zero.dist="binomial")

stargazer(nmes.nb, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

NMES1988 %>% 
  mutate(predict = predict(nmes.nb, type = "response", newdata = .)) %>% 
  ggplot(aes(visits, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

c(obs=sum(NMES1988$visits==0), hpoi=sum(predict(nmes.hpoi,type="prob")[,1]))

nmes.hnb <- hurdle(formula, 
                   data=NMES1988,
                   dist="negbin", zero.dist="binomial")

stargazer(nmes.hnb, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)


NMES1988 %>% 
  mutate(predict = predict(nmes.hnb, type = "response", newdata = .)) %>% 
  ggplot(aes(visits, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

nmes.zip <- zeroinfl(formula, 
                     data=NMES1988,
                     dist="poisson")

stargazer(nmes.zip, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

NMES1988 %>% 
  mutate(predict = predict(nmes.zip, type = "response", newdata = .)) %>% 
  ggplot(aes(visits, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

c(obs=sum(NMES1988$visits==0), zip=sum(predict(nmes.zip,type="prob")[,1]))

nmes.zip <- zeroinfl(formula, 
                    data=NMES1988,
                    dist="poisson")

stargazer(nmes.zip, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

NMES1988 %>% 
  mutate(predict = predict(nmes.zip, type = "response", newdata = .)) %>% 
  ggplot(aes(visits, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")          

c(obs=sum(NMES1988$visits==0), zip=sum(predict(nmes.zip,type="prob")[,1]))

nmes.zinb <- zeroinfl(formula, 
                      data=NMES1988,
                      dist="negbin")

stargazer(nmes.zinb, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

NMES1988 %>% 
  mutate(predict = predict(nmes.zinb, type = "response", newdata = .)) %>% 
  ggplot(aes(visits, predict)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

c(obs=sum(NMES1988$visits==0), zinb=sum(predict(nmes.zinb,type="prob")[,1]))


stargazer(nmes.poi, nmes.nb, nmes.hpoi, nmes.hnb, nmes.zip, nmes.zinb, 
          type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

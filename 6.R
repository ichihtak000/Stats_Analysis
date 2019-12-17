library(ggplot2)
library(dplyr)
library(foreign)
library(car)
library(stargazer)
library(sandwich)


npub <- foreign::read.dta("http://www.stata-press.com/data/lf2/couart2.dta")


summary(npub)


npub %>% 
  summarise(mean = mean(art), var = var(art), ratio = var(art) / mean(art))


ggplot(npub, aes(art)) +
  geom_histogram()


ggplot(npub) + 
  geom_point(aes(ment, art))



npub.poisson1 <- glm(art ~ ., data=npub, 
                     family = poisson(link = "log"))
npub.poisson2 <- glm(art ~ fem + mar + kid5 + ment, data=npub, 
                     family = poisson(link = "log"))

stargazer(npub.poisson1, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

stargazer(npub.poisson2, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

ggplot(npub, aes(ment, art)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "poisson"))


logLik(npub.poisson1)

qchisq(0.95, df.residual(npub.poisson1))

npub.poisson1$deviance

sum((residuals(npub.poisson1,"pearson"))^2)


npub.poisson1$deviance/npub.poisson1$df.res

sum(residuals(npub.poisson1, type="pearson")^2)/npub.poisson1$df.res




anova(npub.poisson1, test="Chisq")

car::Anova(npub.poisson1, type=c("II"))

car::Anova(npub.poisson1, type=c("III"))


AIC(npub.poisson1, npub.poisson2)

anova(npub.poisson1, npub.poisson2, test="Chisq")


step(npub.poisson1)

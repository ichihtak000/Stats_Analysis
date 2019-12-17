#kakei <- readr::read_csv("data/kakei201709_l18.csv")
#dplyr::glimpse(kakei)
#summary(kakei)

#ggplot(data = kakei, 
#       mapping = aes(x=CONS1)) +
#  geom_histogram() +
#  xlab("Consumption (10000 yen)") +
#  ggtitle("Histogram")

#ggplot(data = kakei, 
#      mapping = aes(x=INC1, y=CONS1)) +
#  geom_point() + 
#  xlab("Income (1000 yen)") + 
#  ylab("Consumption (10000 yen)") +
#  ggtitle("Scatter plot")

#kakei.lm1 <- lm(CONS1~INC1, data=kakei)
#summary(kakei.lm1)

#stargazer::stargazer(kakei.lm1,
#                     type = "html",
#                     style="all",
#                     star.cutoffs = NA,
#                     omit.table.layout = 'n',
#                     ci = TRUE,
#                     align=TRUE)

#ggplot(data = kakei, 
#       mapping = aes(x=INC1, y=CONS1)) +
#  geom_point() + 
#  geom_smooth(method = "lm") + 
#  xlab("Income (1000 yen)") + 
#  ylab("Consumption (10000 yen)") +
#  ggtitle("Scatter plot")

#GGally::ggpairs(kakei, columns = 4:6)

#kakei.lm2 <- lm(CONS1~INC1+WORK, data=kakei)
#stargazer::stargazer(kakei.lm2,
#                     type = "html",
#                     style="all",
#                     star.cutoffs = NA,
#                     omit.table.layout = 'n',
#                     ci = TRUE,
#                     align=TRUE)

#coefplot::coefplot(kakei.lm2, intercept = FALSE, lwdOuter = 1)
#autoplot(kakei.lm2, which = 1, ncol = 1)
#autoplot(kakei.lm2, which = 2, ncol = 1)
#autoplot(kakei.lm2, which = 3, ncol = 1)
#autoplot(kakei.lm2, which = 5, ncol = 1)
#autoplot(kakei.lm2, which = 6, ncol = 1)

#kakei.lm3 <- lm(CONS1~WORK, data=kakei)
#stargazer::stargazer(kakei.lm3,
#                     type = "html",
#                     style="all",
#                     star.cutoffs = NA,
#                     omit.table.layout = 'n',
#                     ci = TRUE,
#                     align=TRUE)
#coefplot::coefplot(kakei.lm2, intercept = FALSE, lwdOuter = 1)

#z.kakei <- as.data.frame(scale(kakei))
#kakei.lm4 <- lm(CONS1~INC1+WORK, data=z.kakei)
#stargazer::stargazer(kakei.lm4,
#                     type = "html",
#                     style="all",
#                     star.cutoffs = NA,
#                     omit.table.layout = 'n',
#                     ci = TRUE,
#                     align=TRUE)

#coefplot::coefplot(kakei.lm4, intercept = FALSE, lwdOuter = 1)

#stargazer::stargazer(kakei.lm1, kakei.lm2, kakei.lm3, kakei.lm4,
#                     type = "html",
#                     style="all",
#                     star.cutoffs = NA,
#                     omit.table.layout = 'n',
#                     ci = TRUE,
#                     align=TRUE)

#AIC(kakei.lm1, kakei.lm2, kakei.lm3)

#BIC(kakei.lm1, kakei.lm2, kakei.lm3)

#car::vif(kakei.lm2)
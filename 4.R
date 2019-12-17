names(titanic_train)
train <- replace_with_na_all(titanic_train,
                             condition = ~.x %in% c("NA", ""))
glimpse(train)
head(train)
summary(train)
sapply(train, function(x) sum(is.na(x)))
summarise_all(train, ~ sum(is.na(.)))

train2 <-na.omit(train)
train2<-tidyr::drop_na(train)
head(train2)
summary(train2)

train3 <- train2 %>% 
  mutate_at(vars(Survived, Pclass, Sex, Embarked), as.factor) %>% 
  mutate(CabinLetter = str_sub(Cabin,
                               start = 1,
                               end = 1) %>% 
           as.factor) %>% 
  mutate(CabinNumber = str_sub(Cabin,
                               start = 2,
                               end = -1) %>% 
           as.numeric %>% 
           as.factor) %>% 
  mutate(Title = str_sub(Name,
                         start = str_locate(Name, ",")[,1] + 2,
                         end = str_locate(Name, "\\.")[,1] - 1) %>% 
           as.factor) %>% 
  mutate(Surname = str_sub(Name,
                           start = 1,
                           end = str_locate(Name, ",")[,1] - 1) %>% 
           as.factor)

model1 <- Survived ~ Pclass + Sex + Age + 
  SibSp + Parch + Fare + Embarked +
  CabinLetter + Title

model2 <- Survived ~ Pclass + Sex + Age +
  SibSp + Parch + Fare + Embarked

glm.logit1 <- glm(model1, data = train3, 
                  family = binomial(link = 'logit'))

stargazer(glm.logit1, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

car::vif(glm.logit1)

glm.logit2 <- glm(model2, data = train3, 
                  family = binomial(link = 'logit'))

stargazer(glm.logit2, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)

car::vif(glm.logit2)

coefplot(glm.logit2, intercept = FALSE, lwdOuter = 1)

library(ggfortify)
autoplot(glm.logit2)
train4 <- train3 %>%
  mutate(predict = predict(glm.logit2, type = "response")) %>% 
  mutate(survive = if_else(predict > 0.5, 1, 0))

sum(train4$Survived == train4$survive) / nrow(train4)


glm.probit <- glm(model2, data=train3,
                  family = binomial(link = 'probit'))
stargazer(glm.probit, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)
coefplot(glm.probit, intercept = FALSE, lwdOuter = 1)
autoplot(glm.probit)

train5 <- train3 %>%
  mutate(predict = predict(glm.probit, type = "response")) %>% 
  mutate(survive = if_else(predict > 0.5, 1, 0))
sum(train5$Survived == train5$survive) / nrow(train5)

glm.cloglog <- glm(model2, data = train3,
                   family = binomial(link = 'cloglog'))
stargazer(glm.cloglog, type = "text", style = "all2", ci = TRUE,
          star.cutoffs = NA, omit.table.layout = 'n',
          align = TRUE)
coefplot(glm.cloglog, intercept = FALSE, lwdOuter = 1)
autoplot(glm.cloglog)

train6 <- train3 %>%
  mutate(predict = predict(glm.cloglog, type = "response")) %>% 
  mutate(survive = if_else(predict > 0.5, 1, 0))
sum(train6$Survived == train6$survive) / nrow(train6)


test <- titanic_test %>% 
  replace_with_na_all(condition = ~.x %in% c("NA", "")) %>% 
  tidyr::drop_na() %>% 
  mutate_at(vars(Pclass, Sex, Embarked), as.factor) %>% 
  broom::augment(x=glm.logit2, newdata = ., type.predict = "response") %>% 
  mutate(survive = if_else(.fitted > 0.5, 1, 0))


library(mice)
library(norm2)
library(miceadds)

emResult <- emNorm(train, iter.max = 10000)
max2 <- emResult$iter*2

glm.logit.imp <- train %>% 
  replace_with_na_all(condition = ~.x %in% c("NA", "")) %>% 
  mutate_at(vars(Pclass, Sex, Embarked), as.factor) %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>% 
  mice(m = 100, seed = 20191015,
       meth = c("", "", "", "norm", "", "", "norm", "polyreg"),
       matrix = max2) %>% 
  glm.mids(model2, data = ., family = binomial(link = 'logit'))


glm.logit.imp %>% 
  pool() %>% 
  summary() %>% 
  stargazer(type = "text")


pooled_lm <-  glm.logit.imp$analyses[[1]]
pooled_lm$coefficients <-  summary(pool(glm.logit.imp))$estimate
train7 <- train %>% 
  mutate(predict = predict(pooled_lm, type = "response")) %>% 
  mutate(survive = if_else(predict > 0.5, 1, 0))
sum(train7$Survived == train7$survive) / nrow(train7)

train8 <- train7 %>% 
  replace_with_na_all(condition = ~.x %in% c("NA", "")) %>% 
  tidyr::drop_na() %>% 
  select(imp.survive = survive) %>% 
  bind_cols(train4 %>% 
              select(drop.survive = survive))


sum(train8$imp.survive == train8$drop.survive) / nrow(train8)


test2 <- titanic_test %>% 
  replace_with_na_all(condition = ~.x %in% c("NA", "")) %>% 
  mutate_at(vars(Pclass, Sex, Embarked), as.factor) %>% 
  select(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>% 
  mice(m = 100, seed = 20191015,
       meth = c("", "", "norm", "", "", "norm", ""),
       matrix = max2) %>% 
  complete() %>% 
  mutate(predict = predict(pooled_lm, type = "response", newdata = .)) %>% 
  mutate(survive = if_else(predict > 0.5, 1, 0))

####### The impact of the expected social and economic well-being of the population #####
####### of the Russian Federation on the readiness to participate in protests. #####

library(foreign)
library(dplyr)
library(questionr)
library(lmtest)
library(stargazer)
library(ResourceSelection)

######################## About variables: #############################
##### Dependent:     polit_protest (political protest)
##### Indepenedent:  life (expected change in individual quality of life)
###################  economy (expected change in individual financial situation)
###################  wage_delays (expected wage delays)
###################  wage_reduction (expected wage reduction)
###################  dismissal (expected dismissal)
##### Control:       mood (the mood of the respondent)

########### 2015 ###############

df_15 <- read.spss(file.choose(), to.data.frame = TRUE)
df_15 <- dplyr::select(df_15, qS1, qS2, q2, q17, qA2, q43A, q43B, q43C, q76D)
colnames(df_15) <- c("sex", "age", "mood", "life", "economy", "wage_delay", "wage_reduction", 
                     "dismissal","polit_protest")

#### recoding variables #####

## sex: male - 0, female - 1 ## 
df_15 <- df_15 %>% 
  mutate(sex = recode(sex, 
                      "мужской" = 0,
                      "женский" = 1))

## mood: good - 0, normal - 1, not good - 2, bad&panic - 3 ##

df_15 <- df_15 %>% 
  mutate(mood = recode(mood, 
                       "прекрасное настроение" = 0,
                       "нормальное, ровное состояние" = 1,
                       "испытываю напряжение, раздражение" = 2,
                       "испытываю страх, тоску" = 3))


## life: much better - 0, better - 1, same - 2, worse - 3, much worse - 4 ## 

df_15 <- df_15 %>% 
  mutate(life = recode(life, 
                       "значительно лучше" = 0,
                       "несколько лучше" = 1,
                       "так же, как и сейчас" = 2,
                       "несколько хуже" = 3,
                       "значительно хуже" = 4))

## economy: better - 0, same - 1, worse - 2 ## 

df_15 <- df_15 %>% 
  mutate(economy = recode(economy, 
                               "скорее, улучшится" = 0,
                               "останется без изменения" = 1,
                               "скорее, ухудшится" = 2))


## wage_delay - wage_reduction - dismissal: no one works & it is not gonna happen - 0, 
## it is already happening - 1, it will happen during few weeks - 2,
## it will happen during few months - 3 ## 

df_15 <- df_15 %>% 
  mutate(wage_delay = recode(wage_delay, 
                        "Не подходит: никто из членов семьи в последнее время не работал" = 0,
                        "Думаю, что в ближайшее время этого не случится" = 0,
                        "Это уже происходит" = 1,
                        "Это может случиться в течение ближайших недель" = 2,
                        "Это может случиться в течение ближайших месяцев" = 3))

df_15 <- df_15 %>% 
  mutate(wage_reduction = recode(wage_reduction, 
                        "Не подходит: никто из членов семьи в последнее время не работал" = 0,
                        "Думаю, что в ближайшее время этого не случится" = 0,
                        "Это уже происходит" = 1,
                        "Это может случиться в течение ближайших недель" = 2,
                        "Это может случиться в течение ближайших месяцев" = 3))
df_15 <- df_15 %>% 
  mutate(dismissal = recode(dismissal, 
                        "Не подходит: никто из членов семьи в последнее время не работал" = 0,
                        "Думаю, что в ближайшее время этого не случится" = 0,
                        "Это уже происходит" = 1,
                        "Это может случиться в течение ближайших недель" = 2,
                        "Это может случиться в течение ближайших месяцев" = 3))

## polit_protest: no - 0, yes - 1 ##

df_15 <- df_15 %>% 
  mutate(polit_protest = recode(polit_protest, 
                                "скорее всего, нет" = 0,
                                "скорее всего, да" = 1))


df_15 <- na.omit(df_15) # now it is 1043 obs.

####################

#### Regression model ######

# without controls:
logit_15.1 <- glm(polit_protest ~ life + economy + wage_delay + wage_reduction + dismissal, 
                        data = df_15, 
                        family = binomial(link = "logit"), x = TRUE)
summary(logit_15.1)

OR_15.1 <- odds.ratio(logit_15.1, level = 0.95, signif.stars = T)
OR_15.1_logit <- OR_15.1[,1]
PV_15.1_logit <- OR_15.1[,4]

# with controls:
logit_15.2 <- glm(polit_protest ~  life + economy + wage_delay + wage_reduction + dismissal 
                  + mood + sex + age, data = df_15, 
                  family = binomial(link = "logit"), x = TRUE)
summary(logit_15.2)

OR_15.2 <- odds.ratio(logit_15.2, level = 0.95, signif.stars = T)
OR_15.2_logit <- OR_15.2[,1]
PV_15.2_logit <- OR_15.2[,4]

# Is model on constsnt better?
logit_15.null <- glm(polit_protest ~ 1, data = df_15, 
                           family = binomial(link = "logit"), x = TRUE)

lrtest(logit_15.2, logit_15.null)
## No

# Hosmer-Lemeshow test
hl <- hoslem.test(logit_15.2$y, fitted(logit_15.2), g=10)
hl
## Model is ok

########### 2016 ###############

df_16 <- read.spss(file.choose(), to.data.frame = TRUE)
df_16 <- dplyr::select(df_16, qS1, qS2, q2, q17, qA2, q35A, q35B, q35C, q41B)
colnames(df_16) <- c("sex", "age","mood", "life", "economy", "wage_delay", "wage_reduction", 
                     "dismissal", "polit_protest")


#### recoding variables #####

## sex: male - 0, female - 1 ## 
df_16 <- df_16 %>% 
  mutate(sex = recode(sex, 
                      "мужской" = 0,
                      "женский" = 1))

## mood: good - 0, normal - 1, not good - 2, bad&panic - 3 ##

df_16 <- df_16 %>% 
  mutate(mood = recode(mood, 
                       "прекрасное настроение" = 0,
                       "нормальное, ровное состояние" = 1,
                       "испытываю напряжение, раздражение" = 2,
                       "испытываю страх, тоску" = 3))


## life: much better - 0, better - 1, same - 2, worse - 3, much worse - 4 ## 

df_16 <- df_16 %>% 
  mutate(life = recode(life, 
                           "значительно лучше" = 0,
                           "несколько лучше" = 1,
                           "так же, как и сейчас" = 2,
                           "несколько хуже" = 3,
                           "значительно хуже" = 4))

## economy: better - 0, same - 1, worse - 2 ## 

df_16 <- df_16 %>% 
  mutate(economy = recode(economy, 
                              "скорее, улучшится" = 0,
                              "останется без изменения" = 1,
                              "скорее, ухудшится" = 2))


## wage_delay - wage_reduction - dismissal: no one works & it is not gonna happen - 0, 
## it is already happening - 1, it will happen during few weeks - 2,
## it will happen during few months - 3 ## 

## Here something strange with data and I can do mutate(), so I do this:

# wage_delay
df_16$wage_delay2[df_16$wage_delay == "Не подходит: никто из членов семьи в последнее время не работал"] <- 0
df_16$wage_delay2[df_16$wage_delay == "Думаю, что в ближайшее время этого не случится"] <- 0
df_16$wage_delay2[df_16$wage_delay == "Это уже происходит"] <- 1
df_16$wage_delay2[df_16$wage_delay == "Это может случиться в течение ближайших недель"] <- 2
df_16$wage_delay2[df_16$wage_delay == "Затрудняюсь ответить"] <- 9
df_16$wage_delay2[is.na(df_16$wage_delay2)] <- 3

df_16$wage_delay2[df_16$wage_delay2 == 9] <- NA

df_16$wage_delay <- df_16$wage_delay2
df_16$wage_delay2 <- NULL


# wage_reduction

df_16$wage_reduction2[df_16$wage_reduction == "Не подходит: никто из членов семьи в последнее время не работал"] <- 0
df_16$wage_reduction2[df_16$wage_reduction == "Думаю, что в ближайшее время этого не случится"] <- 0
df_16$wage_reduction2[df_16$wage_reduction == "Это уже происходит"] <- 1
df_16$wage_reduction2[df_16$wage_reduction == "Это может случиться в течение ближайших недель"] <- 2
df_16$wage_reduction2[df_16$wage_reduction == "Затрудняюсь ответить"] <- 9
df_16$wage_reduction2[is.na(df_16$wage_reduction2)] <- 3

df_16$wage_reduction2[df_16$wage_reduction2 == 9] <- NA

df_16$wage_reduction <- df_16$wage_reduction2
df_16$wage_reduction2 <- NULL

# dismissal

df_16$dismissal2[df_16$dismissal == "Не подходит: никто из членов семьи в последнее время не работал"] <- 0
df_16$dismissal2[df_16$dismissal == "Думаю, что в ближайшее время этого не случится"] <- 0
df_16$dismissal2[df_16$dismissal == "Это уже происходит"] <- 1
df_16$dismissal2[df_16$dismissal == "Это может случиться в течение ближайших недель"] <- 2
df_16$dismissal2[df_16$dismissal == "Затрудняюсь ответить"] <- 9
df_16$dismissal2[is.na(df_16$dismissal2)] <- 3

df_16$dismissal2[df_16$dismissal2 == 9] <- NA

df_16$dismissal <- df_16$dismissal2
df_16$dismissal2 <- NULL

## polit_protest: no - 0, yes - 1 ##

df_16 <- df_16 %>% 
  mutate(polit_protest = recode(polit_protest, 
                                "скорее всего, нет" = 0,
                                "скорее всего, да" = 1))

df_16 <- na.omit(df_16) # now it is 1092 obs.

####################

#### Regression model ######

# without controls:
logit_16.1 <- glm(polit_protest ~ life + economy + wage_delay + wage_reduction + dismissal, 
                  data = df_16, 
                  family = binomial(link = "logit"), x = TRUE)
summary(logit_16.1)

OR_16.1 <- odds.ratio(logit_16.1, level = 0.95, signif.stars = T)
OR_16.1_logit <- OR_16.1[,1]
PV_16.1_logit <- OR_16.1[,4]

# with controls:
logit_16.2 <- glm(polit_protest ~  life + economy + wage_delay + wage_reduction + dismissal
                  + mood + sex + age, data = df_16, 
                  family = binomial(link = "logit"), x = TRUE)
summary(logit_16.2)

OR_16.2 <- odds.ratio(logit_16.2, level = 0.95, signif.stars = T)
OR_16.2_logit <- OR_16.2[,1]
PV_16.2_logit <- OR_16.2[,4]

# Is model on constsnt better?
logit_16.null <- glm(polit_protest ~ 1, data = df_16, family = binomial(link = "logit"), x = TRUE)

lrtest(logit_16.2, logit_16.null)
## No

# Hosmer-Lemeshow test
hl <- hoslem.test(logit_16.2$y, fitted(logit_16.2), g=10)
hl

## Model is ok

########### 2017 ###############

df_17 <- read.spss(file.choose(), to.data.frame = TRUE)
df_17 <- dplyr::select(df_17, qS1, qS2, q2, q11, qA2, q27A, q27B, q27C, q36D)
colnames(df_17) <- c("sex", "age", "mood", "life", "economy", "wage_delay", "wage_reduction", 
                     "dismissal", "polit_protest")


#### recoding variables #####

## sex: male - 0, female - 1 ## 
df_17 <- df_17 %>% 
  mutate(sex = recode(sex, 
                      "мужской" = 0,
                      "женский" = 1))

## mood: good - 0, normal - 1, not good - 2, bad&panic - 3 ##

df_17 <- df_17 %>% 
  mutate(mood = recode(mood, 
                       "прекрасное настроение" = 0,
                       "нормальное, ровное состояние" = 1,
                       "испытываю напряжение, раздражение" = 2,
                       "испытываю страх, тоску" = 3))


## life: much better - 0, better - 1, same - 2, worse - 3, much worse - 4 ## 

df_17 <- df_17 %>% 
  mutate(life = recode(life, 
                           "значительно лучше" = 0,
                           "несколько лучше" = 1,
                           "так же, как и сейчас" = 2,
                           "несколько хуже" = 3,
                           "значительно хуже" = 4))

## economy: better - 0, same - 1, worse - 2 ## 

df_17 <- df_17 %>% 
  mutate(economy = recode(economy, 
                          "скорее, улучшится" = 0,
                          "останется без изменения" = 1,
                          "скорее, ухудшится" = 2))


## wage_delay - wage_reduction - dismissal: no one works & it is not gonna happen - 0, 
## it is already happening - 1, it will happen during few weeks - 2,
## it will happen during few months - 3 ## 

## Here something strange with data and I can do mutate(), so I do this:

# wage_delay
df_17$wage_delay2[df_17$wage_delay == "Не подходит: никто из членов семьи в последнее время не работал"] <- 0
df_17$wage_delay2[df_17$wage_delay == "Думаю, что в ближайшее время этого не случится"] <- 0
df_17$wage_delay2[df_17$wage_delay == "Это уже происходит"] <- 1
df_17$wage_delay2[df_17$wage_delay == "Это может случиться в течение ближайших недель"] <- 2
df_17$wage_delay2[df_17$wage_delay == "Затрудняюсь ответить"] <- 9
df_17$wage_delay2[is.na(df_17$wage_delay2)] <- 3

df_17$wage_delay2[df_17$wage_delay2 == 9] <- NA

df_17$wage_delay <- df_17$wage_delay2
df_17$wage_delay2 <- NULL


# wage_reduction

df_17$wage_reduction2[df_17$wage_reduction == "Не подходит: никто из членов семьи в последнее время не работал"] <- 0
df_17$wage_reduction2[df_17$wage_reduction == "Думаю, что в ближайшее время этого не случится"] <- 0
df_17$wage_reduction2[df_17$wage_reduction == "Это уже происходит"] <- 1
df_17$wage_reduction2[df_17$wage_reduction == "Это может случиться в течение ближайших недель"] <- 2
df_17$wage_reduction2[df_17$wage_reduction == "Затрудняюсь ответить"] <- 9
df_17$wage_reduction2[is.na(df_17$wage_reduction2)] <- 3

df_17$wage_reduction2[df_17$wage_reduction2 == 9] <- NA

df_17$wage_reduction <- df_17$wage_reduction2
df_17$wage_reduction2 <- NULL

# dismissal

df_17$dismissal2[df_17$dismissal == "Не подходит: никто из членов семьи в последнее время не работал"] <- 0
df_17$dismissal2[df_17$dismissal == "Думаю, что в ближайшее время этого не случится"] <- 0
df_17$dismissal2[df_17$dismissal == "Это уже происходит"] <- 1
df_17$dismissal2[df_17$dismissal == "Это может случиться в течение ближайших недель"] <- 2
df_17$dismissal2[df_17$dismissal == "Затрудняюсь ответить"] <- 9
df_17$dismissal2[is.na(df_17$dismissal2)] <- 3

df_17$dismissal2[df_17$dismissal2 == 9] <- NA

df_17$dismissal <- df_17$dismissal2
df_17$dismissal2 <- NULL

## polit_protest: no - 0, yes - 1 ##

df_17 <- df_17 %>% 
  mutate(polit_protest = recode(polit_protest, 
                                "скорее всего, нет" = 0,
                                "скорее всего, да" = 1))

df_17 <- na.omit(df_17) # now it is 1108 obs.

####################

#### Regression model ######

# without controls:
logit_17.1 <- glm(polit_protest ~ life + economy + wage_delay + wage_reduction + dismissal, 
                  data = df_17, 
                  family = binomial(link = "logit"), x = TRUE)
summary(logit_17.1)

OR_17.1 <- odds.ratio(logit_17.1, level = 0.95, signif.stars = T)
OR_17.1_logit <- OR_17.1[,1]
PV_17.1_logit <- OR_17.1[,4]

# with controls:
logit_17.2 <- glm(polit_protest ~  life + economy + wage_delay + wage_reduction + dismissal
                  + mood + sex + age, data = df_17, 
                  family = binomial(link = "logit"), x = TRUE)
summary(logit_17.2)

OR_17.2 <- odds.ratio(logit_17.2, level = 0.95, signif.stars = T)
OR_17.2_logit <- OR_17.2[,1]
PV_17.2_logit <- OR_17.2[,4]

# Is model on constsnt better?
logit_17.null <- glm(polit_protest ~ 1, data = df_17, family = binomial(link = "logit"), x = TRUE)

lrtest(logit_17.2, logit_17.null)
## No

# Hosmer-Lemeshow test
hl <- hoslem.test(logit_17.2$y, fitted(logit_17.2), g=10)
hl

## Model is ok

##############

### Robustness check ####

probit_15 <- glm(polit_protest ~  life + economy + wage_delay + wage_reduction + dismissal
                 + mood + sex + age, data = df_15, 
                 family = binomial(link = "probit"))
summary(probit_15)

OR_15_probit <- exp(probit_15$coefficients*1.8)
PV_15_probit <- odds.ratio(probit_15, level = 0.95, signif.stars = T)[,4]

probit_16 <- glm(polit_protest ~  life + economy + wage_delay + wage_reduction + dismissal 
                 + mood + sex + age, data = df_16, 
                 family = binomial(link = "probit"))
summary(probit_16)

OR_16_probit <- exp(probit_16$coefficients*1.8)
PV_16_probit <- odds.ratio(probit_16, level = 0.95, signif.stars = T)[,4]

probit_17 <- glm(polit_protest ~  life + economy + wage_delay + wage_reduction + dismissal 
                 + mood + sex + age, data = df_17, 
                 family = binomial(link = "probit"))
summary(probit_17)

OR_17_probit <- exp(probit_17$coefficients*1.8)
PV_17_probit <- odds.ratio(probit_17, level = 0.95, signif.stars = T)[,4]

####### Results #######

# Models with controls and without
stargazer(logit_15.1, logit_15.2, 
          logit_16.1, logit_16.2,
          logit_17.1, logit_17.2, 
          type = "html", apply.coef=exp, t.auto=F, p.auto=F, 
          title="Таблица 1. Результаты регрессионного анализа",  
          dep.var.labels="Протестная активность",
          column.labels=c("2015", "2015", "2016", "2016", "2017", "2017"),
          covariate.labels=c("Ожидаемое изменение индивидуального уровня жизни",
                             "Ожидаемое изменение индивидуального финансового положения",
                             "Ожидаемые задержки заработной платы",
                             "Ожидаемое уменьшение заработной платы",
                             "Ожидание увольнения",
                             "Настроение респондента",
                             "Пол респондента",
                             "Возраст респондента"),
          model.numbers = FALSE,
          report=('vc*p'), 
          notes =  c("Сравнение с моделью на константу. Нулевая гипотеза: модель на константу лучше модели с предикторами. Приведены значения p-value.",
          "Тест Хосмера-Лемешоу. Нулевая гипотеза: качество прогноза модели приемлимое.Приведены значения p-value."), 
          notes.append = TRUE,
          add.lines = list(c("Сравнение с моделью на константу", 
                             "", "2.521e-06 ***", "", "2.316e-05 ***", "", "9.875e-06 ***"),
                           c("Тест Хосмера-Лемешоу",
                             "", "0.4034", "", "0.2355", "", "0.8178")),
          out = "Essay_results.htm")

# Robustnes check 

COEF <- list(OR_15.2_logit, OR_16.2_logit, OR_17.2_logit, OR_15_probit, OR_16_probit, OR_17_probit)
PV <- list(PV_15.2_logit, PV_16.2_logit, PV_17.2_logit, PV_15_probit, PV_16_probit, PV_17_probit)

stargazer(logit_15.2, logit_16.2, logit_17.2, 
          probit_15, probit_16, probit_17,
          coef = COEF, p=PV,
          type = "html", 
          title="Таблица 2. Проверка на устойчивость",
          dep.var.labels="Протестная активность",
          column.labels=c("2015", "2016", "2017", "2015", "2016", "2017"),
          covariate.labels=c("Ожидаемое изменение индивидуального уровня жизни",
                             "Ожидаемое изменение индивидуального финансового положения",
                             "Ожидаемые задержки заработной платы",
                             "Ожидаемое уменьшение заработной платы",
                             "Ожидание увольнения",
                             "Настроение респондента",
                             "Пол респондента",
                             "Возраст респондента"),
          model.numbers = FALSE,
          report=('vc*p'), 
          out = "Essay_results_check.htm")

############################################
############################################

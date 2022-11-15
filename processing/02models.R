# Code 2: Models

# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, sjPlot, summarytools,
               effectsize, lme4, easystats, stargazer, 
               influence.ME, performance, ggrepel, ggpubr,
               broom, broomExtra, sjlabelled, RColorBrewer, 
               texreg, car, flexplot, ggeffects, misty, optimx)

options(scipen=999)


# 2. Data -----------------------------------------------------------------

load("../output/data/db-proc.RData")

names(db)
sapply(db, class)

# 3. Analysis -------------------------------------------------------------

# 3.1. Multilevel models ----

# Null model
model_0 <- lmer(PSCi ~ 1 + (1 | COUNTRY), 
                data = db, weights = FACTOR, REML = T)

performance::icc(model_0, by_group = T)
## ICC Country = 0.217
## ICC Individual = 0.783


# Influence test
inf_m0 <- influence(model_0, group = "COUNTRY")

# D cook
cooks.distance(inf_m0) # cut point is 4/33 = 0.1212121

plot(inf_m0, which="cook",
     cutoff=0.121, sort=TRUE,
     xlab="Distancia de Cook",
     ylab="País", width=60, height=40)

# Influential countries: South Korea and Hungary. 
# This are the two countries with the highest average PSCi in the sample.


# Model 1: Only Class
model_1 <- lmer(PSCi ~ 1 + CLASS +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

# Model 2: Class + Union
model_2 <- lmer(PSCi ~ 1 + CLASS + UNION +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

# Model 3: Class + Union + Ratio + CorpAll + WAVE
model_3 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

# Model 4: Class + Union + Ratio + CorpAll + WAVE + Controls N1 & N2
model_4 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                  (C_AGE)^2 + SEX + IDEOLOGY + C_SOCEXPEND + 
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

# Model 5: Class + Union + Ratio + CorpAll + WAVE + Controls N1 & N2 + Random Slope CLASS
model_5 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + C_SOCEXPEND +
                    (1 + CLASS | COUNTRY), data = db, weights = FACTOR, REML = T)
# save model 5
saveRDS(model_5, file = "../output/model_5.rds")

# Model 6: Class + Union + Ratio + CorpAll + WAVE + Controls N1 & N2 + Random Slope CLASS + Interaction
model_6 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + C_SOCEXPEND + 
                    CLASS*C_RATIO + 
                    (1 + CLASS | COUNTRY), data = db, weights = FACTOR, REML = T)

# save model 6
saveRDS(model_6, file = "../output/model_6.rds")

model_6.e <- texreg::extract(model = model_6, 
                     include.deviance = T,
                     include.loglik = F, 
                     include.variance = F)

saveRDS(model_6.e, file = "../output/model_6e.rds")


## Table 
screenreg(list(model_2, model_3, model_4, model_6),  
          stars = c(0.01,0.05,0.1), digits = 2)


# 3.2. Adjust  ----

# deviance test (log likelihood ratio test)

res_anova1 <- anova(model_0, model_2, model_3, model_4, model_6) # compare to null/baseline model 

saveRDS(res_anova1, file = "../output/deviancetest.rds")

res_anova2 <- anova(model_4, model_6) # compare ri model and interaction model

performance::test_likelihoodratio(model_4, model_5) # compares ri model and re slope model

performance::test_likelihoodratio(model_4, model_6)

# compare performance (other measures: AIC, BIC, R2 Nakagawa with Johnson extension)

compare_performance(model_0, model_2, model_3, model_4, model_6)

compare_performance(model_0, model_2, model_3, model_4, model_6, rank = T)



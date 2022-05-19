# Code 1: Models

# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, dplyr, sjmisc, kableExtra, sjPlot,
               summarytools, DT, lme4, easystats, gtsummary, 
               stargazer, magrittr, janitor, htmlTable, gridExtra,
               influence.ME, performance, ggrepel, ggthemes, 
               sjlabelled, ggpubr, RColorBrewer, stats, car, texreg)

options(scipen=999)


# 2. Data -----------------------------------------------------------------

load("../output/data/db-proc.RData")

names(db)
sapply(db, class)


# 3. Analysis -------------------------------------------------------------


# 3.1. Hybrid multilevel models ----

# Null model
model_0 <- lmer(PSCi ~ 1 + (1 | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

performance::icc(model_0, by_group = T)
## ICC Country = 0.140
## ICC Country-year = 0.045
## ICC Individual = 0.815


# Influence test
inf_m0 <- influence(model_0, group = "COUNTRY")

# D cook
cooks.distance(inf_m0) # punto de corte es 4/43 = 0.09302326

plot(inf_m0, which="cook",
     cutoff=0.09, sort=TRUE,
     xlab="Distancia de Cook",
     ylab="País", width=60, height=40)

# Paises influyentes: Corea del Sur, Hungria y Chipre. 
# Los dos primeros son los dos con mayor promedio de PSCi, mientras que Chipre es el país con menor promedio de PSCi

# Model 1: Only Class
model_1 <- lmer(PSCi ~ 1 + CLASS + YEAR + 
                  (1 | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

# Model 2: Class + Union
model_2 <- lmer(PSCi ~ 1 + CLASS + UNION + YEAR + 
                  (1 | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

# Model 3: Class + Union + Controls N1
model_3 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + IDEOLOGY + YEAR + 
                  (1 | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

# Model 4: Individual predictors + RATIO + CorpAll # without YEAR variable the effect of CorpAll is significant
model_4 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + IDEOLOGY + 
                  MEAN_RATIO + LAG_RATIO + CorpAll + YEAR + 
                  (1 | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

# Model 5: Individual predictors + Contextual predictors (controls included) # Rescaled some predictors how?
model_5 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + IDEOLOGY + 
                  MEAN_RATIO + LAG_RATIO + CorpAll + GDP + UD + 
                  SOC_EXPEND + YEAR + 
                  (1 | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

# Model 6: Individual predictors + Contextual predictors + Random slope Class
model_6 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + IDEOLOGY + 
                  MEAN_RATIO + LAG_RATIO + CorpAll + GDP + UD + 
                  SOC_EXPEND + YEAR + 
                  (1 + CLASS | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

# Model 7: Individual predictors + Contextual predictors + Random slope Class + Interaciontion class*mean ratio
model_7 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + IDEOLOGY + 
                  MEAN_RATIO + LAG_RATIO + CorpAll + GDP + UD + 
                  SOC_EXPEND + CLASS*MEAN_RATIO + YEAR + 
                  (1 + CLASS | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)

# Model 8: Growth curves 
model_8 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + IDEOLOGY + YEAR +
                  MEAN_RATIO + LAG_RATIO + CorpAll + GDP + UD + 
                  SOC_EXPEND + CLASS*MEAN_RATIO + MEAN_CorpAll*YEAR + 
                  (1 + CLASS | COUNTRY/COUNTRY_WAVE), data = db, weights = FACTOR, REML = F)


save(model_6, file = "../output/data/model.RData")

screenreg(list(model_0, model_1, model_2, model_3, model_4, model_5),  stars = c(0.01,0.05,0.1), digits = 3)

coef(model_4)

# 3.3. Assumptions cheking ----

anova(model_1)
?anova.merMod



performance::r2(model = model_5)

performance::compare_performance(model_0, model_1, model_2, model_3, model_4, model_5, rank = T)


# 3.4. Fit models ----

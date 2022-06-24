# Anexo modelos

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, sjPlot, summarytools,
               effectsize, lme4, easystats, stargazer, 
               influence.ME, performance, ggrepel, ggpubr,
               broom, broomExtra, sjlabelled, RColorBrewer, 
               texreg, car, flexplot, ggeffects, misty, optimx)

options(scipen=999)


load("../output/data/db-proc.RData")



db <- db %>% mutate(cap = if_else(CLASS == "1.Capitalistas", 1,0),
                    peq_emp = if_else(CLASS == "2.Pequeños empleadores", 1,0),
                    peq_bur = if_else(CLASS == "3.Pequeña burguesia", 1,0),
                    exp_dir = if_else(CLASS == "4.Expertos directivos", 1,0),
                    exp_sinaut = if_else(CLASS == "5.Expertos sin autoridad", 1,0),
                    sup_cal = if_else(CLASS == "6.Supervisores calificados", 1,0),
                    sup_nocal = if_else(CLASS == "7.Supervisores no calificados", 1,0),
                    trab_cal = if_else(CLASS == "8.Trabajadores calificados", 1,0),
                    trab_nocal = if_else(CLASS == "9.Trabajadores no calificados", 1,0)) 


db %>% filter(CLASS == "1.Capitalistas") %>% select(CLASS, cap, trab_nocal)

db <- db %>%  mutate_at(vars(26:34), ~ as.factor(.))


## Random Slope Class 1 by 1 (dummy)

model_4.1 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + 
                    C_SOCEXPEND + C_UD +
                    (1 + cap | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.2 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY +
                    C_SOCEXPEND + C_UD +
                    (1 + peq_emp | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.3 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY +
                    C_SOCEXPEND + C_UD +
                    (1 + peq_bur | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.4 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY +
                    C_SOCEXPEND + C_UD +
                    (1 + exp_dir | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.5 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + 
                    C_SOCEXPEND + C_UD +
                    (1 + exp_sinaut | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.6 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + 
                    C_SOCEXPEND + C_UD +
                    (1 + sup_cal | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.7 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + 
                    C_SOCEXPEND + C_UD +
                    (1 + sup_nocal | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.8 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + 
                    C_SOCEXPEND + C_UD +
                    (1 + trab_cal | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.9 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + 
                    C_SOCEXPEND + C_UD +
                    (1 + trab_nocal | COUNTRY), data = db, weights = FACTOR, REML = T, control = lmerControl(optimizer ="Nelder_Mead"))



screenreg(list(model_4, model_4.4, model_4.7, model_4.9))


anova(model_4, model_4.1) # non significant 
anova(model_4, model_4.2) # significant (0.1) en duda pues es mayor al estandar
anova(model_4, model_4.3) # non significant
anova(model_4, model_4.4) # significant (0.05) 
anova(model_4, model_4.5) # significant (0.1) en duda pues es mayor al estandar
anova(model_4, model_4.6) # non significant 
anova(model_4, model_4.7) # significant (0.1) en duda pues es mayor al estandar
anova(model_4, model_4.8) # non significant 
anova(model_4, model_4.9) # significant (0.001)




plot_model(model_4.9, type = "re",
           show.legend = F,
           show.values = T,
           facet.grid = F,
           value.size = 3.5,
           y.offset = .4,
           value.offset = .4)



# restricted 95% significance

model_5 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                  C_AGE + SEX + IDEOLOGY + C_GDP +
                  C_SOCEXPEND + C_UD +
                  (1 +  peq_emp + exp_dir + exp_sinaut + sup_nocal + trab_nocal| COUNTRY), data = db, weights = FACTOR, REML = T,
                control = lmerControl(
                  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))


model_6 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                  (C_AGE)^2 + SEX + IDEOLOGY +
                  C_SOCEXPEND + C_UD + exp_dir*C_RATIO + sup_nocal*C_RATIO +
                  trab_nocal*C_RATIO +
                  (1 + exp_dir + sup_nocal + trab_nocal | COUNTRY), data = db, 
                weights = FACTOR, REML = T,
                control = lmerControl(
                  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))


screenreg(list(model_4, model_6))


## FALLO CONVERGER, INTENTAR MAÑANA CON : https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer


## SI SIGUE FALLANDO EN CONVERGER ENTONCES DEJAR COMO ESTABA ANTES (SIN TODAS LAS DUMMIES)



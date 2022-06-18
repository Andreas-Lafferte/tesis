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
                  C_AGE + SEX + IDEOLOGY + C_GDP +
                  C_SOCEXPEND + C_UD +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

## Random Slope Class 1 by 1 (dummy)

model_4.1 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + cap | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.2 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + peq_emp | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.3 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + peq_bur | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.4 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + exp_dir | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.5 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + exp_sinaut | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.6 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + sup_cal | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.7 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + sup_nocal | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.8 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + trab_cal | COUNTRY), data = db, weights = FACTOR, REML = T)


model_4.9 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 + trab_nocal | COUNTRY), data = db, weights = FACTOR, REML = T, control = lmerControl(optimizer ="Nelder_Mead"))



screenreg(list(model_4, model_4.1, model_4.2, model_4.3, model_4.4,
               model_4.5, model_4.6, model_4.7, model_4.8, model_4.9))


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
       C_AGE + SEX + IDEOLOGY + C_GDP +
       C_SOCEXPEND + C_UD + peq_emp*C_RATIO + exp_dir*C_RATIO + exp_sinaut*C_RATIO + 
       sup_nocal*C_RATIO + trab_nocal*C_RATIO +
       (1 +  peq_emp + exp_dir + exp_sinaut + sup_nocal + trab_nocal| COUNTRY), data = db, 
     weights = FACTOR, REML = T,
     control = lmerControl(
       optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))


screenreg(list(model_4, model_6))


## FALLO CONVERGER, INTENTAR MAÑANA CON : https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer


## SI SIGUE FALLANDO EN CONVERGER ENTONCES DEJAR COMO ESTABA ANTES (SIN TODAS LAS DUMMIES)


# Model 5: Class + Union + Ratio + CorpAll + WAVE + Controls N1 & N2 + Random Slope CLASS
model_5.1 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                  C_AGE + SEX + IDEOLOGY + C_GDP +
                  C_SOCEXPEND + C_UD +
                  (1 + CLASS | COUNTRY), data = db, weights = FACTOR, REML = T)

# Model 6: Class + Union + Ratio + CorpAll + WAVE + Controls N1 & N2 + Random Slope CLASS + Interaction
model_6.1 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                  C_AGE + SEX + IDEOLOGY + C_GDP +
                  C_SOCEXPEND + C_UD + CLASS*C_RATIO +
                  (1 + CLASS | COUNTRY), data = db, weights = FACTOR, REML = T)


# Table 
screenreg(list(model_0, model_2, model_3, model_4, model_5, model_6),  
          stars = c(0.01,0.05,0.1), digits = 2)


# 3.2. Adjust  ----

# deviance test (log likelihood ratio test)

res_anova1 <- anova(model_0, model_2, model_3, model_4, model_6) # compare to null/baseline model 

res_anova2 <- anova(model_4, model_6) # compare ri model and interaction model

performance::test_likelihoodratio(model_4, model_5) # compares ri model and re slope model

performance::test_likelihoodratio(model_4, model_6)

# compare performance (other measures: AIC, BIC, R2 Nakagawa with Johnson extension)

compare_performance(model_0, model_2, model_3, model_4, model_6)

compare_performance(model_0, model_2, model_3, model_4, model_6, rank = T)

## 1 Cómo determinar que no hay variación signiticativa en la pendiente de clase entre países
## Segun deviance el modelo 6 ajusta mejor que el 4, pero esto sin considerar que no hay importante variación de la pendiente
## AIC y BIC son peores en modelo 6 que en el 4, así como también otras medidas de ajuste menos deviance
## 2 Entonces, si tuviera que quedarme con el modelo 6, cómo reportar los random effects de la pendiente en la tabla?
## Centrar clase? 



## 3.3. Plot interaction ----

# plot random slope
plot_model(model_5, type = "re",
           show.legend = F,
           show.values = T,
           facet.grid = F,
           value.size = 3.5,
           y.offset = .4,
           value.offset = .4)

ggpredict(model_5, terms = c("COUNTRY", "CLASS"), type = "re") %>% plot() + coord_flip() 

# plot interaction term
plot_model(model_6, type = "int", pred.type = "re", mdrt.values = "meansd") # option 1

ggpredict(model_6, terms = c("CLASS", "C_RATIO [meansd]")) %>% plot(ci.style = "errorbar")

plot_model(model_6, terms = c("C_RATIO", "CLASS"), type = "pred", pred.type = "re")  + 
  scale_color_manual(values = colores) +
  my_pretty_theme# option 2


# ver si en vez del meansd hay que usar min max en el grafico para desigualdad


plot_model(model_6, type = "int", mdrt.values = "minmax", )

ggpredict(model_6, terms = c("exp_dir", "C_RATIO [minmax]")) %>% 
  plot(show.title = FALSE, ci.style = "errorbar", dot.size = 1.5) +
  geom_line() +
  scale_color_manual(values = c("#B12A90FF", "#0D0887FF"), name="Ratio 80/20", labels = c("Mínimo", "Máximo")) +
  my_pretty_theme

# ----

# Nota: antes de correr una interaccion entre niveles hay que dar cuenta de si la variable 
# que interesa (esto es la var que se ve afectada por otra) varia entre grupos. 

my_colors

colores <- RColorBrewer::brewer.pal(10, "Viridis")

viridisLite::plasma(6)

show_col(viridis_pal(option = "plasma")(6))
show_col(brewer.pal(10, "Blues"))





p <- ggpredict(model_6, c("trab_nocal", "C_RATIO [minmax]"))
p[p$group %in% c("-3.68", "37.69"), ] %>% plot(ci.style = "errorbar")





ggpredict(model_5, terms = c("CLASS"), type = "random") %>% plot()









library(ggeffects)
library(lme4)
data(sleepstudy)
m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
ggpredict(m, "Days")

model_gdp <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll +
                    C_AGE + SEX + IDEOLOGY + C_GDP +
                    C_SOCEXPEND + C_UD +
                    (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

model_wave <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                     C_AGE + SEX + IDEOLOGY + 
                     C_SOCEXPEND + C_UD +
                     (1 | COUNTRY), data = db, weights = FACTOR, REML = T)


performance::check_collinearity(model_4) %>% plot()

performance::check_collinearity(model_gdp) %>% plot()

performance::check_collinearity(model_wave) %>% plot()



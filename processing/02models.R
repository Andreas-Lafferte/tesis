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
                  (C_AGE)^2 + SEX + IDEOLOGY + C_SOCEXPEND + C_UD +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

# Model 5: Class + Union + Ratio + CorpAll + WAVE + Controls N1 & N2 + Random Slope CLASS
model_5 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + C_SOCEXPEND + C_UD +
                    (1 + CLASS | COUNTRY), data = db, weights = FACTOR, REML = T)
# save model 5
saveRDS(model_5, file = "../output/model_5.rds")

# Model 6: Class + Union + Ratio + CorpAll + WAVE + Controls N1 & N2 + Random Slope CLASS + Interaction
model_6 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                    (C_AGE)^2 + SEX + IDEOLOGY + C_SOCEXPEND + C_UD + 
                    CLASS*C_RATIO + 
                    (1 + CLASS | COUNTRY), data = db, weights = FACTOR, REML = T)

# save model 6
saveRDS(model_6, file = "../output/model_6.rds")

model_6.e <- extract(model = model_6, 
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

res_anova2 <- anova(model_4, model_6) # compare ri model and interaction model

performance::test_likelihoodratio(model_4, model_5) # compares ri model and re slope model

performance::test_likelihoodratio(model_4, model_6)

# compare performance (other measures: AIC, BIC, R2 Nakagawa with Johnson extension)

compare_performance(model_0, model_2, model_3, model_4, model_6)

compare_performance(model_0, model_2, model_3, model_4, model_6, rank = T)


## 3.3. Plot interaction ----

# plot random slope
plot_model(model_5.1, type = "re",
           show.legend = F,
           show.values = T,
           facet.grid = F,
           value.size = 3.5,
           y.offset = .4,
           value.offset = .4)

ggpredict(model_5.1, terms = c("COUNTRY", "CLASS"), type = "re") %>% plot() + coord_flip() 

# plot interaction term
plot_model(model_6.1, type = "int", pred.type = "re", mdrt.values = "meansd") # option 1

ggpredict(model_6.1, terms = c("CLASS", "C_RATIO [meansd]")) %>% plot(ci.style = "errorbar")

plot_model(model_6.1, terms = c("C_RATIO", "CLASS"), type = "pred", pred.type = "re")
 


# ver si en vez del meansd hay que usar min max en el grafico para desigualdad


plot_model(model_6.1, type = "int", mdrt.values = "minmax")


ggpredict(model_6.1, terms = c("CLASS", "C_RATIO [minmax]")) %>% 
  plot(ci.style = "errorbar", connect.lines = TRUE, show.title = FALSE, dot.size = 1.5) + 
  scale_color_manual(values = c("#E16462", "#0D0887"), name="Ratio 80/20", labels = c("Mínimo", "Máximo")) 



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



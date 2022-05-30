# Code 2: Models

# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, dplyr, sjmisc, kableExtra, sjPlot,
               summarytools, DT, lme4, easystats, gtsummary, 
               stargazer, magrittr, htmlTable, gridExtra,
               influence.ME, performance, ggrepel, ggthemes, 
               sjlabelled, RColorBrewer, stats, car, texreg)

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
# The first two are the two with the highest average PSCi.


# Model 1: Only Class
model_1 <- lmer(PSCi ~ 1 + CLASS +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

# Model 2: Class + Union
model_2 <- lmer(PSCi ~ 1 + CLASS + UNION +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)


model_3 <- lmer(PSCi ~ 1 + CLASS + UNION + C_RATIO + CorpAll + WAVE +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)

model_4 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + relevel(IDEOLOGY,ref="Derecha") + 
                  C_RATIO + CorpAll + C_GDP + C_SOCEXPEND + C_UD +
                  WAVE + 
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)


model_5 <- lmer(PSCi ~ 1 + CLASS + UNION + AGE + SEX + relevel(IDEOLOGY,ref="Derecha") +  
                   C_GDP + C_RATIO + CorpAll +
                  (1 | COUNTRY), data = db, weights = FACTOR, REML = T)







screenreg(list(model_0, model_2, model_3, model_4, model_5),  stars = c(0.001,0.01,0.05), digits = 2)



# ----

# Nota: antes de correr una interaccion entre niveles hay que dar cuenta de si la variable 
# que interesa (esto es la var que se ve afectada por otra) varia entre grupos. 


ggeffects::ggpredict(model = model_5, terms = c("CLASS")) %>% 
  plot(., color = "red",
       ci.style = "errorbar")+
  labs(title = "", x = "", y = "Perception of conflict") +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10))+
  scale_y_continuous(breaks=c(5,10,15,20,25,30), limits = c(5,30))

ggeffects::ggpredict(model1_cse, terms = c("class_2")) %>% 
  plot(., color = "red",
       ci.style = "errorbar")+
  labs(title = "", x = "", y = "Perception of conflict") +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10))+
  scale_y_continuous(breaks=c(5,10,15,20,25,30), limits = c(5,30))


plot_model(model = model_5, type = "pred", 
           terms = "CLASS [1.Capitalistas, 2.Pequeños empleadores, 3.Pequeña burguesia, 4.Expertos directivos, 5.Expertos sin autoridad, 6.Supervisores calificados, 7.Supervisores no calificados, 8.Trabajadores calificados, 9.Trabajadores no calificados]")

GGally::ggcoef(x = model_5, exclude_intercept = T)
coefplot::multiplot(model_5, intercept = F, color = "blue")




plot_model(model_5, show.values = TRUE, 
           auto.label = T,colors = my_colors2, 
           vline.color = "grey") + theme_classic()


my_colors2 <- RColorBrewer::brewer.pal(10, "Blues")[c(4,7)]



plot_model(model_6, type = "re")

plot_model(model_5, type = "pred", terms = c("C_RATIO", "COUNTRY"))



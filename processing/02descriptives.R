# Code 1: Descriptive analysis


# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, dplyr, stringr, sjmisc, kableExtra, summarytools, 
               DT, gtsummary, stargazer, magrittr, janitor, htmlTable, 
               gridExtra, psych, lessR, ggrepel, ggthemes, sjlabelled, 
               ggpubr, RColorBrewer, sjPlot)

options(scipen=999)


# 2. Data -----------------------------------------------------------------

load("../output/data/db-proc.RData")
load("../output/data/df2-proc.RData")

names(db)
sapply(db, class)
names(df)

# 3. Analysis -------------------------------------------------------------


## 3.1. Descriptive ---- 

#db <- db %>% mutate(PSCi2 = (PSCi/9)*100) 

# dependent PSCi

db %>% select(PSCi) %>% 
  summarytools::descr(., weights = db$FACTOR)

sjPlot::plot_frq(db$PSCi, 
                 weight.by = db$FACTOR,
                 type = "histogram", 
                 show.mean = TRUE,
                 geom.colors = "#2171b5", normal.curve = T) +
  theme_classic() +
  labs(caption = "Fuente: Elaboración propia en base a ISSP (1999-2019)")


# independent N1

db %>% select(CLASS, UNION) %>% 
  sjmisc::frq(., weights = db$FACTOR)

sjPlot::plot_frq(db$CLASS, 
                 weight.by = db$FACTOR, 
                 geom.colors = "#2171b5") + 
  theme_classic()+
  labs(caption = "Fuente: Elaboración propia en base a ISSP (1999-2019)") +
  theme(axis.text.x = element_text(angle = -90))


# independent N2

df %>% select(RATIO_IC, CorpAll, MEAN_RATIO, LAG_RATIO) %>% 
  summarytools::descr(.) 

df[which.max(df$RATIO_IC),][,c(1,2)]
df[which.min(df$RATIO_IC),][,c(1,2)]

df[which.max(df$CorpAll),][,c(1,2)]
df[which.min(df$CorpAll),][,c(1,2)]

df[which.max(df$MEAN_RATIO),][,c(1,2)]
df[which.min(df$MEAN_RATIO),][,c(1,2)]

df[which.max(df$LAG_RATIO),][,c(1,2)]
df[which.min(df$LAG_RATIO),][,c(1,2)]


# control N1

db %>% select(SEX, IDEOLOGY, SUBJEC_CLASS) %>% 
  sjmisc::frq(., weights = db$FACTOR)

db %>% select(AGE) %>% 
  summarytools::descr(., weights = db$FACTOR)

sjPlot::plot_frq(db$AGE, 
                 weight.by = db$FACTOR,
                 type = "histogram", 
                 show.mean = TRUE,
                 geom.colors = "#2171b5") +
  theme_classic() +
  labs(caption = "Fuente: Elaboración propia en base a ISSP (1999-2019)")


sjPlot::plot_frq(db$SUBJEC_CLASS, 
                 weight.by = db$FACTOR,
                 type = "histogram", 
                 show.mean = TRUE,
                 geom.colors = "#2171b5")

# control N2

df %>% select(GDP, MEAN_GDP, LAG_GDP, SOC_EXPEND, UD) %>% 
  summarytools::descr(.)

df %>% group_by(COUNTRY, YEAR) %>% 
  count(UD) %>% 
  arrange(desc(UD)) %>% 
  print(n = nrow(.))

df[which.max(df$GDP),][,c(1,2)]
df[which.min(df$GDP),][,c(1,2)]

df[which.max(df$SOC_EXPEND),][,c(1,2)]
df[which.min(df$SOC_EXPEND),][,c(1,2)]

df[which.max(df$UD),][,c(1,2)]
df[which.min(df$UD),][,c(1,2)]

df[which.max(df$MEAN_GDP),][,c(1,2)]
df[which.min(df$MEAN_GDP),][,c(1,2)]

df[which.max(df$LAG_GDP),][,c(1,2)]
df[which.min(df$LAG_GDP),][,c(1,2)]


### 3.1.1 Descriptive table ----

reset_gtsummary_theme()
theme_gtsummary_compact(set_theme = T)
theme_gtsummary_language(language = "es", decimal.mark = ".", big.mark = ",", set_theme = T)
                          

db %>% select(YEAR, PSCi, CLASS, UNION, AGE, SEX, IDEOLOGY, SUBJEC_CLASS) %>% 
  gtsummary::tbl_summary(by = YEAR,
                         missing = "no",
                         type = list(all_continuous()~ 'continuous2'),
                         statistic = all_continuous() ~ c("{mean} ({sd})", 
                                                          "{min}, {max}")) %>% 
  add_n(last = F, statistic = "{N_nonmiss}") %>% 
  add_overall(., last = T, col_label = "**Total**, N = 81,488") %>% 
  bold_labels(.) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Valores**") %>%
  modify_footnote(update = all_stat_cols() ~ "Media (DE); Rango (Min, Max); Frecuencia (%)") %>% 
  modify_header(update = list(label ~ "**Variable**")) %>% 
  modify_caption("**Tabla 1. Estadísticos descriptivos nivel individual**") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("Fuente: Elaboración propia en base a ISSP 1999-2019"))


df %>% select(YEAR, RATIO_IC, CorpAll, GDP, SOC_EXPEND, UD) %>% 
  gtsummary::tbl_summary(by = YEAR,
                         missing = "no",
                         type = list(all_continuous()~ 'continuous2'),
                         statistic = all_continuous() ~ c("{mean} ({sd})", 
                                                          "{min}, {max}")) %>% 
  add_n(last = F, statistic = "{N_nonmiss}") %>% 
  add_overall(., last = T, col_label = "**Total**, N = 81") %>% 
  bold_labels(.) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Valores**") %>%
  modify_footnote(update = all_stat_cols() ~ "Media (DE); Rango (Min, Max)") %>% 
  modify_header(update = list(label ~ "**Variable**")) %>% 
  modify_caption("**Tabla 2. Estadísticos descriptivos nivel contextual**") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("Fuente: Elaboración propia en base a ISSP 1999-2019"))


## 3.1.2 Graphic PSCi evolution ----

my_colors <- RColorBrewer::brewer.pal(10, "Blues")[9]

formatter <- function(...){
  function(x) format(round(x, 1), ...)
}

db %>% group_by(COUNTRY, YEAR) %>% 
  summarise(promedio = round(mean(PSCi), digits = 1)) %>% 
  ggplot(aes(x = YEAR, y = promedio)) +
  geom_line(colour = my_colors) + 
  geom_point(colour = my_colors)+
  scale_x_continuous(breaks=seq(1999, 2019, 10)) +
  facet_wrap(.~COUNTRY, ncol = 6, scales = "fixed") + # for thesis uses 8 in ncol and free_y in scales
  scale_y_continuous(labels = formatter(nsmall = 1)) +
  geom_hline(aes(yintercept = 3.9), linetype = "dashed", color = "gray40") +
  theme_classic() + # and classic theme
  labs(x = "Año", 
       y = "Promedio PSCi", 
       title = "Evolución percepciones de conflicto social entre 1999 y 2019", 
       caption = "Nota1: Países con una sola observación representados por un punto
       Nota2: Línea horizontal representa promedio para todos los países y años
       Fuente: Elaboración propia en base a ISSP 1999-2019") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10))

#2171b5
## 3.1.3 Association table CLASS, UNION and PSCi ----

# total CLASS

db %>% select(PSCi, CLASS, YEAR) %>% 
  filter(!is.na(PSCi)&!is.na(CLASS)) %>% 
  group_by(YEAR) %>% 
  summarise(n= n())

# Table CLASS - Mean PSCi by Year

db %>% select(PSCi, CLASS, YEAR) %>% 
  filter(!is.na(PSCi)&!is.na(CLASS)) %>% 
  mutate(YEAR = as_factor(YEAR)) %>% 
  group_by(YEAR, CLASS) %>% 
  summarise(promedio = mean(x = PSCi, na.rm = T)) %>% 
  mutate(promedio = round(promedio, digits = 2)) %>% 
  pivot_wider(names_from = YEAR, values_from = c(promedio)) %>% 
  add_row(CLASS = 'Total observaciones', `1999` = 10369,  `2009` = 36244, `2019` = 25624)

oneway.test(PSCi ~ CLASS, data = db) # there is significance differences between categories of class
kruskal.test(PSCi ~ CLASS, data = db) #non parametric

# ANOVA for know the differences between the categories
t <- db %>% filter(!is.na(PSCi)&!is.na(CLASS)) %>% as.data.frame()
ANOVA(PSCi ~ CLASS, data =t)

# total Union

db %>% select(PSCi, UNION, YEAR) %>% 
  filter(!is.na(PSCi)&!is.na(UNION)) %>% 
  mutate(YEAR = as_factor(YEAR)) %>% 
  group_by(YEAR) %>% summarise(n = n())

# Table Union - Mean PSCi by Year

db %>% select(PSCi, UNION, YEAR) %>% 
  filter(!is.na(PSCi)&!is.na(UNION)) %>% 
  mutate(YEAR = as_factor(YEAR)) %>% 
  group_by(YEAR, UNION) %>% 
  summarise(promedio = mean(PSCi, na.rm = T)) %>% 
  pivot_wider(names_from = YEAR, values_from = promedio) %>% 
  add_row(UNION = 'Total observaciones', `1999` = 11785,  `2009` = 40723, `2019` = 25629)

oneway.test(PSCi ~ UNION, data = db) # there is significance differences between union and non-union
kruskal.test(PSCi ~ UNION, data = db)

t2 <- db %>% filter(!is.na(PSCi)&!is.na(UNION)) %>% as.data.frame()
ANOVA(PSCi ~ UNION, data = t2)


## Country - Mean PSCi

db %>% select(PSCi, ISO_COUNTRY) %>% 
  filter(!is.na(PSCi)) %>% 
  group_by(ISO_COUNTRY) %>% 
  summarise(promedio = mean(PSCi, na.rm = T)) %>% 
  print(n = nrow(.))

t3 <- db %>% filter(!is.na(PSCi)&!is.na(ISO_COUNTRY)) %>% as.data.frame()
ANOVA(PSCi ~ ISO_COUNTRY, data = t3) # careful with the residual values for countries CYP, DNK, KOR and THA


## 3.1.4 Correlation matrix ----

db %>% select(PSCi, CLASS, UNION, AGE, SEX, IDEOLOGY, SUBJEC_CLASS,
              RATIO_IC, CorpAll, GDP, SOC_EXPEND, UD) %>% 
  mutate_at(vars(1:12), ~ as_numeric(.)) %>% 
  sjPlot::tab_corr(., triangle = "lower")
 

## 3.1.5 Ratio IC and PSCi by Country and Year ----

# Option 1

dat_text <- data.frame(
  label = c("R = 0.77, p = 0.0002", "R = 0.26, p = 0.11", "R = 0.59, p = 0.0078"),
  YEAR   = c("1999", "2009", "2019"),
  PSCi = c(1.5, 1.5, 1.5),
  RATIO_IC = c(35,35,35))


db %>% select(ISO_COUNTRY, YEAR, PSCi, RATIO_IC) %>% 
  na.omit() %>% 
  group_by(ISO_COUNTRY, YEAR) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ggplot(aes(x = RATIO_IC, y = PSCi, label = ISO_COUNTRY)) +
  geom_point(size = 1.5, alpha = 0.8, colour = my_colors) +
  geom_smooth(stat = "smooth", position = "identity", method = "lm", 
              colour = "#1f1f1f", size = 1) +
  geom_text_repel(aes(label=ISO_COUNTRY), size=2.5, show.legend = FALSE, colour = my_colors) + 
  facet_wrap(.~YEAR, scales = "fixed") +
  scale_y_continuous("Promedio PSCi", limits = c(1, 7),
                     breaks = seq(1, 7, 1),
                     labels = formatter(nsmall = 1)) +
  scale_x_continuous("Ratio S80/S20", limits = c(0, 45),
                     breaks = seq(0, 45, 10)) +
  labs(x = "Ratio S80/S20", 
       y = "Promedio PSCi", 
       title = "Promedio de conflicto social percibido y Ratio 80/20 por país y año", 
       caption = " Fuente: Elaboración propia en base a ISSP 1999-2019") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10))+
  theme_classic() +
  geom_text(data = dat_text,
            mapping = aes(x = RATIO_IC, y = PSCi, label = label))

#2127b5


## 3.1.6 CorpALL and PSCi by Country and Year ----

dat_text2 <- data.frame(
  label = c("R = -0.51, p = 0.038", "R = -0.068, p = 0.7", "R = 0.029, p = 0.92"),
  YEAR   = c("1999", "2009", "2019"),
  PSCi = c(1.5, 1.5, 1.5),
  CorpAll = c(1.0,1.0,1.0))


db %>% select(ISO_COUNTRY, YEAR, PSCi, CorpAll) %>% 
  na.omit() %>% 
  group_by(ISO_COUNTRY, YEAR) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ggplot(aes(x = CorpAll, y = PSCi, label = ISO_COUNTRY)) +
  geom_point(size = 1.5, alpha = 0.8, colour = my_colors) +
  geom_smooth(stat = "smooth", position = "identity", method = "lm", 
              colour = "#1f1f1f", size = 1) +
  geom_text_repel(aes(label=ISO_COUNTRY), size=2.5, show.legend = FALSE, colour = my_colors) + 
  facet_wrap(.~YEAR, scales = "fixed") +
  scale_y_continuous("Promedio PSCi", limits = c(1, 7),
                     breaks = seq(1, 7, 1),
                     labels = formatter(nsmall = 1)) +
  scale_x_continuous("Índice Corporativismo", limits = c(-1.5, 1.5),
                     breaks = seq(-1.5, 1.5, 0.5)) +
  labs(x = "Índice Corporativismo", 
       y = "Promedio PSCi", 
       title = "Promedio de conflicto social percibido y grado de corporativismo por país y año", 
       caption = " Fuente: Elaboración propia en base a ISSP 1999-2019") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10))+
  theme_classic() +
  geom_text(data = dat_text2,
            mapping = aes(x = CorpAll, y = PSCi, label = label))










# option 2 ----

cater <- db %>% select(ISO_COUNTRY, YEAR, PSCi, RATIO_IC) %>% 
  na.omit() %>% 
  group_by(ISO_COUNTRY, YEAR) %>% 
  summarise_all(mean, na.rm = T) %>% as.data.frame()


ggscatter(cater, x = "RATIO_IC", y = "PSCi", color = "#2127b5",palette = "Blues", 
          cor.coef = T,
          facet.by = "YEAR",
          add = "reg.line", 
          add.params = list(color = "black", fill = "lightgray"),
          label = "ISO_COUNTRY",
          cor.coef.coord = c(10, 7),
          size = 1.5,
          conf.int = T,
          cor.coef.size = 4,
          ggtheme = theme_classic(),
          font.label = c(8, "plain"))

cater2 <- db %>% select(ISO_COUNTRY, YEAR, PSCi, CorpAll) %>% 
  na.omit() %>% 
  group_by(ISO_COUNTRY, YEAR) %>% 
  summarise_all(mean, na.rm = T) %>% as.data.frame()


ggscatter(cater2, x = "CorpAll", y = "PSCi", color = "#2127b5",palette = "Blues", 
          cor.coef = T,
          facet.by = "YEAR",
          add = "reg.line", 
          add.params = list(color = "black", fill = "lightgray"),
          label = "ISO_COUNTRY",
          size = 1.5,
          conf.int = T,
          cor.coef.size = 4,
          ggtheme = theme_classic(),
          font.label = c(8, "plain"))














# modelo ----

a <- lm(PSCi ~ SUBJEC_CLASS, data = db)
b <- lm(PSCi ~ SUBJEC_CLASS + UNION, data = db)
c <- lm(PSCi ~ SUBJEC_CLASS + UNION + relevel(IDEOLOGY,ref="Derecha"), data = db)
d <- lm(PSCi ~ SUBJEC_CLASS + UNION + relevel(IDEOLOGY,ref="Derecha") + SEX + AGE, data = db)

sjPlot::tab_model(list(a,b,c,d), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),string.pred = "Predictores", string.est = "β")



results_1 <- lmer(PSCi2 ~ 1 + CLASS + UNION + (1 | COUNTRY), data = db)

results_2 <- lmer(PSCi2 ~ 1 + CLASS + UNION + SEX + AGE + IDEOLOGY + (1 | COUNTRY), data = db)

results_3 <- lmer(PSCi2 ~ 1 + CLASS + UNION + SEX + AGE + IDEOLOGY + RATIO_IC + CorpAll + (1 | COUNTRY), data = db)

results_4 <- lmer(PSCi2 ~ 1 + CLASS + UNION + SEX + AGE + IDEOLOGY + RATIO_IC + CorpAll + GDP + UD + SOC_EXPEND + (1 | COUNTRY), data = db)

results_5 <- lmer(PSCi2 ~ 1 + CLASS + UNION + SEX + AGE + IDEOLOGY + RATIO_IC + CorpAll + GDP + UD + SOC_EXPEND + CLASS*RATIO_IC + (1 | COUNTRY), data = db)


sjPlot::tab_model(list(results_1, results_2, results_3, results_4, results_5), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),string.pred = "Predictores", string.est = "β")

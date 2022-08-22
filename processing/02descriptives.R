# Code 1: Descriptive analysis


# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, dplyr, stringr, sjmisc, summarytools, 
               gtsummary, stargazer, magrittr, janitor, easystats, 
               gridExtra, psych, lessR, ggrepel, ggthemes, sjlabelled, 
               ggpubr, RColorBrewer, sjPlot, patchwork, GGally, broom, 
               effectsize, rstatix, cowplot)

options(scipen=999)


# 2. Data -----------------------------------------------------------------

load("../output/data/db-proc.RData")
load("../output/data/df2-proc.RData")

names(db)
sapply(db, class)
names(df)

# 3. Analysis -------------------------------------------------------------


## 3.1. Univariate ---- 

# dependent PSCi

db %>% select(PSCi) %>% 
  summarytools::descr(., weights = db$FACTOR)

sjPlot::plot_frq(db$PSCi, 
                 weight.by = db$FACTOR,
                 type = "histogram", 
                 show.mean = TRUE,
                 geom.colors = "#2171b5", 
                 normal.curve = T) +
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

df %>% select(RATIO_IC, CorpAll) %>% 
  summarytools::descr(.) 

df[which.max(df$RATIO_IC),][,c(1,2)]
df[which.min(df$RATIO_IC),][,c(1,2)]

df[which.max(df$CorpAll),][,c(1,2)]
df[which.min(df$CorpAll),][,c(1,2)]

# control N1

db %>% select(SEX, IDEOLOGY) %>% 
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


# control N2

df %>% select(GDP, SOC_EXPEND, UD) %>% 
  summarytools::descr(.)

df[which.max(df$GDP),][,c(1,2)]
df[which.min(df$GDP),][,c(1,2)]

df[which.max(df$SOC_EXPEND),][,c(1,2)]
df[which.min(df$SOC_EXPEND),][,c(1,2)]

df[which.max(df$UD),][,c(1,2)]
df[which.min(df$UD),][,c(1,2)]

### 3.1.1 Descriptive table 

reset_gtsummary_theme()
theme_gtsummary_compact(set_theme = T)
theme_gtsummary_language(language = "es", decimal.mark = ".", big.mark = ",", set_theme = T)
                          
t1 <- db %>% select(PSCi, CLASS, UNION, AGE, SEX, IDEOLOGY) %>% 
  gtsummary::tbl_summary(
    missing = "no",
    type = list(all_continuous()~ 'continuous2'),
    statistic = all_continuous() ~ c("{mean} ({sd})", 
                                     "{min}, {max}")) %>% 
  add_n(last = F, statistic = "{N_nonmiss}") 

t2 <- df %>% select(RATIO_IC, CorpAll, GDP, SOC_EXPEND, UD) %>% 
  gtsummary::tbl_summary(
    missing = "no",
    type = list(all_continuous()~ 'continuous2'),
    statistic = all_continuous() ~ c("{mean} ({sd})", 
                                     "{min}, {max}")) %>% 
  add_n(last = F, statistic = "{N_nonmiss}")

tbl_stack(tbls = list(t1, t2)) %>% 
  bold_labels(.) %>% 
  modify_footnote(update = all_stat_cols() ~ "Media (DE); Rango (Min, Max); Frecuencia (%)") %>% 
  modify_header(update = list(label ~ "**Variable**")) %>% 
  modify_header(all_stat_cols() ~ "**Valores**") %>% 
  modify_caption("**Tabla 1. Estadísticos descriptivos nivel individual**") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("Fuente: Elaboración propia en base a ISSP 1999-2019, WIID, ICTWSS y OCDE"))


### 3.1.2 PSCi evolution 1999-2019

my_colors <- RColorBrewer::brewer.pal(10, "Blues")[1]

formatter <- function(...){
  function(x) format(round(x, 1), ...)
}

db %>% group_by(COUNTRY, YEAR) %>% 
  summarise(promedio = round(mean(PSCi), digits = 1)) %>% 
  ggplot(aes(x = YEAR, y = promedio)) +
  geom_line(colour = my_colors) +
  geom_point(colour = my_colors) +
  scale_x_continuous(breaks=seq(1999, 2019, 10)) +
  scale_y_continuous(labels = formatter(nsmall = 1)) +
  geom_hline(aes(yintercept = 3.9), linetype = "dashed", color = "grey30") +
  labs(x = "Año", 
       y = "Promedio PSCi")+
  facet_wrap(.~COUNTRY, ncol = 5, scales = "fixed") +
  theme_classic()


## 3.1.3 PSCi decomposed by indicator

conf <- db %>% select(starts_with("CONFLICT"), COUNTRY) %>% 
  mutate_at(vars(starts_with("CONFLICT")), ~ if_else(. %in% c(0,1),0,1))

rp <- conf %>% select(COUNTRY, CONFLICT_RP) %>% 
  mutate(filtro = if_else(CONFLICT_RP == 1, "RP", "No")) %>% 
  group_by(COUNTRY, filtro) %>% 
  summarise(total = n()) %>% 
  mutate(prop = prop.table(total)*100) %>% 
  filter(filtro == "RP") %>% select(everything(), -total)

mw <- conf %>% select(COUNTRY, CONFLICT_MW) %>% 
  mutate(filtro = if_else(CONFLICT_MW == 1, "MW", "No")) %>% 
  group_by(COUNTRY, filtro) %>% 
  summarise(total = n()) %>% 
  mutate(prop = prop.table(total)*100) %>% 
  filter(filtro == "MW") %>% select(everything(), -total)

wcmc <- conf %>% select(COUNTRY, CONFLICT_WCMC) %>% 
  mutate(filtro = if_else(CONFLICT_WCMC == 1, "WCMC", "No")) %>% 
  group_by(COUNTRY, filtro) %>% 
  summarise(total = n()) %>% 
  mutate(prop = prop.table(total)*100) %>% 
  filter(filtro == "WCMC") %>% select(everything(), -total)

conf_df <- rbind(rp,mw,wcmc)

theme_dotplot <- theme_bw(14) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())



windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))

my_pretty_theme <- theme_bw(base_family = "Roboto Condensed", base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "italic", size = rel(0.85), color = "grey30"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        # Bold legend titles
        legend.title = element_text(face = "bold", size = rel(1)),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(0.7), hjust = 0.5),
        # Bold axis titles
        axis.title = element_text(face = "bold", size = rel(0.85)),
        # Add some space above the x-axis title and make it left-aligned
        axis.title.x = element_text(margin = margin(t = 5)),
        # Add some space to the right of the y-axis title and make it top-aligned
        axis.title.y = element_text(margin = margin(r = 5)),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_rect(fill = "black", color = NA),
        # Add a thin grey border around all the plots to tie in the facet titles
        panel.border = element_rect(color = "#474747", fill = NA),
        axis.text.y = element_text(size = rel(1.1)),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.05, color = "#b4b4b4"),
        panel.grid.minor.x = element_blank())


conf_df %>% 
  ggplot(aes(x = prop, y = fct_reorder2(`COUNTRY`, filtro, prop, .desc = T), group=filtro))+
  geom_point(aes(shape=filtro, color=filtro), size = 1.6)+
  coord_cartesian(xlim = c(0,100))+
  scale_x_continuous(labels = function(prop){paste0(prop, "%")})+
  labs(title = NULL,
       y = NULL,
       x = "Proporción intensidad conflictos",
       color = "Tipo conflicto",
       shape = "Tipo conflicto")+
  scale_shape_manual(values=c(19, 17, 18))+
  scale_color_manual(values = c("#00477b", "#6a9cd8", "#686868"))+
  my_pretty_theme
  

library(scales)
show_col(brewer.pal(10, "BuPu"))



# 3.2. Bivariate ---- 

## 3.2.1 PSCi by Class

db %>% select(PSCi, CLASS, FACTOR) %>% 
  group_by(CLASS) %>% 
  summarise(promedio = weighted.mean(x = PSCi, w = FACTOR, na.rm = T)) %>% 
  mutate(promedio = round(promedio, digits = 2)) # total


db %>% select(PSCi, CLASS, WAVE, FACTOR) %>% 
  group_by(WAVE, CLASS) %>% 
  summarise(promedio = weighted.mean(x = PSCi, w = FACTOR, na.rm = T)) %>% 
  mutate(promedio = round(promedio, digits = 2)) %>% 
  pivot_wider(names_from = WAVE, values_from = c(promedio)) # by year


## Anova test 

# homogeneity of variances 
bartlett.test(PSCi ~ CLASS, data = db) # not equal

# welch anova
res_anova <- oneway.test(PSCi ~ CLASS, data = db, var.equal = F)
efect_anova <- effectsize::omega_squared(res_anova)
interpret_omega_squared(efect_anova) # medium

# post hoc Games Howell
pwc <- games_howell_test(data = db, PSCi ~ CLASS, detailed = T)

# non parametric
db %>% kruskal.test(PSCi ~ CLASS, data = .)

pairwise.wilcox.test(db$PSCi, db$CLASS,
                     p.adjust.method = "BH")


## 3.2.2 PSCi by Union

db %>% select(PSCi, UNION, FACTOR) %>% 
  group_by(UNION) %>% 
  summarise(promedio = weighted.mean(x = PSCi, w = FACTOR, na.rm = T)) %>% 
  mutate(promedio = round(promedio, digits = 2)) # total


db %>% select(PSCi, UNION, WAVE, FACTOR) %>% 
  group_by(WAVE, UNION) %>% 
  summarise(promedio = weighted.mean(x = PSCi, w = FACTOR, na.rm = T)) %>% 
  mutate(promedio = round(promedio, digits = 2)) %>% 
  pivot_wider(names_from = WAVE, values_from = c(promedio)) # by year


## T test 

# homogeneity of variances
var.test(PSCi ~ UNION, data = db) # not equal
levene_test(data = db, PSCi ~ UNION) # other option: not equal

# t test Welch 
res_t <- t.test(PSCi ~ UNION, data = db, var.equal = F)
report_effectsize(res_t)
report(res_t) # complete report 


### 3.2.3 Examples of report 

# examples
a <- aov(PSCi ~ CLASS, data = db)
report(a) # The main effect of CLASS is statistically significant and small (F(8, 39844) = 83.50, p < .001; Eta2 = 0.02, 95% CI [0.01, 1.00]) Effect sizes were labelled following Field's (2013) recommendations.

b <- welch_anova_test(data = db, PSCi ~ CLASS) # for rstatix pck

c <- t_test(data = db, PSCi ~ UNION, var.equal = F)

# in graph
ggboxplot(db, x = "CLASS", y = "PSCi") +
  labs(subtitle = get_test_label(b, detailed = TRUE))

ggboxplot(db, x = "UNION", y = "PSCi") +
  labs(subtitle = get_test_label(c, detailed = TRUE))

## Final report

# REPORT CLASS
res_anova
# Report with Welch Anova:
# The main effect of CLASS is statistically significant and medium (F(8, 6844.2) = 92.803, p < .001; omega2 = 0.10, 95% CI [0.09, 1.00])
# Effect sizes were labelled following Field's (2013) recommendations.

# REPORT UNION
report(res_t)
# Report for Welch Two Sample t test:
# The Welch Two Sample t-test testing the difference of PSCi by UNION (mean in group No = 3.92, mean in group Si = 3.59) suggests that the effect is positive, statistically significant, and very small (difference = 0.33, 95% CI [0.29, 0.37], t(37428.30) = 16.33, p < .001; Cohen's d = 0.17, 95% CI [0.15, 0.19])
# Effect sizes were labelled following Cohen's (1988) recommendations


## Boxplots

box1 <- plot_grpfrq(db$PSCi, db$CLASS, type = "box", coord.flip = T) + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "",
       subtitle = "Anova Welch, F(8, 6844.2) = 92.8, p = <0.001, omega2 = 0.10, n = 39,853",
       y = "Promedio PSCi",
       x = "Posición de clase")


box2 <- plot_grpfrq(db$PSCi, db$UNION, type = "box", coord.flip = T) +
  labs(title = "",
       subtitle = "T test, t(37428.30) = 16.33, p = <0.001; Cohen's d = 0.17, n = 39,853",
       y = "Promedio PSCi",
       x = "Afiliación sindical")

plot_grid(box1, box2, labels=c("A", "B"), ncol = 1, nrow = 2)
ggdraw() +
  draw_plot(box1, 0, .5, 1, .5) +
  draw_plot(box2, 0, 0, 1, .5) +
  draw_plot_label(c("A", "B"), c(0, 0), c(1, 0.5), size = 11)
## 3.2.4 Correlation matrix 

# matriz
lab_cor <- c("Perceived Social Conflict Index", "Posicion de clase",
             "Afiliacion sindical", "Edad", "Sexo", "Identificacion politica",
             "Ratio 80/20", "Indice corporativismo", 
             "GDP Per capita", "Gasto social (%GDP)", "Densidad sindical")

db %>% select(PSCi, CLASS, UNION, AGE, SEX, IDEOLOGY,
              RATIO_IC, CorpAll, GDP, SOC_EXPEND, UD) %>% 
  mutate_at(vars(1:11), ~ as_numeric(.)) %>% 
  sjPlot::tab_corr(., triangle = "lower", var.labels = lab_cor)

# corrplot
ttc <- db %>% select(PSCi, CLASS, UNION, AGE, SEX, IDEOLOGY,
                     RATIO_IC, CorpAll, GDP, SOC_EXPEND, UD) %>% 
  mutate_at(vars(1:11), ~ as_numeric(.)) %>% cor(use = "complete.obs", method = "pearson")

ttc[lower.tri(ttc)] <- NA
ttc

ttc <- ttc %>% as.data.frame() %>% rownames_to_column("measure2") %>% 
  pivot_longer(cols = -measure2,
               names_to = "measure1",
               values_to = "cor") %>% 
  mutate(nice_cor = round(cor, 2)) %>% 
  filter(measure2 != measure1) %>%
  filter(!is.na(cor)) %>% 
  mutate(measure1 = fct_inorder(measure1),
         measure2 = fct_inorder(measure2))

ttc$measure2 <- car::recode(ttc$measure2, recodes = c("'PSCi' = 'PSCi'; 'CLASS' = 'Clase'; 
                                                      'UNION' = 'Sindicato'; 'AGE' = 'Edad';
                                                      'SEX' = 'Sexo'; 'IDEOLOGY' = 'Ideologia';
                                                      'RATIO_IC' = 'Ratio 80/20'; 'CorpAll' = 'Corporativismo';
                                                      'SOC_EXPEND' = 'Gasto social'; 'UD' = 'Densidad sindical'"))

ttc$measure1 <- car::recode(ttc$measure1, recodes = c("'PSCi' = 'PSCi'; 'CLASS' = 'Clase'; 
                                                      'UNION' = 'Sindicato'; 'AGE' = 'Edad';
                                                      'SEX' = 'Sexo'; 'IDEOLOGY' = 'Ideologia';
                                                      'RATIO_IC' = 'Ratio 80/20'; 'CorpAll' = 'Corporativismo';
                                                      'SOC_EXPEND' = 'Gasto social'; 'UD' = 'Densidad sindical'"))


ttc <- ttc %>% mutate(measure1 = fct_inorder(measure1),
               measure2 = fct_inorder(measure2))

ggplot(ttc, 
       aes(x = measure2, y = measure1, fill = cor)) +
  geom_tile() +
  geom_text(aes(label = nice_cor)) +
  scale_fill_gradient2(low = "#E16462", mid = "white", high = "#0D0887",
                       limits = c(-1, 1), name = "correlation") + 
  labs(x = NULL, y = NULL) +
  coord_equal() +
  theme_minimal() + theme(plot.title = element_text(size = 12), 
                          axis.title = element_text(size = 11),
                          plot.caption = element_text(size = 10)) +
  theme(panel.grid = element_blank())


## 3.2.5 PSCi by Ratio 80/20 per country and year 

# Option 1
dat_text <- data.frame(
  label = c("R = 0.71", "R = 0.26", "R = 0.51"),
  YEAR   = c("1999", "2009", "2019"),
  PSCi = c(1.5, 1.5, 1.5),
  RATIO_IC = c(35,35,35))


db %>% select(ISO_COUNTRY, YEAR, PSCi, RATIO_IC) %>% 
  na.omit() %>% 
  group_by(ISO_COUNTRY, YEAR) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ggplot(aes(x = RATIO_IC, y = PSCi, label = ISO_COUNTRY)) +
  geom_point(size = 1, alpha = 0.9, colour = my_colors) +
  geom_smooth(stat = "smooth", position = "identity", method = "lm", 
              colour = "#1f1f1f", size = 0.7) +
  geom_text_repel(aes(label=ISO_COUNTRY), size=2, show.legend = FALSE, colour = my_colors) + 
  facet_wrap(.~YEAR, scales = "fixed") +
  scale_y_continuous("Promedio PSCi", limits = c(1, 7),
                     breaks = seq(1, 7, 1),
                     labels = formatter(nsmall = 1)) +
  scale_x_continuous("Ratio S80/S20", limits = c(0, 45),
                     breaks = seq(0, 45, 10)) +
  labs(x = "Ratio S80/S20", 
       y = "Promedio PSCi") +
  theme_classic() +
  geom_text(data = dat_text,
            mapping = aes(x = RATIO_IC, y = PSCi, label = label),size = rel(3))

## 3.2.6 PSCi by CorpAll per country and year

dat_text2 <- data.frame(
  label = c("R = -0.55", "R = -0.12", "R = -0.04"),
  YEAR   = c("1999", "2009", "2019"),
  PSCi = c(1.5, 1.5, 1.5),
  CorpAll = c(1.0,1.0,1.0))


db %>% select(ISO_COUNTRY, YEAR, PSCi, CorpAll) %>% 
  na.omit() %>% 
  group_by(ISO_COUNTRY, YEAR) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ggplot(aes(x = CorpAll, y = PSCi, label = ISO_COUNTRY)) +
  geom_point(size = 1, alpha = 0.9, colour = my_colors) +
  geom_smooth(stat = "smooth", position = "identity", method = "lm", 
              colour = "#1f1f1f", size = 0.7) +
  geom_text_repel(aes(label=ISO_COUNTRY), size=2, show.legend = FALSE, colour = my_colors) + 
  facet_wrap(.~YEAR, scales = "fixed") +
  scale_y_continuous("Promedio PSCi", limits = c(1, 7),
                     breaks = seq(1, 7, 1),
                     labels = formatter(nsmall = 1)) +
  scale_x_continuous("Índice Corporativismo", limits = c(-1.5, 1.5),
                     breaks = seq(-1.5, 1.5, 0.5)) +
  labs(x = "Índice Corporativismo", 
       y = "Promedio PSCi") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10))+
  theme_classic() +
  geom_text(data = dat_text2,
            mapping = aes(x = CorpAll, y = PSCi, label = label), size = rel(3))

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


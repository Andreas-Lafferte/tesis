# Code 1: Analysis 


# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               dplyr,
               stringr,
               sjmisc, 
               kableExtra,
               sjPlot,
               summarytools,
               DT,
               lme4,
               easystats,
               gtsummary,
               stargazer,
               magrittr,
               janitor,
               htmlTable, 
               gridExtra)

options(scipen=999)


# 2. Data -----------------------------------------------------------------

load("../output/data/db-proc.RData")

names(db)
sapply(db, class)

# 3. Analysis -------------------------------------------------------------


## 3.1. Descriptive ---- 

# dependent PSCi

db %>% select(PSCi) %>% 
  summarytools::descr(., weights = db$FACTOR)

sjPlot::plot_frq(db$PSCi, 
                 weight.by = db$FACTOR,
                 type = "histogram", 
                 show.mean = TRUE,
                 geom.colors = "#2171b5") +
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

db %>% select(RATIO_IC, CorpAll, MEAN_RATIO, LAG_RATIO) %>% 
  summarytools::descr(.) 

db[which.max(db$RATIO_IC),][,c(2,3)]
db[which.min(db$RATIO_IC),][,c(2,3)]

db[which.max(db$CorpAll),][,c(2,3)]
db[which.min(db$CorpAll),][,c(2,3)]

db[which.max(db$MEAN_RATIO),][,c(2,3)]
db[which.min(db$MEAN_RATIO),][,c(2,3)]

db[which.max(db$LAG_RATIO),][,c(2,3)]
db[which.min(db$LAG_RATIO),][,c(2,3)]


# control N1

db %>% select(SEX, DEGREE, INCOME) %>% 
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

db %>% select(GDP, MEAN_GDP, LAG_GDP, SOC_EXPEND, UD) %>% 
  summarytools::descr(.)

db %>% group_by(COUNTRY, YEAR) %>% 
  count(UD) %>% 
  arrange(desc(UD)) %>% 
  print(n = nrow(.))

db[which.max(db$GDP),][,c(2,3)]
db[which.min(db$GDP),][,c(2,3)]

db[which.max(db$SOC_EXPEND),][,c(2,3)]
db[which.min(db$SOC_EXPEND),][,c(2,3)]

db[which.max(db$UD),][,c(2,3)]
db[which.min(db$UD),][,c(2,3)]

db[which.max(db$MEAN_GDP),][,c(2,3)]
db[which.min(db$MEAN_GDP),][,c(2,3)]

db[which.max(db$LAG_GDP),][,c(2,3)]
db[which.min(db$LAG_GDP),][,c(2,3)]


### 3.1.1 Descriptive table ----

reset_gtsummary_theme()
theme_gtsummary_compact(set_theme = T)
theme_gtsummary_language(language = "es", decimal.mark = ".", big.mark = ",", set_theme = T)
                          

tabla1 <- db %>% select(YEAR, PSCi, CLASS, UNION, SEX, DEGREE, AGE, 
              RATIO_IC, LAG_RATIO, CorpAll, GDP, LAG_GDP, SOC_EXPEND, 
              UD) %>% 
  gtsummary::tbl_summary(by = YEAR,
                         missing = "no",
                         type = list(all_continuous()~ 'continuous2'),
                         statistic = all_continuous() ~ c("{mean} ({sd})", 
                                                          "{min}, {max}")) %>% 
  bold_labels() %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Valores**") %>%
  modify_footnote(update = all_stat_cols() ~ "Media (DE); Rango (Min, Max); Frecuencia (%)") %>% 
  modify_header(update = list(
    label ~ "**Variable**")) %>% 
  add_n() %>% 
  modify_caption("**Tabla 1. Estadísticos descriptivos**") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("Fuente: Elaboración propia en base a ISSP 1999-2019"))

# dfsummary
a <- db %>% select(PSCi, CLASS, UNION, SEX, DEGREE, AGE, 
                   RATIO_IC, LAG_RATIO, CorpAll, GDP, LAG_GDP, SOC_EXPEND, 
                   UD) 

view(dfSummary(a, 
               plain.ascii = FALSE,
               varnumbers = F,
               labels.col = T,
               headings = F, 
               valid.col = T, 
               na.col = F, 
               style = "grid", 
               split.cells = 3))


## 3.1.2 Graphic PSCi evolution ----

formatter <- function(...){
  function(x) format(round(x, 1), ...)
}

db %>% group_by(COUNTRY, YEAR) %>% 
  summarise(promedio = round(mean(PSCi), digits = 1)) %>% 
  ggplot(aes(x = YEAR, y = promedio)) +
  geom_line(colour = "#2171b5") + 
  geom_point(colour = "#2171b5")+
  scale_x_continuous(breaks=seq(1999, 2019, 10)) +
  facet_wrap(.~COUNTRY, ncol = 6, scales = "fixed") + # for thesis uses 8 in ncol and free_y in scales
  scale_y_continuous(labels = formatter(nsmall = 1)) +
  geom_hline(aes(yintercept = 3.9), linetype = "dashed", color = "gray40") +
  theme_classic() + # and classic theme
  labs(x = "Año", 
       y = "Promedio PSCi", 
       title = "Evolución percepciones de conflicto social entre 1999 y 2019", 
       caption = "Nota: Países con una sola observación representados por un punto
       Fuente: Elaboración propia en base a ISSP 1999-2019") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10))
  
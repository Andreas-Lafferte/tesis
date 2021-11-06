# Code 2: Process ISSP 2009

# 1. Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               lubridate,
               stringr, 
               sjlabelled, 
               ggplot2, 
               sjmisc, 
               kableExtra,
               sjPlot,
               summarytools,
               haven,
               stargazer,
               ggpubr,
               psych,
               magrittr)
options(scipen=999)

# 2. Data ----
issp09 <- read_dta("../input/data/ISSP2009.dta")
sapply(issp09, class)
names(issp09)

# 3. Codification ----


# Code 6: Process OECD - EURO

# 1. Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               stringr, 
               sjlabelled, 
               sjmisc, 
               kableExtra,
               sjPlot,
               summarytools,
               haven,
               stargazer,
               magrittr,
               Rilostat)
options(scipen=999)

# 2. Data ----
ocde <- read_csv("~/GitHub/tesis/input/data/DP_LIVE_07122021004246333.csv")
euro <- read_csv("~/GitHub/tesis/input/data/gov_10a_exp_1_Data.csv")
sapply(ocde, class)
sapply(euro, class)

# 3. Processing ----

# Input China 2009
# No data for Taiwan, South Africa and Russia

# 3.1 OECD ----
ocde <- ocde %>% group_by(LOCATION) %>% 
  filter(LOCATION == "AUS" & TIME %in% c(1999,2009) |
         LOCATION == "AUT" & TIME == 2009 |
         LOCATION == "BEL" & TIME == 2009 |
         LOCATION == "CAN" & TIME == 1999 |
         LOCATION == "CHE" & TIME %in% c(2009,max(TIME)) |
         LOCATION == "CHL" & TIME %in% c(1999,2009,max(TIME)) |
         LOCATION == "CZE" & TIME %in% c(1999,2009,max(TIME)) |
         LOCATION == "DEU" & TIME %in% c(1999,2009,max(TIME)) |
         LOCATION == "DNK" & TIME %in% c(2009,max(TIME)) |
         LOCATION == "ESP" & TIME %in% c(1999,2009) |
         LOCATION == "EST" & TIME == 2009 |
         LOCATION == "FIN" & TIME %in% c(2009,max(TIME)) |
         LOCATION == "FRA" & TIME == 2009 |
         LOCATION == "GBR" & TIME %in% c(1999,2009, max(TIME)) |
         LOCATION == "HUN" & TIME %in% c(1999,2009) |
         LOCATION == "IRL" & TIME == 1999 |
         LOCATION == "ISL" & TIME %in% c(2009, max(TIME)) |
         LOCATION == "ISR" & TIME %in% c(2009, max(TIME)) |
         LOCATION == "ITA" & TIME %in% c(2009, max(TIME)) |
         LOCATION == "JPN" & TIME %in% c(2009, max(TIME)) |
         LOCATION == "KOR" & TIME == 2009 |
         LOCATION == "LTU" & TIME %in% c(2009, max(TIME)) |
         LOCATION == "LVA" & TIME %in% c(1999,2009) |
         LOCATION == "NOR" & TIME %in% c(1999,2009) |
         LOCATION == "NZL" & TIME %in% c(1999,2009,max(TIME)) |
         LOCATION == "POL" & TIME %in% c(1999,2009) |
         LOCATION == "PRT" & TIME == 2009 |
         LOCATION == "SVK" & TIME %in% c(1999,2009) |
         LOCATION == "SVN" & TIME %in% c(2009,max(TIME)) |
         LOCATION == "SWE" & TIME %in% c(1999,2009) |
         LOCATION == "TUR" & TIME == 2009 |
         LOCATION == "USA" & TIME %in% c(1999,2009)) %>% 
  select(country = LOCATION, year = TIME, value = Value) 

ocde <- ocde %>% mutate(country = case_when(country == "AUS" ~ "Australia",
                                            country == "AUT" ~ "Austria",
                                            country == "BEL" ~ "Belgica",
                                            country == "CAN" ~ "Canada",
                                            country == "CHE" ~ "Suiza",
                                            country == "CHL" ~ "Chile",
                                            country == "CZE" ~ "Rep Checa",
                                            country == "DEU" ~ "Alemania",
                                            country == "DNK" ~ "Dinamarca",
                                            country == "ESP" ~ "España",
                                            country == "EST" ~ "Estonia",
                                            country == "FIN" ~ "Finlandia",
                                            country == "FRA" ~ "Francia",
                                            country == "GBR" ~ "Gran Bretaña",
                                            country == "HUN" ~ "Hungria",
                                            country == "IRL" ~ "Irlanda",
                                            country == "ISL" ~ "Islandia",
                                            country == "ISR" ~ "Israel",
                                            country == "ITA" ~ "Italia",
                                            country == "JPN" ~ "Japon",
                                            country == "KOR" ~ "Corea del Sur",
                                            country == "LTU" ~ "Lituania",
                                            country == "LVA" ~ "Letonia",
                                            country == "NOR" ~ "Noruega",
                                            country == "NZL" ~ "Nueva Zelanda",
                                            country == "POL" ~ "Polonia",
                                            country == "PRT" ~ "Portugal",
                                            country == "SVK" ~ "Eslovaquia",
                                            country == "SVN" ~ "Eslovenia",
                                            country == "SWE" ~ "Suecia",
                                            country == "TUR" ~ "Turquia",
                                            country == "USA" ~ "USA"),
                        year = case_when(year == 1999 ~ 1999,
                                         year == 2009 ~ 2009,
                                         year %in% c(2017,2018,2019) ~ 2019))

# 3.2 EURO STAT ----
euro <- euro %>% group_by(GEO) %>% filter(GEO == "Bulgaria" & TIME %in% c(1999,2009,2019) |
                                            GEO == "Cyprus" & TIME %in% c(1999,2009) |
                                            GEO == "Croatia" & TIME %in% c(2009,2019)) %>% 
  select(country = GEO, year = TIME, value = Value) %>% mutate(year = as.numeric(year),
                                                               value = as.numeric(value))

euro$country <- car::recode(euro$country, recodes = c("'Cyprus' = 'Chipre'; 'Croatia' = 'Croacia'"))

# 3.3 ECLAC and STATISTA ----

a <- rbind(c('Argentina', 2009, 10.5),
           c('China', 2009, 25.65)) %>% as.data.frame()

names(a)<-c("country","year","value")

a <- a %>% mutate(year = as.numeric(year),
                  value = as.numeric(value))

# 3.4 Final data ----
data <- rbind(euro, a)
data <- rbind(data,ocde) %>% select(COUNTRY = country, YEAR = year, SOC_EXPEND = value)

# Labels
data$COUNTRY <- sjlabelled::set_label(data$COUNTRY, label = c('País'))
data$YEAR <- sjlabelled::set_label(data$YEAR, label = c('Año'))
data$SOC_EXPEND <- sjlabelled::set_label(data$SOC_EXPEND, label = c('Gasto social (%GDP)'))

oecd <- data
# 4 Save ----
save(oecd, file = "../output/data/oecd_euro.RData")
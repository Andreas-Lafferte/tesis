# Code 4: Process data level 2

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
wiid_or <- read_dta("~/GitHub/tesis/input/data/WIID_31MAY2021.dta")
ictwss_or <- haven::read_dta(url("https://aias.s3.eu-central-1.amazonaws.com/website/uploads/ICTWSS_v6_1_Stata_release.dta"))
sapply(wiid_or, class)
names(wiid_or)
sapply(ictwss_or, class)
names(ictwss_or)

# 3. Processing WIID -----

wiid <- wiid_or %>% select(country, year, ratio_top20bottom20, 34:39, gdp, source_detailed)

wiid <- wiid %>% filter(country %in% c("Germany", "Argentina", "Austria", "Australia", "Belgium", 
                                       "Bulgaria", "Chile", "China", "Cyprus", "Korea, Republic of", 
                                       "Croatia", "Denmark", "Slovakia", "Slovenia", "Spain", "Philippines",
                                       "Finland", "France", "United Kingdom", "Hungary", "Iceland", "Israel",
                                       "Italy", "Japan", "Latvia", "Lithuania", "Norway", "New Zealand", "Poland",
                                       "Portugal", "Czechia", "Russia", "South Africa", "Sweden", "Switzerland", "Taiwan",
                                       "Turkey", "Ukraine", "United States", "Venezuela", "Thailand", "Canada"))

wiid <- wiid %>% mutate(COUNTRY = case_when(country == "Germany" ~ "Alemania",
                                            country == "Argentina" ~ "Argentina",
                                            country == "Australia" ~ "Australia",
                                            country == "Austria" ~ "Austria",
                                            country == "Belgium" ~ "Belgica",
                                            country == "Bulgaria" ~ "Bulgaria",
                                            country == "Chile" ~ "Chile",
                                            country == "China" ~ "China",
                                            country == "Cyprus" ~ "Chipre",
                                            country == "Korea, Republic of" ~ "Corea del Sur",
                                            country == "Croatia" ~ "Croacia", 
                                            country == "Denmark" ~ "Dinamarca",
                                            country == "Slovakia" ~ "Eslovaquia",
                                            country == "Slovenia" ~ "Eslovenia",
                                            country == "Spain" ~ "España",
                                            country == "Philippines" ~ "Filipinas",
                                            country == "Finland" ~ "Finlandia",
                                            country == "France" ~ "Francia",
                                            country == "United Kingdom" ~ "Gran Bretaña",
                                            country == "Hungary" ~ "Hungria",
                                            country == "Iceland" ~ "Islandia",
                                            country == "Israel" ~ "Israel",
                                            country == "Italy" ~ "Italia",
                                            country == "Japan" ~ "Japon",
                                            country == "Latvia" ~ "Letonia",
                                            country == "Lithuania" ~ "Lituania",
                                            country == "Norway" ~ "Noruega",
                                            country == "New Zealand" ~ "Nueva Zelanda",
                                            country == "Poland" ~ "Polonia",
                                            country == "Portugal" ~ "Portugal",
                                            country == "Czechia" ~ "Rep Checa",
                                            country == "Russia" ~ "Rusia",
                                            country == "South Africa" ~ "Sudafrica",
                                            country == "Sweden" ~ "Suecia",
                                            country == "Switzerland" ~ "Suiza",
                                            country == "Taiwan" ~ "Taiwan",
                                            country == "Turkey" ~ "Turquia",
                                            country == "Ukraine" ~ "Ucrania",
                                            country == "United States" ~ "USA",
                                            country == "Venezuela" ~ "Venezuela",
                                            country == "Thailand" ~ "Tailandia",
                                            country == "Canada" ~ "Canada",
                                            TRUE ~ NA_character_))

wiid$COUNTRY <- sjlabelled::set_label(wiid$COUNTRY, label = c('País'))

wiid <- wiid %>% group_by(COUNTRY) %>% filter(year >= 1996 & year <= 2001 |
                                           year >= 2006 & year <= 2011 |
                                           year >= 2016 & year <= 2019,
                                           resource_detailed %in% c(101, 201),
                                           scale_detailed %in% c(101, 201, 206, 207),
                                           sharing_unit == 1,
                                           reference_unit == 1) %>% 
                                           select(COUNTRY, year, ratio_top20bottom20, resource_detailed, scale, scale_detailed, gdp, source_detailed)
  
wiid$scale_detailed <- as.numeric(wiid$scale_detailed)
wiid$scale_detailed <- car::recode(wiid$scale_detailed, recodes = c("101 = 'Per capita'; 201 = 'Equivalente'; 
                                                                    206 = 'OECD'; 207 = 'Square root'"), as.factor = T)

wiid$resource_detailed <- as.numeric(wiid$resource_detailed)
wiid$resource_detailed <- car::recode(wiid$resource_detailed, recodes = c("101 = 'Net'; 201 = 'Net/gros'"), as.factor = T)

# Japan NA in 2019
wiid <- wiid %>% .[c(30, 54, 60, 129, 231, 284, 311, 360, 428, 454, 480, 522, 545, 2690, 590, 619, 651, 2660, 747, 2666, 
             863, 2672, 962, 1032, 1093, 2663, 1149, 1183, 1248, 1289, 1364, 1403, 1405, 1428, 1445, 1474, 1543,
             1593, 1599, 1607, 1611, 1643, 1683, 1692, 1728, 1818, 1856, 1860, 1875, 1882, 1916, 2003, 2742, 2046,
             2054, 2070, 2107, 2152, 2654, 2243, 2295, 2330, 2361, 2684, 2478, 2558, 2584, 2636),]

wiid <- wiid %>% select(COUNTRY, year, ratio_top20bottom20, gdp)

a <- wiid_or[c(4110, 9268, 14310),] %>% select(country, year, ratio_top20bottom20, gdp)
a <- a %>% mutate(COUNTRY = case_when(country == "Cyprus" ~ "Chipre",
                                      country == "Ireland" ~ "Irlanda",
                                      country == "Philippines" ~ "Filipinas", 
                                      TRUE ~ NA_character_)) %>% select(COUNTRY, year, ratio_top20bottom20, gdp)

wiid <- rbind(wiid, a)

wiid <- wiid %>% mutate(year = case_when(year %in% c(1997, 1998, 1999, 2000, 2004) ~ 1999,
                                year %in% c(2008, 2009, 2010) ~ 2009,
                                year %in% c(2015, 2017, 2018, 2019) ~ 2019,
                                TRUE ~ NA_real_))

wiid$COUNTRY <- sjlabelled::set_label(wiid$COUNTRY, label = c('País'))

wiid <- rename_variables(wiid, year = "YEAR")
wiid$YEAR <- as.numeric(wiid$YEAR)
wiid$YEAR <- sjlabelled::set_label(wiid$YEAR, label = c('Año'))

wiid <- rename_variables(wiid, ratio_top20bottom20 = "RATIO_IC")
wiid$RATIO_IC <- sjlabelled::set_label(wiid$RATIO_IC, label = c('Ratio S80/S20'))

wiid <- rename_variables(wiid, gdp = "GDP")
wiid$GDP <- sjlabelled::set_label(wiid$GDP, label = c('GDP Per capita USD'))

# 4. Processing ICTWSS / ILO ----

# 4.1 Index ----
ictwss <- ictwss_or %>% select(country, year, Coord, Type, Level, EXT)

ictwss <- ictwss %>% filter(country != "Colombia", country != "Costa Rica", country != "India", 
                            country != "Indonesia", country != "Luxembourg", country != "Malaysia", 
                            country != "Malta", country != "Mexico", country != "Romania", country != "Singapore",
                            country != "Slovak Republic", country != "Brazil", country != "Greece", country != "Netherlands",
                            country != "China",
                            year >= 1996 & year <= 2019)

b <- ictwss[is.na(ictwss$Coord),]
c <- ictwss[is.na(ictwss$Type),]
d <- ictwss[is.na(ictwss$Level),]
e <- ictwss[is.na(ictwss$EXT),]

df <- ictwss %>% filter(country == "Denmark" & year == 2018|
                        country == "Germany" & year == 2017|
                        country == "New Zealand" & year == 2017|
                        country == "Slovenia" & year == 2017|
                        country == "Switzerland" & year == 2017|
                        country == "South Africa" & year == 2018)

ictwss <- ictwss %>% group_by(country) %>% filter(year == 1999| year == 2009| year == max(year))

ictwss <- ictwss %>% .[-c(1,3,6,7,9,10,12,15,17,18,22,27,31,33,34,36,37,40,42,45,46,48,51,52,54,
                          56,57,58,60,61,64,67,69,72,73,75,78,81,82,87,88,90,94,96,97,105,106,108,109,
                          111,112,114,117,120),]

ictwss <- rbind(ictwss, df)

ictwss$year <- car::recode(ictwss$year, recodes = c("2017 = 2019; 2018 = 2019"))

# 4.2 Control ----
ict_control <- ictwss_or %>% select(country, year, UD, AdjCov)









ictwss <- ictwss %>% mutate(COUNTRY = case_when(country == "Argentina" ~ "Argentina",
                                                country == "Australia" ~ "Australia",
                                                country == "Austria" ~ "Austria",
                                                country == "Bulgaria" ~ "Bulgaria",
                                                country == "Chile" ~ "Chile",
                                                country == "Canada" ~ "Canada",
                                                country == "Belgium" ~ "Belgica",
                                                country == "Croatia" ~ "Croacia",
                                                country == "Cyprus" ~ "Chipre",
                                                country == "Czech Republic" ~ "Rep Checa",
                                                country == "Denmark" ~ "Dinamarca",
                                                country == "Estonia" ~ "Estonia",
                                                country == "Finland" ~ "Finlandia",
                                                country == "France" ~ "Francia",
                                                country == "Germany" ~ "Alemania",
                                                country == "Hong Kong, China" ~ "China",
                                                country == "Hungary" ~ "Hungria",
                                                country == "Iceland" ~ "Islandia",
                                                country == "Ireland" ~ "Irlanda",
                                                country == "Israel" ~ "Israel",
                                                country == "Italy" ~ "Italia",
                                                country == "Japan" ~ "Japon",
                                                country == "Korea, Republic of" ~ "Corea del Sur",
                                                country == "Latvia" ~ "Letonia",
                                                country == "Lithuania" ~ "Lituania",
                                                country == "New Zealand" ~ "Nueva Zelanda",
                                                country == "Norway" ~ "Noruega",
                                                country == "Philippines" ~ "Filipinas",
                                                country == "Poland" ~ "Polonia",
                                                country == "Portugal" ~ "Portugal",
                                                country == "Russian Federation" ~ "Rusia",
                                                country == "Slovenia" ~ "Eslovenia",
                                                country == "South Africa" ~ "Sudafrica",
                                                country == "Spain" ~ "España",
                                                country == "Sweden" ~ "Suecia",
                                                country == "Switzerland" ~ "Suiza",
                                                country == "Taiwan, China" ~ "Taiwan",
                                                country == "Turkey" ~ "Turquia",
                                                country == "United Kingdom" ~ "Gran Bretaña",
                                                country == "United States of America" ~ "USA",
                                                TRUE ~ NA_character_))









 
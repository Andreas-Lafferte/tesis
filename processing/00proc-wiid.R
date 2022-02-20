# Code 4: Process WIID

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
               magrittr)
options(scipen=999)

# 2. Data ----
wiid_or <- read_dta("~/GitHub/tesis/input/data/WIID_31MAY2021.dta")
ocde <- read_csv("~/GitHub/tesis/input/data/DP_LIVE_06122021194108533.csv")
sapply(wiid_or, class)
names(wiid_or)

# 3. Processing -----

# 3.1 Select and filter country's ----
wiid <- wiid_or %>% select(country, year, ratio_top20bottom20, 34:39, gdp, source_detailed)

wiid <- wiid %>% filter(country %in% c("Germany", "Argentina", "Austria", "Australia", "Belgium", 
                                       "Bulgaria", "Chile", "China", "Cyprus", "Korea, Republic of", 
                                       "Croatia", "Denmark", "Slovakia", "Slovenia", "Spain", "Philippines",
                                       "Finland", "France", "United Kingdom", "Hungary", "Iceland", "Israel",
                                       "Italy", "Japan", "Latvia", "Lithuania", "Norway", "New Zealand", "Poland",
                                       "Portugal", "Czechia", "Russia", "South Africa", "Sweden", "Switzerland", "Taiwan",
                                       "Turkey", "United States", "Canada", "Estonia", "Ireland"))

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
                                            country == "Estonia" ~ "Estonia",
                                            country == "Slovakia" ~ "Eslovaquia",
                                            country == "Slovenia" ~ "Eslovenia",
                                            country == "Spain" ~ "España",
                                            country == "Philippines" ~ "Filipinas",
                                            country == "Finland" ~ "Finlandia",
                                            country == "France" ~ "Francia",
                                            country == "United Kingdom" ~ "Gran Bretaña",
                                            country == "Hungary" ~ "Hungria",
                                            country == "Ireland" ~ "Irlanda",
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
                                            country == "United States" ~ "USA",
                                            country == "Canada" ~ "Canada",
                                            TRUE ~ NA_character_))

wiid$COUNTRY <- sjlabelled::set_label(wiid$COUNTRY, label = c('País'))

# 3.2 Filter years and scales ----
wiid <- wiid %>% group_by(COUNTRY) %>% filter(year >= 1996 & year <= 2001 |
                                           year >= 2004 & year <= 2011 |
                                           year >= 2015 & year <= 2019,
                                           resource_detailed %in% c(101, 201),
                                           scale_detailed %in% c(101, 201, 206, 207),
                                           sharing_unit == 1,
                                           reference_unit == 1) %>% 
                                           select(COUNTRY, year, ratio_top20bottom20, resource_detailed, scale, scale_detailed, gdp, source_detailed)


# 3.3 Recode and unit ----
wiid$scale_detailed <- as.numeric(wiid$scale_detailed)
wiid$scale_detailed <- car::recode(wiid$scale_detailed, recodes = c("101 = 'Per capita'; 201 = 'Equivalente'; 
                                                                    206 = 'OECD'; 207 = 'Square root'"), as.factor = T)

wiid$resource_detailed <- as.numeric(wiid$resource_detailed)
wiid$resource_detailed <- car::recode(wiid$resource_detailed, recodes = c("101 = 'Net'; 201 = 'Net/gros'"), as.factor = T)

# Japan NA in 2019, we use ocde data
wiid <- wiid %>% filter(COUNTRY == "Argentina" & year == 2009 & source_detailed == "SEDLAC" |
                          COUNTRY == "Alemania" & year == 1999 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Alemania" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Alemania" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Australia" & year == 2000 |
                          COUNTRY == "Australia" & year == 2008 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Austria" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Belgica" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Bulgaria" & year == 1999 |
                          COUNTRY == "Bulgaria" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Bulgaria" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Canada" & year == 1999 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Chile" & year == 1999 |
                          COUNTRY == "Chile" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Chile" & year == 2017 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "China" & year == 2009 & ratio_top20bottom20 == 5.90 |
                          COUNTRY == "Chipre" & year == 2004 |
                          COUNTRY == "Chipre" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Corea del Sur" & year == 2009 |
                          COUNTRY == "Croacia" & year == 2009 |
                          COUNTRY == "Croacia" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Dinamarca" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Dinamarca" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Eslovaquia" & year == 1999 |
                          COUNTRY == "Eslovaquia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Eslovenia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Eslovenia" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "España" & year == 1999 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "España" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Estonia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Filipinas" & year == 2009 |
                          COUNTRY == "Filipinas" & year == 2015 |
                          COUNTRY == "Finlandia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Finlandia" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Francia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Gran Bretaña" & year == 1999 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Gran Bretaña" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Gran Bretaña" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Hungria" & year == 1999 & scale_detailed == "Per capita" & source_detailed == "PovcalNet" |
                          COUNTRY == "Hungria" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Irlanda" & year == 1999 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Islandia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Islandia" & year == 2018 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Israel" & year == 2009  & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Israel" & year == 2018  & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Italia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Italia" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Japon" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Letonia" & year == 1999 |
                          COUNTRY == "Letonia" & year == 2009 & ratio_top20bottom20 == 6.90 |
                          COUNTRY == "Lituania" & year == 2009 & ratio_top20bottom20 == 7.50 |
                          COUNTRY == "Lituania" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Noruega" & year == 1999 |
                          COUNTRY == "Noruega" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" |
                          COUNTRY == "Nueva Zelanda" & year == 1998 |
                          COUNTRY == "Nueva Zelanda" & year == 2009 & scale_detailed == "Square root" |
                          COUNTRY == "Nueva Zelanda" & year == 2018 |
                          COUNTRY == "Polonia" & year == 1999 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Polonia" & year == 2009 & ratio_top20bottom20 == 4.90 |
                          COUNTRY == "Portugal" & year == 2009 & ratio_top20bottom20 == 5.80 |
                          COUNTRY == "Rep Checa" & year == 1997 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Rep Checa" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Rep Checa" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Rusia" & year == 2000 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Rusia" & year == 2008 |
                          COUNTRY == "Rusia" & year == 2018 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Sudafrica" & year == 2008 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Sudafrica" & year == 2017 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Suecia" & year == 1999 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Suecia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" | 
                          COUNTRY == "Suiza" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Suiza" & year == 2019 & scale_detailed == "Square root" & source_detailed == "Eurostat microdata" |
                          COUNTRY == "Taiwan" & year == 2010 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Taiwan" & year == 2016 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "Turquia" & year == 2009 & scale_detailed == "Square root" & source_detailed == "OECD.Stat" | 
                          COUNTRY == "USA" & year == 1999 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)" |
                          COUNTRY == "USA" & year == 2009 & scale_detailed == "Square root" & source_detailed == "Luxembourg Income Study (LIS)")

wiid <- distinct(wiid, COUNTRY, year, .keep_all= TRUE) 

wiid <- wiid %>% select(COUNTRY, year, ratio_top20bottom20, gdp)

# OCDE Japan
a <- ocde %>% filter(LOCATION == "JPN") %>% select(COUNTRY = LOCATION, year = TIME, ratio_top20bottom20 = Value)
a$gdp <- 42929
a$COUNTRY <- car::recode(a$COUNTRY, recodes = c("'JPN' = 'Japon'"))

# 3.4 Final data ----
wiid <- rbind(wiid, a)

wiid <- wiid %>% mutate(year = case_when(year %in% c(1997, 1998, 1999, 2000, 2004) ~ 1999,
                                year %in% c(2008, 2009, 2010) ~ 2009,
                                year %in% c(2015, 2016, 2017, 2018, 2019) ~ 2019,
                                TRUE ~ NA_real_))

wiid$COUNTRY <- sjlabelled::set_label(wiid$COUNTRY, label = c('País'))

wiid <- rename_variables(wiid, year = "YEAR")
wiid$YEAR <- as.numeric(wiid$YEAR)
wiid$YEAR <- sjlabelled::set_label(wiid$YEAR, label = c('Año'))

wiid <- rename_variables(wiid, ratio_top20bottom20 = "RATIO_IC")
wiid$RATIO_IC <- sjlabelled::set_label(wiid$RATIO_IC, label = c('Ratio S80/S20'))

wiid <- rename_variables(wiid, gdp = "GDP")
wiid$GDP <- sjlabelled::set_label(wiid$GDP, label = c('GDP Per capita USD'))

# 4. Save ----
save(wiid, file = "../output/data/wiid.RData")
# Code 5: Process ICTWSS

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
ictwss_or <- haven::read_dta(url("https://aias.s3.eu-central-1.amazonaws.com/website/uploads/ICTWSS_v6_1_Stata_release.dta"))
sapply(ictwss_or, class)
names(ictwss_or)

df_j <- read_dta("../input/data/corporatism_3-0_v1211.dta")
sapply(df_j, class)
names(df_j)

# 3. Processing ----

# 3.1 ICTWSS ----
ictwss <- ictwss_or %>% select(country, year, Coord, Type, Level, EXT, WC_STRUCT,
                               WC_RIGHTS, Govint)
ictwss_new <- ictwss

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
                          country == "Switzerland" & year == 2017)

df_2 <- ictwss_or %>% filter(country == "Slovak Republic" & year %in% c(1999,2009)) %>% select(country, year, Coord, Type, Level, EXT, WC_STRUCT,
                                                                                               WC_RIGHTS, Govint)

df <- rbind(df, df_2)

ictwss <- ictwss %>% group_by(country) %>% filter(year == 1999| year == 2009| year == max(year))

ictwss <- ictwss %>% .[-c(1,3,6,7,9,10,12,15,17,18,22,27,31,33,34,36,37,40,42,45,46,48,51,52,54,
                          56,57,58,60,61,64,67,69,72,73,75,78,81,82,87,88,90,94,96,97,102,105,106,108,109,
                          111,112,114,117,120),]

g <- ictwss %>% filter(country == "Hong Kong, China")
g$EXT <- 0

ictwss <- rbind(ictwss, g)
ictwss <- ictwss[-c(26),]

ictwss <- rbind(ictwss, df)

# new data from issp 2019

new_df <- ictwss_new %>% filter(country == "Bulgaria" & year == 2018|
                              country == "United Kingdom" & year == 2018|
                              country == "Iceland" & year == 2018|
                              country == "Israel" & year == 2018|
                              country == "Lithuania" & year == 2018|
                              country == "Taiwan, China" & year == 2018)

ictwss <- rbind(ictwss, new_df)

ictwss$year <- car::recode(ictwss$year, recodes = c("2017 = 2019; 2018 = 2019"))

labels_ictwss <- ictwss

frq(ictwss$Coord)
frq(ictwss$Type)
frq(ictwss$Level)
frq(ictwss$EXT)
frq(ictwss$WC_STRUCT)
frq(ictwss$WC_RIGHTS)
frq(ictwss$Govint)

# 3.2 Control ----

# 3.2.1 ICTWSS ----
ict_control <- ictwss_or %>% select(country, year, UD, AdjCov)

ict_control <- ict_control %>% filter(year >= 1998 & year <= 2000 |
                                        year >= 2008 & year <= 2010 |
                                        year >= 2016 & year <= max(year),
                                      country != "Colombia", country != "Costa Rica", country != "India", 
                                      country != "Indonesia", country != "Luxembourg", country != "Malaysia", 
                                      country != "Malta", country != "Mexico", country != "Romania", country != "Singapore",
                                      country != "Brazil", country != "Greece", country != "Netherlands",
                                      country != "China")

ict_control <- ict_control %>% filter(country == "Argentina" & year == 2009 |
                                        country == "Australia" & year %in% c(1999,2008) |
                                        country == "Austria" & year == 2008 |
                                        country == "Belgium" & year == 2009 |
                                        country == "Bulgaria" & year %in% c(1998,2009, 2016) |
                                        country == "Canada" & year == 1999 |
                                        country == "Chile" & year %in% c(2000,2009,2016) |
                                        country == "Croatia" & year %in% c(2008,2018) |
                                        country == "Cyprus" & year %in% c(2000,2009) |
                                        country == "Czech Republic" & year %in% c(1999,2009,2018) |
                                        country == "Denmark" & year %in% c(2009,2018) |
                                        country == "Estonia" & year == 2009 |
                                        country == "Finland" & year %in% c(2008, 2018) |
                                        country == "France" & year == 2010 |
                                        country == "Germany" & year %in% c(1999,2009,2018) |
                                        country == "Hong Kong, China" & year == 2009 |
                                        country == "Hungary" & year %in% c(1998,2008) |
                                        country == "Iceland" & year %in% c(2008,2018) |
                                        country == "Ireland" & year == 1999 |
                                        country == "Israel" & year %in% c(2009,2017) |
                                        country == "Italy" & year %in% c(2010,2018) |
                                        country == "Japan" & year %in% c(2009,2018) |
                                        country == "Korea, Republic of" & year == 2009 |
                                        country == "Latvia" & year %in% c(1999,2009) |
                                        country == "Lithuania" & year %in% c(2009,2018) |
                                        country == "New Zealand" & year %in% c(1998,2010,2017) |
                                        country == "Norway" & year %in% c(1998,2008) |
                                        country == "Philippines" & year %in% c(2009,2016) |
                                        country == "Poland" & year %in% c(1999,2008) |
                                        country == "Portugal" & year == 2008 |
                                        country == "Russian Federation" & year %in% c(1999,2009,2017) |
                                        country == "Slovenia" & year %in% c(2009,2016) |
                                        country == "Slovak Republic" & year %in% c(1999,2009) |
                                        country == "South Africa" & year %in% c(2009,2018) |
                                        country == "Spain" & year %in% c(1999,2009) |
                                        country == "Sweden" & year %in% c(1999,2009) |
                                        country == "Switzerland" & year %in% c(2009,2017) |
                                        country == "Taiwan, China" & year %in% c(2009,2018)|
                                        country == "Turkey" & year == 2008 |
                                        country == "United Kingdom" & year %in% c(1999,2009, 2018) |
                                        country == "United States of America" & year %in% c(1999,2009))

ict_control$year <- car::recode(ict_control$year, recodes = c("c(1998,1999,2000) = 1999; c(2008,2009,2010) = 2009; c(2016,2017,2018) = 2019"))

# 3.2.2 ILO -----

# Union density 

ud <- Rilostat::get_ilostat("ILR_TUMT_NOC_RT_A") %>% 
  select(iso3c = ref_area, year=time, ud= obs_value)  %>% 
  mutate(year = as.numeric(year)) 

ud <- ud %>% filter(iso3c == "ISR" & year == 2012 |
                      iso3c == "LVA" & year == 2003 |
                      iso3c == "ZAF" & year %in% c(2010,2016) |
                      iso3c == "USA" & year %in% c(2000,2009)) %>% select(country = iso3c, year, ud)

ud$country <- car::recode(ud$country, recodes = c("'ISR' = 'Israel'; 'LVA' = 'Latvia'; 'ZAF' = 'South Africa'; 'USA' = 'United States of America'"))
ud$year <- car::recode(ud$year, recodes = c("2012 = 2009; 2003 = 1999; 2000 = 1999; 2016 = 2019; 2010 = 2009"))

ict_control <- full_join(ict_control, ud, by = c("country", "year"))

ict_control$UD[ict_control$country == "Taiwan, China" & ict_control$year == 2019] <- 32.7539503

# Bargaining coveragee

cbc <- Rilostat::get_ilostat("ILR_CBCT_NOC_RT_A")  %>% 
  select(iso3c = ref_area, year=time, cbc= obs_value) %>% 
  mutate(year = as.numeric(year))

# No data for: rusia 1999/2019 and taiwan
# No dara for suiza 2019, use suiza 2016 ictwss / use bulgaria 2002 also for 1999
cbc <- cbc %>% filter(iso3c == "AUS" & year == 2000 |
                        iso3c == "BGR" & year == 2011 |
                        iso3c == "HRV" & year == 2016 |
                        iso3c == "CZE" & year == 2015 |
                        iso3c == "DNK" & year == 2015 |
                        iso3c == "FIN" & year == 2015 |
                        iso3c == "CHN" & year == 2009 |
                        iso3c == "ISL" & year %in% c(2008,2016) |
                        iso3c == "IRL" & year == 2000 |
                        iso3c == "ISR" & year == 2012 |
                        iso3c == "LVA" & year %in% c(2002,2009) |
                        iso3c == "NZL" & year == 2016 |
                        iso3c == "POL" & year == 2000 |
                        iso3c == "SVK" & year %in% c(2000,2009) |
                        iso3c == "SVN" & year == 2013 |
                        iso3c == "ZAF" & year %in% c(2010,2016) |
                        iso3c == "SWE" & year == 2000)

df_3 <- ictwss_or %>% filter(country == "Bulgaria" & year == 2002 | 
                               country == "Switzerland" & year == 2016) %>% select(iso3c = country, year, cbc = AdjCov)

cbc <- rbind(cbc, df_3)

cbc$country <- car::recode(cbc$iso3c, recodes = c("'AUS' = 'Australia'; 'BGR' = 'Bulgaria'; 'HRV' = 'Croatia';
                                                  'CZE' = 'Czech Republic'; 'DNK' = 'Denmark'; 'FIN' = 'Finland';
                                                  'CHN' = 'Hong Kong, China'; 'ISL' = 'Iceland'; 'IRL' = 'Ireland';
                                                  'ISR' = 'Israel'; 'LVA' = 'Latvia'; 'NZL' = 'New Zealand'; 'POL' = 'Poland';
                                                  'SVK' = 'Slovak Republic'; 'SVN' = 'Slovenia'; 'ZAF' = 'South Africa'; 
                                                  'SWE' = 'Sweden'"))

cbc$year <- car::recode(cbc$year, recodes = c("c(2000,2002) = 1999; c(2008,2009,2010,2011,2012,2013) = 2009; c(2015,2016) = 2019"))
cbc <- cbc[-c(1)]

ict_control <- full_join(ict_control, cbc, by = c("country", "year"))

# 3.3 Final data ICTWSS ----

ict_control <- ict_control %>% mutate(UDA = if_else(is.na(UD), ud, UD))
ict_control <- ict_control %>% mutate(COV = if_else(is.na(AdjCov), cbc, AdjCov))

ict_control <- ict_control %>% select(country, year, UD = UDA, AdjCov = COV)

ictwss <- full_join(ictwss, ict_control, by = c("country", "year"))

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
                                                country == "Slovak Republic" ~ "Eslovaquia",
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

ictwss <- ictwss %>% select(COUNTRY, YEAR = year, COORD = Coord, TYPE = Type, GOVINT = Govint, LEVEL = Level, EXT, WC_STRUCT, WC_RIGHTS, UD, AdjCov)
ictwss <- ictwss[-c(1)]

# 3.4 Jahn Index ----

frq(df_j$iso)
frq(df_j$year)

df_j <- df_j %>% filter(year >= 1996 & year <= max(year)) %>% 
  mutate(country = case_when(iso == 32 ~ "Argentina",
                             iso == 36 ~ "Australia",
                             iso == 40 ~ "Austria",
                             iso == 56 ~ "Belgica",
                             iso == 100 ~ "Bulgaria",
                             iso == 124 ~ "Canada",
                             iso == 152 ~ "Chile",
                             iso == 158 ~ "Taiwan",
                             iso == 191 ~ "Croacia",
                             iso == 196 ~ "Chipre",
                             iso == 203 ~ "Rep Checa",
                             iso == 208 ~ "Dinamarca",
                             iso == 233 ~ "Estonia",
                             iso == 246 ~ "Finlandia",
                             iso == 250 ~ "Francia",
                             iso == 276 ~ "Alemania",
                             iso == 348 ~ "Hungria",
                             iso == 372 ~ "Irlanda",
                             iso == 376 ~ "Israel",
                             iso == 380 ~ "Italia",
                             iso == 392 ~ "Japon",
                             iso == 410 ~ "Corea del Sur",
                             iso == 428 ~ "Letonia",
                             iso == 440 ~ "Lituania",
                             iso == 554 ~ "Nueva Zelanda",
                             iso == 578 ~ "Noruega",
                             iso == 616 ~ "Polonia",
                             iso == 620 ~ "Portugal",
                             iso == 703 ~ "Eslovaquia",
                             iso == 705 ~ "Eslovenia",
                             iso == 710 ~ "Sudafrica",
                             iso == 724 ~ "España",
                             iso == 752 ~ "Suecia",
                             iso == 756 ~ "Suiza",
                             iso == 792 ~ "Turquia",
                             iso == 826 ~ "Gran Bretaña",
                             iso == 840 ~ "USA",
                             TRUE ~ NA_character_ )) %>% 
  filter(!is.na(country)) %>% 
  select(country, year, CorpAll) 

df_j[is.na(df_j$CorpAll),]

df_j <- df_j %>% 
  filter(year %in% c(1999,2009,2017:2019),
        !is.na(CorpAll)) %>% 
  group_by(country) %>% 
  filter(year == 1999 | year == 2009 | year == max(year)) %>% 
  mutate(year = if_else(year == max(year), 2019, year)) 

ictwss %>% group_by(COUNTRY, YEAR) %>% select(COUNTRY, YEAR) %>%
  arrange(desc(COUNTRY)) %>% print(n = nrow(.))

df_j <- df_j %>% filter(country == "Alemania" & year %in% c(1999,2009,2019) |
                          country == "Argentina" & year == 2009 |
                          country == "Australia" & year %in% c(1999,2009) |
                          country == "Austria" & year == 2009 |
                          country == "Belgica" & year == 2009 |
                          country == "Bulgaria" & year %in% c(1999,2009,2019) |
                          country == "Canada" & year == 1999 |
                          country == "Chile" & year %in% c(1999,2009,2019) |
                          country == "Chipre" & year %in% c(1999,2009) |
                          country == "Corea del Sur" & year == 2009 |
                          country == "Croacia" & year %in% c(2009,2019) |
                          country == "Dinamarca" & year %in% c(2009,2019) |
                          country == "Eslovaquia" & year %in% c(1999,2009) |
                          country == "Eslovenia" & year %in% c(2009,2019) |
                          country == "España" & year %in% c(1999,2009) |
                          country == "Estonia" & year == 2009 |
                          country == "Finlandia" & year %in% c(2009,2019) |
                          country == "Francia" & year == 2009 |
                          country == "Gran Bretaña" & year %in% c(1999,2009,2019) |
                          country == "Hungria" & year %in% c(1999,2009) |
                          country == "Irlanda" & year == 1999 |
                          country == "Israel" & year %in% c(2009,2019) |
                          country == "Italia" & year %in% c(2009,2019) |
                          country == "Japon" & year %in% c(2009,2019) |
                          country == "Letonia" & year %in% c(1999,2009) |
                          country == "Lituania" & year %in% c(2009,2019) |
                          country == "Noruega" & year %in% c(1999,2009) |
                          country == "Nueva Zelanda" & year %in% c(1999,2009,2019) |
                          country == "Polonia" & year %in% c(1999,2009) |
                          country == "Portugal" & year == 2009 |
                          country == "Rep Checa" & year %in% c(1999,2009,2019) |
                          country == "Sudafrica" & year %in% c(2009,2019) |
                          country == "Suecia" & year %in% c(1999,2009) |
                          country == "Suiza" & year %in% c(2009,2019) |
                          country == "Taiwan" & year %in% c(2009,2019) |
                          country == "Turquia" & year == 2009 |
                          country == "USA" & year %in% c(1999,2009)) %>% 
  rename(COUNTRY = country,
         YEAR = year)

ictwss <- full_join(ictwss, df_j, by = c("COUNTRY", "YEAR"))


# Labels
ictwss$COUNTRY <- sjlabelled::set_label(ictwss$COUNTRY, label = c('País'))
ictwss$YEAR <- sjlabelled::set_label(ictwss$YEAR, label = c('Año'))
ictwss$COORD <- sjlabelled::set_label(ictwss$COORD, label = c('Coordinación salarial'))
ictwss$GOVINT <- sjlabelled::set_label(ictwss$GOVINT, label = c('Intervención estatal negociación salarial'))
ictwss$LEVEL <- sjlabelled::set_label(ictwss$LEVEL, label = c('Nivel predominante negociación salarial'))
ictwss$EXT <- sjlabelled::set_label(ictwss$EXT, label = c('Clausula extensión negociación colectiva'))
ictwss$WC_STRUCT <- sjlabelled::set_label(ictwss$WC_STRUCT, label = c('Estructura representación consejos empresa'))
ictwss$WC_RIGHTS <- sjlabelled::set_label(ictwss$WC_RIGHTS, label = c('Derechos consejos empresa'))
ictwss$UD <- sjlabelled::set_label(ictwss$UD, label = c('Densidad sindical'))
ictwss$AdjCov <- sjlabelled::set_label(ictwss$AdjCov, label = c('Cobertura ajustada negociación colectiva'))
ictwss$TYPE <- sjlabelled::set_label(ictwss$TYPE, label = c('Tipo coordinación salarial'))
ictwss$CorpAll <- sjlabelled::set_label(ictwss$CorpAll, label = c('Indice corporativismo'))

# 4. Save ----
save(ictwss, file = "../output/data/ictwss.RData")
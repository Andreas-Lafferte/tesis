# Code: Process SWIID

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
load("../input/data/swiid9_1.rda")
load("~/GitHub/tesis/output/data/db-proc.RData")

# 3. Processing -----


db %>% group_by(COUNTRY, YEAR) %>% count() %>% print(n = nrow(.))

swiid_summary <- swiid_summary %>% filter(country %in% c("Germany", "Argentina", "Australia", "Austria",
                                                         "Belgium", "Bulgaria", "Chile", "China", "Cyprus",
                                                         "Korea", "Croatia", "Denmark", "Slovakia", "Slovenia",
                                                         "Spain", "Estonia", "Philippines", "Finland", "France",
                                                         "United Kingdom", "Hungary", "Ireland", "Iceland", "Canada", "Japan",
                                                         "Israel", "Italy", "Latvia", "Lithuania", "Norway",
                                                         "New Zealand", "Poland", "Portugal", "Czech Republic",
                                                         "Russia", "South Africa", "Sweden", "Switzerland", "Suriname", 
                                                         "Thailand", "Taiwan", "Turkey", "Ukraine", "United States", "Venezuela"))

swiid_summary <- swiid_summary %>% group_by(country) %>% filter(year == 1999| year == 2009| year == max(year))

swiid_summary <- swiid_summary %>% 
  mutate(country = case_when(country == "Germany" ~ "Alemania",
                             country == "Argentina" ~ "Argentina",
                             country == "Australia" ~ "Australia",
                             country == "Austria" ~ "Austria",
                             country == "Belgium" ~ "Belgica",
                             country == "Bulgaria" ~ "Bulgaria",
                             country == "Canada" ~ "Canada",
                             country == "Chile" ~ "Chile",
                             country == "China" ~ "China",
                             country == "Croatia" ~ "Croacia",
                             country == "Cyprus" ~ "Chipre",
                             country == "Czech Republic" ~ "Rep Checa",
                             country == "Denmark" ~ "Dinamarca",
                             country == "Estonia" ~ "Estonia",
                             country == "Finland" ~ "Finlandia",
                             country == "France" ~ "Francia",
                             country == "Hungary" ~ "Hungria",
                             country == "Iceland" ~ "Islandia",
                             country == "Ireland" ~ "Irlanda",
                             country == "Israel" ~ "Israel",
                             country == "Italy" ~ "Italia",
                             country == "Japan" ~ "Japon",
                             country == "Korea" ~ "Corea del Sur",
                             country == "Latvia" ~ "Letonia",
                             country == "Lithuania" ~ "Lituania",
                             country == "New Zealand" ~ "Nueva Zelanda",
                             country == "Norway" ~ "Noruega",
                             country == "Philippines" ~ "Filipinas",
                             country == "Poland" ~ "Polonia",
                             country == "Portugal" ~ "Portugal",
                             country == "Russia" ~ "Rusia",
                             country == "Slovakia" ~ "Eslovaquia",
                             country == "Slovenia" ~ "Eslovenia",
                             country == "South Africa" ~ "Sudafrica",
                             country == "Spain" ~ "España",
                             country == "Suriname" ~ "Surinam",
                             country == "Sweden" ~ "Suecia",
                             country == "Switzerland" ~ "Suiza",
                             country == "Taiwan" ~ "Taiwan",
                             country == "Thailand" ~ "Tailandia",
                             country == "Turkey" ~ "Turquia",
                             country == "Ukraine" ~ "Ucrania",
                             country == "United Kingdom" ~ "Gran Bretaña",
                             country == "United States" ~ "USA",
                             country == "Venezuela" ~ "Venezuela"))


swiid_summary <- swiid_summary %>% 
  mutate(year = if_else(year == max(year), 2019, year)) %>% 
  filter(country == "Alemania" & year %in% c(1999,2009,2019) |
           country == "Argentina" & year == 2009 |
           country == "Australia" & year %in% c(1999,2009) |
           country == "Austria" & year == 2009 |
           country == "Belgica" & year == 2009 |
           country == "Bulgaria" & year %in% c(1999,2009,2019) |
           country == "Canada" & year == 1999 |
           country == "China" & year == 2009 |
           country == "Chile" & year %in% c(1999,2009,2019) |
           country == "Chipre" & year %in% c(1999,2009) |
           country == "Corea del Sur" & year == 2009 |
           country == "Croacia" & year %in% c(2009,2019) |
           country == "Dinamarca" & year %in% c(2009,2019) |
           country == "Eslovaquia" & year %in% c(1999,2009) |
           country == "Eslovenia" & year %in% c(2009,2019) |
           country == "España" & year %in% c(1999,2009) |
           country == "Estonia" & year == 2009 |
           country == "Filipinas" & year %in% c(2009,2019) | 
           country == "Finlandia" & year %in% c(2009,2019) |
           country == "Francia" & year == 2009 |
           country == "Gran Bretaña" & year %in% c(1999,2009,2019) |
           country == "Hungria" & year %in% c(1999,2009) |
           country == "Irlanda" & year == 1999 |
           country == "Islandia" & year %in% c(2009,2019) |
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
           country == "Rusia" & year %in% c(1999,2009,2019) |
           country == "Sudafrica" & year %in% c(2009,2019) |
           country == "Suecia" & year %in% c(1999,2009) |
           country == "Suiza" & year %in% c(2009,2019) |
           country == "Surinam" & year == 2019 |
           country == "Tailiandia" & year == 2019 |
           country == "Taiwan" & year %in% c(2009,2019) |
           country == "Turquia" & year == 2009 |
           country == "Ucrania" & year == 2009 |
           country == "USA" & year %in% c(1999,2009) |
           country == "Venezuela" & year == 2009)


swiid_summary <- swiid_summary %>% select(COUNTRY = country, YEAR = year, GINI = gini_disp)

save(swiid_summary, file = "../output/data/swiid.RData")

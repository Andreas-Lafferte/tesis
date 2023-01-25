library(readxl)

wid <- read_excel(path = "../input/data/WID_Data.xlsx",
             range = paste0("A", 2, ":AA", 178),
             col_names = c("country", "percentile", seq(1997,2021,1))) %>% 
  select(-percentile) %>% 
  pivot_longer(., cols = -1,
               names_to = "year",
               values_to = "value")

wid %>% 
  sjmisc::frq(.$country)


owid<-wid %>% filter(country %in% c("Germany", "Argentina", "Austria", "Australia", "Belgium", 
                               "Bulgaria", "Chile", "China", "Cyprus", "Korea", 
                               "Croatia", "Denmark", "Slovakia", "Slovenia", "Spain", "Philippines",
                               "Finland", "France", "United Kingdom", "Hungary", "Iceland", "Israel",
                               "Italy", "Japan", "Latvia", "Lithuania", "Norway", "New Zealand", "Poland",
                               "Portugal", "Czech Republic", "Russian Federation", "South Africa", "Sweden", "Switzerland", "Taiwan",
                               "Turkey", "USA", "Canada", "Estonia", "Ireland", "Japan")) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(country) %>% summarise(country=unique(country))


owid <- owid %>% mutate(country = case_when(country == "Germany" ~ "Alemania",
                                    country == "Argentina" ~ "Argentina",
                                    country == "Australia" ~ "Australia",
                                    country == "Austria" ~ "Austria",
                                    country == "Belgium" ~ "Belgica",
                                    country == "Bulgaria" ~ "Bulgaria",
                                    country == "Chile" ~ "Chile",
                                    country == "China" ~ "China",
                                    country == "Cyprus" ~ "Chipre",
                                    country == "Korea" ~ "Corea del Sur",
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
                                    country == "Czech Republic" ~ "Rep Checa",
                                    country == "Russian Federation" ~ "Rusia",
                                    country == "South Africa" ~ "Sudafrica",
                                    country == "Sweden" ~ "Suecia",
                                    country == "Switzerland" ~ "Suiza",
                                    country == "Taiwan" ~ "Taiwan",
                                    country == "Turkey" ~ "Turquia",
                                    country == "USA" ~ "USA",
                                    country == "Canada" ~ "Canada",
                                    country == "Japan" ~ "Japon",
                                    TRUE ~ NA_character_))
 



ipsos <- wiid %>% 
  ungroup() %>% 
  janitor::clean_names() %>% 
  select(-gdp, -ratio_ic, -year) %>% 
  summarise(country=unique(country))

anti_join(ipsos, owid)


wid_or <- wid

wid <- wid_or %>% 
  filter(country %in% c("Germany", "Argentina", "Austria", "Australia", "Belgium", 
                                "Bulgaria", "Chile", "China", "Cyprus", "Korea", 
                                "Croatia", "Denmark", "Slovakia", "Slovenia", "Spain", "Philippines",
                                "Finland", "France", "United Kingdom", "Hungary", "Iceland", "Israel",
                                "Italy", "Japan", "Latvia", "Lithuania", "Norway", "New Zealand", "Poland",
                                "Portugal", "Czech Republic", "Russian Federation", "South Africa", "Sweden", "Switzerland", "Taiwan",
                                "Turkey", "USA", "Canada", "Estonia", "Ireland", "Japan")) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% c(1999,2009,2019)) %>% 
  mutate(country = case_when(country == "Germany" ~ "Alemania",
                             country == "Argentina" ~ "Argentina",
                             country == "Australia" ~ "Australia",
                             country == "Austria" ~ "Austria",
                             country == "Belgium" ~ "Belgica",
                             country == "Bulgaria" ~ "Bulgaria",
                             country == "Chile" ~ "Chile",
                             country == "China" ~ "China",
                             country == "Cyprus" ~ "Chipre",
                             country == "Korea" ~ "Corea del Sur",
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
                             country == "Czech Republic" ~ "Rep Checa",
                             country == "Russian Federation" ~ "Rusia",
                             country == "South Africa" ~ "Sudafrica",
                             country == "Sweden" ~ "Suecia",
                             country == "Switzerland" ~ "Suiza",
                             country == "Taiwan" ~ "Taiwan",
                             country == "Turkey" ~ "Turquia",
                             country == "USA" ~ "USA",
                             country == "Canada" ~ "Canada",
                             country == "Japan" ~ "Japon",
                             TRUE ~ NA_character_)) %>% 
  rename(TOP10 = value,
         COUNTRY = country,
         YEAR = year)
  


left_join(wiid, wid)

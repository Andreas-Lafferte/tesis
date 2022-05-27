# Code 7: Final database

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
               psych, 
               gtsummary,
               datawizard)
options(scipen=999)

# 2. Data ----
load("../output/data/issp99.RData")  
load("../output/data/issp09.RData")
load("../output/data/issp19.RData")
load("../output/data/wiid.RData")
load("../output/data/swiid.RData")
load("../output/data/ictwss.RData")
load("../output/data/oecd_euro.RData")

# 3. Processing ----

# 3.1 ISSP ----

names(issp99)
names(issp09)
names(issp19)

## Join ISSP

db <- rbind(issp99,issp09)
db <- rbind(db,issp19)

## Labels

db$SEX <- sjlabelled::set_label(db$SEX, label = c("Sexo"))
db$DEGREE <- sjlabelled::set_label(db$DEGREE, label = c("Nivel educativo"))
db$INCOME <- sjlabelled::set_label(db$INCOME, label = c("Decil ingreso"))
db$SUBJEC_CLASS <- sjlabelled::set_label(db$SUBJEC_CLASS, label = c("Identidad de clase"))
db$UNION <- sjlabelled::set_label(db$UNION, label = c("Afiliación sindical"))
db$IDEOLOGY <- sjlabelled::set_label(db$IDEOLOGY, label = c("Identificación política"))
db$CLASS <- sjlabelled::set_label(db$CLASS, label = c("Posición de clase"))

## PSCi

db <- db %>% 
  rowwise() %>%
  mutate(PSCi = sum(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, na.rm = F))


db$PSCi <- sjlabelled::set_label(db$PSCi, label = c("Perceived Social Conflict Index"))
frq(db$PSCi)
sjPlot::plot_frq(na.omit(db$PSCi), type = "histogram", show.mean = TRUE) 

## Cronbach's alpha
matriz <- db %>% select(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW)
psych::alpha(matriz) # coef = 0.78

## Polychoric alpha ordinal (Likert scale)
matriz_poly <- polychoric(matriz) 
psych::alpha(matriz_poly$rho) # coef = 0.83

db <- as.data.frame(db) # remove Rowwise type

db %>% filter(!is.na(PSCi)) %>% count(PSCi) %>% mutate(prop = prop.table(n))

# 3.2 WIID, SWIID, ICTWSS, OECD ----
names(wiid)
names(ictwss)
names(oecd)
names(swiid_summary)

df <- full_join(wiid, ictwss, by = c("COUNTRY", "YEAR"))
df <- full_join(df, oecd, by = c("COUNTRY", "YEAR"))
df <- full_join(df, swiid_summary, by = c("COUNTRY", "YEAR"))

df <- df %>% select(COUNTRY, YEAR, RATIO_IC, CorpAll, GDP, SOC_EXPEND, UD, GINI)

# 3.3 Join data and Transforming variables ----

db <- full_join(db, df, by = c("COUNTRY", "YEAR"))

## CGM Ratio 80/20
db <- db %>% mutate(C_RATIO = center(RATIO_IC))

db %>% select(COUNTRY, RATIO_IC) %>% 
  mutate(centrada = center(RATIO_IC))

db %>% select(COUNTRY, RATIO_IC) %>% 
  mutate(promed = mean(RATIO_IC, na.rm = T),
         centrada = (RATIO_IC - promed)) # verified

## GDP log y center CGM
db$GDP_LOG <- log(db$GDP)

db <- db %>% mutate(C_GDP = center(GDP_LOG))


## SOC_EXPEND center CGM
db <- db %>% mutate(C_SOCEXPEND = center(SOC_EXPEND))

## UD center CGM
db <- db %>% mutate(C_UD = center(UD))


# 3.4 ISO code and Labels ----

db <- db %>% mutate(ISO_COUNTRY = case_when(COUNTRY == "Alemania" ~ "DEU",
                                            COUNTRY == "Argentina" ~ "ARG",
                                            COUNTRY == "Australia" ~ "AUS",
                                            COUNTRY == "Austria" ~ "AUT",
                                            COUNTRY == "Belgica" ~ "BEL",
                                            COUNTRY == "Bulgaria" ~ "BGR",
                                            COUNTRY == "Canada" ~ "CAN",
                                            COUNTRY == "Chile" ~ "CHL",
                                            COUNTRY == "China" ~ "CHN",
                                            COUNTRY == "Chipre" ~ "CYP",
                                            COUNTRY == "Corea del Sur" ~ "KOR",
                                            COUNTRY == "Croacia" ~ "HRV",
                                            COUNTRY == "Dinamarca" ~ "DNK",
                                            COUNTRY == "Eslovaquia" ~ "SVK",
                                            COUNTRY == "Eslovenia" ~ "SVN",
                                            COUNTRY == "España" ~ "ESP",
                                            COUNTRY == "Estonia" ~ "EST",
                                            COUNTRY == "Filipinas" ~ "PHL",
                                            COUNTRY == "Finlandia" ~ "FIN",
                                            COUNTRY == "Francia" ~ "FRA",
                                            COUNTRY == "Gran Bretaña" ~ "GBR",
                                            COUNTRY == "Hungria" ~ "HUN",
                                            COUNTRY == "Irlanda" ~ "IRL",
                                            COUNTRY == "Islandia" ~ "ISL",
                                            COUNTRY == "Israel" ~ "ISR",
                                            COUNTRY == "Italia" ~ "ITA",
                                            COUNTRY == "Japon" ~ "JPN",
                                            COUNTRY == "Letonia" ~ "LVA",
                                            COUNTRY == "Lituania" ~ "LTU",
                                            COUNTRY == "Noruega" ~ "NOR",
                                            COUNTRY == "Nueva Zelanda" ~ "NZL",
                                            COUNTRY == "Polonia" ~ "POL",
                                            COUNTRY == "Portugal" ~ "PRT",
                                            COUNTRY == "Rep Checa" ~ "CZE",
                                            COUNTRY == "Rusia" ~ "RUS",
                                            COUNTRY == "Sudafrica" ~ "ZAF",
                                            COUNTRY == "Suecia" ~ "SWE",
                                            COUNTRY == "Suiza" ~ "CHE",
                                            COUNTRY == "Surinam" ~ "SUR",
                                            COUNTRY == "Tailandia" ~ "THA",
                                            COUNTRY == "Taiwan" ~ "TWN",
                                            COUNTRY == "Turquia" ~ "TUR",
                                            COUNTRY == "Ucrania" ~ "UKR",
                                            COUNTRY == "USA" ~ "USA",
                                            COUNTRY == "Venezuela" ~ "VEN",
                                            TRUE ~ NA_character_))
## Wave
db$WAVE <- as.factor(db$YEAR)
db$COUNTRY_WAVE <- do.call(paste, c(db[c("ISO_COUNTRY", "WAVE")], sep = "_"))

## Labels
db$ISO_COUNTRY <- sjlabelled::set_label(db$ISO_COUNTRY, label = c("Código ISO país"))
db$WAVE <- sjlabelled::set_label(db$WAVE, label = c("Ola"))
db$COUNTRY_WAVE <- sjlabelled::set_label(db$COUNTRY_WAVE, label = c("País-ola"))
db$C_RATIO <- sjlabelled::set_label(db$C_RATIO, label = c("Ratio S80/S20 [CGM]"))
db$C_GDP <- sjlabelled::set_label(db$C_GDP, label = c("GDP Per capita [CGM]"))
db$C_SOCEXPEND <- sjlabelled::set_label(db$C_SOCEXPEND, label = c("Gasto social %GDP [CGM]"))
db$C_UD <- sjlabelled::set_label(db$C_UD, label = c("Densidad sindical [CGM]"))
db$GINI <- sjlabelled::set_label(db$GINI, label = c("Gini"))

# 3.5  Final data ----

db_original <- db %>% as_tibble(.) #original

db <- db %>% select(YEAR, COUNTRY, ISO_COUNTRY, WAVE, COUNTRY_WAVE, SEX, AGE,
                    IDEOLOGY, UNION, CLASS, PSCi, GINI, RATIO_IC, CorpAll, 
                    GDP, GDP_LOG, UD, SOC_EXPEND, C_RATIO, C_GDP, C_SOCEXPEND, C_UD, 
                    FACTOR)

db <- db %>% as_tibble(.)

db <- db %>% na.omit()

db <- tibble::rowid_to_column(db, "ID_SUBJECT")

db$ID_SUBJECT <- sjlabelled::set_label(db$ID_SUBJECT, label = c("ID individuo"))

df_original <- df

df <- df %>% na.omit()
df <- df %>% filter(!COUNTRY %in% c("Japon", "Canada", "Irlanda"))
df <- df %>% as_tibble(.)

# 4. Save ----

save(db, file = "../output/data/db-proc.RData")
save(df, file = "../output/data/df2-proc.RData")
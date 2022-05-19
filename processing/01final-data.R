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
               gtsummary)
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

## ID subject and labels
db <- tibble::rowid_to_column(db, "ID_SUBJECT")

db$ID_SUBJECT <- sjlabelled::set_label(db$ID_SUBJECT, label = c("ID individuo"))
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

# 3.2.1 Cross-sectional and Longitudinal effects ----

## Cross-sectional effects

# Country mean
df <- df %>% group_by(COUNTRY) %>% 
  mutate(MEAN_RATIO = mean(RATIO_IC, na.rm = T), # MEAN RATIO
         MEAN_CorpAll = mean(CorpAll, na.rm = T), # MEAN CorpAll
         MEAN_GDP = mean(GDP, na.rm = T)) %>% # MEAN GDP
    ungroup()

df %>% group_by(COUNTRY) %>% count(MEAN_RATIO) %>% print(n = nrow(.)) # View


## Longitudinal effects 

# Mean centered

df <- df %>% mutate(LAG_RATIO = (RATIO_IC - MEAN_RATIO), # LAG RATIO
                    LAG_GDP = (GDP - MEAN_GDP)) # LAG GDP

df %>% group_by(COUNTRY) %>% count(LAG_RATIO) %>% print(n = nrow(.)) # View

df[sapply(df, is.nan)] <- NA

# 3.3 Join data ----

db <- full_join(db, df, by = c("COUNTRY", "YEAR"))

# 3.4 Labels ----
db$MEAN_RATIO <- sjlabelled::set_label(db$MEAN_RATIO, label = c("Promedio Ratio S80/S20"))
db$MEAN_GDP <- sjlabelled::set_label(db$MEAN_GDP, label = c("Promedio GDP"))
db$MEAN_CorpAll <- sjlabelled::set_label(db$MEAN_CorpAll, label = c("Promedio Corporativismo"))
db$LAG_RATIO <- sjlabelled::set_label(db$LAG_RATIO, label = c("Lag Ratio S80/S20"))
db$LAG_GDP <- sjlabelled::set_label(db$LAG_GDP, label = c("Lag GDP"))
db$GINI <- sjlabelled::set_label(db$GINI, label = c("Gini"))

# 3.5 ISO code ----
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


db$ISO_COUNTRY <- sjlabelled::set_label(db$ISO_COUNTRY, label = c("Código ISO país"))

db$WAVE <- as.factor(db$YEAR)
db$COUNTRY_WAVE <- do.call(paste, c(db[c("ISO_COUNTRY", "WAVE")], sep = "_"))

db$WAVE <- sjlabelled::set_label(db$WAVE, label = c("Ola"))
db$COUNTRY_WAVE <- sjlabelled::set_label(db$COUNTRY_WAVE, label = c("País-ola"))


# 4. Save ----
db <- db %>% select(ID_SUBJECT, YEAR, COUNTRY, ISO_COUNTRY, WAVE, COUNTRY_WAVE, SEX, AGE, DEGREE, INCOME, IDEOLOGY, SUBJEC_CLASS, UNION, 
                    CLASS, CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, CONFLICT_TB, PSCi, GINI, RATIO_IC, CorpAll, GDP,
                    UD, SOC_EXPEND, MEAN_RATIO, MEAN_CorpAll, MEAN_GDP, LAG_RATIO, LAG_GDP, FACTOR)

db <- db %>% filter(!is.na(PSCi))

db <- as_tibble(db)

save(db, file = "../output/data/db-proc.RData")
save(df, file = "../output/data/df2-proc.RData")
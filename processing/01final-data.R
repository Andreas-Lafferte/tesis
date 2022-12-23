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


# 3.1 Individual level variables (ISSP) ----

names(issp99)
names(issp09)
names(issp19)

## Join ISSP

db <- rbind(issp99,issp09)
db <- rbind(db,issp19)

### Re-level ideology

frq(db$IDEOLOGY)
levels(db$IDEOLOGY)

db$IDEOLOGY <- car::recode(db$IDEOLOGY, recodes = c("'Derecha' = 'Derecha'; 
                                                'Centro' = 'Centro';
                                                'Izquierda' = 'Izquierda';
                                                'Sin identificación' = 'Sin identificación'")) %>% 
  factor(., levels = c("Derecha", "Centro", "Izquierda", "Sin identificación"),
         labels = c("Derecha", "Centro", "Izquierda", "Sin identificación"))


## Dependent variable: PSCi

db <- db %>% 
  rowwise() %>%
  mutate(PSCi = sum(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, na.rm = F)) 

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

# 3.2 Contextual level variables (WIID, SWIID, ICTWSS, OECD) ----

names(wiid)
names(ictwss)
names(oecd)
names(swiid_summary)

df <- full_join(wiid, ictwss, by = c("COUNTRY", "YEAR"))
df <- full_join(df, oecd, by = c("COUNTRY", "YEAR"))
df <- full_join(df, swiid_summary, by = c("COUNTRY", "YEAR"))

df <- df %>% select(COUNTRY, YEAR, RATIO_IC, CorpAll, GDP, SOC_EXPEND, UD, GINI)

# 3.3 Final data and transforming variables ----

db <- full_join(db, df, by = c("COUNTRY", "YEAR"))

# ID subject
db <- tibble::rowid_to_column(db, "ID_SUBJECT")

db_original <- db %>% as_tibble(.) #original

db <- db %>% select(everything(), -DEGREE, -SUBJEC_CLASS, -INCOME, -GINI, -CONFLICT_TB, -EGP, -ISEI)

db <- db %>% na.omit()

db <- left_join(db, db_original[,c(1,12,13)], by = "ID_SUBJECT")

df_original <- df

df <- df %>% na.omit()
df <- df %>% filter(!COUNTRY %in% c("Japon", "Canada", "Irlanda"))
df <- df %>% as_tibble(.)

# 3.4 Transforming variables ----

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

## AGE center CWC (group mean)

db <- db %>% group_by(COUNTRY) %>% 
  mutate(mean.age = mean(AGE, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(COUNTRY) %>% 
  mutate(C_AGE = AGE - mean.age)

# 3.5 Identification variables ----



# ISO code
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

# 3.6 Final data ----

db <- db %>% select(YEAR, COUNTRY, ISO_COUNTRY, WAVE, COUNTRY_WAVE, SEX, AGE,
                    IDEOLOGY, UNION, CLASS, EGP, ISEI, PSCi, RATIO_IC, CorpAll, GDP, GDP_LOG, 
                    UD, SOC_EXPEND, C_RATIO, C_GDP, C_SOCEXPEND, C_UD, C_AGE, FACTOR, starts_with("CONFLICT"))

db <- db %>% as_tibble(.)

## Labels

db$YEAR <- sjlabelled::set_label(db$YEAR, label = c("Año"))
db$SEX <- sjlabelled::set_label(db$SEX, label = c("Sexo"))
db$AGE <- sjlabelled::set_label(db$AGE, label = c("Edad"))
db$UNION <- sjlabelled::set_label(db$UNION, label = c("Afiliación sindical"))
db$IDEOLOGY <- sjlabelled::set_label(db$IDEOLOGY, label = c("Identificación política"))
db$CLASS <- sjlabelled::set_label(db$CLASS, label = c("Posición de clase EOW"))
db$EGP <- sjlabelled::set_label(db$EGP, label = c("Posición de clase EGP"))
db$ISEI <- sjlabelled::set_label(db$ISEI, label = c("International Socio-Economic Index of occupational status"))
db$ISO_COUNTRY <- sjlabelled::set_label(db$ISO_COUNTRY, label = c("Código ISO país"))
db$WAVE <- sjlabelled::set_label(db$WAVE, label = c("Ola"))
db$COUNTRY_WAVE <- sjlabelled::set_label(db$COUNTRY_WAVE, label = c("País-ola"))
db$C_RATIO <- sjlabelled::set_label(db$C_RATIO, label = c("Ratio S80/S20 [CGM]"))
db$C_GDP <- sjlabelled::set_label(db$C_GDP, label = c("GDP Per capita [CGM]"))
db$C_SOCEXPEND <- sjlabelled::set_label(db$C_SOCEXPEND, label = c("Gasto social %GDP [CGM]"))
db$C_UD <- sjlabelled::set_label(db$C_UD, label = c("Densidad sindical [CGM]"))
db$C_AGE <- sjlabelled::set_label(db$C_AGE, label = c("Edad [CWC]"))
db$FACTOR <- sjlabelled::set_label(db$FACTOR, label = c("Factor expansión"))
db$COUNTRY <- sjlabelled::set_label(db$COUNTRY, label = c("País"))
db$PSCi <- sjlabelled::set_label(db$PSCi, label = c("Perceived Social Conflict Index"))
db$RATIO_IC <- sjlabelled::set_label(db$RATIO_IC, label = c("Ratio S80/S20"))
db$GDP <- sjlabelled::set_label(db$GDP, label = c("GDP per capita USD"))
db$GDP_LOG <- sjlabelled::set_label(db$GDP_LOG, label = c("Log GDP per capita USD"))
db$CorpAll <- sjlabelled::set_label(db$CorpAll, label = c("Indice corporativismo"))
db$UD <- sjlabelled::set_label(db$UD, label = c("Densidad sindical"))
db$SOC_EXPEND <- sjlabelled::set_label(db$SOC_EXPEND, label = c("Gasto social (%GDP)"))
db$CONFLICT_RP <- sjlabelled::set_label(db$CONFLICT_RP, label = c("Conflictos: ricos - pobres"))
db$CONFLICT_WCMC <- sjlabelled::set_label(db$CONFLICT_WCMC, label = c("Conflictos: clase obrera - clase media"))
db$CONFLICT_MW <- sjlabelled::set_label(db$CONFLICT_MW, label = c("Conflictos: directivos - trabajadores"))


# 4. Save ----

save(db, file = "../output/data/db-proc.RData")
save(df, file = "../output/data/df2-proc.RData")
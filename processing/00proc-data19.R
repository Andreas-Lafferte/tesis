# Code 3: Process ISSP 2019


# 1. Packages -------------------------------------------------------------


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


# 2. Data -----------------------------------------------------------------


issp19 <- read_dta("../input/data/ZA7600_v2-0-0.dta")
sapply(issp19, class)
names(issp19)


# 3. Processing -----------------------------------------------------------


issp19 <- issp19 %>% select(country,
                            v36,
                            v37,
                            v38,
                            v61,
                            SEX,
                            AGE,
                            DEGREE,
                            WORK,
                            EMPREL,
                            WRKSUP,
                            NSUP,
                            ISCO08,
                            UNION,
                            222:243,
                            WEIGHT,
                            PARTY_LR,
                            BG_PRTY,
                            SR_PRTY,
                            TW_PRTY)

str(issp19)

# 3.1 YEAR ----
issp19$YEAR <- 2019
issp19$YEAR <- as.numeric(issp19$YEAR) 
issp19$YEAR <- sjlabelled::set_label(issp19$YEAR, label = c("Año"))

# 3.2 FACTOR ----
issp19 <- rename_variables(issp19, WEIGHT = "FACTOR")
issp19$FACTOR <- sjlabelled::set_label(issp19$FACTOR, label = c("Factor expansión"))

# 3.3 COUNTRY ----
frq(issp19$country)
issp19 <- issp19 %>% mutate(COUNTRY = case_when(country == 152 ~ "Chile",
                                                country == 191 ~ "Croacia",
                                                country == 203 ~ "Rep Checa",
                                                country == 208 ~ "Dinamarca",
                                                country == 246 ~ "Finlandia",
                                                country == 276 ~ "Alemania",
                                                country == 380 ~ "Italia",
                                                country == 392 ~ "Japon", 
                                                country == 554 ~ "Nueva Zelanda",
                                                country == 608 ~ "Filipinas",
                                                country == 643 ~ "Rusia",
                                                country == 705 ~ "Eslovenia",
                                                country == 710 ~ "Sudafrica",
                                                country == 756 ~ "Suiza",
                                                country == 764 ~ "Tailandia",
                                                country == 100 ~ "Bulgaria",
                                                country == 158 ~ "Taiwan",
                                                country == 352 ~ "Islandia",
                                                country == 376 ~ "Israel",
                                                country == 440 ~ "Lituania",
                                                country == 826 ~ "Gran Bretaña",
                                                country == 740 ~ "Surinam",
                                                TRUE ~ NA_character_))
issp19$COUNTRY <- sjlabelled::set_label(issp19$COUNTRY, label = c("País"))

# 3.4 SEX ----
frq(issp19$SEX)
issp19$SEX <- as.numeric(issp19$SEX)
issp19$SEX <- car::recode(issp19$SEX, recodes = c("-9 = NA; 1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T)
issp19$SEX <- sjlabelled::set_label(issp19$SEX, label = c("Sexo"))

# 3.5 AGE ----
frq(issp19$AGE)
issp19$AGE <- set_na(issp19$AGE, na = c(-9), drop.levels = T, as.tag = F)
issp19$AGE <- as.numeric(issp19$AGE)
issp19$AGE <- sjlabelled::set_label(issp19$AGE, label = c("Edad"))

# 3.6 UNION ----
frq(issp19$UNION)
issp19$UNION <- as.numeric(issp19$UNION)
issp19$UNION <- car::recode(issp19$UNION, recodes = c("c(-9,-7,-4) = NA; 1 = 'Si'; 2 = 'Si'; 3 = 'No'"), as.factor = T)
issp19$UNION <- sjlabelled::set_label(issp19$UNION, label = c("Afiliación sindical"))



# 3.7 POLICIAL IDENTIFICATION ----
frq(issp19$PARTY_LR)
frq(issp19$BG_PRTY)
frq(issp19$SR_PRTY)
frq(issp19$TW_PRTY)

issp19 <- issp19 %>% mutate(IDEOLOGY = case_when(PARTY_LR %in% c(1,2) ~ "Izquierda",
                                       PARTY_LR == 3 ~ "Centro",
                                       PARTY_LR %in% c(4,5) ~ "Derecha",
                                       PARTY_LR %in% c(-4,6) ~ "Sin identificación",
                                       BG_PRTY %in% c(2,9) ~ "Izquierda",
                                       BG_PRTY %in% c(4,6,7) ~ "Centro",
                                       BG_PRTY %in% c(1,3,5,8,12,13) ~ "Derecha",
                                       BG_PRTY == -4 ~ "Sin identificación",
                                       SR_PRTY %in% c(1,4) ~ "Izquierda",
                                       SR_PRTY %in% c(2,3,5) ~ "Centro",
                                       SR_PRTY %in% c(-4,95) ~ "Sin identificación",
                                       TW_PRTY %in% c(2) ~ "Izquierda",
                                       TW_PRTY %in% c(1,3) ~ "Derecha",
                                       TW_PRTY == -4 ~ "Sin identificación",
                                       TRUE ~ NA_character_))


issp19$IDEOLOGY <- as.factor(issp19$IDEOLOGY)
issp19$IDEOLOGY <- sjlabelled::set_label(issp19$IDEOLOGY, label = c("Identificación política"))


# 3.8 SUBJECTIVE SOCIAL CLASS ----
frq(issp19$v61)
issp19 <- issp19 %>% mutate(SUBJEC_CLASS = case_when(v61 == 1 ~ "6.Clase baja",
                                                     v61 == 2 ~ "5.Clase trabajadora",
                                                     v61 == 3 ~ "4.Clase media-baja",
                                                     v61 == 4 ~ "3.Clase media",
                                                     v61 == 5 ~ "2.Clase media_alta",
                                                     v61 == 6 ~ "1.Clase alta",
                                                     TRUE ~ NA_character_))

issp19$SUBJEC_CLASS <- as.factor(issp19$SUBJEC_CLASS)
issp19$SUBJEC_CLASS <- sjlabelled::set_label(issp19$SUBJEC_CLASS, label = c("Clase social subjetiva"))

# 3.9 INCOME ----
frq(issp19$CH_RINC)
frq(issp19$RU_RINC)
frq(issp19$CL_RINC)
frq(issp19$CZ_RINC)
frq(issp19$DE_RINC)
frq(issp19$DK_RINC)
frq(issp19$FI_RINC)
frq(issp19$HR_RINC)
frq(issp19$IT_RINC)
frq(issp19$JP_RINC)
frq(issp19$NZ_RINC)
frq(issp19$PH_RINC)
frq(issp19$SI_RINC)
frq(issp19$TH_RINC)
frq(issp19$ZA_RINC)
frq(issp19$BG_RINC)
frq(issp19$GB_RINC)
frq(issp19$IS_RINC)
frq(issp19$TW_RINC)
frq(issp19$LT_RINC)
frq(issp19$IL_RINC)

issp19 <- issp19 %>% 
  mutate_at(vars(15:36), ~ as.numeric(.)) %>% 
  mutate_at(vars(15:36), funs(car::recode(. ,"-9 = NA; -8 = NA; -7 = NA; -2 = NA")))

# ntile function without NAs
ntile_na <- function(x,n)
{
  notna <- !is.na(x)
  out <- rep(NA_real_,length(x))
  out[notna] <- ntile(x[notna],n)
  return(out)
}

issp19 <- issp19 %>% 
  mutate_at(vars(15:36), ~ ntile_na(., 10))

issp19 %>% filter(COUNTRY == "Chile") %>% count(CL_RINC) # works

df <- issp19 %>% select(15:36)
df <- t(df)
test <- colSums(df, na.rm = T)
test <- as.data.frame(test)
test$test <- car::recode(test$test, recodes = c("0 = NA"))

issp19$INCOME <- test$test
issp19$INCOME <- as.factor(issp19$INCOME)
issp19$INCOME <- sjlabelled::set_label(issp19$INCOME, label = c("Decil ingreso"))

# 3.10 EDUCATION ----
frq(issp19$DEGREE) # 5 & 6 

# For control var
issp19 <- issp19 %>% mutate(DEGREE_1 = case_when(DEGREE == 0 ~ "1.Primaria incompleta o menos",
                                                 DEGREE == 1 ~ "2.Primaria completa",
                                                 DEGREE == 2 ~ "3.Secundaria incompleta",
                                                 DEGREE == 3 ~ "4.Secundaria completa",
                                                 DEGREE == 4 ~ "5.Universitaria incompleta",
                                                 DEGREE %in% c(5,6) ~ "6.Universitaria completa",
                                                 TRUE ~ NA_character_))

issp19$DEGREE_1 <- as.factor(issp19$DEGREE_1)
issp19$DEGREE_1 <- sjlabelled::set_label(issp19$DEGREE_1, label = c("Nivel educativo"))

# For control skills in class var
issp19 <- issp19 %>% mutate(EDUC = case_when(DEGREE %in% c(0:4) ~ 'No',
                                             DEGREE %in% c(5:6) ~ 'Si',
                                             TRUE ~ NA_character_))
issp19$EDUC <- as.factor(issp19$EDUC)
issp19$EDUC <- sjlabelled::set_label(issp19$EDUC, label = c("Nivel educativo terciario completo"))
table(issp19$DEGREE, useNA = "ifany")
table(issp19$EDUC, useNA = "ifany")

# 3.11 CLASS ESCHEME E.O WRIGHT ----

## Employment relation
issp19 <- issp19 %>% filter(WORK != 3)
frq(issp19$EMPREL)

issp19$EMPREL <- as.numeric(issp19$EMPREL)
issp19 <- issp19 %>% mutate(prop_salaried = case_when(EMPREL == 1 ~ "Asalariado",
                                                       EMPREL == 2 ~ "3.Pequeña burguesia",
                                                       EMPREL == 5 ~ "3.Pequeña burguesia",
                                                       EMPREL == 3 ~ "2.Pequeños empleadores",
                                                       EMPREL == 4 ~ "1.Capitalistas",
                                                       TRUE ~ NA_character_))

issp19 %>% count(prop_salaried) %>% mutate(prop = prop.table(n))
issp19 %>% count(EMPREL) %>% mutate(prop = prop.table(n))

## Control
frq(issp19$WRKSUP)
issp19$WRKSUP <- as.numeric(issp19$WRKSUP)
issp19 <- issp19 %>% mutate(control = case_when(WRKSUP == 1 ~ "Control",
                                                WRKSUP == 2 ~ "No control",
                                                TRUE ~ NA_character_))

issp19 %>% count(WRKSUP) %>% mutate(prop = prop.table(n))
issp19 %>% count(control) %>% mutate(prop = prop.table(n))

## Skills
frq(issp19$ISCO08)
issp19 <- issp19 %>% filter(ISCO08 != -9, ISCO08 != -8, ISCO08 != 110, ISCO08 != 210, ISCO08 != 310) # eliminated FA & don't labor force people
issp19$ISCO08_2 <- as.numeric(issp19$ISCO08)

issp19$ISCO08_2 <- substr(issp19$ISCO08_2, start = 1, stop = 2)
frq(issp19$ISCO08)
frq(issp19$ISCO08_2)

### Skills variable
issp19 <- issp19 %>% mutate(skills = case_when(ISCO08_2 %in% c(10:26) ~ 'Expertos',
                                               ISCO08_2 %in% c(30, 31, 32, 33, 34, 35, 60, 61, 72) ~ 'Calificados',
                                               ISCO08_2 %in% c(40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 62, 63, 70, 71, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96) ~ 'No calificados',
                                               TRUE ~ NA_character_))

issp19 %>% count(skills) %>% mutate(prop = prop.table(n))

## Education control effect
issp19 <- issp19 %>% mutate(skillsA = if_else(skills == "Expertos" & EDUC == "Si", "Expertos",
                                              if_else(skills == "Expertos" & EDUC == "No", "Calificados", skills))) %>% 
                                                 mutate(skillsA = if_else(is.na(skillsA), skills, skillsA))

issp19 %>% count(skills) %>% mutate(prop = prop.table(n))
issp19 %>% count(skillsA) %>% mutate(prop = prop.table(n)) # Skilled up

ctable(issp19$prop_salaried, issp19$skillsA)

## FINAL VARIABLE CLASS 

issp19$CLASS <- NA
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="1.Capitalistas", 1, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="2.Pequeños empleadores", 2, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="3.Pequeña burguesia", 3, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="Asalariado" & control=="Control" & skillsA=="Expertos", 4, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="Asalariado" & control=="No control" & skillsA=="Expertos", 5, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="Asalariado" & control=="Control" & skillsA=="Calificados", 6, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="Asalariado" & control=="Control" & skillsA=="No calificados", 7, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="Asalariado" & control=="No control" & skillsA=="Calificados", 8, CLASS))
issp19$CLASS <- with(issp19, ifelse(prop_salaried=="Asalariado" & control=="No control" & skillsA=="No calificados", 9, CLASS))

issp19$CLASS <- factor(issp19$CLASS,levels = c(1:9),
                       labels = c("1.Capitalistas","2.Pequeños empleadores","3.Pequeña burguesia",
                                  "4.Expertos directivos","5.Expertos sin autoridad",
                                  "6.Supervisores calificados","7.Supervisores no calificados",
                                  "8.Trabajadores calificados","9.Trabajadores no calificados"))

issp19 %>% filter(!is.na(CLASS)) %>% count(CLASS) %>% mutate(prop = prop.table(n)) 
issp19$CLASS <- sjlabelled::set_label(issp19$CLASS, label = c("Posición de clase"))

# 3.12 PERCEIVED SOCIAL CONFLICT INDEX ----

## Rich and poor
frq(issp19$v36)
issp19 <- issp19 %>% mutate(CONFLICT_RP = case_when(v36 == 1 ~ 3,
                                                    v36 == 2 ~ 2,
                                                    v36 == 3 ~ 1,
                                                    v36 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp19$CONFLICT_RP)
sjPlot::plot_frq(na.omit(issp19$CONFLICT_RP), type = "histogram", show.mean = TRUE)
issp19$CONFLICT_RP <- sjlabelled::set_label(issp19$CONFLICT_RP, label = c("Conflictos: ricos - pobres"))

## Working class and middle class
frq(issp19$v37)
issp19 <- issp19 %>% mutate(CONFLICT_WCMC = case_when(v37 == 1 ~ 3,
                                                      v37 == 2 ~ 2,
                                                      v37 == 3 ~ 1,
                                                      v37 == 4 ~ 0,
                                                      TRUE ~ NA_real_))
frq(issp19$CONFLICT_WCMC)
sjPlot::plot_frq(na.omit(issp19$CONFLICT_WCMC), type = "histogram", show.mean = TRUE)
issp19$CONFLICT_WCMC <- sjlabelled::set_label(issp19$CONFLICT_WCMC, label = c("Conflictos: clase trabajadora - clase media"))

## Management and workers 
frq(issp19$v38)
issp19 <- issp19 %>% mutate(CONFLICT_MW = case_when(v38 == 1 ~ 3,
                                                    v38 == 2 ~ 2,
                                                    v38 == 3 ~ 1,
                                                    v38 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp19$CONFLICT_MW)
sjPlot::plot_frq(na.omit(issp19$CONFLICT_MW), type = "histogram", show.mean = TRUE)
issp19$CONFLICT_MW <- sjlabelled::set_label(issp19$CONFLICT_MW, label = c("Conflictos: directivos - trabajadores"))

## People at the top and people at the bottom 
issp19$CONFLICT_TB <- NA
issp19$CONFLICT_TB <- sjlabelled::set_label(issp19$CONFLICT_TB, label = c("Conflictos: gente de arriba - gente de abajo"))

## PSCI 
issp19 <- issp19 %>% 
  rowwise() %>% 
  mutate(PSCi = sum(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, na.rm = F))

issp19$PSCi <- sjlabelled::set_label(issp19$PSCi, label = c("Perceived Social Conflict Index"))
frq(issp19$PSCi)
sjPlot::plot_frq(na.omit(issp19$PSCi), type = "histogram", show.mean = TRUE) # Follows a normal distribution. Careful with the NA

## Cronbach's alpha
matriz <- issp19 %>% select(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW)
psych::alpha(matriz) # coef = 0.80

## Polychoric alpha ordinal (Likert scale)
matriz_poly <- polychoric(matriz) 
psych::alpha(matriz_poly$rho) # coef = 0.84

# View
issp19 %>% filter(!is.na(PSCi)) %>% count(PSCi)

# 4. Save ----
issp19 <- issp19 %>% select(YEAR,
                            COUNTRY,
                            SEX,
                            AGE,
                            DEGREE = DEGREE_1,
                            INCOME,
                            SUBJEC_CLASS,
                            UNION,
                            IDEOLOGY,
                            CLASS,
                            55:58,
                            FACTOR)

sapply(issp19, class)
save(issp19, file = "../output/data/issp19.RData")
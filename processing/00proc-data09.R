# Code 2: Process ISSP 2009


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


issp09 <- read_dta("../input/data/ISSP2009.dta")
sapply(issp09, class)
names(issp09)


# 3. Processing -----------------------------------------------------------


issp09 <- issp09 %>% select(V3, 
                            V5,
                            SEX,
                            AGE,
                            DEGREE,
                            UNION,
                            WEIGHT,
                            V40,
                            V41,
                            V42,
                            V43,
                            V64,
                            V66,
                            ISCO88,
                            NEMPLOY,
                            WRKSUP,
                            141:181)
str(issp09)

# 3.1 YEAR ----
issp09$YEAR <- 2009
issp09$YEAR <- as.numeric(issp09$YEAR)
issp09$YEAR <- sjlabelled::set_label(issp09$YEAR, label = c("Año"))

# 3.2 FACTOR ----
issp09 <- rename_variables(issp09, WEIGHT = "FACTOR")
issp09$FACTOR <- sjlabelled::set_label(issp09$FACTOR, label = c("Factor expansión"))

# 3.3 COUNTRY ----
frq(issp09$V5)
issp09 <- issp09 %>% mutate(COUNTRY = case_when(V5 == 32 ~ "Argentina",
                                                V5 == 36 ~ "Australia",
                                                V5 == 40 ~ "Austria",
                                                V5 == 56 ~ "Belgica",
                                                V5 == 100 ~ "Bulgaria",
                                                V5 == 152 ~ "Chile",
                                                V5 == 156 ~ "China",
                                                V5 == 158 ~ "Taiwan",
                                                V5 == 191 ~ "Croacia",
                                                V5 == 196 ~ "Chipre",
                                                V5 == 203 ~ "Rep Checa",
                                                V5 == 208 ~ "Dinamarca",
                                                V5 == 233 ~ "Estonia",
                                                V5 == 246 ~ "Finlandia",
                                                V5 == 250 ~ "Francia",
                                                V5 == 276 ~ "Alemania",
                                                V5 == 348 ~ "Hungria",
                                                V5 == 352 ~ "Islandia",
                                                V5 == 376 ~ "Israel",
                                                V5 == 380 ~ "Italia",
                                                V5 == 392 ~ "Japon",
                                                V5 == 410 ~ "Corea del Sur",
                                                V5 == 428 ~ "Letonia",
                                                V5 == 440 ~ "Lituania",
                                                V5 == 554 ~ "Nueva Zelanda",
                                                V5 == 578 ~ "Noruega",
                                                V5 == 608 ~ "Filipinas",
                                                V5 == 616 ~ "Polonia",
                                                V5 == 620 ~ "Portugal",
                                                V5 == 643 ~ "Rusia",
                                                V5 == 703 ~ "Eslovaquia",
                                                V5 == 705 ~ "Eslovenia",
                                                V5 == 710 ~ "Sudafrica",
                                                V5 == 724 ~ "España",
                                                V5 == 752 ~ "Suecia",
                                                V5 == 756 ~ "Suiza",
                                                V5 == 792 ~ "Turquia",
                                                V5 == 804 ~ "Ucrania",
                                                V5 == 826 ~ "Gran Bretaña",
                                                V5 == 840 ~ "USA",
                                                V5 == 862 ~ "Venezuela",
                                                TRUE ~ NA_character_))
issp09$COUNTRY <- sjlabelled::set_label(issp09$COUNTRY, label = c("País"))

# 3.4 SEX ----
frq(issp09$SEX)
issp09$SEX <- as.numeric(issp09$SEX)
issp09$SEX <- car::recode(issp09$SEX, recodes = c("9 = NA; 1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T)
issp09$SEX <- sjlabelled::set_label(issp09$SEX, label = c("Sexo"))


# 3.5 AGE ----
frq(issp09$AGE)
issp09$AGE <- set_na(issp09$AGE, na = c(99), drop.levels = T, as.tag = F)
issp09$AGE <- as.numeric(issp09$AGE)
issp09$AGE <- sjlabelled::set_label(issp09$AGE, label = c("Edad"))

# 3.6 UNION ----
frq(issp09$UNION)
issp09$UNION <- as.numeric(issp09$UNION)
issp09$UNION <- car::recode(issp09$UNION, recodes = c("0 = NA; c(8,9) = NA; c(1,2) = 'Si'; 3 = 'No'"), as.factor = T)
issp09$UNION <- sjlabelled::set_label(issp09$UNION, label = "Afiliación sindical")

# 3.7 SUBJECTIVE SOCIAL CLASS ----
frq(issp09$V66)
issp09 <- issp09 %>% mutate(SUBJEC_CLASS = case_when(V66 == 1 ~ "6.Clase baja",
                                                     V66 == 2 ~ "5.Clase trabajadora",
                                                     V66 == 3 ~ "4.Clase media-baja",
                                                     V66 == 4 ~ "3.Clase media",
                                                     V66 == 5 ~ "2.Clase media_alta",
                                                     V66 == 6 ~ "1.Clase alta",
                                                     TRUE ~ NA_character_))

issp09$SUBJEC_CLASS <- as.factor(issp09$SUBJEC_CLASS)
issp09$SUBJEC_CLASS <- sjlabelled::set_label(issp09$SUBJEC_CLASS, label = c("Clase social subjetiva"))

# 3.8 INCOME ----
frq(issp09$AR_RINC)
frq(issp09$AT_RINC)
frq(issp09$AU_RINC)
frq(issp09$BE_RINC)
frq(issp09$BG_RINC)
frq(issp09$CH_RINC)
frq(issp09$CL_RINC)
frq(issp09$CN_RINC)
frq(issp09$CY_RINC)
frq(issp09$CZ_RINC)
frq(issp09$DE_RINC)
frq(issp09$DK_RINC)
frq(issp09$EE_RINC)
frq(issp09$ES_RINC)
frq(issp09$FI_RINC)
frq(issp09$FR_RINC)
frq(issp09$GB_RINC)
frq(issp09$HR_RINC)
frq(issp09$HU_RINC)
frq(issp09$IL_RINC)
frq(issp09$IS_RINC)
frq(issp09$IT_RINC)
frq(issp09$JP_RINC)
frq(issp09$KR_RINC)
frq(issp09$LT_RINC)
frq(issp09$LV_RINC)
frq(issp09$NO_RINC)
frq(issp09$NZ_RINC)
frq(issp09$PH_RINC)
frq(issp09$PL_RINC)
frq(issp09$PT_RINC)
frq(issp09$RU_RINC)
frq(issp09$SE_RINC)
frq(issp09$SI_RINC)
frq(issp09$SK_RINC)
frq(issp09$TR_RINC)
frq(issp09$TW_RINC)
frq(issp09$UA_RINC)
frq(issp09$US_RINC)
frq(issp09$VE_RINC)
frq(issp09$ZA_RINC)

issp09 <- issp09 %>% 
  mutate_at(vars(17:57), ~ as.numeric(.)) %>% 
  mutate_at(vars(17:57), funs(car::recode(. ,"999990 = NA; 999997 = NA; 999998 = NA; 999999 = NA; 
                                              9999990 = NA; 9999997 = NA; 9999998 = NA; 9999999 = NA;
                                              99999990 = NA; 99999997 = NA; 99999998 = NA; 99999999 = NA")))
  
# ntile function without NAs
ntile_na <- function(x,n)
{
  notna <- !is.na(x)
  out <- rep(NA_real_,length(x))
  out[notna] <- ntile(x[notna],n)
  return(out)
}

issp09 <- issp09 %>%  
  mutate_at(vars(17:57), ~ ntile_na(., 10)) 
  
issp09 %>% filter(COUNTRY == "Argentina") %>% count(AR_RINC) # works

df <- issp09 %>% select(17:57)
df <- t(df)
test <- colSums(df,na.rm=T)
test <- as.data.frame(test)
test$test <- car::recode(test$test, recodes = c("0 = NA"))

issp09$INCOME <- test$test
issp09$INCOME <- as.factor(issp09$INCOME)
issp09$INCOME <- sjlabelled::set_label(issp09$INCOME, label = c("Decil ingreso"))

# 3.9 EDUCATION ----
frq(issp09$DEGREE)

# For control var
issp09 <- issp09 %>% mutate(DEGREE_1 = case_when(DEGREE == 0 ~ "Primaria incompleta o menos",
                                               DEGREE == 1 ~ "Primaria completa",
                                               DEGREE == 2 ~ "Secundaria incompleta",
                                               DEGREE == 3 ~ "Secundaria completa",
                                               DEGREE == 4 ~ "Universitaria incompleta",
                                               DEGREE == 5 ~ "Universitaria completa",
                                               TRUE ~ NA_character_))

issp09$DEGREE_1 <- as.factor(issp09$DEGREE_1)
issp09$DEGREE_1 <- sjlabelled::set_label(issp09$DEGREE_1, label = c("Nivel educativo"))

# For control skills in class var
issp09 <- issp09 %>% mutate(EDUC = case_when(DEGREE %in% c(0:4) ~ 'No',
                                             DEGREE == 5 ~ 'Si',
                                             TRUE ~ NA_character_))
issp09$EDUC <- as.factor(issp09$EDUC)
issp09$EDUC <- sjlabelled::set_label(issp09$EDUC, label = c("Nivel educativo terciario completo"))
table(issp09$DEGREE, useNA = "ifany")
table(issp09$EDUC, useNA = "ifany")

# 3.10 CLASS ESCHEME E.O WRIGHT ----

# Employment relation
frq(issp09$V64)
issp09 <- issp09 %>% filter(V64 != 0)

## Salaried 
issp09$V64 <- as.numeric(issp09$V64)
issp09 <- issp09 %>% mutate(prop_salaried1 = case_when(V64 == 1 ~ 'Asalariado',
                                                       V64 == 2 ~ 'Asalariado',
                                                       V64 == 3 ~ 'Asalariado',
                                                       V64 == 4 ~ 'Auto-empleado',
                                                       V64 %in% c(5:9) ~ NA_character_))

issp09 %>% count(V64) %>% mutate(prop = prop.table(n))
issp09 %>% count(prop_salaried1) %>% mutate(prop = prop.table(n))

## Owners
#1. Pequeña burguesia: 0 a 1 empleados
#2. Pequeños empleadores: de 2 a 9 empleados
#3. Capitalistas: de 10 a más empleados

frq(issp09$NEMPLOY)
issp09$owners <- as.numeric(issp09$NEMPLOY)
issp09 <- issp09 %>% mutate(owners = case_when(owners %in% c(0, 1, 9995, 9997, 9998, 9999) & prop_salaried1 == 'Auto-empleado' ~ '3.Pequeña burguesia',
                                               owners %in% c(2:9) ~ '2.Pequeños empleadores',
                                               owners %in% c(10: 4000) ~ '1.Capitalistas',
                                               TRUE ~ NA_character_))

issp09 %>% count(owners) %>% mutate(prop = prop.table(n))

## Salaried final
issp09 <- issp09 %>% mutate(salaried = if_else(is.na(owners), prop_salaried1, owners)) # if NA in owners, return the value in prop_salaried

issp09 %>% count(prop_salaried1) %>% mutate(prop = prop.table(n))
issp09 %>% count(salaried) %>% mutate(prop = prop.table(n))

## Control
frq(issp09$WRKSUP)
issp09$WRKSUP <- as.numeric(issp09$WRKSUP)
issp09 <- issp09 %>% mutate(control = case_when(WRKSUP == 1 ~ 'Control',
                                                WRKSUP == 2 ~ 'No control',
                                                WRKSUP %in% c(0, 7, 8, 9) ~ NA_character_))

issp09$control <- as.factor(issp09$control)
issp09 %>% count(control) %>% mutate(prop = prop.table(n))
issp09 %>% count(WRKSUP) %>% mutate(prop = prop.table(n))

# skills
frq(issp09$ISCO88)
issp09 <- issp09 %>% filter(ISCO88 != 0, ISCO88 != 110) # eliminated FFAA and don't labor force people
issp09$ISCO88 <- as.numeric(issp09$ISCO88)
table(issp09$ISCO88)

issp09$ISCO88 <- substr(issp09$ISCO88, start = 1, stop = 2) # 2 digits
table(issp09$ISCO88)

### Skills variable
issp09 <- issp09 %>% mutate(skills = case_when(ISCO88 %in% c(10:24) ~ 'Expertos',
                                               ISCO88 %in% c(30, 31, 32, 33, 34, 60, 61, 70, 71, 72, 73, 74) ~ 'Calificados',
                                               ISCO88 %in% c(40, 41, 42, 50, 51, 52, 62, 80, 81, 82, 83, 90, 91, 92, 93) ~ 'No calificados',
                                               ISCO88 == 99 ~ NA_character_))

issp09 %>% count(skills) %>% mutate(prop = prop.table(n))

## Education control effect
issp09 <- issp09 %>% mutate(skillsA = if_else(skills =="Expertos" & EDUC=="Si", "Expertos",
                                              if_else(skills == "Expertos" & EDUC=="No", "Calificados", skills))) %>% 
                                              mutate(skillsA = if_else(is.na(skillsA), skills, skillsA))
                                                     
issp09 %>% count(skills) %>% mutate(prop = prop.table(n))
issp09 %>% count(skillsA) %>% mutate(prop = prop.table(n)) # Skilled up

ctable(issp09$salaried, issp09$skillsA)
## FINAL VARIABLE CLASS 

issp09$CLASS <- NA
issp09$CLASS <- with(issp09, ifelse(salaried=="1.Capitalistas", 1, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="2.Pequeños empleadores", 2, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="3.Pequeña burguesia", 3, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="Asalariado" & control=="Control" & skillsA=="Expertos", 4, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="Asalariado" & control=="No control" & skillsA=="Expertos", 5, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="Asalariado" & control=="Control" & skillsA=="Calificados", 6, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="Asalariado" & control=="Control" & skillsA=="No calificados", 7, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="Asalariado" & control=="No control" & skillsA=="Calificados", 8, CLASS))
issp09$CLASS <- with(issp09, ifelse(salaried=="Asalariado" & control=="No control" & skillsA=="No calificados", 9, CLASS))

issp09$CLASS <- factor(issp09$CLASS,levels = c(1:9),
                       labels = c("1.Capitalistas","2.Pequeños empleadores","3.Pequeña burguesia",
                                  "4.Expertos directivos","5.Expertos sin autoridad",
                                  "6.Supervisores calificados","7.Supervisores no calificados",
                                  "8.Trabajadores calificados","9.Trabajadores no calificados"))

issp09 %>% filter(!is.na(CLASS)) %>% count(CLASS) %>% mutate(prop = prop.table(n)) 
issp09$CLASS <- sjlabelled::set_label(issp09$CLASS, label = c("Posición de clase"))

# 3.11 PERCEIVED SOCIAL CONFLICT INDEX ----
## Rich and poor 
frq(issp09$V40)
issp09 <- issp09 %>% mutate(CONFLICT_RP = case_when(V40 == 1 ~ 3,
                                                    V40 == 2 ~ 2,
                                                    V40 == 3 ~ 1,
                                                    V40 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp09$CONFLICT_RP)
sjPlot::plot_frq(na.omit(issp09$CONFLICT_RP), type = "histogram", show.mean = TRUE)
issp09$CONFLICT_RP <- sjlabelled::set_label(issp09$CONFLICT_RP, label = c("Conflictos: ricos - pobres"))

## Working class and middle class
frq(issp09$V41)                                                                                                   
issp09 <- issp09 %>% mutate(CONFLICT_WCMC = case_when(V41 == 1 ~ 3,
                                                      V41 == 2 ~ 2,
                                                      V41 == 3 ~ 1,
                                                      V41 == 4 ~ 0,
                                                      TRUE ~ NA_real_))
frq(issp09$CONFLICT_WCMC)
sjPlot::plot_frq(na.omit(issp09$CONFLICT_WCMC), type = "histogram", show.mean = TRUE)
issp09$CONFLICT_WCMC <- sjlabelled::set_label(issp09$CONFLICT_WCMC, label = c("Conflictos: clase trabajadora - clase media"))

## Management and workers 
frq(issp09$V42)
issp09 <- issp09 %>% mutate(CONFLICT_MW = case_when(V42 == 1 ~ 3,
                                                    V42 == 2 ~ 2,
                                                    V42 == 3 ~ 1,
                                                    V42 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp09$CONFLICT_MW)
sjPlot::plot_frq(na.omit(issp09$CONFLICT_MW), type = "histogram", show.mean = TRUE)
issp09$CONFLICT_MW <- sjlabelled::set_label(issp09$CONFLICT_MW, label = c("Conflictos: directivos - trabajadores"))

## People at the top and people at the bottom 
frq(issp09$V43)
issp09 <- issp09 %>% mutate(CONFLICT_TB = case_when(V43 == 1 ~ 3,
                                                    V43 == 2 ~ 2,
                                                    V43 == 3 ~ 1,
                                                    V43 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp09$CONFLICT_TB)
sjPlot::plot_frq(na.omit(issp09$CONFLICT_TB), type = "histogram", show.mean = TRUE)
issp09$CONFLICT_TB <- sjlabelled::set_label(issp09$CONFLICT_TB, label = c("Conflictos: gente de arriba - gente de abajo"))

## PSCI 
issp09 <- issp09 %>% 
  rowwise() %>% 
  mutate(PSCi = sum(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, CONFLICT_TB, na.rm = F))

issp09$PSCi <- sjlabelled::set_label(issp09$PSCi, label = c("Perceived Social Conflict Index"))
frq(issp09$PSCi)
sjPlot::plot_frq(na.omit(issp09$PSCi), type = "histogram", show.mean = TRUE) # Follows a normal distribution. Careful with the NA

## Cronbach's alpha
matriz <- issp09 %>% select(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, CONFLICT_TB)
psych::alpha(matriz) # coef = 0.83

## Polychoric alpha ordinal (Likert scale)
matriz_poly <- polychoric(matriz) 
psych::alpha(matriz_poly$rho) # coef = 0.87

# View
issp09 %>% 
  filter(!is.na(PSCi)) %>% 
  count(PSCi) %>% 
  mutate(proporcion = prop.table(n))

# 4. Save ----
issp09 <- issp09 %>% select(YEAR,
                            COUNTRY,
                            SEX,
                            AGE,
                            DEGREE = DEGREE_1,
                            INCOME,
                            SUBJEC_CLASS,
                            UNION,
                            CLASS,
                            71:74,
                            FACTOR)

sapply(issp09, class)
save(issp09, file = "../output/data/issp09.RData")
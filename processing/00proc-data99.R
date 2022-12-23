# Code 1: Process ISSP 1999


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

issp99 <- read_dta("../input/data/ISSP1999.dta")
sapply(issp99, class)
names(issp99)


# 3. Processing -----------------------------------------------------------

## Select 
issp99 <- issp99 %>% select(v1,
                            v3,
                            sex,
                            age,
                            union,
                            party_lr,
                            class,
                            rincomer,
                            nemploy,
                            wrkgovt, 
                            wrksup,
                            educyrs,
                            degree,
                            isco88_4,
                            selfemp,
                            v41,
                            v42,
                            v43,
                            v44,
                            weight)
str(issp99)

# 3.1 YEAR ----
issp99 <- rename_variables(issp99, v1 = "YEAR") 
issp99$YEAR <- as.numeric(issp99$YEAR)
issp99$YEAR <- sjlabelled::set_label(issp99$YEAR, label = c("Año"))

# 3.2 FACTOR ----
issp99 <- rename_variables(issp99, weight = "FACTOR")
issp99$FACTOR <- sjlabelled::set_label(issp99$FACTOR, label = c("Factor expansión"))

# 3.3 COUNTRY ----
frq(issp99$v3)
issp99 <- issp99 %>% mutate(COUNTRY = case_when(v3 == 1 ~ "Australia",
                                                v3 == 2 | v3 == 3 ~ "Alemania",
                                                v3 == 4 ~ "Gran Bretaña",
                                                v3 == 5 ~ "Irlanda",
                                                v3 == 6 ~ "USA",
                                                v3 == 7 ~ "Austria",
                                                v3 == 8 ~ "Hungria",
                                                v3 == 10 ~ "Irlanda",
                                                v3 == 12 ~ "Noruega",
                                                v3 == 13 ~ "Suecia",
                                                v3 == 14 ~ "Rep Checa",
                                                v3 == 15 ~ "Eslovenia",
                                                v3 == 16 ~ "Polonia",
                                                v3 == 17 ~ "Bulgaria",
                                                v3 == 18 ~ "Rusia",
                                                v3 == 19 ~ "Nueva Zelanda",
                                                v3 == 20 ~ "Canada",
                                                v3 == 21 ~ "Filipinas",
                                                v3 == 22 ~ "Israel",
                                                v3 == 24 ~ "Japon",
                                                v3 == 25 ~ "España",
                                                v3 == 26 ~ "Letonia",
                                                v3 == 27 ~ "Francia",
                                                v3 == 28 ~ "Chipre",
                                                v3 == 29 ~ "Portugal",
                                                v3 == 30 ~ "Chile",
                                                v3 == 33 ~ "Eslovaquia",
                                                TRUE ~ NA_character_))
issp99$COUNTRY <- sjlabelled::set_label(issp99$COUNTRY, label = c("País"))

# 3.4 SEX ----
frq(issp99$sex)
issp99 <- issp99 %>% mutate(SEX = if_else(sex == 1, 'Hombre', 'Mujer', missing = NULL))
issp99$SEX <- as.factor(issp99$SEX)
issp99$SEX <- sjlabelled::set_label(issp99$SEX, label = c("Sexo"))

# 3.5 AGE ----
frq(issp99$age)
issp99$age <- set_na(issp99$age, na = c(98,99), drop.levels = T, as.tag = F)
issp99$age <- as.numeric(issp99$age)
issp99 <- rename_variables(issp99, age = "AGE")
issp99$AGE <- sjlabelled::set_label(issp99$AGE, label = c("Edad"))

# 3.6 UNION ----
frq(issp99$union)
issp99$union <- car::recode(issp99$union, recodes = c("0 = NA; c(8,9) = NA"))
issp99 <- issp99 %>% mutate(UNION = if_else(union ==  1, 'Si', 'No'))
issp99$UNION <- as.factor(issp99$UNION)
issp99$UNION <- sjlabelled::set_label(issp99$UNION, label = "Afiliación sindical")

# 3.7 POLITICAL IDENTIFICATION ----
frq(issp99$party_lr)
issp99 <- issp99 %>% mutate(IDEOLOGY = case_when(party_lr %in% c(1,2) ~ "Izquierda",
                                                 party_lr == 3 ~ "Centro",
                                                 party_lr %in% c(4,5) ~ "Derecha",
                                                 party_lr %in% c(6,7) ~ "Sin identificación",
                                                 TRUE ~ NA_character_))
issp99$IDEOLOGY <- as.factor(issp99$IDEOLOGY)
issp99$IDEOLOGY <- sjlabelled::set_label(issp99$IDEOLOGY, label = c("Identificación política"))

# 3.8 SUBJECTIVE SOCIAL CLASS ----
frq(issp99$class)
issp99 <- issp99 %>% mutate(SUBJEC_CLASS = case_when(class == 1 ~ "6.Clase baja",
                                                     class == 2 ~ "5.Clase trabajadora",
                                                     class == 3 ~ "4.Clase media-baja",
                                                     class == 4 ~ "3.Clase media",
                                                     class == 5 ~ "2.Clase media-alta",
                                                     class == 6 ~ "1.Clase alta",
                                                     TRUE ~ NA_character_))

issp99$SUBJEC_CLASS <- as.factor(issp99$SUBJEC_CLASS)
issp99$SUBJEC_CLASS <- sjlabelled::set_label(issp99$SUBJEC_CLASS, label = c("Clase social subjetiva"))

# 3.9 INCOME ----
frq(issp99$rincomer)
issp99$rincomer <- car::recode(issp99$rincomer, recodes = c("0 = 1; c(97,98,99) = NA")) 
issp99 <- rename_variables(issp99, rincomer = "INCOME") 
issp99$INCOME <- as.factor(issp99$INCOME)
issp99$INCOME <- sjlabelled::set_label(issp99$INCOME, label = c("Decil ingreso"))

# 3.10 EDUCATION ----
frq(issp99$educyrs) # We don't know if this is equal in every country; We use degree
frq(issp99$degree)

## For control var
issp99 <- issp99 %>% mutate(DEGREE = case_when(degree %in% c(1,2) ~ "1.Primaria incompleta o menos",
                                               degree == 3 ~ "2.Primaria completa",
                                               degree == 4 ~ "3.Secundaria incompleta",
                                               degree == 5 ~ "4.Secundaria completa",
                                               degree == 6 ~ "5.Universitaria incompleta",
                                               degree == 7 ~ "6.Universitaria completa",
                                               TRUE ~ NA_character_))

issp99$DEGREE <- as.factor(issp99$DEGREE)
issp99$DEGREE <- sjlabelled::set_label(issp99$DEGREE, label = c("Nivel educativo"))


## For control skills in class var
issp99 <- issp99 %>% mutate(EDUC = case_when(degree %in% c(1:6) ~ 'No',
                                             degree == 7 ~ 'Si',
                                             TRUE ~ NA_character_))
issp99$EDUC <- as.factor(issp99$EDUC)
issp99$EDUC <- sjlabelled::set_label(issp99$EDUC, label = c("Nivel educativo terciario completo"))
table(issp99$degree, useNA = "ifany")
table(issp99$EDUC, useNA = "ifany")

# 3.11 CLASS ESCHEME E.O WRIGHT----

## Employment relation
frq(issp99$wrkgovt)
issp99 <- issp99 %>% filter(wrkgovt != 0)

## Salaried 
issp99$wrkgovt <- as.numeric(issp99$wrkgovt)
issp99 <- issp99 %>% mutate(prop_salaried1 = case_when(wrkgovt == 1 ~ 'Asalariado',
                                                       wrkgovt == 2 ~ 'Asalariado',
                                                       wrkgovt == 3 ~ 'Asalariado',
                                                       wrkgovt == 6 ~ 'Asalariado',
                                                       wrkgovt == 8 ~ 'Auto-empleado',
                                                       wrkgovt == 9 ~ NA_character_))

issp99 %>% count(wrkgovt) %>% mutate(prop = prop.table(n))
issp99 %>% count(prop_salaried1) %>% mutate(prop = prop.table(n))

## Owners
#1. Pequeña burguesia: 0 a 1 empleados
#2. Pequeños empleadores: de 2 a 9 empleados
#3. Capitalistas: de 10 a más empleados

frq(issp99$nemploy)
issp99$owners <- as.numeric(issp99$nemploy)
issp99 <- issp99 %>% mutate(owners = case_when(owners %in% c(0, 1, 9995, 9997, 9999) & prop_salaried1 == 'Auto-empleado' ~ '3.Pequeña burguesia',
                                               owners %in% c(2:9) ~ '2.Pequeños empleadores',
                                               owners %in% c(10: 9990) ~ '1.Capitalistas',
                                               TRUE ~ NA_character_))

issp99 %>% count(owners) %>% mutate(prop = prop.table(n))

## Salaried final
issp99 <- issp99 %>% mutate(salaried = if_else(is.na(owners), prop_salaried1, owners)) # if NA in owners, return the value in prop_salaried

issp99 %>% count(prop_salaried1) %>% mutate(prop = prop.table(n))
issp99 %>% count(salaried) %>% mutate(prop = prop.table(n))

## Control
issp99$wrksup <- as.numeric(issp99$wrksup)
frq(issp99$wrksup)
issp99 <- issp99 %>% mutate(control = case_when(wrksup == 1 ~ 'Control',
                                                wrksup == 2 ~ 'No control',
                                                wrksup %in% c(0, 7, 8, 9) ~ NA_character_))

issp99$control <- as.factor(issp99$control)
issp99 %>% count(control) %>% mutate(prop = prop.table(n))
issp99 %>% count(wrksup) %>% mutate(prop = prop.table(n))

## Skills
issp99 <- rename_variables(issp99, isco88_4 = "ISCO88")
frq(issp99$ISCO88)
issp99 <- issp99 %>% filter(ISCO88!=0,ISCO88!=1,ISCO88!=2,ISCO88!=110) # eliminated FFAA and don't labor force people
issp99$ISCO88 <- as.numeric(issp99$ISCO88)
table(issp99$ISCO88)

issp99$ISCO88_or <- issp99$ISCO88
issp99$ISCO88 <- substr(issp99$ISCO88, start = 1, stop = 2) # 2 digits

### Skills variable
issp99 <- issp99 %>% mutate(skills = case_when(ISCO88 %in% c(1:25) ~ 'Expertos',
                                                ISCO88 %in% c(30, 31, 32, 33, 34, 61, 70, 71, 72, 73, 74, 75, 79) ~ 'Calificados',
                                                ISCO88 %in% c(40, 41, 42, 51, 52, 62, 80, 81, 82, 83, 84, 90, 91, 92, 93) ~ 'No calificados',
                                                ISCO88 == 99 ~ NA_character_))

issp99 %>% count(skills) %>% mutate(prop = prop.table(n))

## Education control effect
issp99 <- issp99 %>% mutate(skillsA = if_else(skills =="Expertos" & EDUC=="Si", "Expertos",
                                         if_else(skills == "Expertos" & EDUC=="No", "Calificados", skills))) %>% 
                                                 mutate(skillsA = if_else(is.na(skillsA), skills, skillsA))
                                                 


issp99 %>% count(skills) %>% mutate(prop = prop.table(n))
issp99 %>% count(skillsA) %>% mutate(prop = prop.table(n)) # Unskilled up

## FINAL VARIABLE CLASS 

issp99$CLASS <- NA
issp99$CLASS <- with(issp99, ifelse(salaried=="1.Capitalistas", 1, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="2.Pequeños empleadores", 2, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="3.Pequeña burguesia", 3, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="Asalariado" & control=="Control" & skillsA=="Expertos", 4, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="Asalariado" & control=="No control" & skillsA=="Expertos", 5, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="Asalariado" & control=="Control" & skillsA=="Calificados", 6, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="Asalariado" & control=="Control" & skillsA=="No calificados", 7, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="Asalariado" & control=="No control" & skillsA=="Calificados", 8, CLASS))
issp99$CLASS <- with(issp99, ifelse(salaried=="Asalariado" & control=="No control" & skillsA=="No calificados", 9, CLASS))

issp99$CLASS <- factor(issp99$CLASS,levels = c(1:9),
                     labels = c("1.Capitalistas","2.Pequeños empleadores","3.Pequeña burguesia",
                                "4.Expertos directivos","5.Expertos sin autoridad",
                                "6.Supervisores calificados","7.Supervisores no calificados",
                                "8.Trabajadores calificados","9.Trabajadores no calificados"))

issp99 %>% filter(!is.na(CLASS)) %>% count(CLASS) %>% mutate(prop = prop.table(n)) 
issp99$CLASS <- sjlabelled::set_label(issp99$CLASS, label = c("Posición de clase"))


# 3.12 CLASS ESCHEME EGP ----
frq(issp99$selfemp)
frq(issp99$nemploy)
frq(issp99$wrkgovt)

issp99 <- issp99 %>% 
  mutate(selfemp_egp = if_else(wrkgovt == 8, 1, 0),
         nemploy_egp = car::recode(nemploy, recodes = c("9995 = 0; c(9997,9999) = NA")))


issp99 <- issp99 %>% 
  mutate(EGP = occupar::isco88toEGP(isco88 = ISCO88_or, 
                                    n.employees = nemploy_egp, 
                                    self.employed = selfemp_egp,
                                    n.classes = 11))


# 3.13 International Socio-Economic Index of occupational status ISEI ----

issp99 <- issp99 %>% 
  mutate(ISCO08_isei = occupar::isco88to08(ISCO88_or),
         ISEI = occupar::isco08toISEI08(ISCO08_isei))


# 3.14 PERCEIVED SOCIAL CONFLICT INDEX ----
## Rich and poor 
frq(issp99$v41)
issp99 <- issp99 %>% mutate(CONFLICT_RP = case_when(v41 == 1 ~ 3,
                                                    v41 == 2 ~ 2,
                                                    v41 == 3 ~ 1,
                                                    v41 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp99$CONFLICT_RP)
sjPlot::plot_frq(na.omit(issp99$CONFLICT_RP), type = "histogram", show.mean = TRUE)
issp99$CONFLICT_RP <- sjlabelled::set_label(issp99$CONFLICT_RP, label = c("Conflictos: ricos - pobres"))

## Working class and middle class
frq(issp99$v42)                                                                                                   
issp99 <- issp99 %>% mutate(CONFLICT_WCMC = case_when(v42 == 1 ~ 3,
                                                      v42 == 2 ~ 2,
                                                      v42 == 3 ~ 1,
                                                      v42 == 4 ~ 0,
                                                      TRUE ~ NA_real_))
frq(issp99$CONFLICT_WCMC)
sjPlot::plot_frq(na.omit(issp99$CONFLICT_WCMC), type = "histogram", show.mean = TRUE)
issp99$CONFLICT_WCMC <- sjlabelled::set_label(issp99$CONFLICT_WCMC, label = c("Conflictos: clase trabajadora - clase media"))

## Management and workers 
frq(issp99$v43)
issp99 <- issp99 %>% mutate(CONFLICT_MW = case_when(v43 == 1 ~ 3,
                                                    v43 == 2 ~ 2,
                                                    v43 == 3 ~ 1,
                                                    v43 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp99$CONFLICT_MW)
sjPlot::plot_frq(na.omit(issp99$CONFLICT_MW), type = "histogram", show.mean = TRUE)
issp99$CONFLICT_MW <- sjlabelled::set_label(issp99$CONFLICT_MW, label = c("Conflictos: directivos - trabajadores"))

## People at the top and people at the bottom 
frq(issp99$v44)
issp99 <- issp99 %>% mutate(CONFLICT_TB = case_when(v44 == 1 ~ 3,
                                                    v44 == 2 ~ 2,
                                                    v44 == 3 ~ 1,
                                                    v44 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp99$CONFLICT_TB)
sjPlot::plot_frq(na.omit(issp99$CONFLICT_TB), type = "histogram", show.mean = TRUE)
issp99$CONFLICT_TB <- sjlabelled::set_label(issp99$CONFLICT_TB, label = c("Conflictos: gente de arriba - gente de abajo"))

## PSCI 
issp99 <- issp99 %>% 
  rowwise() %>% 
  mutate(PSCi = sum(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, CONFLICT_TB, na.rm = F))

issp99$PSCi <- sjlabelled::set_label(issp99$PSCi, label = c("Perceived Social Conflict Index"))
frq(issp99$PSCi)
sjPlot::plot_frq(na.omit(issp99$PSCi), type = "histogram", show.mean = TRUE) # Follows a normal distribution. Careful with the NA

## Cronbach's alpha
matriz <- issp99 %>% select(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, CONFLICT_TB)
psych::alpha(matriz) # coef = 0.79

## Polychoric alpha ordinal (Likert scale)
matriz_poly <- polychoric(matriz) 
psych::alpha(matriz_poly$rho) # coef = 0.84

# View
issp99 %>% 
  filter(!is.na(PSCi)) %>% 
  count(PSCi)

# 4. Save -----------------------------------------------------------------

issp99 <- issp99 %>% select(YEAR,
                            COUNTRY,
                            SEX,
                            AGE,
                            DEGREE,
                            INCOME,
                            SUBJEC_CLASS,
                            UNION,
                            IDEOLOGY,
                            CLASS,
                            EGP,
                            ISEI,
                            41:44,
                            FACTOR)


sapply(issp99, class)
save(issp99, file = "../output/data/issp99.RData")
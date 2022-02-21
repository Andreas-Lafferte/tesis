# Code 8: Corporativism index/measure


# 1. Packages -------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, summarytools, sjPlot, psych, 
               lavaan, sjlabelled, polycor, corrplot, nortest, tseries, coin)

options(scipen=999)

# 2. Data -----------------------------------------------------------------

load(file = "../output/data/ictwss.RData")

names(ictwss)
sapply(ictwss, class)

# 3. Recode -------------------------------------------------------------

levels(ictwss$COORD)
levels(ictwss$TYPE) # reco
levels(ictwss$LEVEL)
levels(ictwss$EXT)# reco

label_ictwss <- ictwss

ictwss$TYPE <- car::recode(ictwss$TYPE, recodes = c("0 = 1; 1 = 2; 2 = 3; 3 = 4; 4 = 5; 5 = 6; 6 = 7"), as.factor = T)

table(ictwss$TYPE, label_ictwss$TYPE)

ictwss$EXT <- car::recode(ictwss$EXT, recodes = c("0 = 1; 1= 2; 2 = 3; 3 = 4"), as.factor = T)

table(ictwss$EXT, label_ictwss$EXT)

ictwss$TYPE <- sjlabelled::set_labels(ictwss$TYPE, labels = c("No specific mechanism identified" = 1, "Government sets signals (public sector wages, minimum wage)" = 2, 
                                                              "Pattern bargaining" = 3,
                                                              "Intra-associational (informal centralisation)" = 4, "Inter-associational by peak associations" = 5,
                                                              "Government sponsored bargaining (this includes pacts)" = 6, "Government imposed bargaining (incl. Statutory controls in lieu of bargaining)" = 7))


ictwss$EXT <- sjlabelled::set_labels(ictwss$EXT, labels = c("There are neither legal provisions for mandatory extension, nor is there a functional equivalent" = 1,
                                                            "Extension is exceptional, used in some industries only, because of absence of sector agreements, very high thresholds" = 2,
                                                            "Extension is used in many industries, but with thresholds and Ministers candecide not to extend agreements" = 3,
                                                            "Extension is virtually automatic and more or less general (including enlargement)" = 4))



# 4. Analysis -------------------------------------------------------------


## 4.1. Correlation matrix ----

# Pearson (include UD)

matriz_c <- ictwss %>% select(COORD, TYPE, LEVEL, EXT, UD) %>% na.omit() %>% as.data.frame()
matriz_c <- as_numeric(matriz_c)

pearson <- cor(matriz_c, use = "everything", method = "pearson")

psych::alpha(matriz_c) # Cronbach's Alpha = 0.85

hetcor(data = matriz_c, ML = T, use = "complete.obs") # polycor package

corrplot.mixed(pearson)

# Polyserial

matriz_ps <- matriz_c %>% mutate_at(vars(1:4), ~ as.factor(.))

polyserial <- hetcor(data = matriz_ps, ML = F, use = "complete.obs", thresholds = T) # polycor packages

psych::alpha(polyserial$correlations) # Ordinal Alpha = 0.87

corrplot.mixed(polyserial$correlations)

# Polychoric 

### Psych
matriz_po <- ictwss %>% select(COORD, TYPE, LEVEL, EXT) %>% na.omit() %>% as.data.frame()

matriz_po$TYPE <- car::recode(matriz_po$TYPE, recodes = c("6 = 5")) # with this re codification, the cor increase psych

polycorica_1 <- psych::polychoric(matriz_po, ML = F)

psych::alpha(polycorica_1$rho) # Ordinal Alpha = 0.78

corrplot.mixed(polycorica_1$rho)

### Polycor
matriz_po <- ictwss %>% select(COORD, TYPE, LEVEL, EXT) %>% na.omit() %>% as.data.frame()

polycorica_2 <- hetcor(data = matriz_po, ML = F, use = "complete.obs", thresholds = T) # why is different?

psych::alpha(polycorica_2$correlations) # Ordinal Alpha = 0.89

corrplot.mixed(polycorica_2$correlations)

## 4.2. Normality test ----

sjPlot::plot_frq(data = matriz_po, type = "histogram", geom.colors = "#545454", show.mean = T, show.mean.val = T,
                 show.sd = T, normal.curve = T)

boxplot(matriz_c$COORD)
boxplot(matriz_c$TYPE)
boxplot(matriz_c$LEVEL)
boxplot(matriz_c$EXT)

## Kolmogorov–Smirnov
ks.test(matriz_c$COORD, "pnorm", alternative = "two.sided")
ks.test(matriz_c$LEVEL, "pnorm", alternative = "two.sided")
ks.test(matriz_c$TYPE, "pnorm", alternative = "two.sided")
ks.test(matriz_c$EXT, "pnorm", alternative = "two.sided")

## Lilliefors
lillie.test(matriz_c$COORD)
lillie.test(matriz_c$LEVEL)
lillie.test(matriz_c$TYPE)
lillie.test(matriz_c$EXT)

## Kurtosis and skew
sjmisc::descr(matriz_po, show = "all")

psych::mardia(matriz_c[,1:4])

## Non parametric test Wilcox Signed Rank

# H0 --> the median difference between pairs of observation is zero
# H1 --> the median difference between pairs of observation is NOT zero
# if p < 0.05 = reject H0 (i.e accept H1)

wilcox.test(matriz_c$COORD, matriz_c$TYPE, paired = T) # reject H0 = accept H1
wilcox.test(matriz_c$COORD, matriz_c$LEVEL, paired = T) # non reject H0 = reject H1
wilcox.test(matriz_c$COORD, matriz_c$EXT, paired = T) # non reject H0 = reject H1
wilcox.test(matriz_c$TYPE, matriz_c$LEVEL, paired = T) # reject H0 = accept H1
wilcox.test(matriz_c$TYPE, matriz_c$EXT, paired = T) # non reject H0 = reject H1
wilcox.test(matriz_c$LEVEL, matriz_c$EXT, paired = T) # non reject H0 = reject H1

## Exact p-value
coin::wilcoxsign_test(matriz_po$COORD ~ matriz_po$TYPE, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$COORD ~ matriz_po$LEVEL, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$COORD ~ matriz_po$EXT, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$TYPE ~ matriz_po$LEVEL, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$TYPE ~ matriz_po$EXT, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$LEVEL ~ matriz_po$EXT, data = matriz_po, distribution = "exact")

## 4.3. Factor analysis ----


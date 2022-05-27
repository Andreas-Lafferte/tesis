# Hybrid MLM

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
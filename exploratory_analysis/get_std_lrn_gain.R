# Calculates the standardized learning gains

library(tidyverse)
library(here)

PREPOST_PATH <- here("data/prepost_cleaned.csv")
DATA_OUTPATH <- here("data/processed/prepost_cleaned_w_std_lrn_gain.csv")

prepost <- read_csv(PREPOST_PATH)

prepost %>%
  mutate(s_ckpk = (CKPK_post - CKPK_pre) / (13 - CKPK_pre),
         s_ck = (CK_post - CK_pre) / (6 - CK_pre),
         s_pk = (PK_post - PK_pre) / (7 - PK_pre)) %>%
  lapply(function(x) replace(x, (is.infinite(x) | is.nan(x)), NA)) -> df

# some NaN and -Inf were produced because students got perfect pretest scores (div by 0)

write.csv(df, DATA_OUTPATH, 
          na = "", row.names = F)
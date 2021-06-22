# gets all the variables needed to replicate the analysis in Tomo's CogSci paper
# not done

library(tidyverse)
library(here)

PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PATH <- here("data/all_by_student_problem.txt")

prepost <- read_csv(PREPOST_PATH)
logdata <- read_tsv(LOG_DATA_PATH)

logdata %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  group_by(`Anon Student Id`) %>%
  summarise(num_solved = n(),
            hint = sum(Hints)/sum(Steps),
            incs = sum(Incorrects)/sum(Steps)) %>%
  rename(username = `Anon Student Id`) -> df_logdata

prepost %>%
  select(c(username, condition, CK_post, PK_post, CKPK_pre)) -> df_prepost

df <- inner_join(df_logdata, df_prepost, by = "username")

write_csv(df, here("data/replicate.csv"))

##############
## NOTES
##############


# ask about time
# no "bigotter" "smallrabbit" (present in prepost) in the logdata
# but there are "smallotter" and "bigrabbit" in the logdata (but not in prepost)


# post_pk
# post_ck
# cond
# pre
# num_solved
# incs *
# hint *
# time ***
# incs_dia ***
# time_dia ***


# mod1 <- lm(post_pk ~ cond + pre + cond:pre + num_solved)
# mod1 <- lm(post_ck ~ cond + pre + cond:pre + num_solved)
# 
# mod_num_solved <- lm(num_solved ~ cond + pre + cond:pre)
# mod_inc <- lm(inc ~ cond + pre + cond:pre + num_solved)
# mod_hint <- lm(hint ~ cond + pre + cond:pre + num_solved)
# mod_time <- lm(time ~ cond + pre + cond:pre + num_solved)
# 
# mod_inc <- lm(inc ~ pre + inc_dia + time_dia + num_solved)
# mod_hint <- lm(hint ~ pre + inc_dia + time_dia + num_solved)
# mod_time <- lm(time ~ pre + inc_dia + time_dia + num_solved)

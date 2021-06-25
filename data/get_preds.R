# gets all the variables needed to replicate the analysis in Tomo's CogSci paper
# not done yet

library(tidyverse)
library(here)

# Data I/O

PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PROB_PATH <- here("data/all_by_student_problem.txt")
LOG_DATA_STEP_PATH <- here("data/all_by_student_step.txt")
LOG_DATA_TRAN_PATH <- here("data/all_by_transaction.txt")
DATA_OUTPATH <- here("data/replicate.csv")

prepost <- read_csv(PREPOST_PATH)
logdata_p <- read_tsv(LOG_DATA_PROB_PATH)
logdata_s <- read_tsv(LOG_DATA_STEP_PATH)
logdata_t <- read_tsv(LOG_DATA_TRAN_PATH)



# Gets number of problems solved, average number of hints per step, and average
# number of incorrects per step
logdata_p %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  group_by(`Anon Student Id`) %>%
  summarise(num_solved = n(),
            hint = sum(Hints)/sum(Steps),
            incs = sum(Incorrects)/sum(Steps)) %>%
  rename(username = `Anon Student Id`) -> df_logdata_p





logdata_s %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  mutate(used_hint = ifelse(Hints >= 1, 1, 0)) %>%
  group_by(`Anon Student Id`, `Problem Name`) -> asdf








# Gets the condition, pretest score (CK + PK), and posttest scores (CK and PK seperately)
prepost %>%
  select(c(username, condition, CK_post, PK_post, CKPK_pre)) -> df_prepost

df <- inner_join(df_logdata_p, df_prepost, by = "username")

write_csv(df, DATA_OUTPATH)

##############
## NOTES
##############


# for the time variable
# for each problem, take the time the student did the last step and subtract 
# the time the student did the first step


# no "bigotter" "smallrabbit" (present in prepost) in the logdata_p
# but there are "smallotter" and "bigrabbit" in the logdata_p (but not in prepost)


# post_pk
# post_ck
# cond
# pre
# num_solved
# incs
# hint
# time ***
# incs_dia ***
# time_dia ***

# percentage of steps with hints ***


########################################################################
## THE LINEAR MODELS USED IN TOMO'S PAPER EXCLUDING GRADE AS A PREDICTOR
########################################################################


# can do now

# mod1 <- lm(post_pk ~ cond + pre + cond:pre + num_solved)
# mod2 <- lm(post_ck ~ cond + pre + cond:pre + num_solved)
# 
# mod_num_solved <- lm(num_solved ~ cond + pre + cond:pre)
# mod_inc <- lm(inc ~ cond + pre + cond:pre + num_solved)
# mod_hint <- lm(hint ~ cond + pre + cond:pre + num_solved)



# can't do yet (missing time, incs_dia, and time_dia)

# mod_time <- lm(time ~ cond + pre + cond:pre + num_solved)
# 
# mod_inc <- lm(inc ~ pre + incs_dia + time_dia + num_solved)
# mod_hint <- lm(hint ~ pre + incs_dia + time_dia + num_solved)
# mod_time <- lm(time ~ pre + incs_dia + time_dia + num_solved)

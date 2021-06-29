# Gets all the variables needed to replicate the analysis in Tomo's CogSci paper
# not done yet

library(tidyverse)
library(here)


# Data I/O
PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PROB_PATH <- here("data/all_by_student_problem.txt")
LOG_DATA_STEP_PATH <- here("data/all_by_student_step.txt")
LOG_DATA_TRAN_PATH <- here("data/all_by_transaction.txt")
DATA_OUTPATH <- here("data/processed/replicate.csv")

prepost <- read_csv(PREPOST_PATH)
logdata_p <- read_tsv(LOG_DATA_PROB_PATH)
logdata_s <- read_tsv(LOG_DATA_STEP_PATH)
logdata_t <- read_tsv(LOG_DATA_TRAN_PATH)


# Get number of problems solved and total steps for each student
logdata_p %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  group_by(`Anon Student Id`) %>%
  summarise(num_solved = n(),
            tot_steps = sum(Steps)) %>%
  rename(username = `Anon Student Id`) -> df1


# Gets steps with hints (divide by total steps gives percentage of steps w/ hints)
logdata_s %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  mutate(used_hint = ifelse(Hints >= 1, 1, 0)) %>%
  group_by(`Anon Student Id`) %>%
  summarise(steps_with_hints = sum(used_hint)) %>%
  rename(username = `Anon Student Id`) -> df2


# Gets the number of incorrects (diagrammatic and symbolic seperately) and
# number of hints per step
logdata_s %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  mutate(used_hint = ifelse(Hints >= 1, 1, 0)) %>%
  group_by(`Anon Student Id`) %>%
  summarise(inc_dia = sum(Incorrects[grepl("selectd", `KC (Default)`)]),
            inc_sym = sum(Incorrects[!grepl("selectd", `KC (Default)`)]),
            hints = sum(Hints)) %>%
  rename(username = `Anon Student Id`) -> df3 # no one used hints on diagrammatic steps


# Gets the time spent on each problem

duration <- function(vec) 
{
  vec <- sort(vec)
  d <- 0
  len <- length(vec)
  for (i in 1:(len - 1))
  {
    dif <- vec[i + 1] - vec[i]
    d <- d + ifelse(as.double(dif) > 40, 40, dif)
  }
  return(d)
}

logdata_t %>%
  filter(`Anon Student Id` %in% prepost$username,
         !is.na(`CF (tool_event_time)`)) %>%
  group_by(`Anon Student Id`, `Problem Name`) %>%
  mutate(time_stamp = as.POSIXlt(`CF (tool_event_time)`, 
                                 format = "%Y-%m-%d %H:%M:%OS", 
                                 tz = "UTC")) %>%
  summarise(tot_time = max(time_stamp) - min(time_stamp),
            adj_tot_time = duration(time_stamp)) %>%
  rename(username = `Anon Student Id`) -> df4


# Gets the condition, pretest score (CK + PK), and posttest scores (CK and PK seperately)
prepost %>%
  select(c(username, condition, CK_post, PK_post, CKPK_pre)) -> df5


# Getting everything together
df_a <- inner_join(df1, df2, by = "username")
df_b <- inner_join(df3, df5, by = "username")
df <- inner_join(df_a, df_b, by = "username") %>%
  mutate(perc_hint_steps = steps_with_hints / tot_steps) %>%
  select(-c(tot_steps, steps_with_hints))

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
# time
# incs_dia
# time_dia *** can't get this because correct first tries don't have timestamp
# percentage of steps with hints


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
# mod_time <- lm(time ~ cond + pre + cond:pre + num_solved)


# can't do because can't get time_dia

# mod_inc <- lm(inc ~ pre + incs_dia + time_dia + num_solved)
# mod_hint <- lm(hint ~ pre + incs_dia + time_dia + num_solved)
# mod_time <- lm(time ~ pre + incs_dia + time_dia + num_solved)

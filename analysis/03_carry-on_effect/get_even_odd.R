# Data cleaning to do the even/odd analysis

library(tidyverse)
library(here)

# Data I/O
PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PROB_PATH <- here("data/all_by_student_problem.txt")
LOG_DATA_STEP_PATH <- here("data/all_by_student_step.txt")
PROB_DATA_OUTPATH <- here("data/processed/even_odd_by_student_problem.csv")
COMP_DATA_OUTPATH <- here("data/processed/even_odd_metrics.csv")

prepost <- read_csv(PREPOST_PATH)
logdata_p <- read_tsv(LOG_DATA_PROB_PATH)
logdata_s <- read_tsv(LOG_DATA_STEP_PATH)


# Groups by parity and condition and gets the predictors used for analyses
foo <- function(num, cond)
{
  if (num %% 2 == 0 && cond == "interleaved")
    return("eve_int")
  else if (num %% 2 == 1 && cond == "interleaved")
    return("odd_int")
  else if (num %% 2 == 0 && cond == "alldiagram")
    return("eve_dia")
  else
    return("odd_dia")
}


# Gets the list of problem prefixes in order
# `bigbird` and `slowcat` did the most problems in the red and yellow problem sets
logdata_p %>%
  filter(`Anon Student Id` == "bigbird",
         `Problem View` == 1) %>%
  mutate(prob_pre = sub("\\-.*", "", `Problem Name`),
         prob_num = row_number()) %>%
  select(prob_pre, prob_num, `Problem Hierarchy`) -> problems_dia

logdata_p %>%
  filter(`Anon Student Id` == "slowcat") %>%
  mutate(prob_pre = sub("\\-.*", "", `Problem Name`),
         prob_num = row_number()) %>%
  select(prob_pre, prob_num, `Problem Hierarchy`) -> problems_int


# Adds columns prob_pre, prob_num, condition, and group to student-problems logdata
logdata_p %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  rename(username = `Anon Student Id`) %>%
  mutate(prob_pre = sub("\\-.*", "", `Problem Name`)) %>%
  inner_join(select(prepost, c("username", "condition")), by = "username") %>%
  inner_join(problems_dia, by = c("prob_pre", "Problem Hierarchy")) %>%
  mutate(group = unlist(map2(prob_num, condition, foo))) -> df_p1

logdata_p %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  rename(username = `Anon Student Id`) %>%
  mutate(prob_pre = sub("\\-.*", "", `Problem Name`)) %>%
  inner_join(select(prepost, c("username", "condition")), by = "username") %>%
  inner_join(problems_int, by = c("prob_pre", "Problem Hierarchy")) %>%
  mutate(group = unlist(map2(prob_num, condition, foo))) -> df_p2

df_p <- union(df_p1, df_p2) %>%
  arrange(Row)


# Gets the number of problems solved and total number of steps for each student
df_p %>%
  group_by(username, group) %>%
  summarise(tot_steps = sum(Steps)) -> total_steps

df_p %>%
  filter(`Problem View` == 1) %>%
  group_by(username, group) %>%
  summarise(num_solved = n()) -> problems_solved

df1 <- inner_join(total_steps, problems_solved, by = c("username", "group"))


# Adds group (even/odd and interleaved/alldiagrams) information to student-steps logdata
logdata_s %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  rename(username = `Anon Student Id`) -> a
b <- select(df_p, c("username", "Problem Name", "Problem Hierarchy", "group"))
df_s <- inner_join(a, b, by = c("username", "Problem Name", "Problem Hierarchy")) %>% unique()


# Compute various performance metrics
df_s %>%
  mutate(used_hint = ifelse(Hints >= 1, 1, 0)) %>%
  group_by(username, group) %>%
  summarise(steps_with_hints = sum(used_hint),
            inc_dia = sum(Incorrects[grepl("selectd", `KC (Default)`)]),
            inc_sym = sum(Incorrects[!grepl("selectd", `KC (Default)`)]),
            hints = sum(Hints),
            first_try = sum(`First Attempt` == "correct")) %>%
  inner_join(df1, by = c("username", "group")) %>%
  mutate(prop_hint_steps = steps_with_hints / tot_steps,
         prop_first_try = first_try / tot_steps#,
         # avg_time_per_step = tot_time / tot_steps,
         # adj_avg_tps = adj_tot_time / tot_steps
  ) %>%
  select(-c(tot_steps, steps_with_hints, first_try)) -> df


write_csv(df_p, PROB_DATA_OUTPATH)
write_csv(df, COMP_DATA_OUTPATH)
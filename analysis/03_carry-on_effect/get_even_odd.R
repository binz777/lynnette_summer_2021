# Data cleaning to do the even/odd analysis

library(tidyverse)
library(here)

# Data I/O
PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PROB_PATH <- here("data/all_by_student_problem.txt")
LOG_DATA_STEP_PATH <- here("data/all_by_student_step.txt")
LOG_DATA_TRAN_PATH <- here("data/logdata_WVW_withIDs.txt")
PROB_DATA_OUTPATH <- here("data/processed/even_odd_by_student_problem.csv")
COMP_DATA_OUTPATH <- here("data/processed/even_odd_metrics.csv")
KC_DATA_OUTPATH <- here("data/processed/even_odd_kc.csv")

prepost <- read_csv(PREPOST_PATH)
logdata_p <- read_tsv(LOG_DATA_PROB_PATH)
logdata_s <- read_tsv(LOG_DATA_STEP_PATH)
logdata_t <- read_tsv(LOG_DATA_TRAN_PATH)

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


# Adds group (even/odd and interleaved/alldiagrams) information to student-steps and transaction logdata
logdata_s %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  rename(username = `Anon Student Id`) -> a
b <- select(df_p, c("username", "Problem Name", "Problem Hierarchy", "group"))
df_s <- inner_join(a, b, by = c("username", "Problem Name", "Problem Hierarchy")) %>% unique()

logdata_t %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  rename(username = `Anon Student Id`) %>%
  mutate(`Problem Hierarchy` = paste0("Assignment ", `Level (Assignment)`, ", ProblemSet ", `Level (ProblemSet)`)) %>%
  select(username, `Duration (sec)`, `Problem Name`, `Problem Hierarchy`, `KC (Default)`) -> a
df_t <- inner_join(a, b, by = c("username", "Problem Name", "Problem Hierarchy")) %>% 
  unique() %>%
  filter(!is.na(`Duration (sec)`))

df_t %>%
  group_by(username, group) %>%
  summarise(tot_time = sum(`Duration (sec)`),
            tot_time_sym = sum(`Duration (sec)`[!grepl("selectd", `KC (Default)`)])) -> c

# Compute various performance metrics
df_s %>%
  mutate(used_hint = ifelse(Hints >= 1, 1, 0)) %>%
  group_by(username, group) %>%
  summarise(steps_with_hints = sum(used_hint),
            inc_dia = sum(Incorrects[grepl("selectd", `KC (Default)`)]),
            inc_sym = sum(Incorrects[!grepl("selectd", `KC (Default)`)]),
            hints = sum(Hints),
            first_try = sum(`First Attempt` == "correct"),
            first_try_sym = sum(`First Attempt`[!grepl("selectd", `KC (Default)`)] == "correct"),
            total_sym_step = sum(!grepl("selectd", `KC (Default)`))) %>%
  inner_join(df1, by = c("username", "group")) %>%
  inner_join(c, by = c("username", "group")) %>%
  mutate(prop_hint_steps = steps_with_hints / tot_steps,
         prop_first_try = first_try / tot_steps,
         prop_first_try_sym = first_try_sym / total_sym_step,
         avg_inc_sym = inc_sym / total_sym_step,
         avg_hints_sym = hints /  total_sym_step,
         avg_tps = tot_time / tot_steps,
         avg_tps_sym = tot_time_sym / total_sym_step) %>%
  select(-c(tot_steps, steps_with_hints, first_try, total_sym_step, first_try_sym)) -> df


# Specific KCs
df_s %>%
  group_by(username, group) %>%
  summarise(div_comp = sum(`First Attempt`[grepl("division-complex", `KC (Default)`)] == "correct"),
            div_comp_steps = sum(grepl("division-complex", `KC (Default)`)),
            div_simp = sum(`First Attempt`[grepl("division-simple", `KC (Default)`)] == "correct"),
            div_simp_steps = sum(grepl("division-simple", `KC (Default)`)),
            sub_con = sum(`First Attempt`[grepl("subtraction-const", `KC (Default)`)] == "correct"),
            sub_con_steps = sum(grepl("subtraction-const", `KC (Default)`)),
            sub_var = sum(`First Attempt`[grepl("subtraction-var", `KC (Default)`)] == "correct"),
            sub_var_steps = sum(grepl("subtraction-var", `KC (Default)`))) %>%
  inner_join(df1, by = c("username", "group")) %>%
  mutate(division_complex = div_comp / div_comp_steps,
         division_simple = div_simp / div_simp_steps,
         subtraction_const = sub_con / sub_con_steps,
         subtraction_var = sub_var / sub_var_steps) -> df_kc


write_csv(df_p, PROB_DATA_OUTPATH)
write_csv(df, COMP_DATA_OUTPATH)
write_csv(df_kc, KC_DATA_OUTPATH)

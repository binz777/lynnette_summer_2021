library(tidyverse)
library(here)

# Data I/O
PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PROB_PATH <- here("data/all_by_student_problem.txt")
LOG_DATA_STEP_PATH <- here("data/all_by_student_step.txt")
LOG_DATA_TRAN_PATH <- here("data/all_by_transaction.txt")

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

logdata_p %>%
  filter(`Anon Student Id` %in% prepost$username,
         `Problem View` == 1) %>%
  rename(username = `Anon Student Id`) %>%
  group_by(username) %>%
  mutate(prob_num = row_number()) %>%
  inner_join(select(prepost, c("username", "condition")), by = "username") %>%
  mutate(group = unlist(map2(prob_num, condition, foo))) -> df_p

df_p %>%
  group_by(username, group) %>%
  summarise(num_solved = n(),
            tot_steps = sum(Steps)) -> df1

logdata_s %>%
  filter(`Anon Student Id` %in% prepost$username,
         `Problem View` == 1) %>%
  rename(username = `Anon Student Id`) -> a
b <- select(df_p, c("username", "Problem Name", "group"))

df_s <- inner_join(a, b, by = c("username", "Problem Name"))

df_s %>%
  mutate(used_hint = ifelse(Hints >= 1, 1, 0)) %>%
  group_by(group, username) %>%
  summarise(steps_with_hints = sum(used_hint),
            inc_dia = sum(Incorrects[grepl("selectd", `KC (Default)`)]),
            inc_sym = sum(Incorrects[!grepl("selectd", `KC (Default)`)]),
            hints = sum(Hints),
            first_try = sum(`First Attempt` == "correct")) %>%
  inner_join(df1, by = c("username", "group")) %>%
  mutate(perc_hint_steps = steps_with_hints / tot_steps,
         perc_first_try = first_try / tot_steps#,
         # avg_time_per_step = tot_time / tot_steps,
         # adj_avg_tps = adj_tot_time / tot_steps
         ) %>%
  select(-c(tot_steps, steps_with_hints, first_try)) -> df
  

# Hypothesis: Most hints comes from the no diagram problems in the interleaved
df %>%
  group_by(group) %>%
  summarize(avg_num_solved = mean(num_solved),
            avg_num_hints = mean(hints),
            avg_inc_sym = mean(inc_sym))
# Most hints comes from yes diagram problems in the interleaved condition (odd_int)

# Problems with diagrams in the interleaved condition also has the greatest
# number of incorrect symbolic steps on average

# Even problems in the Diagram condition has the lowest average number of 
# incorrect symbolic steps

# Average number of even interleaved problems solved > odd interleaved??
# Probably due to filtering?


eve_dia <- filter(df, group == "eve_dia")
eve_int <- filter(df, group == "eve_int")
odd_dia <- filter(df, group == "odd_dia")
odd_int <- filter(df, group == "odd_int")


# Testing some hypotheses regarding performance (in terms of percent of steps 
# that was correct on the first try*) based on condition and parity


# Hypothesis: Students in both conditions performs the same on even problems
t.test(eve_dia$perc_first_try, eve_int$perc_first_try)
# Students in the Diagram condition did significantly better on even problems

# Hypothesis: Students in the Diagram condition does better on the even problems 
# than the odd problems (because they should learn from the odd problems)
t.test(eve_dia$perc_first_try, odd_dia$perc_first_try, alternative = "g")
# Students performed significantly better on the even problems than the odd problems
# in the Diagram condition

# Hypothesis: Students in the Interleaved condition does better on the even problems
# than the odd problems (because they also should learn from the odd problems)
t.test(eve_int$perc_first_try, odd_int$perc_first_try, alternative = "g")
# There is no significant difference in performance at the 0.05 alpha level
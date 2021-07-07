# testing differences between odd and even problems in hints
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(rstatix)
library(here)

PREPOST_PATH <- here("data/prepost_cleaned.csv")
STUDENT_STEP_PATH <- here("data/all_by_student_step.txt")
LOG_DATA_PATH <- here("data/all_by_student_problem.txt")

prepost <- read_csv(PREPOST_PATH)
logdata <- read_tsv(LOG_DATA_PATH)
student_step <- read_tsv(STUDENT_STEP_PATH)

# student step: filter out rows with null step durations
nrow(student_step) # 18608
student_step %>% 
  filter(`Step Duration (sec)` != .) %>%
  filter(!is.na(`Step Duration (sec)`)) -> student_step
nrow(student_step) # 18594 - removed 14 rows with NA or .

# i want to test whether the two groups perform the same on the
# even problems and the odd problems
# i'm going to ignore the problem view for now and i'll copy
# that in when bin finishes

# temporarily filter out all problem views > 1, deletes 93 rows
logdata %>%
  mutate(problem_prefix = sub("\\-.*", "", `Problem Name`)) %>%
  rename(username = `Anon Student Id`) %>%
  rename(condition = `Problem Hierarchy`) %>%
  filter(!`Problem View`>1 )-> df_logdata

interleaved_df <- filter(df_logdata, grepl("Yellow",condition))
alldiagram_df <- filter(df_logdata, grepl("Red",condition))

# Choosing even rows aka same problem but with diagram and without
interleaved_df %>% filter(row_number() %% 2 == 0) -> interleaved_df_evens ## Select even rows
alldiagram_df %>% filter(row_number() %% 2 == 0) -> alldiagram_df_evens ## Select even rows
interleaved_df %>% filter(row_number() %% 2 == 1) -> interleaved_df_odds ## Select even rows
alldiagram_df %>% filter(row_number() %% 2 == 1) -> alldiagram_df_odds ## Select even rows


# Now we can see both have the same problems
# 606 questions in interleaved, 721 in all diagram
interleaved_df_evens$`Problem Name`[1:10]
alldiagram_df_evens$`Problem Name`[1:10]

# create joint dfs of all odd/all even problems
all_odds_df = rbind(interleaved_df_odds, alldiagram_df_odds)
all_odds_df$condition <- ifelse(grepl("Yellow", all_odds_df$condition), "interleaved", "alldiagram" )
all_evens_df = rbind(interleaved_df_evens, alldiagram_df_evens)
all_evens_df$condition <- ifelse(grepl("Yellow", all_evens_df$condition), "interleaved", "alldiagram" )

# adding parity column to each condition
alldiagram_df$parity <- rep(c("odd", "even"), length.out=nrow(alldiagram_df))
interleaved_df$parity <- rep(c("odd", "even"), length.out=nrow(interleaved_df))
interleaved_df %>% select("parity", `Problem Name`)

# hypothesis: students in both conditions use equal amount of hints 
# on odd problems

# summary stats
all_odds_df %>%
  group_by(condition) %>%
  get_summary_stats(Hints, type="mean_sd")

# t test
stat.test <- all_odds_df %>% 
  t_test(Hints ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test
# significant at p = 7.7e-9
# the interleaved group use significantly more hints on diagram problems
# than the all diagram group

# hypothesis: students in both conditions use equal amount of hints 
# on even problems

# t test
stat.test <- all_evens_df %>% 
  t_test(Hints ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test
# the interleaved group use significantly more hints on even problems
# than the all diagram group

# hypothesis: students in the diagram condition use less hints on the even problems 
# than the odd problems (because they learn from the odd problems)
# t test
stat.test <- alldiagram_df %>% 
  t_test(Hints ~ parity, var.equal = TRUE) %>%
  add_significance()
stat.test
# no significant difference between odd an even problem hint usage
# in the all diagram condition

# hypothesis: students in the interleaved condition use less hints on the even problems 
# than the odd problems (because they learn from the odd problems)
# t test

interleaved_df %>%
  group_by(parity) %>%
  get_summary_stats(Hints, type="mean_sd")

stat.test <- interleaved_df %>% 
  t_test(Hints ~ parity, var.equal = TRUE) %>%
  add_significance()
stat.test
# no significant difference between odd and even problem hint usage
# in the interleaved condition

# testing differences between odd and even problems in hints/step/student
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(rstatix)
library(here)
library(gdata) # Used for combine function

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

# Filter students who did both pre and post test
# Filter students who viewed problem multiple times (problem view > 1)
# total from 18594 to 14784
student_step %>%
  mutate(problem_prefix = sub("\\-.*", "", `Problem Name`)) %>%
  rename(username = `Anon Student Id`) %>%
  rename(condition = `Problem Hierarchy`) %>%
  filter(username %in% prepost$username) %>%
  filter(!`Problem View`>1 ) -> student_step

# Split into interleaved and alldiagram
# 8326 all diagram steps vs 6458 interleaved steps
interleaved_step_df <- filter(student_step, grepl("Yellow",condition))
alldiagram_step_df <- filter(student_step, grepl("Red",condition))

# Splitting into odd and even problems
interleaved_step_df %>% filter(grepl('diagrams-first', `Problem Name`)) -> interleaved_odd_steps
interleaved_step_df %>% 
  filter(grepl('no-diagrams', `Problem Name`)) ->
  interleaved_even_steps
alldiagram_step_df %>% 
  filter(alldiagram_step_df$problem_prefix %in% interleaved_step__odds$problem_prefix) %>%
  filter(!(grepl('Bonus Level', condition) & problem_prefix == "4xplus1eqxplus10")) %>%
  filter(!(grepl('Bonus Level', condition) & problem_prefix == "8xplus3eq5plus6x")) %>%
  filter(!(grepl('Level 8', condition) & problem_prefix == "6xplus1eq13plus2x")) ->
  alldiagram_odd_steps
alldiagram_step_df %>%
  filter(alldiagram_step_df$problem_prefix %in% interleaved_step_evens$problem_prefix) %>%
  filter(!(grepl('Level 7', condition) & problem_prefix == "4xplus1eqxplus10")) %>%
  filter(!(grepl('Level 8', condition) & problem_prefix == "8xplus3eq5plus6x")) %>%
  filter(!(grepl('Bonus Level', condition) & problem_prefix == "8xplus3eq5plus6x")) ->
  alldiagram_even_Steps


# Compare the performance of the interleaved condition on even 
# problems against that of the diagram condition on the 
# symbolic steps only of even problems

# get only symbolic steps from even problems
alldiagram_even_steps %>%
  filter(!grepl("Diagrams", `Step Name`)) -> alldiagram_symbolic_even_steps

# Calculate average hint use/step/student 
# For interleaved_step_evens (no diagram steps)
aggregate(interleaved_even_steps$Hints, by=list(Username=interleaved_even_steps$username), FUN=sum) -> sum_hints_ilev
# Problem view used as a proxy for steps because its always 1
aggregate(interleaved_even_steps$`Problem View`, by=list(Username=interleaved_even_steps$username), FUN=sum) -> sum_steps_ilev
counts_ilev <- sum_hints_ilev$x/sum_steps_ilev$x
counts_ilev # (same value as in hints_averaged.R)

# for alldiagram_symbolic_even_steps (no diagram steps)
aggregate(alldiagram_symbolic_even_steps$Hints, by=list(Username=alldiagram_symbolic_even_steps$username), FUN=sum) -> sum_hints_adsev
# Problem view used as a proxy for steps because its always 1
aggregate(alldiagram_symbolic_even_steps$`Problem View`, by=list(Username=alldiagram_symbolic_even_steps$username), FUN=sum) -> sum_steps_adsev
counts_adsev <- sum_hints_adsev$x/sum_steps_adsev$x
counts_adsev # (same value as in hints_averaged.R)

barplot(counts_adsev, main="All Diagram Symbolic Even Problem Hints",
        xlab="Student")

# Combine dfs for t test
merge(sum_hints_ilev, sum_steps_ilev, by="Username") %>%
  rename("Hints" = x.x) %>%
  rename("Steps" = x.y) %>%
  mutate(avg = `Hints`/`Steps`) -> even_interleaved
merge(sum_hints_adsev, sum_steps_adsev, by="Username") %>%
  rename("Hints" = x.x) %>%
  rename("Steps" = x.y) %>% 
  mutate(avg = `Hints`/`Steps`) -> even_symbolic_alldiagram
combine(even_interleaved, even_symbolic_alldiagram) %>%
  rename(condition = "source") -> t_test_df

# summary stats
t_test_df %>%
  group_by(condition) %>%
  get_summary_stats(avg, type="mean_sd")

stat.test <- t_test_df %>% 
  t_test(avg ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test

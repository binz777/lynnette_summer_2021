library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(rstatix)

PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PATH <- here("data/all_by_student_problem.txt")

prepost <- read_csv(PREPOST_PATH)
logdata <- read_tsv(LOG_DATA_PATH)

# lizzy's file accessing code
prepost = read_csv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/prepost_cleaned.csv")
logdata = read_tsv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/all_by_student_problem.txt")
student_step = read_tsv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/all_by_student_step_csv.txt")

logdata %>%
  mutate(problem_prefix = sub("\\-.*", "", `Problem Name`)) %>%
  mutate(proportion_correct_first = `Correct First Attempts`/Steps) %>% 
  rename(username = `Anon Student Id`) %>%
  rename(condition = `Problem Hierarchy`)%>%
  rename(avg_assistance = `Avg Assistance Score`)-> df_logdata

interleaved_df <- filter(df_logdata, grepl("Yellow",condition))
alldiagram_df <- filter(df_logdata, grepl("Red",condition))

# Choosing even rows aka same problem but with diagram and without
interleaved_df %>% filter(row_number() %% 2 == 0) -> interleaved_df_evens ## Select even rows
alldiagram_df %>% filter(row_number() %% 2 == 0) -> alldiagram_df_evens ## Select even rows

# Now we can see both have the same problems
# 606 questions in interleaved, 721 in all diagram
interleaved_df_evens$`Problem Name`[1:10]
alldiagram_df_evens$`Problem Name`[1:10]

# Compare Avg Assistance Score across conditions
barplot <- ggplot(data=interleaved_df_evens[1:18,], aes(x=`Problem Name`, y=avg_assistance)) +
  geom_bar(stat="identity") + ylim(0, 2)
barplot
barplot2 <- ggplot(data=alldiagram_df_evens[1:18,], aes(x=`Problem Name`, y=avg_assistance)) +
  geom_bar(stat="identity") + ylim(0, 2)
barplot2

# combine these 2 in order to run anova test
combined_df = rbind(interleaved_df_evens, alldiagram_df_evens)
combined_df$condition <- ifelse(grepl("Yellow", combined_df$condition), "interleaved", "alldiagram" )


# summary stats
combined_df %>%
  group_by(condition) %>%
  get_summary_stats(avg_assistance, type="mean_sd")


# t test to check for difference
stat.test <- combined_df %>% 
  t_test(avg_assistance ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test
# the interleaved group used more hints w p = 0.000025, p < 0.0001

# Compare proportion of steps correct on the first attempt
# = (# steps correct on the first attempt)/(# steps)
# Compare Proportion Correct on First Attempt across conditions
barplot3 <- ggplot(data=interleaved_df_evens[20:40,], aes(x=`Problem Name`, y=proportion_correct_first)) +
  geom_bar(stat="identity") + ylim(0, 1)
barplot3
barplot4 <- ggplot(data=alldiagram_df_evens[20:40,], aes(x=`Problem Name`, y=proportion_correct_first)) +
  geom_bar(stat="identity") + ylim(0, 1)
barplot4

# based on the box plot, it seems like the majority of all diagram values are really close to 1
boxplot <- ggplot(data=combined_df, aes(x=condition, y=proportion_correct_first)) +
  geom_boxplot()
boxplot

# summary stats
combined_df %>%
  group_by(condition) %>%
  get_summary_stats(proportion_correct_first, type="mean_sd")

# t test to check for difference
stat.test <- combined_df %>% 
  t_test(proportion_correct_first ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test
# the all diagram group got more correct on the first attempt w 
# p = 5.87e-9, p < 0.0001

# Compare # hints used across conditions 
barplot5 <- ggplot(data=interleaved_df_evens, aes(x=`Problem Name`, y=Hints)) +
  geom_bar(stat="identity") + ylim(0, 200)
barplot5
barplot6 <- ggplot(data=alldiagram_df_evens, aes(x=`Problem Name`, y=Hints)) +
  geom_bar(stat="identity") + ylim(0, 200)
barplot6

#boxplot2 <- ggplot(data=combined_df, aes(x=condition, y=Hints)) +
#  geom_boxplot()
#boxplot2

# summary stats
combined_df %>%
  group_by(condition) %>%
  get_summary_stats(Hints, type="mean_sd")

# t test to check for difference
stat.test <- combined_df %>% 
  t_test(Hints ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test
# the interleaved group used more hints with p = 1.8e-6, p < 0.0001


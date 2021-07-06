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
#  mutate(problem_prefix = sub("\\-.*", "", `Problem Name`)) %>%
  rename(username = `Anon Student Id`) %>%
  rename(condition = `Problem Hierarchy`)%>%
  rename(avg_assistance = `Avg Assistance Score`)-> df_logdata


interleaved_df <- filter(df_logdata, grepl("Yellow",condition))
alldiagram_df <- filter(df_logdata, grepl("Red",condition))

interleaved_df %>% filter(`Problem Name` %in% alldiagram_df$`Problem Name`) -> interleaved_df
# 631 student problems in interleaved
alldiagram_df %>% filter(`Problem Name` %in% interleaved_df$`Problem Name`) -> alldiagram_df
# 787 student problems in alldiagram

barplot <- ggplot(data=interleaved_df[1:10,], aes(x=`Problem Name`, y=avg_assistance)) +
  geom_bar(stat="identity") + ylim(0, 10)
barplot
barplot2 <- ggplot(data=alldiagram_df[1:10,], aes(x=`Problem Name`, y=avg_assistance)) +
  geom_bar(stat="identity") + ylim(0, 10)
barplot2

# combine these 2 in order to run anova test
combined_df = rbind(interleaved_df, alldiagram_df)
combined_df$condition <- ifelse(grepl("Yellow", combined_df$condition), "interleaved", "alldiagram" )


# summary stats
interleaved_df %>% 
  get_summary_stats(avg_assistance, type="mean_sd")
alldiagram_df %>% 
  get_summary_stats(avg_assistance, type="mean_sd")
combined_df %>%
  group_by(condition) %>%
  get_summary_stats(avg_assistance, type="mean_sd")


# t test to check for difference
stat.test <- combined_df %>% 
  t_test(avg_assistance ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test
# the interleaved group used more hints w p = 0.0118, p < 0.05
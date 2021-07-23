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

# temporarily filter out all problem views > 1, deletes 93 rows
# only around 2% of rows are problem view > 1
# also filter out students who didn't complete pre and post (2655->2047 rows)
logdata %>%
  mutate(problem_prefix = sub("\\-.*", "", `Problem Name`)) %>%
  mutate(proportion_correct_first_try = `Correct First Attempts`/`Steps`) %>%
  rename(username = `Anon Student Id`) %>%
  filter(username %in% prepost$username) %>%
  rename(condition = `Problem Hierarchy`) %>%
  filter(!`Problem View`>1 )-> df_logdata

interleaved_df <- filter(df_logdata, grepl("Yellow",condition))
alldiagram_df <- filter(df_logdata, grepl("Red",condition))

# SPLITTING EVEN/ODD ROWS
# for each of these problems, the bonus level problem is the same but either
# with/without diagrams, so I added additional filtering
# for these specific cases
# now totals are consistent! evens = 702, odds = 700
interleaved_df_odds %>% 
  filter(interleaved_df_odds$problem_prefix %in% interleaved_df_evens$problem_prefix) %>%
  select(problem_prefix) -> shared_problems
unique(shared_problems)
# 4x+1=x+10, 8x+3=5+6x, 6x+1=13+2x problems are in both even and odd

# Choosing even rows aka same problem but with diagram and without
# it checks diagram/no diagram because even/odd rows don't necessarily correspond 
interleaved_df %>% filter(grepl('diagrams-first', `Problem Name`)) -> interleaved_df_odds
interleaved_df %>% 
  filter(grepl('no-diagrams', `Problem Name`)) ->
  interleaved_df_evens
alldiagram_df %>% 
  filter(alldiagram_df$problem_prefix %in% interleaved_df_odds$problem_prefix) %>%
  filter(!(grepl('Bonus Level', condition) & problem_prefix == "4xplus1eqxplus10")) %>%
  filter(!(grepl('Bonus Level', condition) & problem_prefix == "8xplus3eq5plus6x")) %>%
  filter(!(grepl('Level 8', condition) & problem_prefix == "6xplus1eq13plus2x")) ->
  alldiagram_df_odds
alldiagram_df %>%
  filter(alldiagram_df$problem_prefix %in% interleaved_df_evens$problem_prefix) %>%
  filter(!(grepl('Level 7', condition) & problem_prefix == "4xplus1eqxplus10")) %>%
  filter(!(grepl('Level 8', condition) & problem_prefix == "8xplus3eq5plus6x")) %>%
  filter(!(grepl('Bonus Level', condition) & problem_prefix == "8xplus3eq5plus6x")) ->
  alldiagram_df_evens

# Now we can see both have the same problems
interleaved_df_evens$`Problem Name`[1:10]
alldiagram_df_evens$`Problem Name`[1:10]

# create joint dfs of all odd/all even problems
all_odds_df = rbind(interleaved_df_odds, alldiagram_df_odds)
all_odds_df$condition <- ifelse(grepl("Yellow", all_odds_df$condition), "interleaved", "alldiagram" )
all_evens_df = rbind(interleaved_df_evens, alldiagram_df_evens)
all_evens_df$condition <- ifelse(grepl("Yellow", all_evens_df$condition), "interleaved", "alldiagram" )

# adding parity column to each condition
# can't just alternate, need to use this ifelse
interleaved_df$parity <- ifelse(grepl("diagrams-first", interleaved_df$`Problem Name`), "odd", "even" )
# if the problem prefix is in interleaved evens, it's no diagram aka even
alldiagram_df$parity <- ifelse(alldiagram_df$problem_prefix %in% interleaved_df_evens$problem_prefix, "even", "odd")
# check for correctness
interleaved_df %>% select("parity", `Problem Name`)

# create graphs that show the average numbers of 
# proportion of steps correct on the first try, 
# per student, compared across conditions and compared  
# across odd and even conditions

# interleaved proportion correct first per student
aggregate(interleaved_df$proportion_correct_first_try, by=list(Username=interleaved_df$username), FUN=mean) -> counts_il
counts_il

ggplot(counts_il, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Proportion") + ggtitle("Interleaved Group Proportion Correct First Try/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# all diagram proportion correct first per student
aggregate(alldiagram_df$proportion_correct_first_try, by=list(Username=alldiagram_df$username), FUN=mean) -> counts_ad
counts_ad

ggplot(counts_ad, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Proportion") + ggtitle("All Diagram Group Proportion Correct First Try/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# interleaved even problems hints per step per student
aggregate(interleaved_df_evens$proportion_correct_first_try, by=list(Username=interleaved_df_evens$username), FUN=mean) -> counts_ilev

ggplot(counts_ilev, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Proportion") + ggtitle("Interleaved Even Problem Proportion Correct First Try/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# interleaved odd problems hints per step per student
aggregate(interleaved_df_odds$proportion_correct_first_try, by=list(Username=interleaved_df_odds$username), FUN=mean) -> counts_ilod

ggplot(counts_ilod, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Proportion") + ggtitle("Interleaved Odd Problem Proportion Correct First Try/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# all diagram even problems hints per step per student
aggregate(alldiagram_df_evens$proportion_correct_first_try, by=list(Username=alldiagram_df_evens$username), FUN=mean) -> counts_adev

ggplot(counts_adev, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Proportion") + ggtitle("All Diagram Even Problem Proportion Correct First Try/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# all diagram odd problems hints per step per student
aggregate(alldiagram_df_odds$proportion_correct_first_try, by=list(Username=alldiagram_df_odds$username), FUN=mean) -> counts_adod

ggplot(counts_adod, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Proportion") + ggtitle("All Diagram Odd Problem Proportion Correct First Try/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

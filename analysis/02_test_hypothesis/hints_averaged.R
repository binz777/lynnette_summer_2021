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

# i want to test whether the two groups perform the same on the
# even problems and the odd problems
# i'm going to ignore the problem view for now and i'll copy
# that in when bin finishes

# temporarily filter out all problem views > 1, deletes 93 rows
# only around 2% of rows are problem view > 1
# also filter out students who didn't complete pre and post (2655->2047 rows)
logdata %>%
  mutate(problem_prefix = sub("\\-.*", "", `Problem Name`)) %>%
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
# check for correctneess
interleaved_df %>% select("parity", `Problem Name`)

# create graphs that show the average numbers of hints per step, 
# per student, compared across conditions and compared across 
# odd and even conditions
# avg hints/step/student between interleaved and alldiagram conditions
#mean(interleaved_df$Hints/interleaved_df$Steps)
#mean(alldiagram_df$Hints/alldiagram_df$Steps)
#length(unique(interleaved_df$username))
#length(unique(alldiagram_df$username))
aggregate(interleaved_df$Hints, by=list(Username=interleaved_df$username), FUN=sum) -> sum_hints_il
aggregate(interleaved_df$Steps, by=list(Username=interleaved_df$username), FUN=sum) -> sum_steps_il
merge(sum_hints_il, sum_steps_il, by="Username") %>%
  mutate(hints = x.x/x.y) %>%
  select(Username, hints) %>%
  rename(x = hints) -> counts_il

ggplot(counts_il, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Hints/Step") + ggtitle("Interleaved Group Hints/Step/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# all diagram hints per step per student
aggregate(alldiagram_df$Hints, by=list(Username=alldiagram_df$username), FUN=sum) -> sum_hints_ad
aggregate(alldiagram_df$Steps, by=list(Username=alldiagram_df$username), FUN=sum) -> sum_steps_ad
merge(sum_hints_ad, sum_steps_ad, by="Username") %>%
  mutate(hints = x.x/x.y) %>%
  select(Username, hints) %>%
  rename(x = hints) -> counts_ad

ggplot(counts_ad, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Hints/Step") + ggtitle("All Diagram Group Hints/Step/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# intereleaved even problems hints per step per student
aggregate(interleaved_df_evens$Hints, by=list(Username=interleaved_df_evens$username), FUN=sum) -> sum_hints_ilev
aggregate(interleaved_df_evens$Steps, by=list(Username=interleaved_df_evens$username), FUN=sum) -> sum_steps_ilev
merge(sum_hints_ilev, sum_steps_ilev, by="Username") %>%
  mutate(hints = x.x/x.y) %>%
  select(Username, hints) %>%
  rename(x = hints) -> counts_ilev

ggplot(counts_ilev, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Hints/Step") + ggtitle("Interleaved Even Problem Hints/Step/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# interleaved odd problems hints per step per student
aggregate(interleaved_df_odds$Hints, by=list(Username=interleaved_df_odds$username), FUN=sum) -> sum_hints_ilod
aggregate(interleaved_df_odds$Steps, by=list(Username=interleaved_df_odds$username), FUN=sum) -> sum_steps_ilod
merge(sum_hints_ilod, sum_steps_ilod, by="Username") %>%
  mutate(hints = x.x/x.y) %>%
  select(Username, hints) %>%
  rename(x = hints) -> counts_ilod

ggplot(counts_ilod, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Hints/Step") + ggtitle("Interleaved Odd Problem Hints/Step/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# all diagram even problems hints per step per student
aggregate(alldiagram_df_evens$Hints, by=list(Username=alldiagram_df_evens$username), FUN=sum) -> sum_hints_adev
aggregate(alldiagram_df_evens$Steps, by=list(Username=alldiagram_df_evens$username), FUN=sum) -> sum_steps_adev
merge(sum_hints_adev, sum_steps_adev, by="Username") %>%
  mutate(hints = x.x/x.y) %>%
  select(Username, hints) %>%
  rename(x = hints) -> counts_adev

ggplot(counts_adev, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Hints/Step") + ggtitle("All Diagram Even Problem Hints/Step/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# all diagram odd problems hints per step per student
aggregate(alldiagram_df_odds$Hints, by=list(Username=alldiagram_df_odds$username), FUN=sum) -> sum_hints_adod
aggregate(alldiagram_df_odds$Steps, by=list(Username=alldiagram_df_odds$username), FUN=sum) -> sum_steps_adod
merge(sum_hints_adod, sum_steps_adod, by="Username") %>%
  mutate(hints = x.x/x.y) %>%
  select(Username, hints) %>%
  rename(x = hints) -> counts_adod

ggplot(counts_adod, aes(x=Username, y=x)) + geom_bar(stat="identity") + 
  labs(x="Student", y="Hints/Step") + ggtitle("All Diagram Even Problem Hints/Step/Student") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Create dfs for hints/step/student by condition
# Interleaved
merge(sum_hints_ilev, sum_steps_ilev, by="Username") %>%
  rename("Hints" = x.x) %>%
  rename("Steps" = x.y) %>%
  mutate(avg = `Hints`/`Steps`) -> even
merge(sum_hints_ilod, sum_steps_ilod, by="Username") %>%
  rename("Hints" = x.x) %>%
  rename("Steps" = x.y) %>% 
  mutate(avg = `Hints`/`Steps`) -> odd
# there are 2 students who did only odd problems: slowshark and 1 more
combine(even, odd) %>%
  rename(parity = "source") -> interleaved_avg_df
merge(x = even, y=odd, by="Username") %>%
  mutate(delta = avg.y-avg.x) -> interleaved_delta 

# All diagram
merge(sum_hints_adev, sum_steps_adev, by="Username") %>%
  rename("Hints" = x.x) %>%
  rename("Steps" = x.y) %>%
  mutate(avg = `Hints`/`Steps`) -> even
merge(sum_hints_adod, sum_steps_adod, by="Username") %>%
  rename("Hints" = x.x) %>%
  rename("Steps" = x.y) %>% 
  mutate(avg = `Hints`/`Steps`) -> odd
# there are 2 students who did only odd problems: slowshark and 1 more
combine(even, odd) %>%
  rename(parity = "source") -> alldiagram_avg_df
# removing smallshark because it doesn't show up in interleaved
merge(x = even, y=odd, by="Username") %>%
  filter(Username != "smallshark") %>%
  mutate(delta = avg.y-avg.x) -> alldiagram_delta 

# BEGIN T TESTS
# hypothesis: students in interleaved conditions use less
# hints on odd problems than even problems
# because they apply knowledge from diagram to no diagram problems
interleaved_avg_df %>%
  group_by(parity) %>%
  get_summary_stats(avg, type="mean_sd")

stat.test <- interleaved_avg_df %>% 
  t_test(avg ~ parity, var.equal = TRUE) %>%
  add_significance()
stat.test
# Found no significant difference in average hints/step/student
# between even/odd problems in the interleaved condition

# COMPARING ODD EVEN DELTA BETWEEN CONDITIONS
# ie. compare the hint/step/student difference on odd 
# vs even problems between the conditions
combine(alldiagram_delta, interleaved_delta) %>%
  rename(condition = "source") -> delta_df
delta_df
interleaved_delta
alldiagram_delta

# Hypothesis: students in the interleaved condition should 
# have a larger difference in odd/even delta 
# Summary stats
delta_df %>%
  group_by(condition) %>%
  get_summary_stats(delta, type="mean_sd")

stat.test <- delta_df %>% 
  t_test(delta ~ condition, var.equal = TRUE) %>%
  add_significance()
stat.test

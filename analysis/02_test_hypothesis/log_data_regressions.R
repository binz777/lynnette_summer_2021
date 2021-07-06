library("readxl")
library("ggplot2")
library(tidyverse)
library(here)

PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PATH <- here("data/all_by_student_problem.txt")

prepost <- read_csv(PREPOST_PATH)
logdata <- read_tsv(LOG_DATA_PATH)

# lizzy's file accessing code
prepost = read_csv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/prepost_cleaned.csv")
logdata = read_tsv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/all_by_student_problem.txt")
student_step = read_tsv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/all_by_student_step_csv.txt")

logdata %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  group_by(`Anon Student Id`) %>%
  summarise(num_solved = n(),
            hint = sum(Hints)/sum(Steps),
            incs = sum(Incorrects)/sum(Steps)) %>%
  rename(username = `Anon Student Id`) -> df_logdata

prepost %>%
  select(c(username, condition, CK_post, PK_post, PK_diagrm_post, 
           PK_nodiagram_post, CKPK_pre, CKPK_post,
           PK_diagram_pre, PK_nodiagram_pre)) -> df_prepost

df <- inner_join(df_logdata, df_prepost, by = "username")
df

# pre test on post test (simple lin reg)
lm_post = lm(CKPK_post ~ CKPK_pre, data = prepost)
summary(lm_post)
ggplot(prepost, aes(x=CKPK_pre, y=CKPK_post)) + geom_point() +
  geom_smooth(method="lm", formula=y~x)

# pretest on PK and CK post test
# condition is a more relevant predictor for PK than CK
lm_prior_pk = lm(PK_post ~ condition + CKPK_pre + condition:CKPK_pre + num_solved, data = df)
summary(lm_prior_pk)
lm_prior_ck = lm(CK_post ~ condition + CKPK_pre + condition:CKPK_pre + num_solved, data = df)
summary(lm_prior_ck)

# 2 linear regressions of pretest on posttest, for each condition
# graph to look for noticeable differences
df %>% filter(condition == "alldiagram") -> all_diagram_df
all_diagram_df
lm_post_all_diagram = lm(CKPK_post ~ CKPK_pre + num_solved, data = all_diagram_df)
summary(lm_post_all_diagram)

df %>% filter(condition == "interleaved") -> interleaved_df
interleaved_df
lm_post_interleaved = lm(CKPK_post ~ CKPK_pre + num_solved, data = interleaved_df)
summary(lm_post_interleaved)

ggplot(df, aes(x=CKPK_pre, y=CKPK_post, col=condition)) + geom_point() +
  geom_smooth(method="lm", se=FALSE, formula=y ~ x)

# difference between conditions on PK post score?
df %>% filter(condition == "alldiagram") -> all_diagram_df
all_diagram_df
lm_pkpost_all_diagram = lm(PK_post ~ CKPK_pre + num_solved, data = all_diagram_df)
summary(lm_pkpost_all_diagram)

df %>% filter(condition == "interleaved") -> interleaved_df
interleaved_df
lm_pkpost_interleaved = lm(PK_post ~ CKPK_pre + num_solved, data = interleaved_df)
summary(lm_pkpost_interleaved)

ggplot(df, aes(x=CKPK_pre , y=PK_post, col=condition)) + geom_point() +
  geom_smooth(method="lm", se=FALSE, formula=y ~ x)
# comments: seems like interleaved has a stronger linear trend
# i.e. students who started worse do worse on interleaved, only the best students do better
# hypothesis: students in the all diagram tend to improve more? 
# since there's a bit of a ceiling at the end - see gain regression

# is this statistically significant? try to see if the interaction term is
lm_pkpost = lm(PK_post ~ condition + CKPK_pre + condition:CKPK_pre + num_solved, data = df)
lm_ckpost = lm(CK_post ~ condition + CKPK_pre + condition:CKPK_pre + num_solved, data = df)
summary(lm_pkpost)
summary(lm_ckpost)

# difference between conditions on PK post score for problems without diagrams
df %>% filter(condition == "alldiagram") -> all_diagram_df
all_diagram_df
lm_pkdiagrampost_ad = lm(PK_diagrm_post ~ PK_diagram_pre + num_solved, data = all_diagram_df)
summary(lm_pkdiagrampost_ad)

df %>% filter(condition == "interleaved") -> interleaved_df
interleaved_df
lm_pkdiagrampost_il = lm(PK_diagrm_post ~ PK_diagram_pre + num_solved, data = interleaved_df)
summary(lm_pkdiagrampost_il)

ggplot(df, aes(x=PK_diagram_pre , y=PK_diagrm_post, col=condition)) + geom_point() +
  geom_smooth(method="lm", se=FALSE, formula=y ~ x)

# is this statistically significant? try to see if the interaction term is
lm_pkdiagrampost = lm(PK_diagrm_post ~ condition + PK_diagram_pre + PK_diagram_pre:condition + num_solved, data = df)
summary(lm_pkdiagrampost)

# difference between conditions on PK post score for problems without diagrams
lm_pknodiagrampost_ad = lm(PK_nodiagram_post ~ PK_nodiagram_pre + num_solved, data = all_diagram_df)
summary(lm_pknodiagrampost_ad)

lm_pknodiagrampost_il = lm(PK_nodiagram_post ~ PK_nodiagram_pre + num_solved, data = interleaved_df)
summary(lm_pknodiagrampost_il)

ggplot(df, aes(x=PK_nodiagram_pre , y=PK_nodiagram_post, col=condition)) + geom_point() +
  geom_smooth(method="lm", se=FALSE, formula=y ~ x)

# is this statistically significant? try to see if the interaction term is
lm_pknodiagrampost = lm(PK_nodiagram_post ~ condition + PK_nodiagram_pre + PK_nodiagram_pre:condition + num_solved, data = df)
summary(lm_pknodiagrampost)

# running a model testing how performance on diagram steps predicts symbolic steps 
# for no diagram problems in interleaved group
df %>% filter(condition == 'interleaved') -> interleaved_df
interleaved_df

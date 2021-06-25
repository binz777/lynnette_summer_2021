install.packages("readxl")
install.packages("ggplot2")
install.packages("stringi", type = "win.binary")

library("readxl")
library("ggplot2")
library(tidyverse)
library(here)

PREPOST_PATH <- here("data/prepost_cleaned.csv")
LOG_DATA_PATH <- here("data/all_by_student_problem.txt")

prepost <- read_csv(PREPOST_PATH)
logdata <- read_tsv(LOG_DATA_PATH)

# lizzy's file accessing code
# prepost = read_csv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/prepost_cleaned.csv")
# logdata = read_tsv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/all_by_student_problem.txt")


logdata %>%
  filter(`Anon Student Id` %in% prepost$username) %>%
  group_by(`Anon Student Id`) %>%
  summarise(num_solved = n(),
            hint = sum(Hints)/sum(Steps),
            incs = sum(Incorrects)/sum(Steps)) %>%
  rename(username = `Anon Student Id`) -> df_logdata

prepost %>%
  select(c(username, condition, CK_post, PK_post, CKPK_pre, CKPK_post)) -> df_prepost

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
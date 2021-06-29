# Some exploration on prepost data

library(tidyselect)
library(here)


PREPOST_PATH <- here("data/prepost_cleaned.csv")
prepost <- read_csv(PREPOST_PATH)

# Table of means
prepost %>%
  group_by(condition) %>%
  summarize(pk_pre = mean(PK_pre),
            pk_post = mean(PK_post),
            ck_pre = mean(CK_pre),
            ck_post = mean(CK_post),
            ckpk_pre = mean(CKPK_pre),
            ckpk_post = mean(CKPK_post))

# Table of standard deviations
prepost %>%
  group_by(condition) %>%
  summarize(pk_pre = sd(PK_pre),
            pk_post = sd(PK_post),
            ck_pre = sd(CK_pre),
            ck_post = sd(CK_post),
            ckpk_pre = sd(CKPK_pre),
            ckpk_post = sd(CKPK_post))


# Distribution of the pretest scores
pretest %>%
  group_by(condition) %>%
  gather("type", "score", -condition) %>%
  ggplot(aes(score, fill = condition)) +
  geom_histogram(alpha = .5, position = "Identity") + 
  facet_wrap(~type, scales = "free")



prepost %>%
  filter(condition == "alldiagram") -> diagrams

prepost %>%
  filter(condition == "interleaved") -> interleaved


# Are the differences in pretest score between conditions 
# statistically significant?
t.test(diagrams$CKPK_pre, interleaved$CKPK_pre)
t.test(diagrams$CK_pre, interleaved$CK_pre)
t.test(diagrams$PK_pre, interleaved$PK_pre)
# Nope


# Is there a significant increase from pre and posttest scores
# by condition?

# Diagrams
t.test(diagrams$CKPK_pre, diagrams$CKPK_post, alternative = "l")
t.test(diagrams$CK_pre, diagrams$CK_post, alternative = "l")
t.test(diagrams$PK_pre, diagrams$PK_post, alternative = "l")
# Nope

# Interleaved
t.test(interleaved$CKPK_pre, interleaved$CKPK_post, alternative = "l")
t.test(interleaved$CK_pre, interleaved$CK_post, alternative = "l")
t.test(interleaved$PK_pre, interleaved$PK_post, alternative = "l")
# Nope




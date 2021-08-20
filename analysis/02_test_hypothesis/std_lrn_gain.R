# Simple linear regression models to predict standardized learning gain (from pretest scores)
# Hypothesis: students with lower prior knowledge (lower pretest scores) have greater 
# gain (ie. students who know less learn more from the tutor and can “catch up”)


library(tidyverse)
library(here)

DATA_PATH <- here("data/processed/prepost_cleaned_w_std_lrn_gain.csv")

df <- read_csv(DATA_PATH) %>%
  filter(!is.na(s_pk))

df1 <- filter(df, condition == "alldiagram")
df2 <- filter(df, condition == "interleaved")


# All data
mod1 <- lm(s_ckpk ~ CKPK_pre, data = df)
mod2 <- lm(s_ckpk ~ CK_pre, data = df)
mod3 <- lm(s_ckpk ~ PK_pre, data = df)

mod4 <- lm(s_pk ~ CKPK_pre, data = df)
mod5 <- lm(s_pk ~ CK_pre, data = df)
mod6 <- lm(s_pk ~ PK_pre, data = df)

mod7 <- lm(s_ck ~ CKPK_pre, data = df)
mod8 <- lm(s_ck ~ CK_pre, data = df) # β = -.162, p = .001
mod9 <- lm(s_ck ~ PK_pre, data = df)


# Diagrams
mod1 <- lm(s_ckpk ~ CKPK_pre, data = df1)
mod2 <- lm(s_ckpk ~ CK_pre, data = df1)
mod3 <- lm(s_ckpk ~ PK_pre, data = df1)

mod4 <- lm(s_pk ~ CKPK_pre, data = df1)
mod5 <- lm(s_pk ~ CK_pre, data = df1)
mod6 <- lm(s_pk ~ PK_pre, data = df1)

mod7 <- lm(s_ck ~ CKPK_pre, data = df1)
mod8 <- lm(s_ck ~ CK_pre, data = df1) # β = -.155, p = .045
mod9 <- lm(s_ck ~ PK_pre, data = df1)


# Interleaved
mod1 <- lm(s_ckpk ~ CKPK_pre, data = df2)
mod2 <- lm(s_ckpk ~ CK_pre, data = df2)
mod3 <- lm(s_ckpk ~ PK_pre, data = df2)

mod4 <- lm(s_pk ~ CKPK_pre, data = df2) # β = .073, p = .035
mod5 <- lm(s_pk ~ CK_pre, data = df2) # β = .035, p = .033
mod6 <- lm(s_pk ~ PK_pre, data = df2)

mod7 <- lm(s_ck ~ CKPK_pre, data = df2)
mod8 <- lm(s_ck ~ CK_pre, data = df2) # β = -.166, p = .013
mod9 <- lm(s_ck ~ PK_pre, data = df2)

# It seems like people who did well in the CK component of the pretest
# would have less standardized learning gain in CK in general, which 
# supports our hypothesis. Additionally, students who have higher pretest CK 
# scores seems to have more standardized learning gain (less in magnitude in 
# comparison to the previous finding) in PK in the Interleaved condition.

# All data
mod1 <- lm(s_ckpk ~ CKPK_pre + condition + CKPK_pre:condition, data = df)
mod2 <- lm(s_ckpk ~ CK_pre + condition + CK_pre:condition, data = df)
mod3 <- lm(s_ckpk ~ PK_pre + condition + PK_pre:condition, data = df)

mod4 <- lm(s_pk ~ CKPK_pre + condition + CKPK_pre:condition, data = df)
mod5 <- lm(s_pk ~ CK_pre + condition + CK_pre:condition, data = df)
mod6 <- lm(s_pk ~ PK_pre + condition + PK_pre:condition, data = df)

mod7 <- lm(s_ck ~ CKPK_pre + condition + CKPK_pre:condition, data = df)
mod8 <- lm(s_ck ~ CK_pre + condition + CK_pre:condition, data = df) # β = -.162, p = .001
mod9 <- lm(s_ck ~ PK_pre + condition + PK_pre:condition, data = df)


# Diagrams
mod1 <- lm(s_ckpk ~ CKPK_pre, data = df1)
mod2 <- lm(s_ckpk ~ CK_pre, data = df1)
mod3 <- lm(s_ckpk ~ PK_pre, data = df1)

mod4 <- lm(s_pk ~ CKPK_pre, data = df1)
mod5 <- lm(s_pk ~ CK_pre, data = df1)
mod6 <- lm(s_pk ~ PK_pre, data = df1)

mod7 <- lm(s_ck ~ CKPK_pre, data = df1)
mod8 <- lm(s_ck ~ CK_pre, data = df1) # β = -.155, p = .045
mod9 <- lm(s_ck ~ PK_pre, data = df1)


# Interleaved
mod1 <- lm(s_ckpk ~ CKPK_pre, data = df2)
mod2 <- lm(s_ckpk ~ CK_pre, data = df2)
mod3 <- lm(s_ckpk ~ PK_pre, data = df2)

mod4 <- lm(s_pk ~ CKPK_pre, data = df2) # β = .073, p = .035
mod5 <- lm(s_pk ~ CK_pre, data = df2) # β = .035, p = .033
mod6 <- lm(s_pk ~ PK_pre, data = df2)

mod7 <- lm(s_ck ~ CKPK_pre, data = df2)
mod8 <- lm(s_ck ~ CK_pre, data = df2) # β = -.166, p = .013
mod9 <- lm(s_ck ~ PK_pre, data = df2)
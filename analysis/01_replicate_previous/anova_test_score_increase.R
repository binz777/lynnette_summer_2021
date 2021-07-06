library(tidyverse)
library(ggpubr)
library(rstatix)

# repeated measures ANOVA for CK increase and PK decrease

# data preparation
# lizzy's file accessing code - CK pretest/posttest data
prepost = read_csv("C://Users/wuhan/Documents/cs/cmu-lynnette-research/prepost_cleaned.csv")
# gather columns into long format
#adjust column names and filter based on needs
prepost_df <- prepost %>% 
  filter(condition == "interleaved") %>%
  gather(key="test", value="score", CK_pre, CK_post) %>% 
  select(c(username, test, score))
prepost_df <- prepost %>% 
  filter(condition == "interleaved") %>%
  gather(key="test", value="score", PK_pre, PK_post) %>% 
  select(c(username, test, score))
# summary statistics
prepost_df %>% 
  group_by(test) %>%
  get_summary_stats(score, type="mean_sd")
# box plot
bxp <- ggboxplot(prepost_df, x = "test", y = "score", add = "point")
bxp # visually doesn't look significantly different
# for alldiagram, the post has a higher 3rd quartile
# look for outliers
prepost_df %>%
  group_by(test) %>%
  identify_outliers(score) # found none
# check normality
prepost_df %>%
  group_by(test) %>%
  shapiro_test(score) # might not be normally dist for CK or PK, small p vals
ggqqplot(prepost_df, "score", facet.by = "test") # might be due to the categorical nature
# ANOVA test for increase in CK
res.aov <- anova_test(data = prepost_df, dv = score, wid = username, within = test)
get_anova_table(res.aov) 
# for ALL data (both conditions)
# p = .206 for CK, found positive but non-significant pretest-posttest gain for the conceptual knowledge items
# p = .662 for PK
# for alldiagram condition
# p = 0.062 for CK
# p = 0.546 for PK
# for interleaved condition
# p = 0.904 for CK
# p = 0.895 for PK
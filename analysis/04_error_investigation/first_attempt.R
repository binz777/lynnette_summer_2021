# Gets the instances where students continues to do the step
# after they get it correct on the first try

library(tidyverse)
library(here)

LOG_DATA_STEP_PATH <- here("data/all_by_student_step.txt")
LOG_DATA_TRAN_PATH <- here("data/all_by_transaction.txt")
STU_PROB_OUTPATH <- here("data/processed/first_attempt_student_problem.csv")
TRAN_OUTPATH <- here("data/processed/first_attemp_transactions.txt")

logdata_s <- read_tsv(LOG_DATA_STEP_PATH)
logdata_t <- read_tsv(LOG_DATA_TRAN_PATH)

# Gets the student problems
logdata_s %>%
  filter(`First Attempt` == "correct", 
         Incorrects > 0) -> df
df_sp <- distinct(df, `Problem Name`, `Anon Student Id`)

# Gets the transactions
logdata_t %>%
  filter(`Anon Student Id` %in% df_sp$`Anon Student Id`,
         `Problem Name` %in% df_sp$`Problem Name`) -> df_t


write_csv(df_sp, STU_PROB_OUTPATH)
write_tsv(df_t, TRAN_OUTPATH)
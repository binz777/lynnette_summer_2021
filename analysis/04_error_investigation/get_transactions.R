# Filters the transactions of a specific student problem

library(tidyverse)
library(here)

LOG_DATA_TRAN_PATH <- here("data/all_by_transaction.txt")
logdata_t <- read_tsv(LOG_DATA_TRAN_PATH)


# Replace the following with the desired inputs, run the script, and open `df`
username <- "smallbee"
problem <- "2xplus3eq7-diagrams-first"
variables <- c("Row", "Step Name", "Attempt At Step", 
              "Is Last Attempt", "Outcome", "Input", 
              "Level (ProblemSet)", "Problem View", "CF (tool_event_time)")

logdata_t %>%
  filter(`Anon Student Id` == username,
         `Problem Name` == problem) %>%
  select(all_of(variables)) -> df


# SEND EMAIL, mess up learning curve?
### correct/incorrect first try
# bigalpaca 4plus5xeq8plus3x-diagrams-first
# slowowl 15eq4xplus7-no-diagrams 
# bigmonkey 3xplus1eq10-diagrams-first
# slowkoala 7eq5plus2x-diagrams-first
### got correct on the first try and kept on entering


### high problem view
# fastduck 8xplus3eq5plus6x-diagrams-first # Missing 4 and 6, some steps missing?
# smallbee 2xplus3eq7-diagrams-first
# fastbat 15eq4xplus3-no-diagrams
# fastbee xplus3eq5-no-diagrams
### counted as different steps
### does problem view increase if students hit refresh?




### lots of incorrects
# bigalpaca 4xplus1eqxplus10-diagrams-first 
# slowfish 3xplus5eqxplus11-no-diagrams
# smallbison xplus2eq6-diagrams-first
# fastbee xplus3eq5-no-diagrams
### repeatedly entered the same thing


### problems with highest error rate (Interleaved)
# xplus2eq6-diagrams-first (1) 
### clicking done at the start, not used to seeing tape diagrams, lots of hints
# 2xplus3eq7-diagrams-first (3)
### also clicking done from the start, skipping steps
# 8eqxplus3-diagrams-first (2)
### x on the left side
# 7xeq2xplus5-no-diagrams (5)
### hard problem? combine-like-var


### problems with highest error rate (Diagrams)
# xplus2eq6-diagrams-first (1)
### done from start, not used to tape diagrams, lots of hints
# 8eqxplus3-diagrams-first (2)
### x on left
# 2xplus3eq7-diagrams-first (3)
### done from start, skip steps (but less than interleaved)
# 4xplus1eqxplus10-diagrams-first (7)
### skipping steps
# Merge environmental data and response to create model-ready dfs
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)

# Dependencies ------------------------------------------------------------

library(tidyverse)

envframe <- readRDS("envframe_surfacevars_withouttimelags.RDS")
kudoa <- read.csv("Infect_Hemo_Cleaned.csv")
# Both not available in repo
nrow(kudoa)

# Infection prevalence model ----------------------------------------------

infect <- kudoa %>% 
  dplyr::select(-Sample,
                - NT,
                - Date.Unload,
                - Date.Collected,
                - Time.Collected,
                - AJ,
                - FO,
                - Month,
                - Date_assessed,
                - Micros_infected,
                - Molec_infected,
                - DNA_mg,
                - DNA_ngul,
                - DNA_260_280,
                - Date.Assessed,
                - TotalFish,
                - TotalSample) %>% 
  distinct #remove duplicate rows caused by sample column
nrow(infect)

# Merge env and response --------------------------------------------------

comp2PREV <- subset(infect, Company == "REDACTED") #Redacted
comp2PREV$Vessel <- NULL
comp2ENV <- subset(envframe, Company == "REDACTED") #Redacted
comp2ENV$Vessel <- NULL
comp2PREV$Trip <- as.factor(comp2PREV$Trip) 
comp2_all <- left_join(comp2PREV, comp2ENV, by = c("Trip", "Company")) 
comp2_all$Vessel <- NA 

comp1PREV <- subset(infect, Company == "REDACTED") #Redacted
comp1PREV$Vessel <- as.factor(comp1PREV$Vessel)
levels(comp1PREV$Vessel) <- c("REDACTED", "REDACTED", "REDACTED", "REDACTED", "REDACTED")
comp1PREV$Company <- "REDACTED" #Redacted
comp1ENV <- subset(envframe, Company == "REDACTED") #Redacted
comp1PREV$Trip <- as.factor(comp1PREV$Trip)
comp1_all <- left_join(comp1PREV, comp1ENV, by = c("Trip", "Vessel", "Company"))

nrow(comp2_all) + nrow(comp1_all); nrow(infect)
# Both are the same; merge worked, OK to move on



# Operation random effect -------------------------------------------------

comp2_all$operation <- NA
for (i in 1:nrow(comp2_all)) {
  comp2_all$operation[i] <- paste0(comp2_all$Trip[i], "_BOATS")
}

comp1_all$operation <- NA
for (i in 1:nrow(comp1_all)) {
  comp1_all$operation[i] <- paste0(comp1_all$Trip[i], 
                                   "_", 
                                   stringr::str_to_upper(comp1_all$Vessel[i]))
}

total <- rbind(comp1_all, comp2_all)
total$operation <- as.factor(total$operation)
nrow(total); nrow(infect)  #are the same 
total$Trip <- as.factor(total$Trip)
prev <- total[order(total$Trip),] #reorder
colSums(is.na(prev)) #No problem NAs. 


# HEMO --------------------------------------------------------------------

hemo <- read.csv("Hemo_Cleaned.csv") #hemocytometer ONLY data

# Remove nuisance variables
hemo$Sample <- NULL
hemo$X <- NULL
hemo$NT <- NULL
hemo$Date.Assessed <- NULL
hemo$TotalSample <- NULL

# Filter out duplicate rows caused by sample column
hemo <- hemo %>% distinct
hemo$Trip <- as.factor(hemo$Trip)

# Merge hemo counts and prev df
test <- left_join(prev, hemo, by = c("Trip", "Fish"))
nrow(test); nrow(infect) # Is the same! 

saveRDS(test, "modelreadydf.RDS")
# not available in repo
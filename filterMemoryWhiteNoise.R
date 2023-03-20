###Data Cleaning Step 1##########

# by Pia Elbe
# March 18, 2023

# In this file, we filter and tidy the data from a serial recall task. This example shows the process for
# the first control dataset, which can be repeated for the other datasets. Only includes task performance,
# not demographic questions.

#Import the datasets into one data frame

list.files('/Users/piaelb/Desktop/Control Group/First Run Normal')

library(dplyr)

raw.files <- data_frame(filename = list.files('/Users/piaelb/Desktop/Control Group/First Run Normal'))

raw.file.paths <- raw.files  %>%
  mutate(filepath = paste0("/Users/piaelb/Desktop/Control Group/First Run Normal/", filename))

read.csv.and.add.filename <- function(filepath){
  read.csv(filepath) %>%
    mutate(filepath=filepath)
}

library(readr)
C1 <- list.files(path="/Users/piaelb/Desktop/Control Group/First Run Normal/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
C2 <- list.files(path="/Users/piaelb/Desktop/Control Group/Second Run Normal/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

###Data Tidying########

library(tidyverse)

C1  %>% 
  rename(code = "ID",
         age = "participant_age",
         gender = "participant_gender",
         calibrate = "headphone_pass") %>% 
  select(ID, age, gender, calibrate)   #if you want to erase some columns, simply add a minus in front of the name.
# %>% 
# drop_na() #this will drop any rows with an NA value somewhere in it
# we don't really need "calibrate", since we know all participants in Prolific passed this test.

#identify the rows that have results with the list of true and false results we want.
listCorrResp <- grep(",", C1$correctPositions) #this takes any cells that have a comma in them.

# print the identified rows
print(listCorrResp)

# now we need to get rid of rows that are labelled NA in the column 
# that has the list of true and false values in it.
C1_long <- C1 %>%
  pivot_longer(cols = correctPositions, names_to = "position", values_to = "is_correct") %>%
  filter(!is.na(is_correct))

# group the data by participant ID and position, and count the number of rows in each group
C1_long <- C1_long %>%
  group_by(code, position) %>%
  mutate(row_count = n())

# create a new column called "trial_type" that labels the first 4 rows of each participant's practice trials
C1_long <- C1_long %>%
  mutate(trial_type = ifelse(position == "practice" & row_count <= 4, "practice", "test"))

# remove the row count column
C1_long <- C1_long %>%
  select(-row_count)

# now label the first 4 practice trials by participant
C1_long <- C1_long %>%
  group_by(code) %>%
  mutate(trial_type = ifelse(row_number() <= 4, "practice", "test"))

#let's see how many times the response "true" is given in each row.
library(stringr)
C1_long <- C1_long %>%
  mutate(num_true = str_count(is_correct, "true"))

#In case we want to look at serial position effects, we can make another df for that:
C1_extralong <- C1_long %>%
  separate_rows(is_correct, sep = ",")

#We also want to number the trials
C1_extralong <- C1_extralong %>%
  mutate(trial_number = ceiling(row_number() / 8))

#The resulting data frames will have "code" as the participant ID, 
#"trial_type" as a label for practice or test trials,
#"is_correct" as a list of true and false words in the C1_long df, and
#"trial_number" in C1_extralong df, along with true or false in separate rows.

write.csv(C1, "C1.csv")
write.csv(C1_long, "C1_long.csv")
write.csv(C1_extralong, "C1_extralong.csv")

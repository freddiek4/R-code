library(tidyverse)

file_path = "Downloads/W23 ASUCDxRSO_April 7, 2023_13.56 4.csv"


survey_data <- read_csv(file_path) %>%  # use the file path name on your computer
  mutate(across(c(18:38), ~ ifelse(is.na(.x), "No Response", .x)))


survey_data = survey_data[, 18:38]
survey_data 


survey_names = c("Club_name", "Club_Position", "Club_Position_Text", "Applied_CFC_Grant",
                 "CFC_find_out", "CFC_find_out_text", "CFC_issues", "CFC_issues_Text",
                 "Why_not_CFC", "Why_not_CFC_Text", "Storage", "Storage_type", "Storage_type_text",
                 "Storage_time", "Storage_time_Text", "ASUCD_Assitance", "ASUCD_engagement", "Endorsement_pariticpation",
                 "Endorsement_explanation","Endorsement_explanation_Text", "Feedback" )
names(survey_data) = survey_names
survey_data = survey_data[-c(1:3),]

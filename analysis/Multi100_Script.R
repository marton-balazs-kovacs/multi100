#### Multi100 Data Processing ####

#### Task 1 Data ####

# import Task 1 data
Task1 <- read.csv("Desktop/Multi100 Analysis/Task1.csv")

# there are 23 variables in this dataset
# 1 - Submission Timestamp
# 2 - Email
# 3 - Analyst ID > 'Please provide your analyst ID'
# 4 - Paper ID > 'Please provide the ID of your paper'
# 5 - Current Position > 'Which title best describes your current position?'
# 6 - Gender > 'What is your gender identity?'
# 7 - Age > 'What is your age in years?'
# 8 - Education Level > 'What is the highest level of education you have completed?'
# 9 - Country of Residence
# 10 - Discipline > 'Which discipline is closest to your research area?'
# 11 - Keywords > 'What keywords would best describe the topics of your own research?'
# 12 - Experience > 'How many years of experience do you have in data analysis?'
# 13 - Data Analysis Frequency > 'How regularly do you perform data analysis?'
# 14 - Self Rating > 'How do you rate your level of expertise in the field of data analysis?'
# 15 - Familiarity with Paper > 'Were you familiar with the paper that this dataset belongs to before beginning your work on this project?'
# 16 - Communication Check > 'Did you communicate about the details of your analysis with co analysts of the same dataset?'
# 17 - Analysis Software > 'What language software tools did you use in your data analysis?'
# 18 - Analysis Report > 'Please report the most important steps of the analysis to the level of detail you would provide in a methods analysis section of a typical research article'
#                        'Include any pre-processing steps that you conducted on the dataset'
#                        'Describe the exact statistical hypothesis you tested and explain the reason for choosing the statistical procedures applied'
#                        'Finally, please report the results of your statistical tests'
# 19 - Conclusion > 'What is the conclusion of your analysis in words?'
# 20 - Categorisation > 'How would you categorise your result?'
# 21 - Confidence in Approach > 'How confident are you that your statistical approach is suitable for analysing the specific claim?'
# 22 - Data Suitability > 'How suitable did you find the dataset of the study for analysing the selected claim?'
# 23 - Analyst Comments > 'Do you have a further comment to share?'

# rename variables
colnames(Task1)[1] <- "Task1_Timestamp"
colnames(Task1)[2] <- "Task1_Email"
colnames(Task1)[3] <- "Analyst_ID"
colnames(Task1)[4] <- "Paper_ID"
colnames(Task1)[5] <- "Current_Position"
colnames(Task1)[6] <- "Gender"
colnames(Task1)[7] <- "Age"
colnames(Task1)[8] <- "Education_Level"
colnames(Task1)[9] <- "Country_of_Residence"
colnames(Task1)[10] <- "Analyst_Discipline"
colnames(Task1)[11] <- "Keywords"
colnames(Task1)[12] <- "Years_of_Experience"
colnames(Task1)[13] <- "Analysis_Frequency"
colnames(Task1)[14] <- "Expertise_Self_Rating"
colnames(Task1)[15] <- "Familiar_with_Paper"
colnames(Task1)[16] <- "Communication_Check"
colnames(Task1)[17] <- "Task1_Software"
colnames(Task1)[18] <- "Task1_Analysis_Report"
colnames(Task1)[19] <- "Task1_Conclusion"
colnames(Task1)[20] <- "Task1_Categorisation"
colnames(Task1)[21] <- "Confidence_in_Approach"
colnames(Task1)[22] <- "Data_Suitability"
colnames(Task1)[23] <- "Task1_Comments"

# add 'Submission_Number' as a reference variable
Task1$Task1_Submission_Number <- 1:nrow(Task1)

# remove leading and trailing spaces from Analyst_ID and Paper_ID variables
Task1$Analyst_ID <- trimws(Task1$Analyst_ID, "both")
Task1$Paper_ID <- trimws(Task1$Paper_ID, "both")

# the first 3 rows of 'Task1' data are test runs (pilot data), remove with negative indexing:
Task1 <- Task1[-c(1:3), ]

# check how many analyses were submitted with '_2' ID's
# filter Task1 data and print the number of times they appear in 'task1_multiple_analyses' dataframe
# check that there are no '_3's' etc...
library(tidyverse)
task1_multiple_analyses <- Task1 %>% filter(str_detect(Analyst_ID, "_")) %>% select(Analyst_ID, Paper_ID) %>% arrange(Analyst_ID) %>% group_by(Analyst_ID) %>% mutate(n = n())

# important step: remove '_2' from the end of Analyst ID's in both Task 1 and Task 2
# use gsub() to remove '_2' from the 'Analyst_ID' variable:
Task1$Analyst_ID <- gsub("_2$", "", Task1$Analyst_ID)

# import 'all_people' data (to be used for checking that ID's match)
all_people <- read.csv("Desktop/Multi100 Analysis/all_people - form_answers.csv")

# check whether each analyst ID matches with the ID they were assigned in 'all_people'
# check if all 'Analyst_ID' in 'task1' are present in 'all_people'
matching_ids <- Task1$Analyst_ID %in% all_people$analyst_id

# check if there are any 'analyst_id' in 'task1' that are not in 'all_people'
any_missing_ids <- any(!matching_ids)
# if 'FALSE' then all ID's match

# import '100_papers' data to check paper ID's against
One_Hundred_Papers <- read.csv("Desktop/Multi100 Analysis/100_papers.csv")

# remove leading and trailing spaces from p_id
One_Hundred_Papers$paper_id <- trimws(One_Hundred_Papers$paper_id, "both")

# check whether the Paper ID provided by the analyst matches the correct ID
# check if all Paper_ID's in 'Task1' are present in 'One_Hundred_Papers'
missing_p_ids <- Task1$Paper_ID %in% One_Hundred_Papers$paper_id
any_missing_p_ids <- any(!missing_p_ids) # if 'FALSE' then ID's match

# find non-matching p_ID's
# find Paper ID values in 'Task1' that are not present in 'One_Hundred_Papers'
non_matching_p_ids <- Task1$Paper_ID[!Task1$Paper_ID %in% One_Hundred_Papers$paper_id]
print(non_matching_p_ids)

# 'XAS95 Raley_JournMarFam_2012_D2LY' should be 'Raley_JournMarFam_2012_D2LY'
incorrect_index <- which(Task1$Paper_ID == "XAS95 Raley_JournMarFam_2012_D2LY")
Task1$Paper_ID[incorrect_index] <- "Raley_JournMarFam_2012_D2LY"

# 'Rjp9' should be 'Jiang_AmJourPoliSci_2018_Rjp9'
incorrect_index <- which(Task1$Paper_ID == "Rjp9")
Task1$Paper_ID[incorrect_index] <- "Jiang_AmJourPoliSci_2018_Rjp9"

# verify that 'any_missing_p_ids' is now FALSE
missing_p_ids <- Task1$Paper_ID %in% One_Hundred_Papers$paper_id
any_missing_p_ids <- any(!missing_p_ids)

# check for duplicate responses
# use the 'tidyr' package to create a Combined ID from Analyst_ID and Paper_ID
library(tidyr)
Task1 <- transform(Task1, Combined_ID = paste(Analyst_ID, Paper_ID, sep = "_"))

# use duplicated() to check for duplicates in 'Combined_ID'
duplicates <- duplicated(Task1$Combined_ID)

# attend to each of these cases manually
# create new dataframe with duplicate responses (with their originals) only
# use subset() to filter the dataframe for original and duplicated responses
Task1_duplicates <- subset(Task1, duplicates)

# GNUQA_GROSSMAN
# SN. 23 to be removed... responses largely identical, but the comments of the analyst indicate that SN. 24 is their preferred version
Task1 <- subset(Task1, Task1_Submission_Number != 23)
# KZGB8_Ku
# SN. 25 to be removed... analyst forgot to include something in original
Task1 <- subset(Task1, Task1_Submission_Number != 25)
# AJIUW_Angrist
# SN. 64 to be removed... submission is identical apart from conclusion... consider duplicate response a replacement of the original
Task1 <- subset(Task1, Task1_Submission_Number != 64)
# 419LV_Bursztyn
# SN. 110 to be removed... submission is identical apart from 'years of experience'... 
# SN. 110 states that the analyst is 30 years old and has 30 years of experience. SN. 108 reports 6 years of experience...
# consider SN. 110 an error
Task1 <- subset(Task1, Task1_Submission_Number != 110)
# T6S8L_Robertson
# SN. 193 to be removed... original submission states the claim cannot be tested, but the duplicate SN. 235 corrects this
Task1 <- subset(Task1, Task1_Submission_Number != 193)
# YMESM_Cohen
# SN. 91 to be removed... SN. 237 is stated as an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 91)
# IFVKR_Robertson
# SN. 52 to be removed... SN. 250 is an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 52)
# 5GT7K_Wlezien
# SN. 49 to be removed... SN. 282 is an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 49)
# XF5GJ_Wlezien
# SN. 146 to be removed... SN. 283 is an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 146)
# KNXVJ_Wlezien
# SN. 216 to be removed... SN. 284 is an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 216)
# YEJO9_Andreoni
# SN. 316 to be removed... SN. 324 is stated as an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 316)
# J5BBA_Caldero
# SN. 337 to be removed... SN. 345 is an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 337)
# 8AYYN_Menaldo
# SN. 378 + 379 to be removed... three identical submissions
Task1 <- subset(Task1, Task1_Submission_Number != 378)
Task1 <- subset(Task1, Task1_Submission_Number != 379)
# K0XHI_Platt
# SN. 386 + 387 to be removed... SN. 388 states that this is their third and final submission with minor changes made from the previous
Task1 <- subset(Task1, Task1_Submission_Number != 386)
Task1 <- subset(Task1, Task1_Submission_Number != 387)
# HDP26_Cleave
# SN. 498 to be removed... SN. 497 is stated as an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 498)
# K9J6C_Hendricks
# SN. 415 to be removed... SN. 536 is an updated entry
Task1 <- subset(Task1, Task1_Submission_Number != 415)

# verify that there are no longer any duplicates in 'Combined_ID'
has_duplicates <- duplicated(Task1$Combined_ID)
num_duplicates <- sum(has_duplicates)


#### Task 2 Data ####

# import Task 2 data
Task2 <- read.csv("Desktop/Multi100 Analysis/Task2.csv")

# there are 18 variables in this dataset
# some variables represent identical information requested in Task 1 (e.g., Analyst ID)
# 1 - Submission Timestamp
# 2 - Email
# 3 - Analyst ID > 'Please provide your analyst ID'
# 4 - Paper ID > 'Please provide the ID of your paper'
# 5 - Software > 'What language software tools did you use in your analysis?'
# 6 - Analysis Report > 'Please report the most important steps of the analysis to the level of detail you would provide in a methods analysis section
#                        'Include any pre-processing steps that you conducted on the dataset'
#                        'Describe the exact hypothesis you tested and explain the reason for choosing the statistical procedure you applied'
#                        'Finally, please report the results of your statistical tests'
#                        'Please also describe the steps you took in order to satisfy the specific instructions of Task 2'
# 7 - Type of Statistic > 'What type of test statistic did you calculate?'
# 8 - Statistic Report > 'Please report the numeric value of your test statistic with three decimals precision. Please use '.' as the decimal separator'
# 9 - Model Sample Size > 'What is the sample size of your model?'
# 10 - Model Degrees of Freedom > 'What is the degrees of freedom of your model if applicable? (i.e., in case your result is based on Chi, t or F statistic, separate them by ';''
# 11 - p value or Bayes > 'Did you base your conclusion on a p value or a bayes factor?'
# 12 - p value Report > 'What is the p value or Bayes factor of your main result? Report only the values here'
# 13 - Conclusion > 'What is the conclusion of your analysis in words?'
# 14 - Direction of Result > 'Please indicate the direction to which your result points disregarding whether it is conclusive (significant or not)'
# 15 - Same Conclusion as Task 1? - 'Did you arrive at the same statistical conclusion in Task 2 as in Task 1?'
# 16 - Additional Calculations - 'Did you have to make additional calculations to complete Task 2?'
# 17 - Total Hours - 'Approximately how many hours did it require from you to perform Task 1 and Task 2 together?'
# 18 - Comments - 'We are very interested to know and thoughts and comments you have about the survey you just completed or the Multi100 project more generally
#                 'We would be grateful for any feedback you could provide here'

# rename variables
colnames(Task2)[1] <- "Task2_Timestamp"
colnames(Task2)[2] <- "Task2_Email"
colnames(Task2)[3] <- "Analyst_ID"
colnames(Task2)[4] <- "Paper_ID"
colnames(Task2)[5] <- "Task2_Software"
colnames(Task2)[6] <- "Task2_Analysis_Report"
colnames(Task2)[7] <- "Type_of_Statistic"
colnames(Task2)[8] <- "Statistic_Report"
colnames(Task2)[9] <- "Model_Sample_Size"
colnames(Task2)[10] <- "Model_Degrees_of_Freedom"
colnames(Task2)[11] <- "p_value_or_Bayes"
colnames(Task2)[12] <- "p_value_Report"
colnames(Task2)[13] <- "Task2_Conclusion"
colnames(Task2)[14] <- "Direction_of_Result"
colnames(Task2)[15] <- "Same_Conclusion_as_Task1"
colnames(Task2)[16] <- "Additional_Calculations"
colnames(Task2)[17] <- "Total_Hours"
colnames(Task2)[18] <- "Task2_Comments"

# add 'Submission_Number' as a reference variable
Task2$Task2_Submission_Number <- 1:nrow(Task2)

# remove leading and trailing spaces from Analyst_ID and Paper_ID variables
Task2$Analyst_ID <- trimws(Task2$Analyst_ID, "both")
Task2$Paper_ID <- trimws(Task2$Paper_ID, "both")

# use gsub() to remove '_2' from the 'Analyst_ID' variable:
Task2$Analyst_ID <- gsub("_2$", "", Task2$Analyst_ID)

# check whether each analyst ID matches with the ID they were assigned in 'all_people'
# check if all 'Analyst_ID' in 'Task2' are present in 'all_people'
matching_ids_2 <- Task2$Analyst_ID %in% all_people$analyst_id

# check if there are any 'analyst_id' in 'task1' that are not in 'all_people'
any_missing_ids_2 <- any(!matching_ids_2)
# if 'FALSE' then all ID's match

# find non-matching ID's
# find Analyst ID values in 'Task2' that are not present in 'all_people'
non_matching_ids <- Task2$Analyst_ID[!Task2$Analyst_ID %in% all_people$analyst_id]

# print the non-matching 'analyst_id' values
print(non_matching_ids)

# replace non-matching ID's

# PGT48 should be PTG48
# find the index of the incorrect 'analyst_id' 'PGT48' in 'Task2'
incorrect_index <- which(Task2$Analyst_ID == "PGT48")
# replace the incorrect 'analyst_id' with the correct matching ID 'PTG48'
Task2$Analyst_ID[incorrect_index] <- "PTG48"

# h46k4 should be H46K4
incorrect_index <- which(Task2$Analyst_ID == "h46k4")
Task2$Analyst_ID[incorrect_index] <- "H46K4"

# CO9RR should be C09RR (zero rather than capital o)
incorrect_index <- which(Task2$Analyst_ID == "CO9RR")
Task2$Analyst_ID[incorrect_index] <- "C09RR"

# verify that 'any_missing_ids_2' is now FALSE
matching_ids_2 <- Task2$Analyst_ID %in% all_people$analyst_id
any_missing_ids_2 <- any(!matching_ids_2)

# both Task1 and Task2 Analyst ID's now match with the ID they were assigned

# check how many Analyst ID's are present in Task1 data but NOT Task2 data
# then verify that there are no Analyst ID's in Task2 that do not appear in Task1
setdiff(Task1$Analyst_ID, Task2$Analyst_ID) 
setdiff(Task2$Analyst_ID, Task1$Analyst_ID) # should return 'character(0)'

# check whether the Paper ID provided by the analyst matches the correct ID

# check if all Paper_ID's in 'Task2' are present in 'One_Hundred_Papers'
missing_p_ids_2 <- Task2$Paper_ID %in% One_Hundred_Papers$paper_id
any_missing_p_ids_2 <- any(!missing_p_ids_2) # if 'FALSE' then ID's match

# find non-matching p_ID's
# find Paper ID values in 'Task2' that are not present in 'One_Hundred_Papers'
non_matching_p_ids_2 <- Task2$Paper_ID[!Task2$Paper_ID %in% One_Hundred_Papers$paper_id]
print(non_matching_p_ids_2)

# 'Tenye_EurSocioRev_2016_qXX2' should be 'Teney_EurSocioRev_2016_qXX2'
incorrect_index <- which(Task2$Paper_ID == "Tenye_EurSocioRev_2016_qXX2")
Task2$Paper_ID[incorrect_index] <- "Teney_EurSocioRev_2016_qXX2"

# 'qQ9Z' should be 'Desmond_Demography_2015_qQ9Z'
incorrect_index <- which(Task2$Paper_ID == "qQ9Z")
Task2$Paper_ID[incorrect_index] <- "Desmond_Demography_2015_qQ9Z"

# 'RJP9' should be 'Jiang_AmJourPoliSci_2018_Rjp9'
incorrect_index <- which(Task2$Paper_ID == "RJP9")
Task2$Paper_ID[incorrect_index] <- "Jiang_AmJourPoliSci_2018_Rjp9"

# verify that 'any_missing_p_ids_2' is now FALSE
missing_p_ids_2 <- Task2$Paper_ID %in% One_Hundred_Papers$paper_id
any_missing_p_ids_2 <- any(!missing_p_ids_2) # if 'FALSE' then ID's match

# remove duplicate Combined_ID's (for Task2)

# use the 'tidyr' package to create a Combined ID from Analyst_ID and Paper_ID
library(tidyr)
Task2 <- transform(Task2, Combined_ID = paste(Analyst_ID, Paper_ID, sep = "_"))

# use duplicated() to check for duplicates in 'Combined_ID'
duplicates_2 <- duplicated(Task2$Combined_ID)

# attend to each of these cases manually
# create new dataframe with duplicate responses (with their originals) only
# use subset() to filter the dataframe for original and duplicated responses
Task2_duplicates <- subset(Task2, duplicates_2)

# manual inspection of the duplicate responses confirms that the last instance of each analysis is the analysis that should remain in the dataset
# subset the data to contain only the rows with the last occurrences of each duplicate 'Combined_ID'

# identify the duplicated rows based on the 'Combined_ID' column
duplicated_rows <- duplicated(Task2$Combined_ID, fromLast = TRUE)

# subset the data frame to keep only the non-duplicated rows
Task2 <- Task2[!duplicated_rows, ]

# check that this has worked using YSAKF_Chen
# submission 133 should remain in the dataset, submission 132 should have been removed
# check if value 132 is present in Task2$Task2_Submission_Number
if (132 %in% Task2$Task2_Submission_Number) {
  cat("Value 132 is present - did not work")
} else {
  cat("Value 132 is not present - worked")
}
# worked

# check which Combined ID's appear in Task 1 but not Task 2 and vice versa
setdiff(Task1$Combined_ID, Task2$Combined_ID) 
setdiff(Task2$Combined_ID, Task1$Combined_ID) 

# JX7YO wrongly submitted Task 2 Paper ID as McLaren... should be Christensen... rectify
# Task2_Submission_Number is 199
Task2$Paper_ID[Task2$Task2_Submission_Number == 199] <- "Christensen_EurJournPersonality_2018_8R9d"

#### Merged Dataframe ####

# use the 'dplyr' package to join task 1 and task 2 data based on both Analyst_ID and Paper_ID
library(dplyr)
multi100_data <- inner_join(Task1, Task2, by = c("Analyst_ID", "Paper_ID"))

# create a subset of 'multi100_data' with only relevant variables
multi100_data <- subset(multi100_data, select = c(Analyst_ID, Paper_ID, Task1_Email, Task2_Email, Task1_Timestamp, Task2_Timestamp, 
                                                  Current_Position, Age, Gender, Education_Level, 
                                                  Country_of_Residence, Analyst_Discipline, Keywords, Years_of_Experience, Analysis_Frequency,
                                                  Expertise_Self_Rating, Familiar_with_Paper, Communication_Check, Task1_Software,
                                                  Task1_Analysis_Report, Task1_Conclusion, Task1_Categorisation, Confidence_in_Approach,
                                                  Data_Suitability, Task1_Comments, Task2_Software, Task2_Analysis_Report, Type_of_Statistic,
                                                  Statistic_Report, Model_Sample_Size, Model_Degrees_of_Freedom, p_value_or_Bayes,
                                                  p_value_Report, Task2_Conclusion, Direction_of_Result, Same_Conclusion_as_Task1,
                                                  Additional_Calculations, Total_Hours, Task2_Comments))

# check number of analyses based on Paper_ID
Paper_ID_Count <- multi100_data %>% count(Paper_ID)
colnames(Paper_ID_Count) <- c("Paper_ID", "Count")

# use the 'tidyr' package to create a combined ID
library(tidyr)
multi100_data <- transform(multi100_data, Combined_ID = paste(Analyst_ID, Paper_ID, sep = "_"))

# add 'Analysis_Number' as a reference variable
multi100_data$Analysis_Number <- 1:nrow(multi100_data)


#### Merge Signup Variables ####

# there are 14 variables in all_people
# 1 - Timestamp 
# 2 - Signup Email
# 3 - First Name
# 4 - Surname
# 5 - Title
# 6 - Email to Avoid Spam
# 7 - University Research Institute
# 8 - Experience Conducting Analyses > 'I have experience conducting statistical analyses, reporting results, and documenting my work in code'
# 9 - Disclosure Agreement > 'Analysts in this project should not disclose any information to any other researcher until all analysts have submitted their analysis'
#                            'This means analysts are not allowed to share or discuss the methodology and results of their analyses with other re-analysts of the given dataset'
#                            'Researchers and peer evaluators agree to refrain from posting anything about the project via social media or in the public domain until the project is completed'
# 10 - Name Attached to Analysis > 'After submitting your analysis, your name will be linked to the submitted analysis and you will remain responsible for its content'
#                                  'Your email address will be used exclusively to communicate during the project'
#                                  'By filling out this form, you agree to the use of your email address to communicate during the project'
# 11 - Willingness to Peer Evaluate > 'In addition to co-analysts, we are looking for researchers to serve as evaluators of the submitted analyses'
#                                     'An evaluators job is to check the plausibility and sensibility of an analysis based on the summary of the analysis submitted by the analyst'
#                                     'Our expectation is that evaluators complete evaluations of 20 different analyses, and evaluators are eligible for a payment if they complete their task in time'
#                                     'Are you interested in serving as an evaluator for this project?'
# 12 - Analyst ID
# 13 - Comment
# 14 - Removed 

# rename variables
colnames(all_people)[2] <- "Signup_Email"
colnames(all_people)[3] <- "First_Name"
colnames(all_people)[4] <- "Last_Name"
colnames(all_people)[7] <- "University_Research_Institute"
colnames(all_people)[8] <- "Experience_Conducting_Analyses"
colnames(all_people)[9] <- "Disclosure_Agreement"
colnames(all_people)[10] <- "Name_Attached_to_Analysis"
colnames(all_people)[11] <- "Willing_to_Peer_Eval"
colnames(all_people)[12] <- "Analyst_ID"

# merge with multi100_data based on Analyst_ID
multi100_data <- merge(multi100_data, all_people, by = "Analyst_ID")

# select and re-order variables
multi100_data <- subset(multi100_data, select = c(Analyst_ID, Paper_ID, First_Name, Last_Name, Signup_Email, Task1_Email, Task2_Email, 
                                                  Task1_Timestamp, Task2_Timestamp, Current_Position, University_Research_Institute, 
                                                  Age, Gender, Education_Level, Country_of_Residence, Experience_Conducting_Analyses, 
                                                  Disclosure_Agreement, Name_Attached_to_Analysis, Willing_to_Peer_Eval, Analyst_Discipline, 
                                                  Keywords, Years_of_Experience, Analysis_Frequency, Expertise_Self_Rating, Familiar_with_Paper, 
                                                  Communication_Check, Task1_Software, Task1_Analysis_Report, Task1_Conclusion, 
                                                  Task1_Categorisation, Confidence_in_Approach, Data_Suitability, Task1_Comments, 
                                                  Task2_Software, Task2_Analysis_Report, Type_of_Statistic, Statistic_Report, Model_Sample_Size, 
                                                  Model_Degrees_of_Freedom, p_value_or_Bayes, p_value_Report, Task2_Conclusion, Direction_of_Result, 
                                                  Same_Conclusion_as_Task1, Additional_Calculations, Total_Hours, Task2_Comments,
                                                  Combined_ID, Analysis_Number))

#### Merge Original Paper Info ####

# rename variables in One_Hundred_Papers
colnames(One_Hundred_Papers)[1] <- "Original_Title"
colnames(One_Hundred_Papers)[2] <- "Paper_ID"
colnames(One_Hundred_Papers)[3] <- "Paper_Discipline"
colnames(One_Hundred_Papers)[4] <- "General_OSF_Link"
colnames(One_Hundred_Papers)[5] <- "Instructions_Given"
colnames(One_Hundred_Papers)[6] <- "Original_Claim"
colnames(One_Hundred_Papers)[7] <- "Original_Materials"
colnames(One_Hundred_Papers)[8] <- "Paper_Link"

# merge with multi100_data based on Paper_ID
multi100_data <- merge(multi100_data, One_Hundred_Papers, by = "Paper_ID")

# select and re-order variables
multi100_data <- subset(multi100_data, select = c(Analyst_ID, Paper_ID, First_Name, Last_Name, Signup_Email, Task1_Email, Task2_Email, 
                                                  Task1_Timestamp, Task2_Timestamp, Current_Position, University_Research_Institute, 
                                                  Age, Gender, Education_Level, Country_of_Residence, Experience_Conducting_Analyses, 
                                                  Disclosure_Agreement, Name_Attached_to_Analysis, Willing_to_Peer_Eval, Analyst_Discipline, 
                                                  Keywords, Years_of_Experience, Analysis_Frequency, Expertise_Self_Rating, Familiar_with_Paper, 
                                                  Communication_Check, Paper_Discipline, Original_Title, Original_Claim, Original_Materials, 
                                                  Paper_Link, General_OSF_Link, Task1_Software, Task1_Analysis_Report, Task1_Conclusion, 
                                                  Task1_Categorisation, Confidence_in_Approach, Data_Suitability, Task1_Comments, Instructions_Given,
                                                  Task2_Software, Task2_Analysis_Report, Type_of_Statistic, Statistic_Report, Model_Sample_Size, 
                                                  Model_Degrees_of_Freedom, p_value_or_Bayes, p_value_Report, Task2_Conclusion, Direction_of_Result, 
                                                  Same_Conclusion_as_Task1, Additional_Calculations, Total_Hours, Task2_Comments,
                                                  Combined_ID, Analysis_Number))

#### Merge Effect Size Info ####

# import +MasterSpreadsheet
MasterSpreadsheet <- read.csv("Desktop/Multi100 Analysis/+MasterSpreadsheet.csv")

# rename variables
colnames(MasterSpreadsheet)[2] <- "Paper_ID"
colnames(MasterSpreadsheet)[4] <- "Reproduction_Type_OSF"
colnames(MasterSpreadsheet)[5] <- "Reproduction_Result_OSF"
colnames(MasterSpreadsheet)[7] <- "Categorisation_of_Claim"
colnames(MasterSpreadsheet)[11] <- "In_Text_Original_Statistical_Result"
colnames(MasterSpreadsheet)[13] <- "Related_Original_Statistical_Result"
colnames(MasterSpreadsheet)[19] <- "Original_Result_Page_Number"

# merge with multi100_data
multi100_data <- merge(multi100_data, MasterSpreadsheet, by = "Paper_ID")

# import Multi-100_EffectSizes
# variables: Original_Type_of_Statistic, Original_Statistic_Report, Degrees_of_Freedom_1, Degrees_of_Freedom_2, Original_Model_Sample_Size, Comment_on_Reproduction_Materials
EffectSize_data <- read.csv("Desktop/Multi100 Analysis/Multi-100_EffectSizes.csv")

# rename variables
colnames(EffectSize_data)[1] <- "Paper_ID"
colnames(EffectSize_data)[3] <- "Original_Type_of_Statistic"
colnames(EffectSize_data)[4] <- "Original_Statistic_Report"
colnames(EffectSize_data)[5] <- "Degrees_of_Freedom_1"
colnames(EffectSize_data)[6] <- "Degrees_of_Freedom_2"
colnames(EffectSize_data)[7] <- "Original_Model_Sample_Size"
colnames(EffectSize_data)[9] <- "Comment_on_Reproduction_Materials"

multi100_data <- merge(multi100_data, EffectSize_data, by = "Paper_ID")

# select and re-order variables
multi100_data <- subset(multi100_data, select = c(Analyst_ID, Paper_ID, First_Name, Last_Name, Signup_Email, Task1_Email, Task2_Email, 
                                                  Task1_Timestamp, Task2_Timestamp, Current_Position, University_Research_Institute, 
                                                  Age, Gender, Education_Level, Country_of_Residence, Experience_Conducting_Analyses, 
                                                  Disclosure_Agreement, Name_Attached_to_Analysis, Willing_to_Peer_Eval, Analyst_Discipline, 
                                                  Keywords, Years_of_Experience, Analysis_Frequency, Expertise_Self_Rating, Familiar_with_Paper, 
                                                  Communication_Check, Paper_Discipline, Reproduction_Type_OSF, Reproduction_Result_OSF, 
                                                  Categorisation_of_Claim, Original_Title, Original_Claim, Original_Materials, 
                                                  In_Text_Original_Statistical_Result, Related_Original_Statistical_Result,
                                                  Original_Type_of_Statistic, Original_Statistic_Report, Degrees_of_Freedom_1,
                                                  Degrees_of_Freedom_2, Original_Model_Sample_Size, Original_Result_Page_Number,
                                                  Comment_on_Reproduction_Materials, Paper_Link, General_OSF_Link, Task1_Software, Task1_Analysis_Report, Task1_Conclusion, 
                                                  Task1_Categorisation, Confidence_in_Approach, Data_Suitability, Task1_Comments, Instructions_Given,
                                                  Task2_Software, Task2_Analysis_Report, Type_of_Statistic, Statistic_Report, Model_Sample_Size, 
                                                  Model_Degrees_of_Freedom, p_value_or_Bayes, p_value_Report, Task2_Conclusion, Direction_of_Result, 
                                                  Same_Conclusion_as_Task1, Additional_Calculations, Total_Hours, Task2_Comments,
                                                  Combined_ID, Analysis_Number))

# export a version of multi100_data without the peer evaluation info
write.csv(multi100_data, file = "Multi100_Big_Dataset_without_PeerEvals.csv")

#### Peer Evaluation Data ####

# import peer eval data
Peer_Evals <- read.csv("Desktop/Multi100 Analysis/Peer_Evals.csv")

# rename variables
colnames(Peer_Evals)[1] <- "PeerEval_Timestamp"
colnames(Peer_Evals)[2] <- "Evaluator_Email"
colnames(Peer_Evals)[3] <- "Evaluator_ID"
colnames(Peer_Evals)[4] <- "Paper_ID"
colnames(Peer_Evals)[5] <- "Analyst_ID"
colnames(Peer_Evals)[6] <- "Task1_Pipeline_Acceptable"
colnames(Peer_Evals)[7] <- "Why_is_Task1_Pipeline_Unacceptable"
colnames(Peer_Evals)[8] <- "Task1_Conclusion_Follows_Results"
colnames(Peer_Evals)[9] <- "Why_Does_Conclusion_Not_Follow_Results"
colnames(Peer_Evals)[10] <- "Task1_Categorisation_is_Accurate"
colnames(Peer_Evals)[11] <- "Task2_Pipeline_Acceptable"
colnames(Peer_Evals)[12] <- "Why_is_Task2_Pipeline_Unacceptable"
colnames(Peer_Evals)[13] <- "Any_Code_Mismatches"
colnames(Peer_Evals)[14] <- "Description_of_Mismatches"
colnames(Peer_Evals)[15] <- "Evaluator_Comments"

# add PeerEval_Submission_Number
Peer_Evals$PeerEval_Submission_Number <- 1:nrow(Peer_Evals)

# we pay attention to the first 500 peer evaluations only... remove the final 7 rows
Peer_Evals <- Peer_Evals[-c(501:507), ]

# remove 'OTHER' submission - evaluator did not provide the ID of the co-analyst and so should be removed for now
# this is submission number 459
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 459)

# create combined ID as in other datasets
library(tidyr)
Peer_Evals <- transform(Peer_Evals, Combined_ID = paste(Analyst_ID, Paper_ID, sep = "_"))

# create Eval_ID which concatenates Combined_ID and Evaluator_ID
Peer_Evals <- transform(Peer_Evals, Eval_ID = paste(Combined_ID, Evaluator_ID, sep = "_"))

# check that all Evaluator_ID's appear in multi100_data$Analyst_ID
matching_ids_3 <- Peer_Evals$Evaluator_ID %in% multi100_data$Analyst_ID
any_missing_ids_3 <- any(!matching_ids_3)
# if 'FALSE' then all ID's match

# check that all Analyst_ID's appear in multi100_data$Analyst_ID
matching_ids_4 <- Peer_Evals$Analyst_ID %in% multi100_data$Analyst_ID
any_missing_ids_4 <- any(!matching_ids_4)
# if 'FALSE' then all ID's match

# check that all Paper_ID's appear in multi100_data$Paper_ID
missing_p_ids_3 <- Peer_Evals$Paper_ID %in% multi100_data$Paper_ID
any_missing_p_ids_3 <- any(!missing_p_ids_3)
# if 'FALSE' then all ID's match

# check with setdiff that all of the Combined_ID's in PeerEvals appear in multi100_data
setdiff(Peer_Evals$Combined_ID, multi100_data$Combined_ID)
# 8 x Combined ID's

# AUX74_Bursztyn_JournPoliEco_2012_jaK4 (SN. 141): should be AUX74_Bursztyn_AmEcoRev_2017_VB9K
# UNX4S_Bursztyn_AmEcoRev_2017_VB9K (SN. 208): should be UNX4S_Bursztyn_JournPoliEco_2012_jaK4
# CCU09_Baillon_Econometrica_2018_QYNq (SN. 284): should be CC3UD_Baillon_Econometrica_2018_QYNq - analyst describes that the analyst does not provide their code. This is the case for CC3UD (similar to CCU09) on the Baillon paper.
# 8GS2H_Hurst_EvoHumanBehavior_2017_yypJ (SN. 333): should be 8GS27_Hurst_EvoHumanBehavior_2017_yypJ
# QPS9D_Bigoni_Econometrica_2015_VBx1 (two cases) (SN. 343, 352): should be QPSID_Bigoni_Econometrica_2015_VBx1 - both QPS9D and QPSID exist as separate analysts
# J3ODA_Sliwka_JournLabEco_2017_VDJV (SN. 350): should be JE3RS_Sliwka_JournLabEco_2017_VDJV - similar ID exists on sliwka OSF page... two other analysts evaluated J3ODA, both for a different paper (Nyhan)... evaluator here states that they found evidence for the null hypothesis, which is the case in the submission of JE3RS_Sliwka (and this is different to the two other evaluations)
# 0XXWZ_Kucik_BritJournPoliSci_2016_L22B (SN. 406): should be 0XXW0_Kucik_BritJournPoliSci_2016_L22B - there exists a 0XXWZ_BATESON, but on the Kucik page the ID is 0XXW0
# DAIUV_Jiang_AmJourPoliSci_2018_Rjp9 (two cases) (SN. 446, 451): should be DAILV_Jiang_AmJourPoliSci_2018_Rjp9 - DAIUV_Baillon exists, DAILV_Rovny exists and DAILV_2_Jiang exists (mislabelled on OSF)
# SN. 446 submitted the co-analysts ID as their own ID - it should be NSDML (based on the email address they provided)

# alter either the Analyst_ID or Paper_ID as appropriate 
Peer_Evals$Paper_ID[Peer_Evals$PeerEval_Submission_Number == 141] <- "Bursztyn_AmEcoRev_2017_VB9K"
Peer_Evals$Paper_ID[Peer_Evals$PeerEval_Submission_Number == 208] <- "Bursztyn_JournPoliEco_2012_jaK4"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 284] <- "CC3UD"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 333] <- "8GS27"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 343] <- "QPSID"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 352] <- "QPSID"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 350] <- "JE3RS"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 406] <- "0XXW0"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 446] <- "DAILV"
Peer_Evals$Evaluator_ID[Peer_Evals$PeerEval_Submission_Number == 446] <- "NSDML"
Peer_Evals$Analyst_ID[Peer_Evals$PeerEval_Submission_Number == 451] <- "DAILV"

# re-run code to produce Combined_ID and Eval_ID
# the code will not overwrite the existing variables, so need to remove before re-running code
library(dplyr)
Peer_Evals <- Peer_Evals %>% select(-Combined_ID)
Peer_Evals <- Peer_Evals %>% select(-Eval_ID)
library(tidyr)
Peer_Evals <- transform(Peer_Evals, Combined_ID = paste(Analyst_ID, Paper_ID, sep = "_"))
Peer_Evals <- transform(Peer_Evals, Eval_ID = paste(Combined_ID, Evaluator_ID, sep = "_"))

# verify that all Combined_ID's now appear in multi100_data
setdiff(Peer_Evals$Combined_ID, multi100_data$Combined_ID)

# remove duplicates
library(dplyr)
duplicated_peer_evals <- Peer_Evals %>%
  group_by(Eval_ID) %>%
  filter(n() > 1) %>%
  pull(Eval_ID) %>%
  unique()
print(duplicated_peer_evals)

# WCZJU_Luttrell_JournExpSocPsych_2016_rjb_EZI7J: !SN. 17
# MOYDE_Axt_JournExpSocPsych_2018_zK2_EZI7J: !SN. 45
# PFQSU_Miller_JournConflictRes_2011_zV1O_AMDFX: !SN. 103
# CC3UD_Baillon_Econometrica_2018_QYNq_AHW5W: !SN. 284
# 67DKH_Ihme_JournExpPoliSci_2018_xYbO_R08MV: !SN. 298
# 9U4RV_Hou_ChildDev_2017_YOXl_EZI7J: !SN. 355
# Q5U26_Wang_AmEcoJourn_2013_7d4J_SJAS8: !SN. 375
# GHN4R_Menaldo_AmJourPoliSci_2016_Vx4e_WANJC: !SN. 465, 466
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 17)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 45)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 103)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 284)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 298)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 355)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 375)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 465)
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 466)

# remove case where evaluator is also co-analyst (!SN. 462)
# WJVLO_Pastötter_Cognition_2013_EQxa_WJVLO
Peer_Evals <- subset(Peer_Evals, PeerEval_Submission_Number != 462)


#### Merge Peer Eval Data ####

# create new column in multi100_data which contains a count of peer evaluations per analysis
# count the number of times each Combined_ID appears in Peer_Evals
library(dplyr)
peer_eval_counts <- Peer_Evals %>%
  group_by(Combined_ID) %>%
  summarise(N_Peer_Evals = n())

# merge the counts back into multi100_data
multi100_data <- multi100_data %>%
  left_join(peer_eval_counts, by = c("Combined_ID" = "Combined_ID"))

# if there are Combined_IDs with no peer evaluations, replace missing values with 0
multi100_data$N_Peer_Evals[is.na(multi100_data$N_Peer_Evals)] <- 0


# create Eval_Count variable in Peer_Evals
library(dplyr)
Peer_Evals <- Peer_Evals %>%
  arrange(Combined_ID, PeerEval_Timestamp) %>%  # Arrange by Combined_ID and PeerEval_Timestamp
  group_by(Combined_ID) %>%
  mutate(Eval_Count = row_number()) %>%
  ungroup()

# rename Evaluator_ID
colnames(Peer_Evals)[3] <- "Evaluated_By"

# create subsets of Peer_Evals based on Eval_Count (1,2,3,4)
# then merge peer eval information, retaining empty columns

# first evals
first_evals <- Peer_Evals %>%
  filter(Eval_Count == 1)
# re-order variables
first_evals <- subset(first_evals, select = c(Combined_ID, Evaluated_By, Evaluator_Email, PeerEval_Timestamp, Task1_Pipeline_Acceptable,
                                              Why_is_Task1_Pipeline_Unacceptable, Task1_Conclusion_Follows_Results, Why_Does_Conclusion_Not_Follow_Results,
                                              Task1_Categorisation_is_Accurate, Task2_Pipeline_Acceptable, Why_is_Task2_Pipeline_Unacceptable,
                                              Any_Code_Mismatches, Description_of_Mismatches, Evaluator_Comments))
# merge with multi100_data
multi100_data <- merge(multi100_data, first_evals, by = "Combined_ID", all.x = TRUE)

# second evals
second_evals <- Peer_Evals %>%
  filter(Eval_Count == 2)
# rename variables
colnames(second_evals)[1] <- "PeerEval_Timestamp_2"
colnames(second_evals)[2] <- "Evaluator_Email_2"
colnames(second_evals)[3] <- "Evaluated_By_2"
colnames(second_evals)[6] <- "Task1_Pipeline_Acceptable_2"
colnames(second_evals)[7] <- "Why_is_Task1_Pipeline_Unacceptable_2"
colnames(second_evals)[8] <- "Task1_Conclusion_Follows_Results_2"
colnames(second_evals)[9] <- "Why_Does_Conclusion_Not_Follow_Results_2"
colnames(second_evals)[10] <- "Task1_Categorisation_is_Accurate_2"
colnames(second_evals)[11] <- "Task2_Pipeline_Acceptable_2"
colnames(second_evals)[12] <- "Why_is_Task2_Pipeline_Unacceptable_2"
colnames(second_evals)[13] <- "Any_Code_Mismatches_2"
colnames(second_evals)[14] <- "Description_of_Mismatches_2"
colnames(second_evals)[15] <- "Evaluator_Comments_2"
# re-order variables
second_evals <- subset(second_evals, select = c(Combined_ID, Evaluated_By_2, Evaluator_Email_2, PeerEval_Timestamp_2, Task1_Pipeline_Acceptable_2,
                                              Why_is_Task1_Pipeline_Unacceptable_2, Task1_Conclusion_Follows_Results_2, Why_Does_Conclusion_Not_Follow_Results_2,
                                              Task1_Categorisation_is_Accurate_2, Task2_Pipeline_Acceptable_2, Why_is_Task2_Pipeline_Unacceptable_2,
                                              Any_Code_Mismatches_2, Description_of_Mismatches_2, Evaluator_Comments_2))
# merge with multi100_data
multi100_data <- merge(multi100_data, second_evals, by = "Combined_ID", all.x = TRUE)

# third evals
third_evals <- Peer_Evals %>%
  filter(Eval_Count == 3)
# rename variables
colnames(third_evals)[1] <- "PeerEval_Timestamp_3"
colnames(third_evals)[2] <- "Evaluator_Email_3"
colnames(third_evals)[3] <- "Evaluated_By_3"
colnames(third_evals)[6] <- "Task1_Pipeline_Acceptable_3"
colnames(third_evals)[7] <- "Why_is_Task1_Pipeline_Unacceptable_3"
colnames(third_evals)[8] <- "Task1_Conclusion_Follows_Results_3"
colnames(third_evals)[9] <- "Why_Does_Conclusion_Not_Follow_Results_3"
colnames(third_evals)[10] <- "Task1_Categorisation_is_Accurate_3"
colnames(third_evals)[11] <- "Task2_Pipeline_Acceptable_3"
colnames(third_evals)[12] <- "Why_is_Task2_Pipeline_Unacceptable_3"
colnames(third_evals)[13] <- "Any_Code_Mismatches_3"
colnames(third_evals)[14] <- "Description_of_Mismatches_3"
colnames(third_evals)[15] <- "Evaluator_Comments_3"
# re-order variables
third_evals <- subset(third_evals, select = c(Combined_ID, Evaluated_By_3, Evaluator_Email_3, PeerEval_Timestamp_3, Task1_Pipeline_Acceptable_3,
                                              Why_is_Task1_Pipeline_Unacceptable_3, Task1_Conclusion_Follows_Results_3, Why_Does_Conclusion_Not_Follow_Results_3,
                                              Task1_Categorisation_is_Accurate_3, Task2_Pipeline_Acceptable_3, Why_is_Task2_Pipeline_Unacceptable_3,
                                              Any_Code_Mismatches_3, Description_of_Mismatches_3, Evaluator_Comments_3))
# merge with multi100_data
multi100_data <- merge(multi100_data, third_evals, by = "Combined_ID", all.x = TRUE)


# fourth evals
fourth_evals <- Peer_Evals %>%
  filter(Eval_Count == 4)
# rename variables
colnames(fourth_evals)[1] <- "PeerEval_Timestamp_4"
colnames(fourth_evals)[2] <- "Evaluator_Email_4"
colnames(fourth_evals)[3] <- "Evaluated_By_4"
colnames(fourth_evals)[6] <- "Task1_Pipeline_Acceptable_4"
colnames(fourth_evals)[7] <- "Why_is_Task1_Pipeline_Unacceptable_4"
colnames(fourth_evals)[8] <- "Task1_Conclusion_Follows_Results_4"
colnames(fourth_evals)[9] <- "Why_Does_Conclusion_Not_Follow_Results_4"
colnames(fourth_evals)[10] <- "Task1_Categorisation_is_Accurate_4"
colnames(fourth_evals)[11] <- "Task2_Pipeline_Acceptable_4"
colnames(fourth_evals)[12] <- "Why_is_Task2_Pipeline_Unacceptable_4"
colnames(fourth_evals)[13] <- "Any_Code_Mismatches_4"
colnames(fourth_evals)[14] <- "Description_of_Mismatches_4"
colnames(fourth_evals)[15] <- "Evaluator_Comments_4"
# re-order variables
fourth_evals <- subset(fourth_evals, select = c(Combined_ID, Evaluated_By_4, Evaluator_Email_4, PeerEval_Timestamp_4, Task1_Pipeline_Acceptable_4,
                                                Why_is_Task1_Pipeline_Unacceptable_4, Task1_Conclusion_Follows_Results_4, Why_Does_Conclusion_Not_Follow_Results_4,
                                                Task1_Categorisation_is_Accurate_4, Task2_Pipeline_Acceptable_4, Why_is_Task2_Pipeline_Unacceptable_4,
                                                Any_Code_Mismatches_4, Description_of_Mismatches_4, Evaluator_Comments_4))
# merge with multi100_data
multi100_data <- merge(multi100_data, fourth_evals, by = "Combined_ID", all.x = TRUE)


#### Export Big Dataframe ####
write.csv(multi100_data, file = "Multi100_Big_Dataset.csv")

# hello world
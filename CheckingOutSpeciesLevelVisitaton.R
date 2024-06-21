summed_count_X_FNum <- read_csv("/Users/jenanel-hifnawi/Downloads/DriveDownload/AAADataEntry/AnalysisTroubleshooting/4_CorrectingBeeCount/summed_count_X_FNum.csv", name_repair = "universal")

FlowerTable <- read_csv("/Users/jenanel-hifnawi/Downloads/DriveDownload/AAADataEntry/AnalysisTroubleshooting/!!!FinalDataFiles/FlowerTables/FinalFlowerTable_MaxID_29SEP23.csv", name_repair = "universal")


CupheaTest <- FlowerTable %>%
  merge(summed_count_X_FNum, by= c("SurveyNum", "FNum"), all.x = TRUE, ) %>%
 # filter(grepl("Cuphea", FSpecies, ignore.case = TRUE)) %>%
  summarise(.by = FSpecies, 
            NumberofSurveys = n_distinct(SurveyNum),
            SumofCount = sum(Count, na.rm = TRUE))

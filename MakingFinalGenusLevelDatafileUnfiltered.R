#### creating the final flower tables ####

#I noticed that I had filtered out 0 bee surveys prior to creating the rankavgfloral rank. This is fine for bumble bee analysis, but I need a version of the flower table which is final (ie. all dupes resolved, Reranked floral rank) but COMPLETELY unfiltered, as I can do all filtering later on! 



##### here I confirm that the genusleveldatafileunfiltered is exactly what I need, but without the 0 bee survey filter applied! This is proven true as there are 716 rows present in the unfiltered one, which is the correct number of rows corresponding to surveys with 0 bumbles! 

genusleveldatafileunfiltered <-
  read_csv("/Users/jenanel-hifnawi/Downloads/DriveDownload/AAADataEntry/AnalysisTroubleshooting/11_Oops_ForgotToRemoveSurveysWithNoBees/AABBGenusLevelDataTableBeforeRemovingNoBeeSurveys.csv",
name_repair = "universal")


checkingdifferentrows1 <- genusleveldatafileinprogress %>%
  select(!RankAvgFloralRank:CorrRankAvgFlRank)


checkingdifferentrows <- anti_join(genusleveldatafileunfiltered, checkingdifferentrows1)

finalgenusleveldatafileunfiltered <-
  genusleveldatafileunfiltered %>%
  dplyr::group_by(SurveyNum) %>%
  dplyr::mutate("RankAvgFloralRank" = rank(FloralRank)) %>%
  dplyr::mutate("TestCorrFlRank" = FloralRank * 10/max(FloralRank)) %>%
  dplyr::mutate("CorrRankAvgFlRank" = RankAvgFloralRank * 10/max(RankAvgFloralRank))%>%
  dplyr::select(SurveyNum:corrFloralRank, #just reordering columns
                RankAvgFloralRank,
                TestCorrFlRank,
                CorrRankAvgFlRank,
                BBcount,
                CorrBBCount)

write_csv(finalgenusleveldatafileunfiltered, 
          here("data",
               "cleaned.data",
               "FinalGenusLevelDatafileUnfiltered"))


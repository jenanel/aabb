library(dplyr)
library(purrr)

######## Verifying that the loop is all correct by adding the same filters as in the "Bombus" (genus level for bees) analysis and then checking that the AllBombus analysis is identical to the one where I manually combined all Bombus species.

cleanedgenusleveldatasetTEST <- 
  genusleveldatafileinprogress %>%
  full_join(genustofamilytable, by = "FGenus") %>%
  left_join(CleanWideBeeTable, by = c("SurveyNum", "FGenus")) %>%
  mutate(across(AllBombus:Bombus.terricola, ~replace(., is.na(.), 0))) %>%
  group_by(SurveyNum) %>% #in the next line I sum count on duplicated genera
  mutate(across(AllBombus:Bombus.terricola, ~ sum(.), .names = "sum_{.col}")) %>%
  ungroup() %>%
  left_join(rawsurveyconditionstable, by = "SurveyNum") %>%
  dplyr::group_by(SurveyNum)  %>%
  dplyr::filter(sum(BBcount) > 4) %>% #Remove surveys with under 5 bees
  dplyr::mutate("SumofBBcount" = sum(BBcount)) %>% #to verify that the filter worked
  dplyr::mutate("NumberofFlowers" = n()) %>% #to verify that the filter worked
  dplyr::filter(NumberofFlowers > 4) %>% #survey 88 should no longer be present
  dplyr::ungroup() %>%
  relocate(ID:Longitude, .before = SurveyNum) %>%
  filter(!grepl("NoFRHASBEES", ErrorCode)) %>% # removes surveys with that error code
  dplyr::add_count(FGenus, name = "FlowerSampleSize") 
  

###### 
old version: 
cleanedgenusleveldataset <- 
  genusleveldatafileinprogress %>%
  full_join(genustofamilytable, by = "FGenus") %>%
  merge(CleanWideBeeTable, by = c("SurveyNum", "FGenus"), all.x = "TRUE") %>%
  replace(is.na(.), 0) %>%
  group_by(SurveyNum) %>% #in the next line I sum count on duplicated genera
  mutate(across(AllBombus:Bombus.terricola, ~ sum(.), .names = "sum_{.col}")) %>%
  ungroup() %>%
  left_join(rawsurveyconditionstable, by = "SurveyNum") %>%
  relocate(ID:Longitude, .before = SurveyNum) %>%
  filter(!grepl("NoFRHASBEES", ErrorCode)) %>% # removes surveys with that error code
  filter()
#old version
#####

cleanedgenusleveldatasetOGFiltersRemoved <- #returns 11542 rows, math predicts 11542! 
  genusleveldatafileinprogress %>%
  full_join(genustofamilytable, by = "FGenus") %>%
  dplyr::left_join(rawsurveyconditionstable, by = "SurveyNum") %>%
  dplyr::mutate(CorrRankAvgFlRankXCorrBBCount = CorrRankAvgFlRank * CorrBBCount) %>%
  dplyr::mutate(SeasonalChunk = case_when(
    month %in% c(2, 3, 4, 5) ~ 1,
    month %in% c(6, 7, 8) ~ 2,
    month %in% c(9, 10, 11) ~ 3)) %>%
  dplyr::select(ID:ErrorCode,
                SeasonalChunk,
                SurveyNum,
                Family,
                FGenus:RankAvgFloralRank,
                CorrRankAvgFlRank:CorrBBCount,
                CorrRankAvgFlRankXCorrBBCount) %>%
 # dplyr::filter(!grepl("NoFRHASBEES", ErrorCode)) %>% # removes surveys with that error code
  dplyr::add_count(FGenus, name = "FlowerSampleSize")%>%
  dplyr::group_by(SurveyNum)  %>%
 # dplyr::filter(sum(BBcount) > 4) %>% #Remove surveys with under 5 bees
  dplyr::mutate("SumofBBcount" = sum(BBcount)) %>% #to verify that the filter worked
  dplyr::mutate("NumberofFlowers" = n()) %>% #to verify that the filter worked
  #dplyr::filter(NumberofFlowers > 4) %>% #survey 88 should no longer be present
  dplyr::ungroup()




#| label: preparing filtered datasets for pilot analysis

#Note that this requires the beespeciesdatasubsets_list item, which is created in the Fowler adapted Harmon-Threatt model code chunk. 


# Specify the additional columns you want to select
additional_columnsTEST  <- names(dplyr::select(cleanedgenusleveldatasetTEST,
                                           ID:Family))

######################

# Iterate through the list and extract the relevant species column
resultTEST <- lapply(names(beespeciesdatasubsets_list), function(df_name) {
  # Extract the column name without the ".filtered_data" suffix
  column_name <- sub("\\.filtered_data$", "", df_name)
  
  # concatonate your additional column and species columns
  selected_columns <- c(additional_columns, column_name)
  
  # Extract the specified columns
  selected_data <- beespeciesdatasubsets_list[[df_name]][, selected_columns, drop = FALSE]
  
  return(selected_data)
})


######################


#renaming the dataframes within the list
colnames <- lapply(names(beespeciesdatasubsets_list), function(df_name) {
  # Extract the column name without the ".filtered_data" suffix
  column_name <- sub("\\.filtered_data$", "", df_name)
  return(column_name)
})

renamed_filtered_dataframesTEST <- setNames(resultTEST, colnames)


######################
# Create and apply a function to rename the 23rd column to "Count"

# Create the function 
rename_column <- function(dataset) {
  colnames(dataset)[23] <- "Count"
  return(dataset)
}

# Apply the function to each dataset in the list
dataframesforpilotanalysisTEST <- lapply(renamed_filtered_dataframesTEST, rename_column)

######################
# Create the SumofCount, PropBees, and BScore columns! 

finaldataframesforpilotanalysisTEST <- map(
  dataframesforpilotanalysisTEST,
  function(df) df %>%
    group_by(SurveyNum) %>%
    mutate(SumofCount = sum(Count)) %>%
    ungroup() %>%
    mutate(PropBees = Count /SumofCount) %>%
    mutate(BeeScore = PropBees * CorrRankAvgFlRank) 
)


# Plug in the list of final datasheets into the pilot analysis table

finalpilotanalysistablesTEST <- map(finaldataframesforpilotanalysisTEST, generalpilotanalysistable)


##############
# Function to rename the 23rd column to "Count" and add a new column "SumofCount"
rename_and_add_sum <- function(dataset) {
  colnames(dataset)[23] <- "Count"
  
  # Calculate sum by grouping SurveyNum
  dataset$SumofCount <- ave(dataset$Count, dataset$SurveyNum, FUN = sum)
  
  return(dataset)
}

# Apply the function to each dataset in the list
list_of_datasets_modified <- lapply(renamed_filtered_dataframes, rename_and_add_sum)





rename_and_add_sum <- function(dataset) {
  dataset %>%
    rename(Count = .[[23]]) %>%
    group_by(SurveyNum) %>%
    mutate(SumofCount = sum(Count)) %>%
    ungroup()
}

list_of_datasets_modified <- map(renamed_filtered_dataframes, rename_and_add_sum)


# Print the list of datasets with the renamed column
print(list_of_datasets_renamed)

######################

# Function to rename the 23rd column to "Count" and add a new column "SumofCount"
add_sum <- function(dataset) {
  list_of_datasets_renamed %>%
    group_by(SurveyNum) %>%
    mutate(SumofCount = sum(Count)) %>%
    ungroup()
}

# Apply the function to each dataset in the list
list_of_datasets_modified <- lapply(list_of_datasets_renamed, add_sum)





# Function to calculate corrected bee count for each bee species
calculate_correlation <- function(data, y) {
  data %>%
    group_by(SurveyNum) %>%
    mutate(across(all_of(y), ~ ./sum(.))) %>%
    select(SurveyNum, starts_with("Corr")) %>%
    pivot_longer(cols = starts_with("Corr"), names_to = "BeeSpecies", values_to = "CorrValue")
}

calculate_correlation <- function(data, y) {
  data %>%
    group_by(SurveyNum) %>%
    mutate(across(all_of(y), ~ ./sum(.))) %>%
    select(SurveyNum, starts_with("Corr")) %>%
    pivot_longer(cols = starts_with("Corr"), names_to = "BeeSpecies", values_to = "CorrValue")
}

data <- cleanedgenusleveldataset
cont.y.var <- names(dplyr::select(cleanedgenusleveldataset,
                                  AllBombus:Bombus.terricola))

calculate_correlation(cleanedgenusleveldataset, cont.y.var[i])



# Function to perform the general analysis for each bee species
general_pilot_analysis_table <- function(data, y) {
  data %>%
    group_by(FGenus) %>%
    summarise(
      NumberSurveys = n(),
      Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
      across(all_of(y), list(AvgCBcount = ~ mean(.),
                             AvgCorrCBcount = ~ mean(CorrValue),
                             CBScore.CorrFlRankXCorrCBcount = ~ mean(CorrRankAvgFlRank * CorrValue)),
             .names = "{col}_{fn}")
    ) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    arrange(desc(CBScore_CorrFlRankXCorrCBcount))
}

# Loop through each bee species in the variable_names vector
results_list <- map(variable_names, ~ calculate_correlation(cleanedgenusleveldataset, .x)) %>%
  map(~ general_pilot_analysis_table(cleanedgenusleveldataset, .))

# The results_list now contains the analysis for each bee species




library(dplyr)
library(purrr)

# Function to perform the general analysis for each bee species
general_pilot_analysis_table <- function(data, y) {
  data %>%
    group_by(FGenus) %>%
    summarise(
      NumberSurveys = n(),
      Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
      across(all_of(y), list(
        AvgCBcount = ~ mean(.),
        AvgCorrCBcount = ~ mean(get(paste0("sum_", cur_column()))),
        CBScore.CorrFlRankXCorrCBcount = ~ mean(CorrRankAvgFlRank * get(paste0("sum_", cur_column())))
      ), .names = "{col}_{fn}")
    ) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    arrange(desc(CBScore_CorrFlRankXCorrCBcount))
}

# Loop through each bee species in the variable_names vector
results_list <- map(variable_names, ~ general_pilot_analysis_table(cleanedgenusleveldataset, .))

# The results_list now contains the analysis for each bee species

###############################################

####BELOW MAYBE WORKED?!!! IT CREATED A  OUTPUT TABLE, NEED TO CHECK THAT VALUES ARE CORRECT!!  THEY ARE NOT CORRECT LOL NEED TO FILTER FIRST! WILL PLUG IN A LIST OF FILTERED DATAFRAMES!

generalpilotanalysistable <- function(data, y) {
  result <- data %>%
    group_by(FGenus) %>%
    summarise(
      NumberSurveys = n(),
      Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
      across(y, ~ mean(.), .names = "Avg_{.col}_count")
    )
  
  return(result)
}

# Example usage
your_data <- cleanedgenusleveldataset  # Replace with your actual data
your_y_vars <-  variable_names # Replace with your variable names

result <- generalpilotanalysistable(your_data, your_y_vars)

generalpilotanalysistable(cleanedgenusleveldataset, cont.y.var[i])


###############################################

# Assuming data_list is a list of data frames, and y_column is the column name you want to analyze

your_data_list <- beespeciesdatasubsets_list

process_data <- function(data_list) {
  results_list <- map(data_list, ~ {
    data <- .
    col_name <- names(data)[2]  # Assuming the relevant column is the second column
    
    data %>%
      group_by(FGenus) %>%
      summarise(
        NumberSurveys = n(),
        Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
        AvgCount = mean(data[[col_name]]),
        AvgCorrCount = mean(CorrCBcount)
      )
  })
  
  return(results_list)
}

###############################################


process_data <- function(data_list) {
  results_list <- map(data_list, ~ {
    data <- .
    species_name <- gsub("\\.filtered_data", "", gsub(".*\\.", "", deparse(substitute(data))))
    col_name <- paste0(species_name)  
    
    data %>%
      group_by(FGenus) %>%
      summarise(
        NumberSurveys = n(),
        Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
        AvgCount = mean(data[[col_name]]),
        AvgCorrCount = mean(CorrCBcount)
      )
  })
  
  return(results_list)
}

results <- process_data(your_data_list)

###############################################

library(dplyr)
library(purrr)

process_data <- function(data_list) {
  results_list <- map(data_list, ~ {
    data <- .
    species_name <- gsub("\\.filtered_data", "", gsub(".*\\.", "", deparse(substitute(data))))
    col_name <- paste0(species_name)  # Assuming the species name is repeated in the column name
    
    data %>%
      group_by(FGenus) %>%
      summarise(
        NumberSurveys = n(),
        Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
        AvgCount = mean(select(data, col_name)),
        AvgCorrCount = mean(CorrCBcount)
      )
  })
  
  return(results_list)
}


results <- process_data(your_data_list)







library(dplyr)
library(purrr)

process_data <- function(data_list) {
  results_list <- map(data_list, ~ {
    data <- .
    species_name <- gsub("\\.filtered_data", "", gsub(".*\\.", "", deparse(substitute(data))))
    col_name <- paste0(species_name)  # Assuming the species name is repeated in the column name
    
    if (col_name %in% names(data)) {
      data %>%
        group_by(FGenus) %>%
        summarise(
          NumberSurveys = n(),
          Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
          AvgCount = mean(select(data, col_name)),
          AvgCorrCount = mean(CorrCBcount)
        )
    } else {
      message(paste("Column", col_name, "not found in dataframe. Skipping."))
      NULL  # Return NULL for cases where the column is not found
    }
  })
  
  return(results_list)
}

results <- process_data(your_data_list)





library(dplyr)

process_data <- function(data_list) {
  results_list <- lapply(data_list, function(data) {
    species_name <- gsub("\\.filtered_data", "", gsub(".*\\.", "", deparse(substitute(data))))
    col_name <- paste0(species_name)  # Assuming the species name is repeated in the column name
    
    result <- data %>%
      group_by(FGenus) %>%
      summarise(
        NumberSurveys = n(),
        Avg.Corr.FloralRank = mean(CorrRankAvgFlRank),
        AvgCount = mean(mutate(data, col = !!sym(col_name))$col),
        AvgCorrCount = mean(CorrCBcount)
      )
    
    return(result)
  })
  
  return(results_list)
}


results <- process_data(your_data_list)




#I have a list of datasets, each representing a different bee species. While each #dataset contains a different set of rows, they all include the same exact #columns#; this means that every dataset has a column for every bee species.  I #want to #iterate through each dataset, and delete all of the bee species columns #, except #for the single bee species column which is referred to by the name of #the dataset#. The columns to delete are any column beginning with the string "Bombus


# I have a series of dataframes in R in a list in R, with each dataframe representing a different bee species. The dataframes all are derived from the same parent dataframe which contains information on many species, but they have been filtered differently to only select rows which are relevant to a single species. While each dataframe contains a different set of rows, they all include the exact same columns. This means that every dataset has a column for every bee species, even though only one of those species columns is relevant in each dataframe. The species which is relevant in each dataframe is referred to in the dataframe name.   I want to iterate through each dataframe, and select the only the columns which contain the species name referred to in the respective dataframes name. 


# The naming conventions involved are as follows:
# 
# Dataframes in the list are named with the convention: 
# "[genus.species].filtered_data" 
# (e.g. "Bombus.impatiens.filtered_data" is one dataframe, another is Bombus.auricomus.filtered_data")
# 
# Columns are named with the convention: 
# "[genus.species] "
# (e.g. "Bombus.impatiens", and "Bombus.auricomus" respectively. )
# 
# 
# #I have a list of datasets, each representing a different bee species. I want to #iterate through this list of dataframes in R. In order to do so, I need to #select a column name, which is a modified version of the datasheet name. More #specifically, the datasheets are named with the convention filtered [genus##.species].filtered_data (e.g. "Bombus.impatiens.filtered_data" is one dataframe, #another is Bombus.auricomus.filtered_data"), and the columns are simply named [genus.species] (e.g. "Bombus.impatiens", and "Bombus.auricomus" respectively. )
# 
# How can i create a vector of column names?


#####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##### strategy change! Now I want to do the filtering within the loop...

dat <- cleanedgenusleveldataset
cont.y.var <- names(dplyr::select(cleanedgenusleveldataset,
                                  AllBombus:Bombus.terricola))
# a loop to produce the filtered datasets
for (i in seq_along(cont.y.var)) {
  
  # Create the file and assign them to objects named such that we can retrieve all of the ones we want later.
  
  assign(
    paste0(cont.y.var[i],".filtered_data"),  
    
    # Generate the contents of the objects using our function.
    {
      # Extract the variable name without the "sum_" prefix
      var_name <- sub("^sum_", "", cont.y.var[i])
      
      # Filter the dataset based on the condition
      filtered_dat <- dat %>%
        filter(get(paste0("sum_", var_name)) > 0)
      
      # Return the filtered dataset
      filtered_dat
    }
  )
}
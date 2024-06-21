## Taxize playing! 

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
              taxize,
               here,
               usethis,
               update = FALSE)

data <- read_csv(here("data",
                      "cleaned.data",
                      "FinalGenusLevelDataFileWithSurveyConditionsInfoEvenBetter.csv"))

#OPEN RENVIRON FILE AND RUN LINE WITH THE THE ENTREZ_KEY


#### lets just prep the data in a way to minimize the number of queries per search ####

#create a vector of unique genera 

generavector.over10 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize > 10) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.over10.withfamily <- generavector.over10 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)


generavector.exactly10 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 10) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly10.withfamily <- generavector.exactly10 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

##now 9!

generavector.exactly9 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 9) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly9.withfamily <- generavector.exactly9 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

#now 8!

generavector.exactly8 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 8) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly8.withfamily <- generavector.exactly8 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

#7

generavector.exactly7 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 7) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly7.withfamily <- generavector.exactly7 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)


#6

generavector.exactly6 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 6) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly6.withfamily <- generavector.exactly6 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

generavectorwithfamily.6ormoresurveys <- read_csv(here(
  "data",
  "cleaned.data",
  "generavectorwithfamily.6ormoresurveys.csv"
))

#5

generavector.exactly5 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 5) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly5.withfamily <- generavector.exactly5 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

#4

generavector.exactly4 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 4) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly4.withfamily <- generavector.exactly4 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)


#3

generavector.exactly3 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 3) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly3.withfamily <- generavector.exactly3 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)


#2

generavector.exactly2 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 2) %>%
  dplyr::summarise(.by = Genus,
                   n = n())

generavector.exactly2.withfamily <- generavector.exactly2 %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)


#1 !!!! OMG

generavector.exactly1 <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::select(-Species) %>%
  dplyr::filter(FlowerSampleSize == 1) %>%
  dplyr::summarise(.by = Genus,
                   n = n()) %>%
  dplyr::arrange(Genus)


#Split up genera only present on one survey into two smaller vectors arbitrarily!

#first the top 60 rows, A through I
generavector.exactly1.AthroughI <- head(generavector.exactly1, 60)

generavector.exactly1.AthroughI.withfamily <- generavector.exactly1.AthroughI %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

#now the last 79 records, J through Z
generavector.exactly1.JthroughZ <- tail(generavector.exactly1, 79)

#Lets split this J-Z vector up even more

#J-R
generavector.exactly1.JthroughR <- head(generavector.exactly1.JthroughZ, 46)

generavector.exactly1.JthroughR.withfamily <- generavector.exactly1.JthroughR %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

#S-Z
generavector.exactly1.SthroughZ <- tail(generavector.exactly1.JthroughZ, 33)

generavector.exactly1.SthroughZ.withfamily <- generavector.exactly1.SthroughZ %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family)

#### Now bring them back together and create final vector with all genera!!! ####

generavectorwithfamily.exactly1.Jthroughz <-
  bind_rows(generavector.exactly1.JthroughR.withfamily,
            generavector.exactly1.SthroughZ.withfamily)

generavectorwithfamily.2ormoresurveysandsomeof1 <- 
  read_csv(here("data",
                "cleaned.data",
                "generavectorwithfamily.2ormoresurveysandsomeof1.csv"))

generavectorwithfamily <- 
  bind_rows(generavectorwithfamily.2ormoresurveysandsomeof1,
            generavectorwithfamily.exactly1.Jthroughz)


write_csv(generavectorwithfamily,
         here("data",
              "cleaned.data",
              "genustofamilytable.csv"))



#### THIS WORKS PERFECTLY!!! ####

cleandatawithfamily <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                   sep = " ", 
                   remove = TRUE) %>%
  dplyr::filter(SurveyNum == 2) %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family) %>%
  dplyr::select(-Species)
  
#### THIS WORKS FOR A WHILE THEN FAILS - API ISSUE?? ####

cleandatawithfamily <- data %>%
  tidyr::separate(FGenus, 
                  c("Genus", "Species"), 
                  sep = " ", 
                  remove = TRUE) %>%
  dplyr::mutate(Family = tax_name(Genus, get = 'family', db = 'ncbi')$family) %>%
  dplyr::select(-Species)

cleandata <- data %>%
  tidyr::separate(FGenus,
                  c("Genus", "Species"),
                  sep = " ",
                  remove = TRUE) %>%
  dplyr::filter(SurveyNum %in% c(1:10))
  

 familynamevector <-  taxize::tax_name(cleandata$Genus, get = 'family', db = 'ncbi')
 
 
 
 #### TAXIZEDB TESTING ####
 
 # data source: NCBI
 db_download_ncbi()
 src <- src_ncbi()
 df <- tbl(src, "names")
 filter(df, name_class == "scientific name")

 cleandatawithfamily <- data %>%
   tidyr::separate(FGenus, 
                   c("Genus", "Species"), 
                   sep = " ", 
                   remove = TRUE) %>%
   dplyr::filter(SurveyNum == 2) %>%
   dplyr::mutate(Family = taxizedb::tax_name(Genus, get = 'family', db = 'ncbi')$family) %>%
   dplyr::select(-Species)

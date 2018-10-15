require(tidyverse)
require(qualtRics)
library(naniar)

#bs <- readSurvey("/Volumes/GoogleDrive/My Drive/SI/DataScience/Misc/Blue Star Families/Qualtrics Data/Blue_Star_Survey (September 2018).csv")
 setwd("Desktop")
 bs <- readSurvey("Blue_Star_Survey (September 2018).csv")


#Filter out all the incompleted/gated responses
bs <- bs %>%
  add_prop_miss() %>%
  filter(Q84 == "San Diego" | Q84 == "New York City") %>%
  filter(!(Q85 == "No, I am not now nor have I ever been a member of the US military" & Q65 == "No")) %>%
  filter(Status != "Survey Preview") %>%
  filter(Progress > 80)

#Quantity of Social Connections (C1-C19): create friendScore
 replace_with_na_at(data = bs,
                     .vars = c("C5", "C3"),
                      condition = ~.x == -99)
 
bs$C3 <-as.numeric(as.character(bs$C3))
bs$C5 <-as.numeric(as.character(bs$C5))

bs %>% replace_with_na_all(~.x == -99)

bs %>%
  dplyr::mutate_at(vars(C3, C5), funs(.score = case_when(.==-99 ~ 10,
                                         TRUE ~ .)))

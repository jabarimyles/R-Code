library(tidyverse)
library(dplyr)
library(purrr)

Jobs <- read.csv('/Users/jabarimyles/Documents/ASU Advisory Board/Job descriptions for All employers.csv')

count_it_up <- function(data) {
  browser()
  counts <- data.frame(table(unlist(strsplit(tolower(data$Description), " "))))
  return(counts)
}



#Jobs$Description <-  gsub(",","",Jobs$Description)
#Jobs$Description <- gsub(".","",Jobs$Description) 
#Jobs$Description <- gsub(")","",Jobs$Description)

Jobs %>% 
  #group_by(Occupation.Name) %>% 
  mutate(counts = map_dfc(Jobs,count_it_up))



data.frame(table(Jobs$Occupation.Name, unlist(strsplit(tolower(Jobs$Description), " "))))

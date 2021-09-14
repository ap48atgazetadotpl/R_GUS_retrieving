library(tidyr)
library(dplyr)
library(knitr)
library(readxl)
library(readr)
library(lubridate)
library(tidyverse)
library(scales)
library(forcats)
library(webshot)

library(rjson)
library(RCurl)

source("retrieving_functions/units.R")
source("global/functions_global.R")
source("Data/key_gen.rds")

urls <- list()

#194828 - forests 

links <-  list()


for (k in 1:91) {urls <- paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/194828?format=json&page-size=30&unit-parent-id=0", codes[[k]], "&unit-level=6")
                links <<- rbind(links, urls)
}


df_total_result2 <- data.frame()


retrieve <- function(link) 

  {
  
  for (m in 1:91) 
    
      {
    
        link <- URLencode(links[[m]])
        sampledatagus2 <- fromJSON(getURL(links[[m]]))
        glimpse(sampledatagus2)
  
        id <- sampledatagus2$results[[1]]$id
        name <- sampledatagus2$results[[1]]$name
  
          for (j in 1:19){
 
              year <- sampledatagus2$results[[1]]$values[[j]]$year
              val <- sampledatagus2$results[[1]]$values[[j]]$val
  
              result2 <- data.frame(id, name, year, val)
              dfresult2 <- data.frame(result2)
              df_total_result2 <<- rbind(df_total_result2,dfresult2)

  }  

}

}


# Retrieve data from GUS BDL throughot API for units in codes' catalogue 

retrieve(links)

# Correcting names for unit with the same but different id's

func_correct_name ()

# Write results to .rds

write_rds(df_total_result2, "Data/forests.rds")

# Identify gmina(s), which data has been not retrieved

forests <- read_rds("Data/forests.rds")
retrieved <- as.data.frame(unique(forests$name))
names(retrieved)[1] <- c("name")
template <- read_rds("Data/key_gen.rds")
template <- as.data.frame(unique(template$name))
names(template)[1] <- c("name")
missing <- anti_join(template, retrieved) %>% select ('name') %>% view() 

func_handle_missing (missing)
 
    
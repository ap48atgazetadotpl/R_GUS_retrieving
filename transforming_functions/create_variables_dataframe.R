library(tidyr)
library(dplyr)
library(stargazer)
library(kableExtra)
library(knitr)
library(readxl)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(scales)
library(viridis)
library(plotly)
library(hrbrthemes)
library(forcats)
library(webshot)

source("global/functions_global.R")
source("transforming_functions/create_variables.R")


##  1.4. Preparation of dataframe integrating all variables data from p. 1.3. 

### Data integration

variables_dataframe <- func_frames_full_join(list(inhabitants,
                                                  waterworks,
                                                  sewers,
                                                  companies_volume,
                                                  treatments348,
                                                  treatments349,
                                                  profiles_volume,
                                                  animals,
                                                  supplements,
                                                  companies9_volume, 
                                                  companies49_volume,
                                                  companies249_volume,
                                                  companies999_volume,
                                                  companies1000_volume,
                                                  forests
)) %>% 
  replace(., is.na(.), 0)

### Add 'powiat' and 'region' labels for each observation 

variables_dataframe <<- left_join(variables_dataframe, key_gen, by = "name") 

### Arrange dataframe' order according to 'year' 

variables_dataframe <- arrange(variables_dataframe, variables_dataframe$year)
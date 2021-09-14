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

#   I. Load data from .rds datafiles and transform selected variable types

##  1.1. Load data from .rds file with systematics of gmina's 'name', 'region' and 'powiat' 
key_gen <- read_rds("Data/key_gen.rds")

##  1.2. Load data from .rds files with data retrieved throughout GUS API 

inhabitants <- read_rds("Data/inhabitants.rds") %>% rename(inhab = val) 
waterworks <- read_rds("Data/waterworks.rds") %>% rename(water = val) 
forests <- read_rds("Data/forests.rds") %>% rename(forests = val) 
sewers <- read_rds("Data/sewers.rds") %>% rename(sew = val)
companies_volume <- read_rds("Data/companies_Ikw_total.rds") %>% rename(comp = val) 
companies9_volume <- read_rds("Data/companies_Ikw_0_9.rds") %>% rename(comp9 = val) 
companies49_volume <- read_rds("Data/companies_Ikw_10_49.rds") %>% rename(comp49 = val) 
companies249_volume <- read_rds("Data/companies_Ikw_50_249.rds") %>% rename(comp249 = val) 
companies999_volume <- read_rds("Data/companies_Ikw_250_999.rds") %>% rename(comp999 = val) 
companies1000_volume <- read_rds("Data/companies_Ikw_1000.rds") %>% rename(comp1000 = val) 

treatments348 <- read_rds("Data/treatment_plants348.rds") %>% rename(treat348 = val) 
treatments349 <- read_rds("Data/treatment_plants349.rds") %>% rename(treat349 = val) 

animals_chic <- read_rds("Data/farms_cattle_chick_2010.rds") %>% rename(chic = val) 
animals_cows <- read_rds("Data/farms_cattle_cows_2010.rds") %>% rename(cow = val)
animals_hors <- read_rds("Data/farms_cattle_hors_2010.rds") %>% rename(hors = val)
animals_pigs <- read_rds("Data/farms_cattle_pigs_2010.rds") %>% rename(pigs = val)
animals <- func_frames_full_join(list(animals_chic,
                                      animals_cows,
                                      animals_hors,
                                      animals_pigs))

supplements_wapn <- read_rds("Data/farms_cattle_wapn_2010.rds") %>% rename(wapn = val) 
supplements_potas <- read_rds("Data/farms_cattle_potas_2010.rds") %>% rename(potas = val) 
supplements_azot <- read_rds("Data/farms_cattle_azot_2010.rds") %>% rename(azot = val) 
supplements_fosfor <- read_rds("Data/farms_cattle_fosfor_2010.rds") %>% rename(fosfor = val) 
supplements_mineral <- read_rds("Data/farms_cattle_mineral_2010.rds") %>% rename(mineral = val) 
supplements <- func_frames_full_join(list(supplements_wapn,
                                          supplements_potas,
                                          supplements_azot,
                                          supplements_fosfor,
                                          supplements_mineral))

profiles_volume <- read_rds("Data/profiles.rds") 
profiles_volume <- profiles_volume[,c(-1,-3,-22)]

##  1.3. Information in .rds files retrieved throughout GUS API have missing data for some 'gminas'. 
##  Missing data was manually collected and stored in appropiate .xlsx files.
##  Collected data is retrieved from xlsx files and binded to appropiate dataframes.  

add_water <- read_excel("Data/add/add.xlsx", sheet = "water")
waterworks <- rbind(waterworks, add_water)

add_sewer <- read_excel("Data/add/add.xlsx", sheet = "sewer")
sewers <- rbind(sewers, add_sewer)

add_forests <- read_excel("Data/add/add.xlsx", sheet = "forests")
forests <- rbind(forests, add_forests)

add_companies <- read_excel("Data/add/add.xlsx", sheet = "companies")
companies_volume <- rbind(companies_volume, add_companies)

add_treat48 <- read_excel("Data/add/add.xlsx", sheet = "treat48")
treatments348 <- rbind(treatments348, add_treat48)

add_treat49 <- read_excel("Data/add/add.xlsx", sheet = "trat49")
treatments349 <- rbind(treatments349, add_treat49)

add_com9 <- read_excel("Data/add/add.xlsx", sheet = "09")
companies9_volume <- rbind(companies9_volume, add_com9)

add_com49 <- read_excel("Data/add/add.xlsx", sheet = "1049")
companies49_volume <- rbind(companies49_volume, add_com49)

add_com249 <- read_excel("Data/add/add.xlsx", sheet = "50249")
companies249_volume <- rbind(companies249_volume, add_com249)

add_com999 <- read_excel("Data/add/add.xlsx", sheet = "250999")
companies999_volume <- rbind(companies999_volume, add_com999)

add_com1000 <- read_excel("Data/add/add.xlsx", sheet = "1000")
companies1000_volume <- rbind(companies1000_volume, add_com1000)



 
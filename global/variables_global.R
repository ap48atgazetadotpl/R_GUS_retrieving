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
 
source("loading_functions/load_variables_dataframe.R")


#   II. Values aggregation for levels: zlewnia, region, powiat and adjusting dataframes format 

##  2.1. Inhabitants

inhabitants_zlewnia <- aggregate(list(variables_dataframe$inhab), by = list(variables_dataframe$year), sum) 
names(inhabitants_zlewnia)[1:2] <- c("year","inhab_zlewnia") 
inhabitants_zlewnia$year <- as.integer(year(as.Date(inhabitants_zlewnia$year, format = "%Y")))
inhabitants_zlewnia <- inhabitants_zlewnia %>% 
  mutate(inhab_zlewnia, inhab_zlewnia = round(inhab_zlewnia/1000, digits = 1)) %>% 
  arrange(inhabitants_zlewnia$year)

inhabitants_regiony <- aggregate(list(variables_dataframe$inhab), by = list(variables_dataframe$region, variables_dataframe$year), sum)
names(inhabitants_regiony)[1:3] <- c("region", "year", "inhab_regiony")
inhabitants_regiony$year <- as.integer(year(as.Date(inhabitants_regiony$year, format = "%Y")))
inhabitants_regiony <- inhabitants_regiony %>% 
  mutate(inhab_regiony, inhab_regiony = round(inhab_regiony/1000, digits = 1)) %>% 
  arrange(inhabitants_regiony$year)

inhabitants_powiaty <- aggregate(list(variables_dataframe$inhab), by = list(variables_dataframe$powiat, variables_dataframe$region, variables_dataframe$year), sum)
names(inhabitants_powiaty)[1:4] <- c("powiat", "region", "year", "inhab_powiaty")
inhabitants_powiaty$year <- as.integer(year(as.Date(inhabitants_powiaty$year, format = "%Y"))) 
inhabitants_powiaty <- inhabitants_powiaty %>% 
  mutate(inhab_powiaty, inhab_powiaty = round(inhab_powiaty/1000, digits = 1)) %>% 
  arrange(inhabitants_powiaty, inhabitants_powiaty$year)

inhabitants_powiaty_lodzkie <- subset(inhabitants_powiaty, region == "łódzkie")
inhabitants_powiaty_malopolskie <- subset(inhabitants_powiaty, region == "małopolskie")
inhabitants_powiaty_mazowieckie <- subset(inhabitants_powiaty, region == "mazowieckie")
inhabitants_powiaty_slaskie <- subset(inhabitants_powiaty, region == "śląskie")
inhabitants_powiaty_swietokrzyskie <- subset(inhabitants_powiaty, region == "świętokrzyskie")

inhabitants_gminy <- variables_dataframe
inhabitants_gminy <- inhabitants_gminy %>% mutate(inhab, inhab = round(inhabitants_gminy$inhab/1000, digits = 1))
inhabitants_gminy$year <- as.integer(year(as.Date(inhabitants_gminy$year, format = "%Y"))) 
inhabitants_gminy <- arrange(inhabitants_gminy,inhabitants_gminy$year)


##  2.2. Animals

animals_zlewnia <- aggregate(list(variables_dataframe$chic, 
                                   variables_dataframe$cow,
                                   variables_dataframe$hors,
                                   variables_dataframe$pigs), 
                              by = list(variables_dataframe$year), sum)
names(animals_zlewnia)[1:5] <- c("year", "drób", "krowy", "konie", "trzoda.chl.")
animals_zlewnia <- pivot_longer(animals_zlewnia, cols = 2:5, names_to = "animals", values_to = "volume")
animals_zlewnia <- subset(animals_zlewnia, animals_zlewnia$year == "2010" | animals_zlewnia$year == "2020" )
animals_zlewnia <- arrange(animals_zlewnia, animals_zlewnia$year)

animals_regiony <- aggregate(list(variables_dataframe$chic, 
                                   variables_dataframe$cow,
                                   variables_dataframe$hors,
                                   variables_dataframe$pigs),
                              by = list(variables_dataframe$region, variables_dataframe$year), sum)
names(animals_regiony)[1:6] <- c("region", "year", "drób", "krowy", "konie", "trzoda.chl.")
animals_regiony <- pivot_longer(animals_regiony, cols = 3:6, names_to = "animals", values_to = "volume")
animals_regiony <- subset(animals_regiony, animals_regiony$year == "2010" | animals_regiony$year == "2020")
animals_regiony <- arrange(animals_regiony, animals_regiony$year)


##  2.3. Supplements

supplements_zlewnia <- aggregate(list(variables_dataframe$wapn, 
                                      variables_dataframe$potas, 
                                      variables_dataframe$azot, 
                                      variables_dataframe$fosfor, 
                                      variables_dataframe$mineral), 
                                 by = list(variables_dataframe$year), sum)
names(supplements_zlewnia)[1:6] <- c("year", "wapn", "potas", "azot", "fosfor", "mineralne")
supplements_zlewnia <- pivot_longer(supplements_zlewnia, cols = 2:6, names_to = "supplements", values_to = "volume")
supplements_zlewnia <- subset(supplements_zlewnia, supplements_zlewnia$year == "2010" | supplements_zlewnia$year == "2020" )
supplements_zlewnia <- arrange(supplements_zlewnia, supplements_zlewnia$year)

supplements_regiony <- aggregate(list(variables_dataframe$wapn, 
                                      variables_dataframe$potas, 
                                      variables_dataframe$azot, 
                                      variables_dataframe$fosfor, 
                                      variables_dataframe$mineral), 
                                 by = list(variables_dataframe$region, variables_dataframe$year), sum)
names(supplements_regiony)[1:7] <- c("region", "year", "wapn", "potas", "azot", "fosfor", "mineralne")
supplements_regiony <- pivot_longer(supplements_regiony, cols = 3:7, names_to = "supplements", values_to = "volume")
supplements_regiony <- subset(supplements_regiony, supplements_regiony$year == "2010" | supplements_regiony$year == "2020" )
supplements_regiony <- arrange(supplements_regiony, supplements_regiony$year)
 

##  2.4.1. Companies - volume 

companies_volume_zlewnia <- aggregate(list(variables_dataframe$comp), by = list(variables_dataframe$year), sum)
names(companies_volume_zlewnia)[1:2] <- c("year", "comp")
companies_volume_zlewnia$year <- as.integer(year(as.Date(companies_volume_zlewnia$year, format = "%Y"))) 
companies_volume_zlewnia <- subset(companies_volume_zlewnia, companies_volume_zlewnia$year > "2011")
companies_volume_zlewnia <- companies_volume_zlewnia %>% 
  mutate(comp, comp = round(comp/1000, digits = 1)) %>% 
  arrange(companies_volume_zlewnia$year)

companies_volume_regiony <- aggregate(list(variables_dataframe$comp), by = list(variables_dataframe$region, variables_dataframe$year), sum)
names(companies_volume_regiony)[1:3] <- c("region", "year", "comp")
companies_volume_regiony$year <- as.integer(year(as.Date(companies_volume_regiony$year, format = "%Y"))) 
companies_volume_regiony <- subset(companies_volume_regiony, companies_volume_regiony$year > "2011")
companies_volume_regiony <- companies_volume_regiony %>% 
  mutate(comp, comp = round(comp/1000, digits = 1)) %>% 
  arrange(companies_volume_regiony$year)

companies_volume_powiaty <- aggregate(list(variables_dataframe$comp), by = list(variables_dataframe$powiat, variables_dataframe$region, variables_dataframe$year), sum)
names(companies_volume_powiaty)[1:4] <- c("powiat", "region", "year", "comp")
companies_volume_powiaty$year <- as.integer(year(as.Date(companies_volume_powiaty$year, format = "%Y")))
companies_volume_powiaty <- subset(companies_volume_powiaty, companies_volume_powiaty$year > "2011")
companies_volume_powiaty <- companies_volume_powiaty %>% 
  mutate(comp, comp = round(comp/1000, digits = 1)) %>% 
  arrange(companies_volume_powiaty$year)

companies_volume_powiaty_lodzkie <- subset(companies_volume_powiaty, region == "łódzkie")
companies_volume_powiaty_malopolskie <- subset(companies_volume_powiaty, region == "małopolskie")
companies_volume_powiaty_mazowieckie <- subset(companies_volume_powiaty, region == "mazowieckie")
companies_volume_powiaty_slaskie <- subset(companies_volume_powiaty, region == "śląskie")
companies_volume_powiaty_swietokrzyskie <- subset(companies_volume_powiaty, region == "świętokrzyskie")

companies_gminy <- variables_dataframe
companies_gminy$year <- as.integer(year(as.Date(companies_gminy$year, format = "%Y"))) 
companies_gminy <- arrange(companies_gminy,companies_gminy$year)


##  2.4.2. Companies - size 
 
companies_volume_all <- variables_dataframe[,c(1,2,3,37,38,39,40,41,43,44)]

companies_volume_zlewnia_size <- companies_volume_all %>% group_by(year) %>% summarise(sum9 = sum(comp9),
                                                                                                sum49 = sum(comp49),
                                                                                                sum249 = sum(comp249),
                                                                                                sum999 = sum(comp999),
                                                                                                sum1000 = sum(comp1000)
                                                                                                )

companies_volume_regiony_size <- companies_volume_all %>% group_by(year, region) %>% summarise(sum9 = sum(comp9),
                                                                                                sum49 = sum(comp49),
                                                                                                sum249 = sum(comp249),
                                                                                                sum999 = sum(comp999),
                                                                                                sum1000 = sum(comp1000)
                                                                                                )           

companies_volume_powiaty_size <- companies_volume_all %>% group_by(year, powiat) %>% summarise(sum9 = sum(comp9),
                                                                                               sum49 = sum(comp49),
                                                                                               sum249 = sum(comp249),
                                                                                               sum999 = sum(comp999),
                                                                                               sum1000 = sum(comp1000)
                                                                                                )     
 
##2.5.1. Profiles  - present profies  

profiles_zlewnia <- aggregate(list(variables_dataframe$Prof_MSP, 
                                    variables_dataframe$Prof_DF,
                                    variables_dataframe$Rolnictwo,
                                    variables_dataframe$Leśnictwo,
                                    variables_dataframe$Rolnictwo_ekologiczne,
                                    variables_dataframe$Kopalnictwo,
                                    variables_dataframe$Turystyka_agroturystyka,
                                    variables_dataframe$Turystyka_rekreacja_wodna
                                    ), by = list(variables_dataframe$year), sum)

names(profiles_zlewnia)[1:9] <- c("year", "MSP", "Duze_firmy", "Rolnictwo",
                                "Leśnictwo", "Rolnictwo_ekologiczne", "Kopalnictwo",
                                "Turystyka_agroturystyka", "Turystyka_rekreacja_wodna") 


profiles_regiony <- aggregate(list(variables_dataframe$Prof_MSP, 
                                    variables_dataframe$Prof_DF,
                                    variables_dataframe$Rolnictwo,
                                    variables_dataframe$Leśnictwo,
                                    variables_dataframe$Rolnictwo_ekologiczne,
                                    variables_dataframe$Kopalnictwo,
                                    variables_dataframe$Turystyka_agroturystyka,
                                    variables_dataframe$Turystyka_rekreacja_wodna
                                    ), by = list(variables_dataframe$region, variables_dataframe$year), sum)

names(profiles_regiony)[1:10] <- c("region", "year", "MSP", "Duze_firmy", "Rolnictwo",
                                  "Leśnictwo", "Rolnictwo_ekologiczne", "Kopalnictwo",
                                  "Turystyka_agroturystyka", "Turystyka_rekreacja_wodna") 

profiles_powiaty <- aggregate(list(variables_dataframe$Prof_MSP, 
                                    variables_dataframe$Prof_DF,
                                    variables_dataframe$Rolnictwo,
                                    variables_dataframe$Leśnictwo,
                                    variables_dataframe$Rolnictwo_ekologiczne,
                                    variables_dataframe$Kopalnictwo,
                                    variables_dataframe$Turystyka_agroturystyka,
                                    variables_dataframe$Turystyka_rekreacja_wodna
                                    ), 
                               by = list(variables_dataframe$powiat, variables_dataframe$region, variables_dataframe$year), sum)

names(profiles_powiaty)[1:11] <- c("powiat", "region", "year", "MSP", "Duze_firmy", "Rolnictwo",
                                   "Leśnictwo", "Rolnictwo_ekologiczne", "Kopalnictwo",
                                   "Turystyka_agroturystyka", "Turystyka_rekreacja_wodna") 

profiles_powiaty_lodzkie <- subset(profiles_powiaty, region == "łódzkie")
profiles_powiaty_malopolskie <- subset(profiles_powiaty, region == "małopolskie")
profiles_powiaty_mazowieckie <- subset(profiles_powiaty, region == "mazowieckie")
profiles_powiaty_slaskie <- subset(profiles_powiaty, region == "śląskie")
profiles_powiaty_swietokrzyskie <- subset(profiles_powiaty, region == "świętokrzyskie")

profiles_gminy <- variables_dataframe %>% filter(year == 2009)
profiles_gminy <- profiles_gminy[,c(2,12,13,14,15,16,17,18,19,43)]
names(profiles_gminy)[1:9] <- c("gmina", "Rolnictwo", "Leśnictwo", "Rolnictwo ekologiczne",
                              "Kopalnictwo", "Turystyka agroturystyka", "Turystyka rekreacja wodna",
                              "MSP", "Duże firmy")
profiles_gminy[profiles_gminy == 0] <- ""
profiles_gminy[profiles_gminy == 1] <- "TAK"




##2.5.2. Profiles  - prospective profies  

profiles_zlewnia_p <- aggregate(list(variables_dataframe$Prof_MSP_p, 
                                   variables_dataframe$Prof_DF_p,
                                   variables_dataframe$Rolnictwo_p,
                                   variables_dataframe$Leśnictwo_p,
                                   variables_dataframe$Rolnictwo_ekologiczne_p,
                                   variables_dataframe$Kopalnictwo_p,
                                   variables_dataframe$Turystyka_agroturystyka_p,
                                   variables_dataframe$Turystyka_rekreacja_wodna_p
), by = list(variables_dataframe$year), sum)

names(profiles_zlewnia_p)[1:9] <- c("year", "MSP", "Duze_firmy", "Rolnictwo",
                                  "Leśnictwo", "Rolnictwo_ekologiczne", "Kopalnictwo",
                                  "Turystyka_agroturystyka", "Turystyka_rekreacja_wodna") 


profiles_regiony_p <- aggregate(list(variables_dataframe$Prof_MSP_p, 
                                   variables_dataframe$Prof_DF_p,
                                   variables_dataframe$Rolnictwo_p,
                                   variables_dataframe$Leśnictwo_p,
                                   variables_dataframe$Rolnictwo_ekologiczne_p,
                                   variables_dataframe$Kopalnictwo_p,
                                   variables_dataframe$Turystyka_agroturystyka_p,
                                   variables_dataframe$Turystyka_rekreacja_wodna_p
), by = list(variables_dataframe$region, variables_dataframe$year), sum)

names(profiles_regiony_p)[1:10] <- c("region", "year", "MSP", "Duze_firmy", "Rolnictwo",
                                   "Leśnictwo", "Rolnictwo_ekologiczne", "Kopalnictwo",
                                   "Turystyka_agroturystyka", "Turystyka_rekreacja_wodna") 

profiles_powiaty_p <- aggregate(list(variables_dataframe$Prof_MSP_p, 
                                   variables_dataframe$Prof_DF_p,
                                   variables_dataframe$Rolnictwo_p,
                                   variables_dataframe$Leśnictwo_p,
                                   variables_dataframe$Rolnictwo_ekologiczne_p,
                                   variables_dataframe$Kopalnictwo_p,
                                   variables_dataframe$Turystyka_agroturystyka_p,
                                   variables_dataframe$Turystyka_rekreacja_wodna_p
), 
by = list(variables_dataframe$powiat, variables_dataframe$region, variables_dataframe$year), sum)

names(profiles_powiaty_p)[1:11] <- c("powiat", "region", "year", "MSP", "Duze_firmy", "Rolnictwo",
                                   "Leśnictwo", "Rolnictwo_ekologiczne", "Kopalnictwo",
                                   "Turystyka_agroturystyka", "Turystyka_rekreacja_wodna") 

profiles_p_powiaty_lodzkie <- subset(profiles_powiaty_p, region == "łódzkie")
profiles_p_powiaty_malopolskie <- subset(profiles_powiaty_p, region == "małopolskie")
profiles_p_powiaty_mazowieckie <- subset(profiles_powiaty_p, region == "mazowieckie")
profiles_p_powiaty_slaskie <- subset(profiles_powiaty_p, region == "śląskie")
profiles_p_powiaty_swietokrzyskie <- subset(profiles_powiaty_p, region == "świętokrzyskie")

profiles_gminy_p <- variables_dataframe %>% filter(year == 2009)
profiles_gminy_p <- profiles_gminy_p[,c(2,20,21,22,23,24,25,26,27,43)]
names(profiles_gminy_p)[1:9] <- c("gmina", "Rolnictwo", "Leśnictwo", "Rolnictwo ekologiczne",
                                "Kopalnictwo", "Turystyka agroturystyka", "Turystyka rekreacja wodna",
                                "MSP", "Duże firmy")
profiles_gminy_p[profiles_gminy_p == 0] <- ""
profiles_gminy_p[profiles_gminy_p == 1] <- "TAK"


##2.6. Waterworks - values in % (aggregation unable)

##2.7. Sewers - values in % (aggregation unable)

##2.8. Treatments

treatments_zlewnia <- aggregate(list(variables_dataframe$treat349), by = list(variables_dataframe$year), sum) 
names(treatments_zlewnia)[1:2] <- c("year", "oczyszczalnie")
treatments_zlewnia$year <- as.integer(year(as.Date(treatments_zlewnia$year, format = "%Y"))) 
treatments_zlewnia <- arrange(treatments_zlewnia, treatments_zlewnia$year)

treatments_regiony <<- aggregate(list(variables_dataframe$treat349), by = list(variables_dataframe$region, variables_dataframe$year), sum)
names(treatments_regiony)[1:3] <- c("region", "year", "oczyszczalnie")
treatments_regiony$year <- as.integer(year(as.Date(treatments_regiony$year, format = "%Y")))
treatments_regiony <- arrange(treatments_regiony, treatments_regiony$year)

treatments_powiaty <<- aggregate(list(variables_dataframe$treat349), by = list(variables_dataframe$powiat, variables_dataframe$region, variables_dataframe$year), sum)
names(treatments_powiaty)[1:4] <- c("powiat", "region", "year", "oczyszczalnie")
treatments_powiaty$year <- as.integer(year(as.Date(treatments_powiaty$year, format = "%Y"))) 
treatments_powiaty <- arrange(treatments_powiaty, treatments_powiaty$year)

treatments_powiaty_lodzkie <- subset(treatments_powiaty, region == "łódzkie")
treatments_powiaty_malopolskie <- subset(treatments_powiaty, region == "małopolskie")
treatments_powiaty_mazowieckie <- subset(treatments_powiaty, region == "mazowieckie")
treatments_powiaty_slaskie <- subset(treatments_powiaty, region == "śląskie")
treatments_powiaty_swietokrzyskie <- subset(treatments_powiaty, region == "świętokrzyskie")

treatments_gminy <<- variables_dataframe
treatments_gminy$year <- as.integer(year(as.Date(treatments_gminy$year, format = "%Y")))
treatments_gminy <- arrange(treatments_gminy,treatments_gminy$year)
treatments348_gminy <- treatments_gminy[, c(1,2,3,8,43,44)]
treatments348_gminy <-  treatments348_gminy %>% pivot_wider(names_from = year, values_from = treat348, values_fn = mean) 
treatments348_gminy <-  treatments348_gminy %>% mutate(technologia = "biologiczna")
treatments349_gminy <- treatments_gminy[, c(1,2,3,9,43,44)]
treatments349_gminy <-  treatments349_gminy %>% pivot_wider(names_from = year, values_from = treat349, values_fn = mean)
treatments349_gminy <-  treatments349_gminy %>% mutate(technologia = "ocz.biogenów")
treatments_gminy <- treatments348_gminy
treatments_gminy <- rbind(treatments349_gminy, treatments348_gminy)
treatments_gminy <- rbind(treatments349_gminy, treatments348_gminy)


##2.9. Forests - values in % (aggregation unable)


#III. Regions definition

powiatset_lodzkie <- filter(variables_dataframe, region == "łódzkie") 
powiatset_lodzkie <-  unique(powiatset_lodzkie$powiat)

powiatset_malopolskie <- filter(variables_dataframe, region == "małopolskie")
powiatset_malopolskie <-  unique(powiatset_malopolskie$powiat)

powiatset_mazowieckie <- filter(variables_dataframe, region == "mazowieckie")
powiatset_mazowieckie <-  unique(powiatset_mazowieckie$powiat)

powiatset_slaskie <- filter(variables_dataframe, region == "śląskie")
powiatset_slaskie <-  unique(powiatset_slaskie$powiat)

powiatset_swietokrzyskie <- filter(variables_dataframe, region == "świętokrzyskie")
powiatset_swietokrzyskie <-  unique(powiatset_swietokrzyskie$powiat)

#IV. Powiat definitions

gminaset_lodzki_wschodni <- filter(variables_dataframe, powiat == "Powiat łódzki wschodni") 
gminaset_lodzki_wschodni <- unique(gminaset_lodzki_wschodni$name) 

gminaset_opoczenski <- filter(variables_dataframe, powiat == "Powiat opoczyński") 
gminaset_opoczenski <- unique(gminaset_opoczenski$name)

gminaset_piotrkowski <- filter(variables_dataframe, powiat == "Powiat piotrkowski") 
gminaset_piotrkowski <- unique(gminaset_piotrkowski$name)

gminaset_radomszczanski <- filter(variables_dataframe, powiat == "Powiat radomszczański") 
gminaset_radomszczanski <- unique(gminaset_radomszczanski$name)

gminaset_rawski <- filter(variables_dataframe, powiat == "Powiat rawski") 
gminaset_rawski <- unique(gminaset_rawski$name)

gminaset_tomaszowski <- filter(variables_dataframe, powiat == "Powiat tomaszowski") 
gminaset_tomaszowski <- unique(gminaset_tomaszowski$name)

gminaset_miastoPiotrkow <- filter(variables_dataframe, powiat == "Powiat m. Piotrków Trybunalski") 
gminaset_miastoPiotrkow <- unique(gminaset_miastoPiotrkow$name)

gminaset_miechowski <- filter(variables_dataframe, powiat == "Powiat miechowski") 
gminaset_miechowski <- unique(gminaset_miechowski$name)

gminaset_olkuski <- filter(variables_dataframe, powiat == "Powiat olkuski") 
gminaset_olkuski <- unique(gminaset_olkuski$name)

gminaset_bialobrzeski <- filter(variables_dataframe, powiat == "Powiat białobrzeski") 
gminaset_bialobrzeski <- unique(gminaset_bialobrzeski$name)

gminaset_grojecki <- filter(variables_dataframe, powiat == "Powiat grójecki") 
gminaset_grojecki <- unique(gminaset_grojecki$name)

gminaset_kozienicki <- filter(variables_dataframe, powiat == "Powiat kozienicki") 
gminaset_kozienicki <- unique(gminaset_kozienicki$name)

gminaset_przysuski <- filter(variables_dataframe, powiat == "Powiat przysuski") 
gminaset_przysuski <- unique(gminaset_przysuski$name)

gminaset_czestochowski <- filter(variables_dataframe, powiat == "Powiat częstochowski") 
gminaset_czestochowski <- unique(gminaset_czestochowski$name)

gminaset_myszkowski <- filter(variables_dataframe, powiat == "Powiat myszkowski") 
gminaset_myszkowski <- unique(gminaset_myszkowski$name)

gminaset_zawiercianski <- filter(variables_dataframe, powiat == "Powiat zawierciański") 
gminaset_zawiercianski  <- unique(gminaset_zawiercianski$name)

gminaset_jedrzejowski <- filter(variables_dataframe, powiat == "Powiat jędrzejowski") 
gminaset_jedrzejowski  <- unique(gminaset_jedrzejowski$name)

gminaset_kielecki <- filter(variables_dataframe, powiat == "Powiat kielecki") 
gminaset_kielecki  <- unique(gminaset_kielecki$name)

gminaset_konecki <- filter(variables_dataframe, powiat == "Powiat konecki") 
gminaset_konecki  <- unique(gminaset_konecki$name)

gminaset_skarzyski <- filter(variables_dataframe, powiat == "Powiat skarżyski") 
gminaset_skarzyski  <- unique(gminaset_skarzyski$name)

gminaset_wloszczowski <- filter(variables_dataframe, powiat == "Powiat włoszczowski") 
gminaset_wloszczowski  <- unique(gminaset_wloszczowski$name)

gminaset_bialobrzeski <- filter(variables_dataframe, powiat == "Powiat białobrzeski") 
gminaset_bialobrzeski  <- unique(gminaset_bialobrzeski$name)

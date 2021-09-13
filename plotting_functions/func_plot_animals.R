library(plotly)


##  Plot - animals farms in Zlewnia (total, static)

func_plot_animals_zlewnia <- function () {

  animals_zlewnia <- animals_zlewnia %>% pivot_wider(names_from = year, values_from = volume, values_fn = mean) 
  fig <<- plot_ly(animals_zlewnia, x= ~animals, y = ~animals_zlewnia$'2010', type = "bar", name = '2010')
  # fig <<- fig %>% add_trace(y = ~animals_zlewnia$'2020', type = "bar", name = '2020')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = 'Liczba gospodarstw (szt.) \n Liczba gmin - krowy: n=83, trzoda chlewna: n=72, konie: n=72, drób ogółem: n=67'), yaxis = list(title = 'szt.', exponentformat = 'none'), barmode = 'group')
  
}
  
 plot_animals_zlewnia <- func_plot_animals_zlewnia ()

 
##  Plot - animals farms in Zlewnia (animals total regiony, static)
 
func_plot_regiony_animals <- function (animalname) {
  
  animals_regiony <- animals_regiony %>% pivot_wider(names_from = year, values_from = volume, values_fn = mean) 
  animals_regiony <- filter(animals_regiony, animals == animalname)
  fig <<- plot_ly(animals_regiony, x= ~region, y = ~animals_regiony$'2010', type = "bar", name = '2010')
  #  fig <<- fig %>% add_trace(y = ~animals_regiony$'2020', type = "bar", name = '2020')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = 'Liczba gospodarstw - trzoda.chl. (szt.) \n Dane z 72 gmin'), yaxis = list(title = '(szt.)', exponentformat = 'none'), barmode = 'group')
  
}

plot_regiony_animals_chic <- func_plot_regiony_animals("drób")
plot_regiony_animals_cow <- func_plot_regiony_animals("krowy")
plot_regiony_animals_hors <- func_plot_regiony_animals("konie")
plot_regiony_animals_pigs <- func_plot_regiony_animals("trzoda.chl.")


##  Plot - animals farms in Zlewnia (regiony - animals, static)

func_plot_animals_regiony <- function (regionname) {
  
  animals_regiony <- animals_regiony %>% pivot_wider(names_from = year, values_from = volume, values_fn = mean) 
  animals_regiony <- filter(animals_regiony, region == regionname)
  fig <<- plot_ly(animals_regiony, x= ~animals, y = ~animals_regiony$'2010', type = "bar", name = '2010')
  # fig <<- fig %>% add_trace(y = ~animals_regiony$'2020', type = "bar", name = '2020')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = 'Liczba gospodarstw'), yaxis = list(title = 'szt.', exponentformat = 'none'), barmode = 'group')
  
}

plot_animals_regiony_lodzkie <- func_plot_animals_regiony("łódzkie")
plot_animals_regiony_mazowieckie <- func_plot_animals_regiony("mazowieckie")
plot_animals_regiony_slaskie <- func_plot_animals_regiony("śląskie")
plot_animals_regiony_malopolskie <- func_plot_animals_regiony("małopolskie")
plot_animals_regiony_swietokrzyskie <- func_plot_animals_regiony ("świętokrzyskie")


##  Plot - animals farms in Zlewnia (powiaty, static) 

func_plot_animals_gmina <- function (gminaset) {
  
  animals_powiaty_chic <- as.data.frame(variables_dataframe[, c(1:3,28)]) #28 chic, 29 cow, 30 hors, 31 pigs
  animals_powiaty_chic <- filter(animals_powiaty_chic, name %in% gminaset)
  animals_powiaty_chic <- animals_powiaty_chic %>% pivot_wider(names_from = year, values_from = chic, values_fn = mean) 
  animals_powiaty_cow <- as.data.frame(variables_dataframe[, c(1:3,29)]) #28 chic, 29 cow, 30 hors, 31 pigs
  animals_powiaty_cow <- filter(animals_powiaty_cow, name %in% gminaset)
  animals_powiaty_cow <- animals_powiaty_cow %>% pivot_wider(names_from = year, values_from = cow, values_fn = mean) 
  animals_powiaty_hors <- as.data.frame(variables_dataframe[, c(1:3,30)]) #28 chic, 29 cow, 30 hors, 31 pigs
  animals_powiaty_hors <- filter(animals_powiaty_hors, name %in% gminaset)
  animals_powiaty_hors <- animals_powiaty_hors %>% pivot_wider(names_from = year, values_from = hors, values_fn = mean) 
  animals_powiaty_pigs <- as.data.frame(variables_dataframe[, c(1:3,31)]) #28 chic, 29 cow, 30 hors, 31 pigs
  animals_powiaty_pigs <- filter(animals_powiaty_pigs, name %in% gminaset)
  animals_powiaty_pigs <- animals_powiaty_pigs %>% pivot_wider(names_from = year, values_from = pigs, values_fn = mean) 
  
  fig <<- plot_ly(animals_powiaty_chic, x= ~name, y = ~animals_powiaty_chic$'2010', type = "bar", name = 'drób ogółem')
  fig <<- fig %>% add_trace(y = ~animals_powiaty_cow$'2010', type = "bar", name = 'krowy')
  fig <<- fig %>% add_trace(y = ~animals_powiaty_hors$'2010', type = "bar", name = 'konie')
  fig <<- fig %>% add_trace(y = ~animals_powiaty_pigs$'2010', type = "bar", name = 'trzoda chlewna')
  
  fig <- layout(fig, yaxis=list(type='linear'))  
  fig <- layout(fig, xaxis = list(title = ''), yaxis = list(title = 'Liczba gospodarstw', exponentformat = 'none'), barmode = 'group')
  
} 

plot_animals_gminaset_lodzki_wschodni <- func_plot_animals_gmina (gminaset_lodzki_wschodni)
plot_animals_gminaset_opoczenski <- func_plot_animals_gmina (gminaset_opoczenski)
plot_animals_gminaset_piotrkowski <- func_plot_animals_gmina (gminaset_piotrkowski)
plot_animals_gminaset_radomszczanski <- func_plot_animals_gmina (gminaset_radomszczanski)
plot_animals_gminaset_rawski <- func_plot_animals_gmina (gminaset_rawski)
plot_animals_gminaset_tomaszowski <- func_plot_animals_gmina (gminaset_tomaszowski)
plot_animals_gminaset_miastoPiotrkow <- func_plot_animals_gmina (gminaset_miastoPiotrkow)
plot_animals_gminaset_miechowski <- func_plot_animals_gmina (gminaset_miechowski)
plot_animals_gminaset_olkuski <- func_plot_animals_gmina (gminaset_olkuski)
plot_animals_gminaset_bialobrzeski <- func_plot_animals_gmina (gminaset_bialobrzeski)
plot_animals_gminaset_grojecki <- func_plot_animals_gmina (gminaset_grojecki)
plot_animals_gminaset_kozienicki <- func_plot_animals_gmina (gminaset_kozienicki)
plot_animals_gminaset_przysuski <- func_plot_animals_gmina (gminaset_przysuski)
plot_animals_gminaset_czestochowski <- func_plot_animals_gmina (gminaset_czestochowski)
plot_animals_gminaset_myszkowski <- func_plot_animals_gmina (gminaset_myszkowski)
plot_animals_gminaset_zawiercianski <- func_plot_animals_gmina (gminaset_zawiercianski)
plot_animals_gminaset_jedrzejowski <- func_plot_animals_gmina (gminaset_jedrzejowski)
plot_animals_gminaset_kielecki <- func_plot_animals_gmina (gminaset_kielecki)
plot_animals_gminaset_konecki <- func_plot_animals_gmina (gminaset_konecki)
plot_animals_gminaset_skarzyski <- func_plot_animals_gmina (gminaset_skarzyski)
plot_animals_gminaset_wloszczowski <- func_plot_animals_gmina (gminaset_wloszczowski)
 
 
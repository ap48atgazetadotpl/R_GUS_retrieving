library(plotly)


##  Plot - supplements dt in Zlewnia (total, static)

func_plot_supplements_zlewnia <- function () {
  
  supplements_zlewnia <- supplements_zlewnia %>% pivot_wider(names_from = year, values_from = volume, values_fn = mean) 
  fig <<- plot_ly(supplements_zlewnia, x= ~supplements , y = ~supplements_zlewnia$'2010', type = "bar", name = '2010')
  # fig <<- fig %>% add_trace(y = ~supplements_zlewnia$'2020', type = "bar", name = '2020')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = 'Zużycie ogółem w dt (1dt = 100 kg) \n azot: n=70, fosfor: n=73, mineralne: n=62, potas: n=64, wapń: n=63'), yaxis = list(title = 'dt', exponentformat = 'none'), barmode = 'group')

}

plot_supplements_zlewnia <- func_plot_supplements_zlewnia ()

##  Plot - supplements dt in Zlewnia (supplements total regiony, static)

func_plot_regiony_supplements <- function (supplementname) {
  
  supplements_regiony <- supplements_regiony %>% pivot_wider(names_from = year, values_from = volume, values_fn = mean) 
  supplements_regiony <- filter(supplements_regiony, supplements == supplementname)
  fig <<- plot_ly(supplements_regiony, x= ~region, y = ~supplements_regiony$'2010', type = "bar", name = '2010')
  #  fig <<- fig %>% add_trace(y = ~supplements_regiony$'2010', type = "bar", name = '2020')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = 'Zużycie ogółem w dt (1dt = 100 kg)'), yaxis = list(title = 'dt', exponentformat = 'none'), barmode = 'group')
  
}

plot_regiony_supplements_fosfor <- func_plot_regiony_supplements("fosfor")
plot_regiony_supplements_potas <- func_plot_regiony_supplements("potas")
plot_regiony_supplements_azot <- func_plot_regiony_supplements("azot")
plot_regiony_supplements_wapn <- func_plot_regiony_supplements("wapn")
plot_regiony_supplements_mineralne <- func_plot_regiony_supplements("mineralne")


##  Plot - supplements dt in Zlewnia (regiony - supplements, static)

func_plot_supplements_regiony <- function (regionname) {
  
  supplements_regiony <- supplements_regiony %>% pivot_wider(names_from = year, values_from = volume, values_fn = mean) 
  supplements_regiony <- filter(supplements_regiony, region == regionname)
  fig <<- plot_ly(supplements_regiony, x= ~supplements, y = ~supplements_regiony$'2010', type = "bar", name = '2010')
  #  fig <<- fig %>% add_trace(y = ~supplements_regiony$'2020', type = "bar", name = '2020')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = 'Zużycie ogółem w dt (1dt = 100 kg)'), yaxis = list(title = 'dt', exponentformat = 'none'), barmode = 'group')
  
}

plot_supplements_regiony_lodzkie <- func_plot_supplements_regiony("łódzkie")
plot_supplements_regiony_mazowieckie <- func_plot_supplements_regiony("mazowieckie")
plot_supplements_regiony_slaskie <- func_plot_supplements_regiony("śląskie")
plot_supplements_regiony_malopolskie <- func_plot_supplements_regiony("małopolskie")
plot_supplements_regiony_swietokrzyskie <- func_plot_supplements_regiony ("świętokrzyskie")

##  Plot - supplements dtin Zlewnia (powiaty, static) 

func_plot_supplements_gmina <- function (gminaset) {
  
  supplements_powiaty_potas <- as.data.frame(variables_dataframe[, c(1:3,33,37)]) #28 potas, 29 cow, 30 hors, 31 pigs
  supplements_powiaty_potas <- filter(supplements_powiaty_potas, name %in% gminaset)
  supplements_powiaty_potas <- supplements_powiaty_potas %>% pivot_wider(names_from = year, values_from = potas, values_fn = mean) 
  supplements_powiaty_azot <- as.data.frame(variables_dataframe[, c(1:3,34,37)]) #28 chic, 29 azot, 30 wapn, 31 pigs
  supplements_powiaty_azot <- filter(supplements_powiaty_azot, name %in% gminaset)
  supplements_powiaty_azot <- supplements_powiaty_azot %>% pivot_wider(names_from = year, values_from = azot, values_fn = mean) 
  supplements_powiaty_wapn <- as.data.frame(variables_dataframe[, c(1:3,32,37)]) #28 chic, 29 cow, 30 wapn, 31 pigs
  supplements_powiaty_wapn <- filter(supplements_powiaty_wapn, name %in% gminaset)
  supplements_powiaty_wapn <- supplements_powiaty_wapn %>% pivot_wider(names_from = year, values_from = wapn, values_fn = mean) 
  supplements_powiaty_fosfor <- as.data.frame(variables_dataframe[, c(1:3,35,37)]) #28 chic, 29 cow, 30 hors, 31 fosfor
  supplements_powiaty_fosfor <- filter(supplements_powiaty_fosfor, name %in% gminaset)
  supplements_powiaty_fosfor <- supplements_powiaty_fosfor %>% pivot_wider(names_from = year, values_from = fosfor, values_fn = mean) 
  supplements_powiaty_mineralne <- as.data.frame(variables_dataframe[, c(1:3,36,37)]) #28 chic, 29 cow, 30 hors, 31 fosfor
  supplements_powiaty_mineralne <- filter(supplements_powiaty_mineralne, name %in% gminaset)
  supplements_powiaty_mineralne <- supplements_powiaty_mineralne %>% pivot_wider(names_from = year, values_from = mineral, values_fn = mean)
  
  fig <<- plot_ly(supplements_powiaty_potas, x= ~name, y = ~supplements_powiaty_potas$'2010', type = "bar", name = 'potas')
  fig <<- fig %>% add_trace(y = ~supplements_powiaty_azot$'2010', type = "bar", name = 'azot')
  fig <<- fig %>% add_trace(y = ~supplements_powiaty_wapn$'2010', type = "bar", name = 'wapn')
  fig <<- fig %>% add_trace(y = ~supplements_powiaty_fosfor$'2010', type = "bar", name = 'fosfor')
  fig <<- fig %>% add_trace(y = ~supplements_powiaty_mineralne$'2010', type = "bar", name = 'mineralne')
  
  fig <- layout(fig, yaxis=list(type='linear'))  
  fig <- layout(fig, xaxis = list(title = 'Zużycie ogółem w dt (1dt = 100 kg)'), yaxis = list(title = 'dt', exponentformat = 'none'), barmode = 'group')
  
} 

plot_supplements_gminaset_lodzki_wschodni <- func_plot_supplements_gmina (gminaset_lodzki_wschodni)
plot_supplements_gminaset_opoczenski <- func_plot_supplements_gmina (gminaset_opoczenski)
plot_supplements_gminaset_piotrkowski <- func_plot_supplements_gmina (gminaset_piotrkowski)
plot_supplements_gminaset_radomszczanski <- func_plot_supplements_gmina (gminaset_radomszczanski)
plot_supplements_gminaset_rawski <- func_plot_supplements_gmina (gminaset_rawski)
plot_supplements_gminaset_tomaszowski <- func_plot_supplements_gmina (gminaset_tomaszowski)
plot_supplements_gminaset_miastoPiotrkow <- func_plot_supplements_gmina (gminaset_miastoPiotrkow)
plot_supplements_gminaset_miechowski <- func_plot_supplements_gmina (gminaset_miechowski)
plot_supplements_gminaset_olkuski <- func_plot_supplements_gmina (gminaset_olkuski)
plot_supplements_gminaset_bialobrzeski <- func_plot_supplements_gmina (gminaset_bialobrzeski)
plot_supplements_gminaset_grojecki <- func_plot_supplements_gmina (gminaset_grojecki)
plot_supplements_gminaset_kozienicki <- func_plot_supplements_gmina (gminaset_kozienicki)
plot_supplements_gminaset_przysuski <- func_plot_supplements_gmina (gminaset_przysuski)
plot_supplements_gminaset_czestochowski <- func_plot_supplements_gmina (gminaset_czestochowski)
plot_supplements_gminaset_myszkowski <- func_plot_supplements_gmina (gminaset_myszkowski)
plot_supplements_gminaset_zawiercianski <- func_plot_supplements_gmina (gminaset_zawiercianski)
plot_supplements_gminaset_jedrzejowski <- func_plot_supplements_gmina (gminaset_jedrzejowski)
plot_supplements_gminaset_kielecki <- func_plot_supplements_gmina (gminaset_kielecki)
plot_supplements_gminaset_konecki <- func_plot_supplements_gmina (gminaset_konecki)
plot_supplements_gminaset_skarzyski <- func_plot_supplements_gmina (gminaset_skarzyski)
plot_supplements_gminaset_wloszczowski <- func_plot_supplements_gmina (gminaset_wloszczowski)


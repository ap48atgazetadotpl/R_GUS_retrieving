library(plotly)

## Plot - profiles - Zlewnia level (total, static)


func_plot_profiles_zlewnia <- function () {

fig <<- plot_ly(subset(profiles_zlewnia, year == '2009'), x = ~year, y= ~MSP, type = "bar", name = 'MSP')
fig <<- fig %>% add_trace(y = ~Duze_firmy, type = "bar", name = 'Duże firmy')
fig <<- fig %>% add_trace(y = ~Rolnictwo, type = "bar", name = 'Rolnictwo')
fig <<- fig %>% add_trace(y = ~Leśnictwo, type = "bar", name = 'Leśnictwo')
fig <<- fig %>% add_trace(y = ~Rolnictwo_ekologiczne, type = "bar", name = 'Rolnictwo ekol.')
fig <<- fig %>% add_trace(y = ~Kopalnictwo, type = "bar", name = 'Kopalnictwo')
fig <<- fig %>% add_trace(y = ~Turystyka_agroturystyka, type = "bar", name = 'Turystyka agroturystyka')
fig <<- fig %>% add_trace(y = ~Turystyka_rekreacja_wodna, type = "bar", name = 'Turystyka rekreacja wodna')
fig <<- fig %>% layout(yaxis=list(type='linear'))
fig <<- fig %>% layout(xaxis = list(title = ''), yaxis = list(title = 'Liczba gmin'))

}

plot_profiles_volume_zlewnia <- func_plot_profiles_zlewnia ()
  
 
## Plot - profiles - Zlewnia region's level (region, static)

func_plot_profiles_volume_region <- function () {

  fig <<- plot_ly(subset(profiles_regiony, year == '2009'), x = ~region, y = ~MSP, type = "bar", name = 'MSP')
  fig <<- fig %>% add_trace(y = ~Duze_firmy, type = "bar", name = 'Duże firmy')
  fig <<- fig %>% add_trace(y = ~Rolnictwo, type = "bar", name = 'Rolnictwo')
  fig <<- fig %>% add_trace(y = ~Leśnictwo, type = "bar", name = 'Leśnictwo')
  fig <<- fig %>% add_trace(y = ~Rolnictwo_ekologiczne, type = "bar", name = 'Rolnictwo ekol.')
  fig <<- fig %>% add_trace(y = ~Kopalnictwo, type = "bar", name = 'Kopalnictwo')
  fig <<- fig %>% add_trace(y = ~Turystyka_agroturystyka, type = "bar", name = 'Turystyka agroturystyka')
  fig <<- fig %>% add_trace(y = ~Turystyka_rekreacja_wodna, type = "bar", name = 'Turystyka rekreacja wodna')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = 'Region'), yaxis = list(title = 'Liczba gmin'), barmode = 'group')
  
}
  
plot_profiles_volume_region <- func_plot_profiles_volume_region ()


## Plot - profiles - Zlewnia poviat's level (poviats, static)

func_plot_profiles_volume_powiat <- function (regio) {
  
  profiles_powiaty <- subset(profiles_powiaty, region == regio)
  fig <<- plot_ly(profiles_powiaty)
  fig <<- plot_ly(subset(profiles_powiaty, year == '2009'), x= ~powiat, y = ~MSP, type = "bar", name = 'MSP')
  fig <<- fig %>% add_trace(y = ~Duze_firmy, type = "bar", name = 'Duże firmy')
  fig <<- fig %>% add_trace(y = ~Rolnictwo, type = "bar", name = 'Rolnictwo')
  fig <<- fig %>% add_trace(y = ~Leśnictwo, type = "bar", name = 'Leśnictwo')
  fig <<- fig %>% add_trace(y = ~Rolnictwo_ekologiczne, type = "bar", name = 'Rolnictwo ekol.')
  fig <<- fig %>% add_trace(y = ~Kopalnictwo, type = "bar", name = 'Kopalnictwo')
  fig <<- fig %>% add_trace(y = ~Turystyka_agroturystyka, type = "bar", name = 'Turystyka agroturystyka')
  fig <<- fig %>% add_trace(y = ~Turystyka_rekreacja_wodna, type = "bar", name = 'Turystyka rekreacja wodna')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = ''), yaxis = list(title = 'Liczba gmin'), barmode = 'group')
  
}

plot_profiles_volume_powiat_lodzkie <- func_plot_profiles_volume_powiat ("łódzkie")
plot_profiles_volume_powiat_malopolskie <- func_plot_profiles_volume_powiat ("małopolskie")
plot_profiles_volume_powiat_mazowieckie <- func_plot_profiles_volume_powiat ("mazowieckie")
plot_profiles_volume_powiat_slaskie <- func_plot_profiles_volume_powiat ("śląskie")
plot_profiles_volume_powiat_swietokrzyskie <- func_plot_profiles_volume_powiat ("świętokrzyskie")


## Plot - profiles - Zlewnia gmina's level (gminas, static)

func_plot_profile_gmina <- function (gminaset) {
  
  variables_dataframe <- subset(variables_dataframe, name %in% gminaset)
  fig <<- plot_ly(subset(variables_dataframe, year == '2009'), x= ~name, y = ~Prof_MSP, type = "bar", name = 'MSP')
  fig <<- fig %>% add_trace(y = ~Prof_DF, type = "bar", name = 'Duże firmy')
  fig <<- fig %>% add_trace(y = ~Rolnictwo, type = "bar", name = 'Rolnictwo')
  fig <<- fig %>% add_trace(y = ~Leśnictwo, type = "bar", name = 'Leśnictwo')
  fig <<- fig %>% add_trace(y = ~Rolnictwo_ekologiczne, type = "bar", name = 'Rolnictwo ekol.')
  fig <<- fig %>% add_trace(y = ~Kopalnictwo, type = "bar", name = 'Kopalnictwo')
  fig <<- fig %>% add_trace(y = ~Turystyka_agroturystyka, type = "bar", name = 'Turystyka agroturystyka')
  fig <<- fig %>% add_trace(y = ~Turystyka_rekreacja_wodna, type = "bar", name = 'Turystyka rekreacja wodna')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = ''), yaxis = list(title = 'Liczba wskazań'), barmode = 'group')
  
}

plot_profile_gminaset_lodzki_wschodni <- func_plot_profile_gmina (gminaset_lodzki_wschodni)
plot_profile_gminaset_opoczenski <- func_plot_profile_gmina (gminaset_opoczenski)
plot_profile_gminaset_piotrkowski <- func_plot_profile_gmina (gminaset_piotrkowski)
plot_profile_gminaset_radomszczanski <- func_plot_profile_gmina (gminaset_radomszczanski)
plot_profile_gminaset_rawski <- func_plot_profile_gmina (gminaset_rawski)
plot_profile_gminaset_tomaszowski <- func_plot_profile_gmina (gminaset_tomaszowski)
plot_profile_gminaset_miastoPiotrkow <- func_plot_profile_gmina (gminaset_miastoPiotrkow)
plot_profile_gminaset_miechowski <- func_plot_profile_gmina (gminaset_miechowski)
plot_profile_gminaset_olkuski <- func_plot_profile_gmina (gminaset_olkuski)
plot_profile_gminaset_bialobrzeski <- func_plot_profile_gmina (gminaset_bialobrzeski)
plot_profile_gminaset_grojecki <- func_plot_profile_gmina (gminaset_grojecki)
plot_profile_gminaset_kozienicki <- func_plot_profile_gmina (gminaset_kozienicki)
plot_profile_gminaset_przysuski <- func_plot_profile_gmina (gminaset_przysuski)
plot_profile_gminaset_czestochowski <- func_plot_profile_gmina (gminaset_czestochowski)
plot_profile_gminaset_myszkowski <- func_plot_profile_gmina (gminaset_myszkowski)
plot_profile_gminaset_zawiercianski <- func_plot_profile_gmina (gminaset_zawiercianski)
plot_profile_gminaset_jedrzejowski <- func_plot_profile_gmina (gminaset_jedrzejowski)
plot_profile_gminaset_kielecki <- func_plot_profile_gmina (gminaset_kielecki)
plot_profile_gminaset_konecki <- func_plot_profile_gmina (gminaset_konecki)
plot_profile_gminaset_skarzyski <- func_plot_profile_gmina (gminaset_skarzyski)
plot_profile_gminaset_wloszczowski <- func_plot_profile_gmina (gminaset_wloszczowski)
plot_profile_gminaset_bialobrzeski <- func_plot_profile_gmina (gminaset_bialobrzeski)


## Table - profiles - Zlewnia gmina's level (gminas, static)

func_table_profile_gmina <- function(gminaset) {
  
  profiles_gminy <- profiles_gminy %>% filter(gmina %in% gminaset)
  profiles_gminy <- profiles_gminy[,c(-10)]
}

table_profile_gminaset_lodzki_wschodni <- func_table_profile_gmina (gminaset_lodzki_wschodni)
table_profile_gminaset_opoczenski <- func_table_profile_gmina (gminaset_opoczenski)
table_profile_gminaset_piotrkowski <- func_table_profile_gmina (gminaset_piotrkowski)
table_profile_gminaset_radomszczanski <- func_table_profile_gmina (gminaset_radomszczanski)
table_profile_gminaset_rawski <- func_table_profile_gmina (gminaset_rawski)
table_profile_gminaset_tomaszowski <- func_table_profile_gmina (gminaset_tomaszowski)
table_profile_gminaset_miastoPiotrkow <- func_table_profile_gmina (gminaset_miastoPiotrkow)
table_profile_gminaset_miechowski <- func_table_profile_gmina (gminaset_miechowski)
table_profile_gminaset_olkuski <- func_table_profile_gmina (gminaset_olkuski)
table_profile_gminaset_bialobrzeski <- func_table_profile_gmina (gminaset_bialobrzeski)
table_profile_gminaset_grojecki <- func_table_profile_gmina (gminaset_grojecki)
table_profile_gminaset_kozienicki <- func_table_profile_gmina (gminaset_kozienicki)
table_profile_gminaset_przysuski <- func_table_profile_gmina (gminaset_przysuski)
table_profile_gminaset_czestochowski <- func_table_profile_gmina (gminaset_czestochowski)
table_profile_gminaset_myszkowski <- func_table_profile_gmina (gminaset_myszkowski)
table_profile_gminaset_zawiercianski <- func_table_profile_gmina (gminaset_zawiercianski)
table_profile_gminaset_jedrzejowski <- func_table_profile_gmina (gminaset_jedrzejowski)
table_profile_gminaset_kielecki <- func_table_profile_gmina (gminaset_kielecki)
table_profile_gminaset_konecki <- func_table_profile_gmina (gminaset_konecki)
table_profile_gminaset_skarzyski <- func_table_profile_gmina (gminaset_skarzyski)
table_profile_gminaset_wloszczowski <- func_table_profile_gmina (gminaset_wloszczowski)



library(plotly)

## Plot - profiles - Zlewnia level (total, static)


func_plot_profiles_p_zlewnia <- function () {

fig <<- plot_ly(subset(profiles_zlewnia_p, year == '2009'), x = ~year, y= ~MSP, type = "bar", name = 'MSP')
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

plot_profiles_volume_p_zlewnia <- func_plot_profiles_p_zlewnia ()
  
 
## Plot - profiles - Zlewnia region's level (region, static)

func_plot_profiles_p_volume_region <- function () {

  fig <<- plot_ly(subset(profiles_regiony_p, year == '2009'), x = ~region, y = ~MSP, type = "bar", name = 'MSP')
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
  
plot_profiles_p_volume_region <- func_plot_profiles_p_volume_region ()


## Plot - profiles - Zlewnia poviat's level (poviats, static)

func_plot_profiles_p_volume_powiat <- function (regio) {
  
  profiles_powiaty_p <- subset(profiles_powiaty_p, region == regio)
  fig <<- plot_ly(profiles_powiaty_p)
  fig <<- plot_ly(subset(profiles_powiaty_p, year == '2009'), x= ~powiat, y = ~MSP, type = "bar", name = 'MSP')
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

plot_profiles_volume_powiat_p_lodzkie <- func_plot_profiles_p_volume_powiat ("łódzkie")
plot_profiles_volume_powiat_p_malopolskie <- func_plot_profiles_p_volume_powiat ("małopolskie")
plot_profiles_volume_powiat_p_mazowieckie <- func_plot_profiles_p_volume_powiat ("mazowieckie")
plot_profiles_volume_powiat_p_slaskie <- func_plot_profiles_p_volume_powiat ("śląskie")
plot_profiles_volume_powiat_p_swietokrzyskie <- func_plot_profiles_p_volume_powiat ("świętokrzyskie")


## Plot - profiles - Zlewnia gmina's level (gminas, static)

func_plot_profile_p_gmina <- function (gminaset) {
  
  variables_dataframe <- subset(variables_dataframe, name %in% gminaset)
  fig <<- plot_ly(subset(variables_dataframe, year == '2009'), x= ~name, y = ~Prof_MSP_p, type = "bar", name = 'MSP')
  fig <<- fig %>% add_trace(y = ~Prof_DF_p, type = "bar", name = 'Duże firmy')
  fig <<- fig %>% add_trace(y = ~Rolnictwo_p, type = "bar", name = 'Rolnictwo')
  fig <<- fig %>% add_trace(y = ~Leśnictwo_p, type = "bar", name = 'Leśnictwo')
  fig <<- fig %>% add_trace(y = ~Rolnictwo_ekologiczne_p, type = "bar", name = 'Rolnictwo ekol.')
  fig <<- fig %>% add_trace(y = ~Kopalnictwo_p, type = "bar", name = 'Kopalnictwo')
  fig <<- fig %>% add_trace(y = ~Turystyka_agroturystyka_p, type = "bar", name = 'Turystyka agroturystyka')
  fig <<- fig %>% add_trace(y = ~Turystyka_rekreacja_wodna_p, type = "bar", name = 'Turystyka rekreacja wodna')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = ''), yaxis = list(title = 'Liczba wskazań'), barmode = 'group')
  
}

plot_profile_gminaset_p_lodzki_wschodni <- func_plot_profile_p_gmina (gminaset_lodzki_wschodni)
plot_profile_gminaset_p_opoczenski <- func_plot_profile_p_gmina (gminaset_opoczenski)
plot_profile_gminaset_p_piotrkowski <- func_plot_profile_p_gmina (gminaset_piotrkowski)
plot_profile_gminaset_p_radomszczanski <- func_plot_profile_p_gmina (gminaset_radomszczanski)
plot_profile_gminaset_p_rawski <- func_plot_profile_p_gmina (gminaset_rawski)
plot_profile_gminaset_p_tomaszowski <- func_plot_profile_p_gmina (gminaset_tomaszowski)
plot_profile_gminaset_p_miastoPiotrkow <- func_plot_profile_p_gmina (gminaset_miastoPiotrkow)
plot_profile_gminaset_p_miechowski <- func_plot_profile_p_gmina (gminaset_miechowski)
plot_profile_gminaset_p_olkuski <- func_plot_profile_p_gmina (gminaset_olkuski)
plot_profile_gminaset_p_bialobrzeski <- func_plot_profile_p_gmina (gminaset_bialobrzeski)
plot_profile_gminaset_p_grojecki <- func_plot_profile_p_gmina (gminaset_grojecki)
plot_profile_gminaset_p_kozienicki <- func_plot_profile_p_gmina (gminaset_kozienicki)
plot_profile_gminaset_p_przysuski <- func_plot_profile_p_gmina (gminaset_przysuski)
plot_profile_gminaset_p_czestochowski <- func_plot_profile_p_gmina (gminaset_czestochowski)
plot_profile_gminaset_p_myszkowski <- func_plot_profile_p_gmina (gminaset_myszkowski)
plot_profile_gminaset_p_zawiercianski <- func_plot_profile_p_gmina (gminaset_zawiercianski)
plot_profile_gminaset_p_jedrzejowski <- func_plot_profile_p_gmina (gminaset_jedrzejowski)
plot_profile_gminaset_p_kielecki <- func_plot_profile_p_gmina (gminaset_kielecki)
plot_profile_gminaset_p_konecki <- func_plot_profile_p_gmina (gminaset_konecki)
plot_profile_gminaset_p_skarzyski <- func_plot_profile_p_gmina (gminaset_skarzyski)
plot_profile_gminaset_p_wloszczowski <- func_plot_profile_p_gmina (gminaset_wloszczowski)
plot_profile_gminaset_p_bialobrzeski <- func_plot_profile_p_gmina (gminaset_bialobrzeski)


## Table - profiles - Zlewnia gmina's level (gminas, static)

func_table_p_profile_gmina <- function(gminaset) {
  
  profiles_gminy_p <- profiles_gminy_p %>% filter(gmina %in% gminaset)
  profiles_gminy_p <- profiles_gminy_p[,c(-10)]
}

table_profile_p_gminaset_lodzki_wschodni <- func_table_p_profile_gmina (gminaset_lodzki_wschodni)
table_profile_p_gminaset_opoczenski <- func_table_p_profile_gmina (gminaset_opoczenski)
table_profile_p_gminaset_piotrkowski <- func_table_p_profile_gmina (gminaset_piotrkowski)
table_profile_p_gminaset_radomszczanski <- func_table_p_profile_gmina (gminaset_radomszczanski)
table_profile_p_gminaset_rawski <- func_table_p_profile_gmina (gminaset_rawski)
table_profile_p_gminaset_tomaszowski <- func_table_p_profile_gmina (gminaset_tomaszowski)
table_profile_p_gminaset_miastoPiotrkow <- func_table_p_profile_gmina (gminaset_miastoPiotrkow)
table_profile_p_gminaset_miechowski <- func_table_p_profile_gmina (gminaset_miechowski)
table_profile_p_gminaset_olkuski <- func_table_p_profile_gmina (gminaset_olkuski)
table_profile_p_gminaset_bialobrzeski <- func_table_p_profile_gmina (gminaset_bialobrzeski)
table_profile_p_gminaset_grojecki <- func_table_p_profile_gmina (gminaset_grojecki)
table_profile_p_gminaset_kozienicki <- func_table_p_profile_gmina (gminaset_kozienicki)
table_profile_p_gminaset_przysuski <- func_table_p_profile_gmina (gminaset_przysuski)
table_profile_p_gminaset_czestochowski <- func_table_p_profile_gmina (gminaset_czestochowski)
table_profile_p_gminaset_myszkowski <- func_table_p_profile_gmina (gminaset_myszkowski)
table_profile_p_gminaset_zawiercianski <- func_table_p_profile_gmina (gminaset_zawiercianski)
table_profile_p_gminaset_jedrzejowski <- func_table_p_profile_gmina (gminaset_jedrzejowski)
table_profile_p_gminaset_kielecki <- func_table_p_profile_gmina (gminaset_kielecki)
table_profile_p_gminaset_konecki <- func_table_p_profile_gmina (gminaset_konecki)
table_profile_p_gminaset_skarzyski <- func_table_p_profile_gmina (gminaset_skarzyski)
table_profile_p_gminaset_wloszczowski <- func_table_p_profile_gmina (gminaset_wloszczowski)



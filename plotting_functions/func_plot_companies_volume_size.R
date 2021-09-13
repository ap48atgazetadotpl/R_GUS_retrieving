
library(plotly)

## Plot - companies size in Zlewnia (shares, total, interactive)

func_plot_companies_zlewnia_size <- function () {
  
  companies_volume_zlewnia_size_orig <- companies_volume_zlewnia_size

  for (i in 1:27) {
    
    for (j in 2:6)
      
    {
      companies_volume_zlewnia_size[i,j] <- (as.double(companies_volume_zlewnia_size[i,j])/as.double(rowSums(companies_volume_zlewnia_size_orig[i,2:6])))*100
      
    }
    
  }
  
  fig <- plot_ly(companies_volume_zlewnia_size, x = ~year, y = ~sum9, type = 'bar', name = '0-9')
  fig <- fig %>% add_trace(y = ~sum49, name = '10-49')
  fig <- fig %>% add_trace(y = ~sum249, name = '50-249')
  fig <- fig %>% add_trace(y = ~sum999, name = '250-999')
  fig <- fig %>% add_trace(y = ~sum1000, name = '1000-')
  fig <- fig %>% layout(yaxis = list(title = 'Udział % w ogólnej liczbie podmiotów'), 
                        xaxis = list(title = 'Lata'), 
                        barmode = 'stack') 
  
  fig
  
  
}

plot_companies_zlewnia_size <- func_plot_companies_zlewnia_size ()


## Plot - companies size 49-249,249-999,1000 in Zlewnia (number, total, interactive)


func_plot_companies_volume_zlewnia_size_numb <- function () {
  
  companies_volume_zlewnia_size_number <- companies_volume_zlewnia_size
  companies_volume_zlewnia_size_number$year <- as.character.factor(companies_volume_zlewnia_size$year)
  companies_volume_zlewnia_size_number <- filter(companies_volume_zlewnia_size_number, year > 2011)
  fig <- plot_ly(companies_volume_zlewnia_size_number, x = ~year, y = ~sum49, type = 'bar', name = '10-49')
  #fig <- fig %>% add_trace(y = ~sum49, name = '10-49')
  fig <- fig %>% add_trace(y = ~sum249, name = '50-249')
  fig <- fig %>% add_trace(y = ~sum999, name = '250-999')
  fig <- fig %>% add_trace(y = ~sum1000, name = '1000-')
  fig <- fig %>% layout(yaxis = list(title = 'Liczba podmiotów (szt.)'), 
                        xaxis = list(title = 'Lata'), 
                        barmode = 'stack') 
  
  
}

companies_volume_zlewnia_size_numb <- func_plot_companies_volume_zlewnia_size_numb ()

## Plot - companies size in Zlewnia (regions, interactive)

func_plot_companies_volume_region_size <- function (regio) {
  
  companies_volume_regiony_size_orig <- subset(companies_volume_regiony_size, region == regio)
  companies_volume_regiony_size <- subset(companies_volume_regiony_size, region == regio)
  
  
  for (i in 1:27) {
    
    for (j in 3:7)
    
        {
          companies_volume_regiony_size[i,j] <- (as.double(companies_volume_regiony_size[i,j])/as.double(rowSums(companies_volume_regiony_size_orig[i,3:7])))*100
            
          }
    
  }
  
  fig <- plot_ly(companies_volume_regiony_size, x = ~year, y = ~sum9, type = 'bar', name = '0-9')
  fig <- fig %>% add_trace(y = ~sum49, name = '10-49')
  fig <- fig %>% add_trace(y = ~sum249, name = '50-249')
  fig <- fig %>% add_trace(y = ~sum999, name = '250-999')
  fig <- fig %>% add_trace(y = ~sum1000, name = '1000-')
  fig <- fig %>% layout(yaxis = list(title = 'Udział % w ogólnej liczbie podmiotów'), 
                        xaxis = list(title = 'Lata'), 
                        barmode = 'stack') 
  
  fig
  
}

plot_companies_volume_region_size_lodzkie <- func_plot_companies_volume_region_size ("łódzkie")
plot_companies_volume_region_size_malopolskie <- func_plot_companies_volume_region_size ("małopolskie")
plot_companies_volume_region_size_mazowieckie <- func_plot_companies_volume_region_size ("mazowieckie")
plot_companies_volume_region_size_slaskie <- func_plot_companies_volume_region_size ("śląskie")
plot_companies_volume_region_size_swietokrzyskie <- func_plot_companies_volume_region_size ("świętokrzyskie")



## Plot - companies size 49-249,249-999,1000 in Zlewnia (numbers, regions, interactive)


func_plot_companies_volume_region_size_numb <- function (regio) {
  
  companies_volume_regiony_size <- subset(companies_volume_regiony_size, region == regio)
  companies_volume_regiony_size$year <- as.character.factor(companies_volume_regiony_size$year)
  companies_volume_regiony_size <- filter(companies_volume_regiony_size, year > 2011)
  fig <- plot_ly(companies_volume_regiony_size, x = ~year, y = ~sum49, type = 'bar', name = '10-49')
  #fig <- fig %>% add_trace(y = ~sum49, name = '10-49')
  fig <- fig %>% add_trace(y = ~sum249, name = '50-249')
  fig <- fig %>% add_trace(y = ~sum999, name = '250-999')
  fig <- fig %>% add_trace(y = ~sum1000, name = '1000-')
  fig <- fig %>% layout(yaxis = list(title = 'Liczba podmiotów (szt.)'), 
                        xaxis = list(title = 'Lata'), 
                        barmode = 'stack') 
  
  
}

plot_companies_volume_region_size_numb_lodzkie <- func_plot_companies_volume_region_size_numb ("łódzkie")
plot_companies_volume_region_size_numb_malopolskie <- func_plot_companies_volume_region_size_numb ("małopolskie")
plot_companies_volume_region_size_numb_mazowieckie <- func_plot_companies_volume_region_size_numb ("mazowieckie")
plot_companies_volume_region_size_numb_slaskie <- func_plot_companies_volume_region_size_numb ("śląskie")
plot_companies_volume_region_size_numb_swietokrzyskie <- func_plot_companies_volume_region_size_numb ("świętokrzyskie")



##  Plot - companies size in Zlewnia (poviats, interactive)

func_plot_companies_volume_powiat_size <- function (powia) {
  
  companies_volume_powiaty_size$year <- as.character.factor(companies_volume_powiaty_size$year)
  companies_volume_powiaty_size <- filter(companies_volume_powiaty_size, year > 2011)
  companies_volume_powiaty_size <- subset(companies_volume_powiaty_size, powiat == powia)
  fig <- plot_ly(companies_volume_powiaty_size, x = ~year, y = ~sum9, type = 'bar', name = '0-9')
  fig <- fig %>% add_trace(y = ~sum49, name = '10-49')
  fig <- fig %>% add_trace(y = ~sum249, name = '50-249')
  fig <- fig %>% add_trace(y = ~sum999, name = '250-999')
  fig <- fig %>% add_trace(y = ~sum1000, name = '1000-')
  fig <- fig %>% layout(yaxis = list(title = 'Liczba podmiotów (szt.)'), 
                        xaxis = list(title = 'Lata'), 
                        barmode = 'stack') 
  
  
}


companies_volume_powiaty_size_lodzki_wschodni <- func_plot_companies_volume_powiat_size  ("Powiat łódzki wschodni")
companies_volume_powiaty_size_opoczenski <- func_plot_companies_volume_powiat_size  ("Powiat opoczyński")
companies_volume_powiaty_size_piotrkowski <- func_plot_companies_volume_powiat_size  ("Powiat piotrkowski")
companies_volume_powiaty_size_radomszczanski <- func_plot_companies_volume_powiat_size  ("Powiat radomszczański")
companies_volume_powiaty_size_rawski <- func_plot_companies_volume_powiat_size  ("Powiat rawski")
companies_volume_powiaty_size_tomaszowski <- func_plot_companies_volume_powiat_size  ("Powiat tomaszowski")
companies_volume_powiaty_size_miastoPiotrkow <- func_plot_companies_volume_powiat_size  ("Powiat m. Piotrków Trybunalski")
companies_volume_powiaty_size_miechowski <- func_plot_companies_volume_powiat_size  ("Powiat miechowski")
companies_volume_powiaty_size_olkuski <- func_plot_companies_volume_powiat_size  ("Powiat olkuski")
companies_volume_powiaty_size_bialobrzeski <- func_plot_companies_volume_powiat_size  ("Powiat białobrzeski")
companies_volume_powiaty_size_grojecki <- func_plot_companies_volume_powiat_size  ("Powiat grójecki")
companies_volume_powiaty_size_kozienicki <- func_plot_companies_volume_powiat_size  ("Powiat kozienicki")
companies_volume_powiaty_size_przysuski <- func_plot_companies_volume_powiat_size  ("Powiat przysuski")
companies_volume_powiaty_size_czestochowski <- func_plot_companies_volume_powiat_size  ("Powiat częstochowski")
companies_volume_powiaty_size_myszkowski <- func_plot_companies_volume_powiat_size  ("Powiat myszkowski")
companies_volume_powiaty_size_zawiercianski <- func_plot_companies_volume_powiat_size  ("Powiat zawierciański")
companies_volume_powiaty_size_jedrzejowski <- func_plot_companies_volume_powiat_size  ("Powiat jędrzejowski")
companies_volume_powiaty_size_kielecki <- func_plot_companies_volume_powiat_size  ("Powiat kielecki")
companies_volume_powiaty_size_konecki <- func_plot_companies_volume_powiat_size  ("Powiat konecki")
companies_volume_powiaty_size_skarzyski <- func_plot_companies_volume_powiat_size  ("Powiat skarżyski")
companies_volume_powiaty_size_wloszczowski <- func_plot_companies_volume_powiat_size  ("Powiat włoszczowski")
 

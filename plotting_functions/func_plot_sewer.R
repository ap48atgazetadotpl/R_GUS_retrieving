
library(plotly)
library(gapminder)
library(dplyr)
library(lubridate)


##  Plot - sewers % in Zlewnia (total, histogram)

func_plot_sewer_total <- function () {
  
  hist_sewer_total <- variables_dataframe %>% mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
    subset("2020" > rok & rok > "2001") %>% 
    ggplot( aes(x=sew, color=rok, fill=rok)) +
    geom_histogram(alpha=0.6, binwidth = 5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("% podłączonych gospodarstw") +
    ylab("Liczba gmin") +
    facet_wrap(~rok)
  
  
  hist_sewer_total
  
}

plot_sewer_total <- func_plot_sewer_total  ()


##  Plot - sewers % in Zlewnia (regions, histogram)

func_plot_sewer_region <- function (regio) {
  
  hist_sewer_region <- variables_dataframe  %>% mutate(year, rok = as.character.factor(variables_dataframe$year))
  hist_sewer_region <- hist_sewer_region %>% subset(region == regio)
  hist_sewer_region <- hist_sewer_region %>% subset("2020" > rok & rok > "2001")
  hist_sewer_region <- ggplot(hist_sewer_region, aes(x=sew, color=year, fill=year)) +
    geom_histogram(alpha=0.6, binwidth = 5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("% podłączonych gospodarstw") +
    ylab("Liczba gmin") +
    facet_wrap(~year)
  
  hist_sewer_region
    
}
  
  plot_sewer_region_lodzkie <- func_plot_sewer_region ("łódzkie")
  plot_sewer_region_malopolskie <- func_plot_sewer_region ("małopolskie")
  plot_sewer_region_mazowieckie <- func_plot_sewer_region ("mazowieckie")
  plot_sewer_region_slaskie <- func_plot_sewer_region ("śląskie")
  plot_sewer_region_swietokrzyskie <- func_plot_sewer_region ("świętokrzyskie")


##  Plot - sewers % in Zlewnia (regions, interactive)
  
  func_plot_sewer_gminy <- function () {
  
    fig1 <<- variables_dataframe %>%   mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
      subset("2020" > rok & rok > "2001") %>% 
      plot_ly(
        x = ~sew, 
        y = ~inhab, 
        color = ~region, 
        frame = ~rok, 
        text = ~name, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      )  
    
    fig1 <<- fig1 %>% layout(
      xaxis = list(
        title = "% gosp. podłączonych do s. kanalizacyjnej" 
      ), 
      yaxis = list(
        title = "Liczba mieszkańców gminy"
      ),
      title = "Podłączenie do s. kanalizacyjnej w gminach Zlewni: 2002 - 2019",
      annotations = 
        list(x = 0.1, y = -0.1, text = "Źródło: GUS, BDL", 
             showarrow = F, xref='paper', yref='paper', 
             xanchor='right', yanchor='auto', xshift=0, yshift=0,
             font=list(size=10, color="black"))
    ) %>% animation_slider(
      currentvalue = list(prefix = "Rok: ", font = list(color="blue"))
    ) 
    
    
    fig1
}
  
  plot_sewer_gminy <- func_plot_sewer_gminy ()
  
  
##  Plot - sewers % in Zlewnia (powiats, interactive)
  
  func_plot_sewer_powiaty <- function (powiaset) {
  
    fig <- variables_dataframe %>%  mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
      subset("2020" > rok & rok > "2001") %>% 
      filter(powiat %in% powiaset) %>% 
      plot_ly(
        x = ~sew, 
        y = ~inhab, 
        color = ~name, 
        frame = ~rok, 
        text = ~name, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      )  
    
    fig <<- fig %>% layout(
      xaxis = list(
        title = "% gosp. podłączonych do s. kanalizacyjnej" 
      ), 
      yaxis = list(
        title = "Liczba mieszkańców gminy"
      ),
      title = "Podłączenie do s. kanalizacyjnej w gminach województwa Zlewni: 2002 - 2019", 
      annotations = 
        list(x = 1, y = -0.1, text = "Źródło: GUS, BDL", 
             showarrow = F, xref='paper', yref='paper', 
             xanchor='right', yanchor='auto', xshift=0, yshift=0,
             font=list(size=10, color="black"))
          ) 
    fig 
    
}
  
  
  plot_sewer_powiat_lodzkie <- func_plot_sewer_powiaty (powiatset_lodzkie)
  plot_sewer_powiat_malopolskie <- func_plot_sewer_powiaty  (powiatset_malopolskie)
  plot_sewer_powiat_mazowieckie <- func_plot_sewer_powiaty  (powiatset_mazowieckie)
  plot_sewer_powiat_slaskie <- func_plot_sewer_powiaty  (powiatset_slaskie)
  plot_sewer_powiat_swietokrzyskie <- func_plot_sewer_powiaty  (powiatset_swietokrzyskie)
  

##  Plot - sewers % in Zlewnia (gminy, interactive)
  
  func_plot_sewer_gmina <- function (gminaset)
  {
    fig <<- variables_dataframe %>%  mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
      subset("2020" > rok & rok > "2001") %>% 
      filter(name %in% gminaset) %>% 
      plot_ly(
        x = ~sew, 
        y = ~inhab, 
        color = ~name, 
        frame = ~rok, 
        text = ~name, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      )  
    
    fig <<- fig %>% layout(
      xaxis = list(
        title = "% gosp. podłączonych do s. kanalizacyjnej" 
      ), 
      yaxis = list(
        title = "Liczba mieszkańców gminy"
      ),
      title = "Podłączenie do s. kanalizacyjnej w gminach powiatu: 2002 - 2019", 
      annotations = 
        list(x = 0.1, y = -0.1, text = "Źródło: GUS, BDL", 
             showarrow = F, xref='paper', yref='paper', 
             xanchor='right', yanchor='auto', xshift=0, yshift=0,
             font=list(size=10, color="black")) 
    ) %>% animation_slider(
      currentvalue = list(prefix = "Rok: ", font = list(color="blue"))
    )
    
    
    fig 
    
    
  }
  
  
  plot_sewer_gminaset_lodzki_wschodni <- func_plot_sewer_gmina (gminaset_lodzki_wschodni)
  plot_sewer_gminaset_opoczenski <- func_plot_sewer_gmina (gminaset_opoczenski)
  plot_sewer_gminaset_piotrkowski <- func_plot_sewer_gmina (gminaset_piotrkowski)
  plot_sewer_gminaset_radomszczanski <- func_plot_sewer_gmina (gminaset_radomszczanski)
  plot_sewer_gminaset_rawski <- func_plot_sewer_gmina (gminaset_rawski)
  plot_sewer_gminaset_tomaszowski <- func_plot_sewer_gmina (gminaset_tomaszowski)
  plot_sewer_gminaset_miastoPiotrkow <- func_plot_sewer_gmina (gminaset_miastoPiotrkow)
  plot_sewer_gminaset_miechowski <- func_plot_sewer_gmina (gminaset_miechowski)
  plot_sewer_gminaset_olkuski <- func_plot_sewer_gmina (gminaset_olkuski)
  plot_sewer_gminaset_bialobrzeski <- func_plot_sewer_gmina (gminaset_bialobrzeski)
  plot_sewer_gminaset_grojecki <- func_plot_sewer_gmina (gminaset_grojecki)
  plot_sewer_gminaset_kozienicki <- func_plot_sewer_gmina (gminaset_kozienicki)
  plot_sewer_gminaset_przysuski <- func_plot_sewer_gmina (gminaset_przysuski)
  plot_sewer_gminaset_czestochowski <- func_plot_sewer_gmina (gminaset_czestochowski)
  plot_sewer_gminaset_myszkowski <- func_plot_sewer_gmina (gminaset_myszkowski)
  plot_sewer_gminaset_zawiercianski <- func_plot_sewer_gmina (gminaset_zawiercianski)
  plot_sewer_gminaset_jedrzejowski <- func_plot_sewer_gmina (gminaset_jedrzejowski)
  plot_sewer_gminaset_kielecki <- func_plot_sewer_gmina (gminaset_kielecki)
  plot_sewer_gminaset_konecki <- func_plot_sewer_gmina (gminaset_konecki)
  plot_sewer_gminaset_skarzyski <- func_plot_sewer_gmina (gminaset_skarzyski)
  plot_sewer_gminaset_wloszczowski <- func_plot_sewer_gmina (gminaset_wloszczowski)
  plot_sewer_gminaset_bialobrzeski <- func_plot_sewer_gmina (gminaset_bialobrzeski)
  
  
  
##  Plot - sewers % in Zlewnia (gminy, static)
  
  
  func_plot_sewer_2_gmina <- function (gminaset) {
    
    sewerdata <- as.data.frame(variables_dataframe[, c(1:3,6, 37,38)]) 
    sewerdata <- sewerdata %>% pivot_wider(names_from = year, values_from = sew, values_fn = mean) 
    sewerdata <- filter(sewerdata, name %in% gminaset)
    fig <<- plot_ly(sewerdata, x= ~name, y = ~sewerdata$'2002', type = "bar", name = '2002')
    fig <<- fig %>% add_trace(y = ~sewerdata$'2010', type = "bar", name = '2010')
    fig <<- fig %>% add_trace(y = ~sewerdata$'2019', type = "bar", name = '2019')
    fig <<- fig %>% layout(yaxis=list(type='linear'))
    fig <<- fig %>% layout(xaxis = list(title = ''), yaxis = list(title = '% gosp. podłączonych do s. kanalizacyjnej'), barmode = 'group')
    
    
  }
  

  plot_sewer_2_lodzki_wschodni <- func_plot_sewer_2_gmina (gminaset_lodzki_wschodni)
  plot_sewer_2_opoczenski <- func_plot_sewer_2_gmina (gminaset_opoczenski)
  plot_sewer_2_piotrkowski <- func_plot_sewer_2_gmina (gminaset_piotrkowski)
  plot_sewer_2_radomszczanski <- func_plot_sewer_2_gmina (gminaset_radomszczanski)
  plot_sewer_2_rawski <- func_plot_sewer_2_gmina (gminaset_rawski)
  plot_sewer_2_tomaszowski <- func_plot_sewer_2_gmina (gminaset_tomaszowski)
  plot_sewer_2_miastoPiotrkow <- func_plot_sewer_2_gmina (gminaset_miastoPiotrkow)
  plot_sewer_2_miechowski <- func_plot_sewer_2_gmina (gminaset_miechowski)
  plot_sewer_2_olkuski <- func_plot_sewer_2_gmina (gminaset_olkuski)
  plot_sewer_2_bialobrzeski <- func_plot_sewer_2_gmina (gminaset_bialobrzeski)
  plot_sewer_2_grojecki <- func_plot_sewer_2_gmina (gminaset_grojecki)
  plot_sewer_2_kozienicki <- func_plot_sewer_2_gmina (gminaset_kozienicki)
  plot_sewer_2_przysuski <- func_plot_sewer_2_gmina (gminaset_przysuski)
  plot_sewer_2_czestochowski <- func_plot_sewer_2_gmina (gminaset_czestochowski)
  plot_sewer_2_myszkowski <- func_plot_sewer_2_gmina (gminaset_myszkowski)
  plot_sewer_2_zawiercianski <- func_plot_sewer_2_gmina (gminaset_zawiercianski)
  plot_sewer_2_jedrzejowski <- func_plot_sewer_2_gmina (gminaset_jedrzejowski)
  plot_sewer_2_kielecki <- func_plot_sewer_2_gmina (gminaset_kielecki)
  plot_sewer_2_konecki <- func_plot_sewer_2_gmina (gminaset_konecki)
  plot_sewer_2_skarzyski <- func_plot_sewer_2_gmina (gminaset_skarzyski)
  plot_sewer_2_wloszczowski <- func_plot_sewer_2_gmina (gminaset_wloszczowski)
  
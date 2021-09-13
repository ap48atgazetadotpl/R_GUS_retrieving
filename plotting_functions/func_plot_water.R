
library(plotly)
library(gapminder)
library(dplyr)
library(lubridate)

##  Plot - waterworks % in Zlewnia (total, histogram)

func_plot_water_total <- function () {
  
     hist_water_total <- variables_dataframe  %>% mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
       subset("2020" > rok & rok > "2001") %>% 
      ggplot( aes(x=water, color=rok, fill=rok)) +
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
      
    hist_water_total
    
}
  
  plot_water_total <- func_plot_water_total  ()
  

##  Plot - waterworks % in Zlewnia (regions, histogram)
  
  func_plot_water_region <- function (regio) {
    
    hist_water_region <- variables_dataframe  %>% mutate(year, rok = as.character.factor(variables_dataframe$year)) 
    hist_water_region <- hist_water_region %>% subset(region == regio)  
    hist_water_region <- hist_water_region %>% subset("2020" > rok & rok > "2001")
      hist_water_region <- ggplot(hist_water_region, aes(x=water, color=year, fill=year)) +
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
    
    hist_water_region
    
  }
  
  plot_water_region_lodzkie <- func_plot_water_region ("łódzkie")
  plot_water_region_malopolskie <- func_plot_water_region ("małopolskie")
  plot_water_region_mazowieckie <- func_plot_water_region ("mazowieckie")
  plot_water_region_slaskie <- func_plot_water_region ("śląskie")
  plot_water_region_swietokrzyskie <- func_plot_water_region ("świętokrzyskie")
  

  ##  Plot - waterworks % in Zlewnia (regions, interactive)
  
  func_plot_water_gminy <- function () {
    
    fig1 <<- variables_dataframe %>%   mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
      subset("2020" > rok & rok > "2001") %>% 
      plot_ly(
        x = ~water, 
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
        title = "% gosp. podłączonych do s. wodociągowej" 
      ), 
      yaxis = list(
        title = "Liczba mieszkańców gminy"
      ),
      title = "Podłączenie do s. wodociągowej w gminach Zlewni: 2002 - 2019",
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
  
  plot_water_gminy <- func_plot_water_gminy ()
  
  
  
##  Plot - waterworks % in Zlewnia (powiats, interactive)
  
  
  func_plot_water_powiaty <- function (powiaset)
  {
    fig <<- variables_dataframe %>%  mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
      subset("2020" > rok & rok > "2001") %>% 
      filter(powiat %in% powiaset) %>% 
      plot_ly(
        x = ~water, 
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
        title = "% gosp. podłączonych do s. wodociągowej" 
      ), 
      yaxis = list(
        title = "Liczba mieszkańców gminy"
      ),
      title = "Podłączenie do s. wodociągowej w gminach województw Zlewni: 2002 - 2019", 
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
  
  
  plot_water_powiat_lodzkie <- func_plot_water_powiaty (powiatset_lodzkie)
  plot_water_powiat_malopolskie <- func_plot_water_powiaty (powiatset_malopolskie)
  plot_water_powiat_mazowieckie <- func_plot_water_powiaty (powiatset_mazowieckie)
  plot_water_powiat_slaskie <- func_plot_water_powiaty (powiatset_slaskie)
  plot_water_powiat_swietokrzyskie <- func_plot_water_powiaty (powiatset_swietokrzyskie)
  

##  Plot - waterworks % in Zlewnia (gminy, interactive) - skipped
  
  
##  Plot - waterworks % in Zlewnia (gminy, static)
  
  func_plot_water_2_gmina <- function (gminaset) {
    
    waterdata <- as.data.frame(variables_dataframe[, c(1:3,5, 37,38)]) 
    waterdata <- waterdata %>% pivot_wider(names_from = year, values_from = water, values_fn = mean) 
    waterdata <- filter(waterdata, name %in% gminaset)
    fig <<- plot_ly(waterdata, x= ~name, y = ~waterdata$'2002', type = "bar", name = '2002')
    fig <<- fig %>% add_trace(y = ~waterdata$'2010', type = "bar", name = '2010')
    fig <<- fig %>% add_trace(y = ~waterdata$'2019', type = "bar", name = '2019')
    fig <<- fig %>% layout(yaxis=list(type='linear'))
    fig <<- fig %>% layout(xaxis = list(title = ''), yaxis = list(title = '% gosp. podłączonych do s. wodociągowej'), barmode = 'group')
    
  }
  
  plot_water_2_lodzki_wschodni <- func_plot_water_2_gmina (gminaset_lodzki_wschodni)
  plot_water_2_opoczenski <- func_plot_water_2_gmina (gminaset_opoczenski)
  plot_water_2_piotrkowski <- func_plot_water_2_gmina (gminaset_piotrkowski)
  plot_water_2_radomszczanski <- func_plot_water_2_gmina (gminaset_radomszczanski)
  plot_water_2_rawski <- func_plot_water_2_gmina (gminaset_rawski)
  plot_water_2_tomaszowski <- func_plot_water_2_gmina (gminaset_tomaszowski)
  plot_water_2_miastoPiotrkow <- func_plot_water_2_gmina (gminaset_miastoPiotrkow)
  plot_water_2_miechowski <- func_plot_water_2_gmina (gminaset_miechowski)
  plot_water_2_olkuski <- func_plot_water_2_gmina (gminaset_olkuski)
  plot_water_2_bialobrzeski <- func_plot_water_2_gmina (gminaset_bialobrzeski)
  plot_water_2_grojecki <- func_plot_water_2_gmina (gminaset_grojecki)
  plot_water_2_kozienicki <- func_plot_water_2_gmina (gminaset_kozienicki)
  plot_water_2_przysuski <- func_plot_water_2_gmina (gminaset_przysuski)
  plot_water_2_czestochowski <- func_plot_water_2_gmina (gminaset_czestochowski)
  plot_water_2_myszkowski <- func_plot_water_2_gmina (gminaset_myszkowski)
  plot_water_2_zawiercianski <- func_plot_water_2_gmina (gminaset_zawiercianski)
  plot_water_2_jedrzejowski <- func_plot_water_2_gmina (gminaset_jedrzejowski)
  plot_water_2_kielecki <- func_plot_water_2_gmina (gminaset_kielecki)
  plot_water_2_konecki <- func_plot_water_2_gmina (gminaset_konecki)
  plot_water_2_skarzyski <- func_plot_water_2_gmina (gminaset_skarzyski)
  plot_water_2_wloszczowski <- func_plot_water_2_gmina (gminaset_wloszczowski)
  plot_water_2_bialobrzeski <- func_plot_water_2_gmina (gminaset_bialobrzeski)
  
  
  
  

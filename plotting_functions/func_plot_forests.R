
library(plotly)
library(gapminder)
library(dplyr)
library(lubridate)

source("transforming_functions/create_aggregations.R")

##  Plot - forestsworks % in Zlewnia (total, histogram)

func_plot_forests_total <- function () {
  
  hist_forests_total <- variables_dataframe  %>% mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
    subset("2020" > rok & rok > "2001") %>% 
    ggplot( aes(x=forests, color=rok, fill=rok)) +
    geom_histogram(alpha=0.6, binwidth = 5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("Lesistość %") +
    ylab("Liczba gmin") +
    facet_wrap(~rok)
  
  hist_forests_total
  
}

plot_forests_total <- func_plot_forests_total  ()



##  Plot - forestsworks % in Zlewnia only 2002 and 2019  overlapping (total, histogram)

func_plot_forests_total2 <- function () {
  
  hist_forests_total <- variables_dataframe  %>% mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
    subset("2020" > rok & rok > "2001") %>% 
    ggplot( aes(x=forests, color=year, fill=year)) +
    geom_histogram(data=subset(variables_dataframe, year == 2002), alpha=0.6, binwidth = 5) +
    geom_histogram(data=subset(variables_dataframe, year == 2019), alpha=0.6, binwidth = 5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum() +
    theme(
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("Lesistość %") +
    ylab("Liczba gmin") 
  
  
  hist_forests_total
  
}

forests_zlewnia2 <- func_plot_forests_total2 ()



##  Plot - forestsworks % in Zlewnia (regions, histogram)

func_plot_forests_region <- function (regio) {
  
  hist_forests_region <- variables_dataframe  %>% mutate(year, rok = as.character.factor(variables_dataframe$year)) 
  hist_forests_region <- hist_forests_region %>% subset(region == regio)  
  hist_forests_region <- hist_forests_region %>% subset("2020" > rok & rok > "2001")
  hist_forests_region <- ggplot(hist_forests_region, aes(x=forests, color=year, fill=year)) +
    geom_histogram(alpha=0.6, binwidth = 5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("Lesistość %") +
    ylab("Liczba gmin") +
    facet_wrap(~year)
  
  hist_forests_region
  
}

plot_forests_region_lodzkie <- func_plot_forests_region ("łódzkie")
plot_forests_region_malopolskie <- func_plot_forests_region ("małopolskie")
plot_forests_region_mazowieckie <- func_plot_forests_region ("mazowieckie")
plot_forests_region_slaskie <- func_plot_forests_region ("śląskie")
plot_forests_region_swietokrzyskie <- func_plot_forests_region ("świętokrzyskie")


##  Plot - forestsworks % in Zlewnia (regions, interactive)

func_plot_forests_gminy <- function () {
  
  fig1 <<- variables_dataframe %>%   mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
    subset("2020" > rok & rok > "2001") %>% 
    plot_ly(
      x = ~forests, 
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
      title = "Lesistość %" 
    ), 
    yaxis = list(
      title = "Liczba mieszkańców gminy"
    ),
    title = "Lesistość (%) w gminach Zlewni: 2002 - 2019",
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

plot_forests_gminy <- func_plot_forests_gminy ()



##  Plot - forestsworks % in Zlewnia (powiats, interactive)


func_plot_forests_powiaty <- function (powiaset)
{
  fig <<- variables_dataframe %>%  mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
    subset("2020" > rok & rok > "2001") %>% 
    filter(powiat %in% powiaset) %>% 
    plot_ly(
      x = ~forests, 
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
      title = "Lesistość %" 
    ), 
    yaxis = list(
      title = "Liczba mieszkańców gminy"
    ),
    title = "Lesistość (%) w gminach Zlewni: 2002 - 2019", 
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


plot_forests_powiat_lodzkie <- func_plot_forests_powiaty (powiatset_lodzkie)
plot_forests_powiat_malopolskie <- func_plot_forests_powiaty (powiatset_malopolskie)
plot_forests_powiat_mazowieckie <- func_plot_forests_powiaty (powiatset_mazowieckie)
plot_forests_powiat_slaskie <- func_plot_forests_powiaty (powiatset_slaskie)
plot_forests_powiat_swietokrzyskie <- func_plot_forests_powiaty (powiatset_swietokrzyskie)


##  Plot - forestsworks % in Zlewnia (gminy, interactive) - skipped


##  Plot - forestsworks % in Zlewnia (gminy, static)

func_plot_forests_2_gmina <- function (gminaset) {
  
  forestsdata <- as.data.frame(variables_dataframe[, c(1:3,42, 37,38)]) 
  forestsdata <- forestsdata %>% pivot_wider(names_from = year, values_from = forests, values_fn = mean) 
  forestsdata <- filter(forestsdata, name %in% gminaset)
  fig <<- plot_ly(forestsdata, x= ~name, y = ~forestsdata$'2002', type = "bar", name = '2002')
  fig <<- fig %>% add_trace(y = ~forestsdata$'2010', type = "bar", name = '2010')
  fig <<- fig %>% add_trace(y = ~forestsdata$'2019', type = "bar", name = '2019')
  fig <<- fig %>% layout(yaxis=list(type='linear'))
  fig <<- fig %>% layout(xaxis = list(title = ''), yaxis = list(title = 'Lesistość (%)'), barmode = 'group')
  
}

plot_forests_2_lodzki_wschodni <- func_plot_forests_2_gmina (gminaset_lodzki_wschodni)
plot_forests_2_opoczenski <- func_plot_forests_2_gmina (gminaset_opoczenski)
plot_forests_2_piotrkowski <- func_plot_forests_2_gmina (gminaset_piotrkowski)
plot_forests_2_radomszczanski <- func_plot_forests_2_gmina (gminaset_radomszczanski)
plot_forests_2_rawski <- func_plot_forests_2_gmina (gminaset_rawski)
plot_forests_2_tomaszowski <- func_plot_forests_2_gmina (gminaset_tomaszowski)
plot_forests_2_miastoPiotrkow <- func_plot_forests_2_gmina (gminaset_miastoPiotrkow)
plot_forests_2_miechowski <- func_plot_forests_2_gmina (gminaset_miechowski)
plot_forests_2_olkuski <- func_plot_forests_2_gmina (gminaset_olkuski)
plot_forests_2_bialobrzeski <- func_plot_forests_2_gmina (gminaset_bialobrzeski)
plot_forests_2_grojecki <- func_plot_forests_2_gmina (gminaset_grojecki)
plot_forests_2_kozienicki <- func_plot_forests_2_gmina (gminaset_kozienicki)
plot_forests_2_przysuski <- func_plot_forests_2_gmina (gminaset_przysuski)
plot_forests_2_czestochowski <- func_plot_forests_2_gmina (gminaset_czestochowski)
plot_forests_2_myszkowski <- func_plot_forests_2_gmina (gminaset_myszkowski)
plot_forests_2_zawiercianski <- func_plot_forests_2_gmina (gminaset_zawiercianski)
plot_forests_2_jedrzejowski <- func_plot_forests_2_gmina (gminaset_jedrzejowski)
plot_forests_2_kielecki <- func_plot_forests_2_gmina (gminaset_kielecki)
plot_forests_2_konecki <- func_plot_forests_2_gmina (gminaset_konecki)
plot_forests_2_skarzyski <- func_plot_forests_2_gmina (gminaset_skarzyski)
plot_forests_2_wloszczowski <- func_plot_forests_2_gmina (gminaset_wloszczowski)
plot_forests_2_bialobrzeski <- func_plot_forests_2_gmina (gminaset_bialobrzeski)





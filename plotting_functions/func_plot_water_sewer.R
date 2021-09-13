
library(plotly)
library(gapminder)
library(dplyr)
library(lubridate)

##  Plot - sewers _ water % in Zlewnia (gminy, interactive)


func_plot_sewer_water <- function ()
{
  fig3 <<- variables_dataframe %>%   mutate(year, rok = as.character.factor(variables_dataframe$year)) %>% 
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
  
  fig3 <<- fig3 %>% layout(
    xaxis = list(
      title = "% gosp. podłączonych do s. kanalizacyjnej" 
    ), 
    yaxis = list(
      title = "% gosp. podłączonych do s. wodociągowej"
    ),
    title = "Podłączenie do s. wodociągowej i kanalizacyjnej w gminach Zlewni: 2002 - 2019",
    annotations = 
      list(x = 0.1, y = -0.1, text = "Źródło: GUS, BDL", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black"))
  ) %>% animation_slider(
    currentvalue = list(prefix = "Rok: ", font = list(color="blue"))
  )
  
  
  fig3
  #fig <- subplot(fig1,fig2, nrows = 2) 
  
  
}


plot_sewer_water <- func_plot_sewer_water ()





library(plotly)
library(gapminder)
library(dplyr)
library(lubridate)

## Plot - treatments with biogens cleaning in Zlewnia (total, interactive)

func_plot_treatments <- function () {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  df <- treatments_zlewnia 
  df <- subset(df, year < "2021")
  fig <- df 
  fig <- fig %>% accumulate_by(~year)
   
  fig <- fig %>%
    plot_ly(
      x = ~year,
      y = ~oczyszczalnie,
      frame = ~frame, 
      type = 'scatter',
      mode = 'lines', 
      line = list(simplyfy = F)
    )
  fig <- fig %>% layout(
    xaxis = list(
      title = "Lata",
      zeroline = F
    ),
    yaxis = list(
      title = "Liczba oczyszczalni z ocz. biogenów (szt.)",
      #type = "log",
      zeroline = F
    ),
    annotations = 
      list(x = 1, y = -0.1, text = "Źródło: GUS, BDL", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black"))
  ) 
  fig <- fig %>% animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  )
  fig <- fig %>% animation_slider(
    hide = T
  )
  fig <- fig %>% animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
  
  fig
  
}

plot_treatments <- func_plot_treatments ()


## Plot - treatments with biogens cleaning in Zlewnia (regions, interactive)

library(plotly)

func_plot_treat_region <- function () {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  df <- treatments_regiony
  df <- subset(df, year < "2021")
  fig <- df %>%
    filter(region %in% c("łódzkie", "małopolskie", "świętokrzyskie", "mazowieckie", "śląskie"))
  fig <- fig %>% accumulate_by(~year)
  
  
  fig <- fig %>%
    plot_ly(
      x = ~year, 
      y = ~oczyszczalnie,
      split = ~region,
      frame = ~frame, 
      type = 'scatter',
      mode = 'lines', 
      line = list(simplyfy = F)
    )
  fig <- fig %>% layout(
    xaxis = list(
      title = "Lata",
      zeroline = F
    ),
    yaxis = list(
      title = "Liczba oczyszczalni z ocz. biogenów (szt.)",
      zeroline = F
    )
  ) 
  fig <- fig %>% animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  )
  fig <- fig %>% animation_slider(
    hide = T
  )
  fig <- fig %>% animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
  
  fig
  
}

plot_treat_region <- func_plot_treat_region ()


## Plot - treatments with biogens cleaning in Zlewnia (powiaty, interactive)


func_plot_treat_powiat <- function (powia, powiaset) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  
  fig <- treatments_powiaty  %>%  filter(powiat %in% powiaset)
  fig <- subset(fig, year < "2021")
  
  fig <- fig %>% accumulate_by(~year)
  
  fig <- fig %>%
    plot_ly(
      x = ~year, 
      y = ~oczyszczalnie,
      split = ~powiat,
      frame = ~frame, 
      type = 'scatter',
      mode = 'lines', 
      line = list(simplyfy = F)
    )
  fig <- fig %>% layout(
    xaxis = list(
      title = "Lata",
      zeroline = F
    ),
    yaxis = list(
      title = "Liczba oczyszczalni (szt.)",
      #type = "log",
      zeroline = F
    ),
    annotations = 
      list(x = 1, y = -0.1, text = "Żródło: GUS, BDL", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black"))
  ) 
  fig <- fig %>% animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  )
  fig <- fig %>% animation_slider(
    hide = T
  )
  fig <- fig %>% animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
  
}

plot_treat_powiat_lodzkie <- func_plot_treat_powiat (treatments_powiaty_lodzkie, powiatset_lodzkie)
plot_treat_powiat_malopolskie <- func_plot_treat_powiat (treatments_powiaty_malopolskie, powiatset_malopolskie)
plot_treat_powiat_mazowieckie <- func_plot_treat_powiat (treatments_powiaty_mazowieckie, powiatset_mazowieckie)
plot_treat_powiat_slaskie <- func_plot_treat_powiat (treatments_powiaty_slaskie, powiatset_slaskie)
plot_treat_powiat_swietokrzyskie <- func_plot_treat_powiat (treatments_powiaty_swietokrzyskie, powiatset_swietokrzyskie)


## Table with treatments in Zlewnia - number in powiat with division for gmina 

library(plotly)

func_table_treat_gmina <- function(gminaset) {
  
  treatments_gminy <- treatments_gminy %>% filter(name %in% gminaset)
  treatments_gminy <- treatments_gminy[,c(-1,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-31)]
  treatments_gminy <- arrange(treatments_gminy, by = name)
}
 
  
table_treat_gminaset_lodzki_wschodni <- func_table_treat_gmina (gminaset_lodzki_wschodni)
table_treat_gminaset_opoczenski <- func_table_treat_gmina (gminaset_opoczenski)
table_treat_gminaset_piotrkowski <- func_table_treat_gmina (gminaset_piotrkowski)
table_treat_gminaset_radomszczanski <- func_table_treat_gmina (gminaset_radomszczanski)
table_treat_gminaset_rawski <- func_table_treat_gmina (gminaset_rawski)
table_treat_gminaset_tomaszowski <- func_table_treat_gmina (gminaset_tomaszowski)
table_treat_gminaset_miastoPiotrkow <- func_table_treat_gmina (gminaset_miastoPiotrkow)
table_treat_gminaset_miechowski <- func_table_treat_gmina (gminaset_miechowski)
table_treat_gminaset_olkuski <- func_table_treat_gmina (gminaset_olkuski)
table_treat_gminaset_bialobrzeski <- func_table_treat_gmina (gminaset_bialobrzeski)
table_treat_gminaset_grojecki <- func_table_treat_gmina (gminaset_grojecki)
table_treat_gminaset_kozienicki <- func_table_treat_gmina (gminaset_kozienicki)
table_treat_gminaset_przysuski <- func_table_treat_gmina (gminaset_przysuski)
table_treat_gminaset_czestochowski <- func_table_treat_gmina (gminaset_czestochowski)
table_treat_gminaset_myszkowski <- func_table_treat_gmina (gminaset_myszkowski)
table_treat_gminaset_zawiercianski <- func_table_treat_gmina (gminaset_zawiercianski)
table_treat_gminaset_jedrzejowski <- func_table_treat_gmina (gminaset_jedrzejowski)
table_treat_gminaset_kielecki <- func_table_treat_gmina (gminaset_kielecki)
table_treat_gminaset_konecki <- func_table_treat_gmina (gminaset_konecki)
table_treat_gminaset_skarzyski <- func_table_treat_gmina (gminaset_skarzyski)
table_treat_gminaset_wloszczowski <- func_table_treat_gmina (gminaset_wloszczowski)









 
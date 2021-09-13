
library(plotly)
library(gapminder)
library(dplyr)
library(lubridate)


## Plot - inhabitants in Zlewnia (total, interactive)
  
func_plot_inhab_zlewnia <- function () {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  df <- inhabitants_zlewnia 
  df <- subset(df, year < "2021")
  fig <- df 
  fig <- fig %>% accumulate_by(~year)
   
  fig <- fig %>%
    plot_ly(
      x = ~year,
      y = ~inhab_zlewnia,
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
      title = "Liczba mieszkańców (tys.)",
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

plot_inhab_zlewnia <- func_plot_inhab_zlewnia ()


## Plot - inhabitants in Zlewnia (regions, interactive)

func_plot_inhab_region <- function () {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  df <- inhabitants_regiony
  df <- subset(df, year < "2021")
  fig <- df %>%
    filter(region %in% c("łódzkie", "małopolskie", "świętokrzyskie", "mazowieckie", "śląskie"))
  fig <- fig %>% accumulate_by(~year)
  
  
  fig <- fig %>%
    plot_ly(
      x = ~year, 
      y = ~inhab_regiony,
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
      title = "Liczba mieszkańców (tys.)",
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

plot_inhab_region <- func_plot_inhab_region ()


##  Plot - inhabitants in Zlewnia (poviats, interactive)

func_plot_inhab_powiat <- function (powia, powiaset) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  
  fig <- inhabitants_powiaty  %>%  filter(powiat %in% powiaset)
  fig <- subset(fig, year < "2021")
  
  fig <- fig %>% accumulate_by(~year)
  
  fig <- fig %>%
    plot_ly(
      x = ~year, 
      y = ~inhab_powiaty,
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
      title = "Liczba mieszkańców (tys.)",
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

plot_inhab_powiat_lodzkie <- func_plot_inhab_powiat (inhabitants_powiaty_lodzkie, powiatset_lodzkie)
plot_inhab_powiat_malopolskie <- func_plot_inhab_powiat (inhabitants_powiaty_malopolskie, powiatset_malopolskie)
plot_inhab_powiat_mazowieckie <- func_plot_inhab_powiat (inhabitants_powiaty_mazowieckie, powiatset_mazowieckie)
plot_inhab_powiat_slaskie <- func_plot_inhab_powiat (inhabitants_powiaty_slaskie, powiatset_slaskie)
plot_inhab_powiat_swietokrzyskie <- func_plot_inhab_powiat (inhabitants_powiaty_swietokrzyskie, powiatset_swietokrzyskie)


##  Plot - inhabitants in Zlewnia (gminy, interactive)

func_plot_inhab_gmina <- function (gminaset) {
  
  
  fig <- inhabitants_gminy  %>%  filter(name %in% gminaset)
  
  fig <- subset(fig, year < "2021")
  
  #fig <- fig %>% accumulate_by(~year)
  
  fig <- fig %>% 
   plot_ly(
      x = ~year, 
      y = ~inhab,
      split = ~name,
     # frame = ~frame, 
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
      title = "Liczba mieszkańców (tys.)",
      #type = "log",
      zeroline = F
    ),
    annotations = 
      list(x = 1, y = -0.1, text = "Żródło: GUS, BDL", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black"))
  ) 

  fig
  
}

plot_inhab_gminaset_lodzki_wschodni <- func_plot_inhab_gmina (gminaset_lodzki_wschodni)
plot_inhab_gminaset_opoczenski <- func_plot_inhab_gmina (gminaset_opoczenski)
plot_inhab_gminaset_piotrkowski <- func_plot_inhab_gmina (gminaset_piotrkowski)
plot_inhab_gminaset_radomszczanski <- func_plot_inhab_gmina (gminaset_radomszczanski)
plot_inhab_gminaset_rawski <- func_plot_inhab_gmina (gminaset_rawski)
plot_inhab_gminaset_tomaszowski <- func_plot_inhab_gmina (gminaset_tomaszowski)
plot_inhab_gminaset_miastoPiotrkow <- func_plot_inhab_gmina (gminaset_miastoPiotrkow)
plot_inhab_gminaset_miechowski <- func_plot_inhab_gmina (gminaset_miechowski)
plot_inhab_gminaset_olkuski <- func_plot_inhab_gmina (gminaset_olkuski)
plot_inhab_gminaset_bialobrzeski <- func_plot_inhab_gmina (gminaset_bialobrzeski)
plot_inhab_gminaset_grojecki <- func_plot_inhab_gmina (gminaset_grojecki)
plot_inhab_gminaset_kozienicki <- func_plot_inhab_gmina (gminaset_kozienicki)
plot_inhab_gminaset_przysuski <- func_plot_inhab_gmina (gminaset_przysuski)
plot_inhab_gminaset_czestochowski <- func_plot_inhab_gmina (gminaset_czestochowski)
plot_inhab_gminaset_myszkowski <- func_plot_inhab_gmina (gminaset_myszkowski)
plot_inhab_gminaset_zawiercianski <- func_plot_inhab_gmina (gminaset_zawiercianski)
plot_inhab_gminaset_jedrzejowski <- func_plot_inhab_gmina (gminaset_jedrzejowski)
plot_inhab_gminaset_kielecki <- func_plot_inhab_gmina (gminaset_kielecki)
plot_inhab_gminaset_konecki <- func_plot_inhab_gmina (gminaset_konecki)
plot_inhab_gminaset_skarzyski <- func_plot_inhab_gmina (gminaset_skarzyski)
plot_inhab_gminaset_wloszczowski <- func_plot_inhab_gmina (gminaset_wloszczowski)


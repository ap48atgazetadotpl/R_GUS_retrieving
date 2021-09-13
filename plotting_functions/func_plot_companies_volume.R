
library(plotly)

## Plot - companies in Zlewnia (total, interactive)

func_plot_companies_zlewnia <- function () {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  df <- companies_volume_zlewnia
  fig <- df 
  fig <- fig %>% accumulate_by(~year)
  
  fig <- fig %>%
    plot_ly(
      x = ~year, 
      y = ~comp,
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
      title = "Liczba podmiotów (tys.)",
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
  
}

plot_companies_zlewnia <- func_plot_companies_zlewnia ()


## Plot - companies in Zlewnia (regions, interactive)

func_plot_companies_volume_region <- function () {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  df <-  companies_volume_regiony
  fig <- df %>%
    filter(region %in% c("łódzkie", "małopolskie", "świętokrzyskie", "mazowieckie", "śląskie"))
  fig <- fig %>% accumulate_by(~year)
  
  
  fig <- fig %>%
    plot_ly(
      x = ~year, 
      y = ~comp,
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
      title = "Liczba podmiotów (tys.)",
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


plot_companies_volume_region <- func_plot_companies_volume_region ()


##  Plot - companies in Zlewnia (poviats, interactive)

func_plot_companies_volume_powiat <- function (powia, powiaset) {
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  df <- companies_volume_powiaty
  fig <- df  %>%  filter(powiat %in% powiaset)
  fig <- fig %>% accumulate_by(~year)
  
  fig <- fig %>%
    plot_ly(
      x = ~year, 
      y = ~comp,
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
      title = "Liczba podmiotów (tys.)",
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

plot_companies_volume_powiat_lodzkie <- func_plot_companies_volume_powiat (companies_volume_powiaty_lodzkie, powiatset_lodzkie)
plot_companies_volume_powiat_malopolskie <- func_plot_companies_volume_powiat (companies_volume_powiaty_malopolskie, powiatset_malopolskie)
plot_companies_volume_powiat_mazowieckie <- func_plot_companies_volume_powiat (companies_volume_powiaty_mazowieckie, powiatset_mazowieckie)
plot_companies_volume_powiat_slaskie <- func_plot_companies_volume_powiat (companies_volume_powiaty_slaskie, powiatset_slaskie)
plot_companies_volume_powiat_swietokrzyskie <- func_plot_companies_volume_powiat (companies_volume_powiaty_swietokrzyskie, powiatset_swietokrzyskie)


##  Plot - companies in Zlewnia (gminy, interactive)

func_plot_companies__volume_gmina <- function (gminaset) {
  
  fig <- companies_gminy  %>%  filter(name %in% gminaset)
  
  fig <- subset(fig, year > "2011")
  
  #fig <- fig %>% accumulate_by(~year)
  
  fig <- fig %>% 
    plot_ly(
      x = ~year, 
      y = ~comp,
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
      title = "Liczba podmiotów",
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


plot_comp_vol_gminaset_lodzki_wschodni <- func_plot_companies__volume_gmina (gminaset_lodzki_wschodni)
plot_comp_vol_gminaset_opoczenski <- func_plot_companies__volume_gmina (gminaset_opoczenski)
plot_comp_vol_gminaset_piotrkowski <- func_plot_companies__volume_gmina (gminaset_piotrkowski)
plot_comp_vol_gminaset_radomszczanski <- func_plot_companies__volume_gmina (gminaset_radomszczanski)
plot_comp_vol_gminaset_rawski <- func_plot_companies__volume_gmina (gminaset_rawski)
plot_comp_vol_gminaset_tomaszowski <- func_plot_companies__volume_gmina (gminaset_tomaszowski)
plot_comp_vol_gminaset_miastoPiotrkow <- func_plot_companies__volume_gmina (gminaset_miastoPiotrkow)
plot_comp_vol_gminaset_miechowski <- func_plot_companies__volume_gmina (gminaset_miechowski)
plot_comp_vol_gminaset_olkuski <- func_plot_companies__volume_gmina (gminaset_olkuski)
plot_comp_vol_gminaset_bialobrzeski <- func_plot_companies__volume_gmina (gminaset_bialobrzeski)
plot_comp_vol_gminaset_grojecki <- func_plot_companies__volume_gmina (gminaset_grojecki)
plot_comp_vol_gminaset_kozienicki <- func_plot_companies__volume_gmina (gminaset_kozienicki)
plot_comp_vol_gminaset_przysuski <- func_plot_companies__volume_gmina (gminaset_przysuski)
plot_comp_vol_gminaset_czestochowski <- func_plot_companies__volume_gmina (gminaset_czestochowski)
plot_comp_vol_gminaset_myszkowski <- func_plot_companies__volume_gmina (gminaset_myszkowski)
plot_comp_vol_gminaset_zawiercianski <- func_plot_companies__volume_gmina (gminaset_zawiercianski)
plot_comp_vol_gminaset_jedrzejowski <- func_plot_companies__volume_gmina (gminaset_jedrzejowski)
plot_comp_vol_gminaset_kielecki <- func_plot_companies__volume_gmina (gminaset_kielecki)
plot_comp_vol_gminaset_konecki <- func_plot_companies__volume_gmina (gminaset_konecki)
plot_comp_vol_gminaset_skarzyski <- func_plot_companies__volume_gmina (gminaset_skarzyski)
plot_comp_vol_gminaset_wloszczowski <- func_plot_companies__volume_gmina (gminaset_wloszczowski)
 

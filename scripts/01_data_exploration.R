source("./scripts/global.R")
working_data <- readRDS("./data/working_data.rds")

working_data %>% ggplot(aes(x = shooting_rate)) + geom_histogram()

mapview(working_data, zcol = "shooting_rate") + mapview(working_data, zcol = "covid_rate")



generate_bimap <- function(.data, xcol, ycol, style, bidim = 3, xlab, ylab, title, subtitle){
  
  xcol <- enquo(xcol)
  ycol <- enquo(ycol)
  
  bidata <- biscale::bi_class(.data = .data, x = !!xcol, y = !!ycol, style = "quantile", dim = bidim)
  
  legend <- bi_legend(pal = "DkBlue",
                      dim = bidim,
                      xlab = xlab,
                      ylab = ylab,
                      size = 10)
  map <- ggplot() +
    geom_sf(data = bidata, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = bidim) +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    bi_theme() +
    theme(title = element_text(size = 14))
  
  ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.78, 0.2, 0.2, 0.2, scale = 1.2)
}

p1 <- generate_bimap(working_data, x = shooting_rate, y = covid_rate, style = "quantile", 
                     xlab = "More shootings ", ylab = "More infections ", 
                     title = "Shootings and COVID-19",
                     subtitle = "Shootings per 100k and COVID-19 infection rates per 100k people") ; p1

# p2 <- generate_bimap(working_data, x = shooting_rate_jitter, y = covid_rate, style = "quantile", 
#                      xlab = "More shootings ", ylab = "More infections ", 
#                      title = "Shootings and COVID-19",
#                      subtitle = "Shootings per 100k and COVID-19 infection rates per 100k people") ; p2

p3 <- generate_bimap(working_data, x = shooting_rate, y = equity_index, style = "quantile", 
                     xlab = "More shootings ", ylab = "More vulnerable ", 
                     title = "Shootings and Equity",
                     subtitle = "Shootings per 100k and equity index") ; p3

p4 <- generate_bimap(working_data, x = shooting_rate, y = camps_reported, style = "quantile", 
                     xlab = "More shootings ", ylab = "More camps ", 
                     title = "Shootings and Camp Reportings",
                     subtitle = "Shootings per 100k and number of camps reported") ; p4

p5 <- generate_bimap(working_data, x = covid_rate, y = camps_reported, style = "quantile", 
                     xlab = "More infections ", ylab = "More camps ", 
                     title = "COVID-19 Infections and Camp Reportings",
                     subtitle = "COVID-19 infections per 100k and number of camps reported") ; p5


ggsave(file="shooting_v_covid.svg", plot=p1, width=8, height=5)




source("./scripts/global.R")
library(extrafont) ; font_import() ; loadfonts(device = "win")
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
    theme(title = element_text(size = 14)) #  text=element_text(family="Segoe UI")
  
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
                     xlab = "More shootings ", ylab = "Higher equity need ", 
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

p6 <- generate_bimap(working_data, x = shooting_rate, y = housing_index, style = "quantile", 
                     xlab = "More shootings ", ylab = "Higher housing need ", 
                     title = "Shootings and Housing Vulnerability",
                     subtitle = "Shootings per 100k and housing index") ; p6

p7 <- generate_bimap(working_data, x = shooting_rate, y = covid_index, style = "quantile", 
                     xlab = "More shootings ", ylab = "Higher COVID need ", 
                     title = "Shootings and COVID-19 Vulnerability",
                     subtitle = "Shootings per 100k and COVID index") ; p7

p8 <- generate_bimap(working_data, x = shooting_rate, y = total_index, style = "quantile", 
                     xlab = "More shootings ", ylab = "Higher total need ", 
                     title = "Shootings and Total Vulnerability",
                     subtitle = "Shootings per 100k and COVID housing and equity index") ; p8

height <- 7
width <- 8

ggsave(file="./plots/shooting_v_covid.svg", plot=p1, width=width, height=height)
ggsave(file="./plots/shooting_v_equity.svg", plot=p3, width=width, height=height)
ggsave(file="./plots/shooting_v_homeless.svg", plot=p4, width=width, height=height)
ggsave(file="./plots/covid_v_homeless.svg", plot=p5, width=width, height=height)
ggsave(file="./plots/shooting_v_housing.svg", plot=p6, width=width, height=height)
ggsave(file="./plots/shooting_v_covid-index.svg", plot=p7, width=width, height=height)
ggsave(file="./plots/shooting_v_total-index.svg", plot=p8, width=width, height=height)




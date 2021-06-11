source("./scripts/global.R")

working_data <- readRDS("./data/working_data.rds") %>%
  mutate(cn_score_mean_inverse = 100 - cn_score_mean,
         qtl_shooting_rate = ntile(range01(shooting_rate), 100) * 2, 
         qtl_covid_rate = ntile(range01(covid_rate), 100),
         qtl_equity_index = ntile(range01(equity_index), 100), 
         qtl_cn_score_mean_inverse = ntile(range01(cn_score_mean_inverse), 100),
         prioritization_index = range01(qtl_shooting_rate + qtl_covid_rate + qtl_equity_index + qtl_cn_score_mean_inverse) * 100)

working_data %>% ggplot(aes(x = shooting_rate)) + geom_histogram()

mapview(working_data, zcol = "shooting_rate") + mapview(working_data, zcol = "covid_rate")


##### Bivariate Choropleth Maps ######
generate_bimap <- function(.data, xcol, ycol, style, bidim = 3, xlab, ylab, title, subtitle){
  
  xcol <- enquo(xcol)
  ycol <- enquo(ycol)
  
  bidata <<- biscale::bi_class(.data = .data, x = !!xcol, y = !!ycol, style = "quantile", dim = bidim)
  
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
    theme(title = element_text(size = 14)) # text=element_text(family="Segoe UI")
  
  ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.92, 0.85, 0.2, 0.2, scale = 1.2, hjust = 1, vjust = 1)
}

p1 <- generate_bimap(working_data, x = shooting_rate, y = covid_rate, style = "quantile", 
                     xlab = "More shootings ", ylab = "More infections ", 
                     title = "Shootings and COVID-19",
                     subtitle = "Shootings per 100k and COVID-19 infection rates per 100k people") ; p1

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

p9 <- generate_bimap(working_data, x = shooting_rate, y = cn_score_mean_inverse, style = "quantile", 
                     xlab = "More shootings ", ylab = "Less complete ", 
                     title = "Shootings and Neighborhood Completeness",
                     subtitle = "Shootings per 100k and Complete Neighborhood Score") ; p9

p10<-working_data %>% ggplot() +
  geom_sf(aes(fill = prioritization_index), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_viridis(option = "inferno") +
  labs(
    title = "Composite Prioritization Index",
    subtitle = "Shootings, COVID-19 Infections, Equity and Completeness",
    fill = "Prioritization Index"
  ) +
  bi_theme() +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.position="bottom")


p11<-working_data %>% 
  mutate(equity_index_quantile = equity_index_quantile * 100) %>%
  ggplot() +
  geom_sf(aes(fill = equity_index_quantile), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_viridis(option = "viridis") +
  labs(
    title = "Equity Index Quantile",
    subtitle = "By Census Tract",
    caption = "Source: Urban Institute. Analysis by Portland Bureau of Planning & Sustainability.",
    fill = "Equity Index Quantile"
  ) +
  bi_theme() +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 10),#legend.position="bottom"
        ) ; p11

p12<-working_data %>%  ggplot() +
  geom_sf(aes(fill = covid_rate), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_viridis(option = "plasma") +
  labs(
    title = "COVID-19 Infection Rate",
    subtitle = "By Census Tract",
    caption = "Source: Oregon Health Authority. Analysis by Portland Bureau of Planning & Sustainability.",
    fill = "Infection Rate"
  ) +
  bi_theme() +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 10),#legend.position="bottom"
  ) ; p12

height <- 7
width <- 8

ggsave(file="./plots/shooting_v_covid.svg", plot=p1, width=width, height=height)
ggsave(file="./plots/shooting_v_equity.svg", plot=p3, width=width, height=height)
ggsave(file="./plots/shooting_v_homeless.svg", plot=p4, width=width, height=height)
ggsave(file="./plots/covid_v_homeless.svg", plot=p5, width=width, height=height)
ggsave(file="./plots/shooting_v_housing.svg", plot=p6, width=width, height=height)
ggsave(file="./plots/shooting_v_covid-index.svg", plot=p7, width=width, height=height)
ggsave(file="./plots/shooting_v_total-index.svg", plot=p8, width=width, height=height)
ggsave(file="./plots/shooting_v_completeness.svg", plot=p9, width=width, height=height)
ggsave(file="./plots/composite_prioritization.svg", plot=p10, width=width, height=height)
ggsave(file="./plots/base_equity_index.svg", plot=p11, width=width, height=height)
ggsave(file="./plots/base_covid_rate.svg", plot=p12, width=width, height=height)

##### Charts and Graphs ######
glimpse(working_data)

custom.col <- c("#FFDB6D", "#C4961A", 
                "#D16103", "#52854C", "#4E84C4", "#293352", "#F4EDCA",
                "#C3D7A4")

g1 <- working_data %>%
  ggplot(aes(x = shooting_rate, y = covid_rate, size = total_shootings, color = liaison_distrct, text=neighborhood)) +
  geom_point(alpha = 5/10) +
  # geom_smooth(fullrange= TRUE, aes(group = 1), method='lm', color = "black", alpha = 0.6) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_log10() +
  scale_color_manual(values = custom.col) +
  # scale_y_continuous(labels = scales::percent_format())+
  theme_minimal() +
  theme(text=element_text(family="Open Sans SemiBold"),
        # axis.title.x=element_blank(),
        # legend.position = "bottom"
  ) +
  labs(title = "Shootings and COVID-19 Infections", subtitle = "Census Tract-Level Estimates by District, May 2020 — May 2021",
       x = "Shooting Rate (Log scale)", y = "COVID-19 Infection Rate", 
       color = "District", size = "Total Shootings",
       caption = "Note: Data pulled 5/28/2021. Rates expressed per 100,000 residents.\nShooting data are those after 5/20/2020. Infection rates are for duration of COVID-19. \n\nSource: Portland Police Bureau; Oregon Health Authority; Data pulled 5/28/2021. \n Analysis by Portland Bureau of Planning & Sustainability.")

ggsave(file="./plots/GRAPH_shootings_v_covid.svg", plot=g1, width=7.2, height=5)

g2 <- working_data %>%
  ggplot(aes(x = cn_score_mean, y = covid_rate, size = shooting_rate, color = liaison_distrct, text=neighborhood)) +
  geom_point(alpha = 5/10) +
  scale_y_continuous(labels = scales::comma_format()) +
  # scale_x_log10() +
  scale_color_manual(values = custom.col) +
  # scale_y_continuous(labels = scales::percent_format())+
  theme_minimal() +
  theme(text=element_text(family="Open Sans SemiBold"),
        # axis.title.x=element_blank(),
        # legend.position = "bottom"
  ) +
  labs(title = "COVID-19 Infections and Neighborhood Completeness", subtitle = "Census Tract-Level Estimates by District, May 2020 — May 2021",
       x = "Complete Neighborhood Score", y = "COVID-19 Infection Rate", 
       color = "District", size = "Shooting Rate",
       caption = "Note: Data pulled 5/28/2021. Rates expressed per 100,000 residents.\nShooting data are those after 5/20/2020. Infection rates are for duration of COVID-19. \n\nSource: Portland Police Bureau; Oregon Health Authority; Data pulled 5/28/2021. \n Analysis by Portland Bureau of Planning & Sustainability.")

ggsave(file="./plots/GRAPH_completeness_v_covid.svg", plot=g2, width=7.2, height=5)

working_data %>%
  arrange(desc(prioritization_index)) %>%
  st_drop_geometry() %>%
  select(neighborhood, prioritization_index, shooting_rate, covid_rate, total_shootings, equity_index_quantile, cn_score_mean) %>%
  clipr::write_clip()




g3 <- working_data %>%
  mutate(equity_index_quantile = equity_index_quantile * 100) %>%
  ggplot(aes(x = equity_index_quantile, y = covid_rate, size = shooting_rate, color = liaison_distrct, text=neighborhood)) +
  geom_point(alpha = 5/10) +
  scale_y_continuous(labels = scales::comma_format()) +
  # scale_x_log10() +
  scale_color_manual(values = custom.col) +
  # scale_y_continuous(labels = scales::percent_format())+
  theme_minimal() +
  theme(text=element_text(family="Open Sans SemiBold"),
        # axis.title.x=element_blank(),
        # legend.position = "bottom"
  ) +
  labs(title = "Equity Index and COVID-19 Infections", subtitle = "Census Tract-Level Estimates by District",
       caption = "Note: Data pulled 5/28/2021. Rates expressed per 100,000 residents.\nInfection rates are for duration of COVID-19.\n\nSource: Urban Institute; Oregon Health Authority; Data pulled 5/28/2021. \n Analysis by Portland Bureau of Planning & Sustainability.",
       x = "Equity Index Quantile", y = "COVID-19 Infection Rate", 
       color = "District", size = "Shooting Rate" ) ; g3

ggsave(file="./plots/GRAPH_equity_v_covid.svg", plot=g3, width=7.2, height=5)




### Shootings
shootings2 <- shootings %>%
  st_transform(4326) %>%
  sfc_as_cols(., names = c("longitude", "latitude"))

box <- st_bbox(shootings2)
shootings_kde <- raster(kde2d(x = st_coordinates(shootings2)[,1], y = st_coordinates(shootings2)[,2], 
                              h = .03, n = c(700,700), 
                              lims = c(box[1]-.17, box[3]+.17, box[2]-.12, box[4]+.12)))

tm_shape(shootings_kde) +
  tm_raster(style = "cont", legend.show = TRUE, alpha = 0.7, palette = "inferno", title = "Shootings Density") +
  tm_scale_bar(position = c("left", "bottom"))


### Camps reported
camp_reportings_weekly2 <- camp_reportings_weekly %>%
  filter(STATUS == "Open") %>%
  st_transform(4326) %>%
  sfc_as_cols(., names = c("longitude", "latitude"))

box <- st_bbox(camp_reportings_weekly2)
camps_kde <- raster(kde2d(x = st_coordinates(camp_reportings_weekly2)[,1], y = st_coordinates(camp_reportings_weekly2)[,2], 
                              h = .03, n = c(700,700), 
                              lims = c(box[1]-.17, box[3]+.17, box[2]-.12, box[4]+.12)))

tm_shape(camps_kde) +
  tm_raster(style = "cont", legend.show = TRUE, alpha = 0.5, palette = "inferno", title = "Camps Density") +
  tm_scale_bar(position = c("left", "bottom"))


### Camps HUCIRP
camps_hucirp2 <- camps_hucirp %>%
  filter(is_active == 1) %>%
  st_transform(4326) %>%
  sfc_as_cols(., names = c("longitude", "latitude"))

box <- st_bbox(camps_hucirp2)
camps_hucirp_kde <- raster(kde2d(x = st_coordinates(camps_hucirp2)[,1], y = st_coordinates(camps_hucirp2)[,2], 
                          h = .03, n = c(700,700), 
                          lims = c(box[1]-.27, box[3]+.27, box[2]-.19, box[4]+.19)))

tm_shape(camps_hucirp_kde) +
  tm_raster(style = "cont", legend.show = TRUE, alpha = 0.7, palette = "inferno", title = "Camps Density\n(HUCIRP)") +
  tm_scale_bar(position = c("left", "bottom"))


liaison_districts %>%
  arrange(desc(liaison_distrct)) %>%
  mapview(., zcol = "liaison_distrct", layer.name = "Liaison District")

library(plotly)
plotly::ggplotly(g1)
plotly::ggplotly(g2)
plotly::ggplotly(g3)

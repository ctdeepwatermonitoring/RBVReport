setwd("P:/RBVReport-main/Annual_Report_Development_2024")

library(dplyr)
library(leaflet)
library(sf)
library(htmltools)

data <- read.csv("RBV_Summary_2024.csv")
sites <- read.csv("awx_stations_webservice2024.csv")
colnames(sites)[1] <- ("AWQ..Station")
data <- merge(data, sites, by = "AWQ..Station")

rivers <- read_sf(dsn = ".", layer = "CT_305b_Assessed_River_2022")
rivers <- st_transform(rivers, crs = 4326) #this was giving me some funky errors

fullaql <- rivers %>% 
  filter(CT2022_A_1 == "Fully Supporting")

bad <- rivers %>% 
  filter(CT2022_A_1 == "Not Supporting") 

CT <- read_sf(dsn = ".", layer = "cb_2022_us_state_500k")
CT <- subset(CT, CT$STUSPS == "CT")
CT <- st_transform(CT, crs = 4326)

circle <- makeIcon(
  iconUrl = "circle marker.png", # Replace with actual URL or path to the icon
  iconWidth = 18, iconHeight = 18
)

yellow_star <- makeIcon(
  iconUrl = "map marker.png", # Replace with actual URL or path to the icon
  iconWidth = 25, iconHeight = 25
)

data$icon <- ifelse(data$RBV_most_wanted_count <= 3, "circle", "yellow_star")

data_black_circle <- data %>% filter(icon == "circle")
data_yellow_star <- data %>% filter(icon == "yellow_star")

legend_labels <- c("Sites with 3 or less MW", "Sites with 4+ MW", 
                   "Rivers supporting full AQL", "Impaired rivers for AQL")
legend_colors <- c("#0D2D6C", "#F2AB19", "#00AAE7", "darkred")

leaflet(options = leafletOptions(minZoom = 8, maxZoom = 16)) %>%
  setView(lng = -72.6246, lat = 41.49601, zoom = 8.9) %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "Esri GrayCanvas (default)") %>%
  addPolylines(data = fullaql, color = "#00AAE7", label = ~ASSESSME_1, opacity = 1, weight = 1.5, group = "Rivers") %>%
  addPolylines(data = bad, color = "darkred", label = ~ASSESSME_1, opacity = 1, weight = 1.5, group = "Rivers") %>%
  addPolylines(data = CT, color = "black", opacity = 1, weight = 1.5) %>%
  addMarkers(
    data = data_black_circle,
    ~xlong,
    ~ylat,
    icon = circle,
    label = ~lapply(paste0(AWQ..Station, ": ", WaterbodyName.x, "<br>", "MW Count: ", "<b>", RBV_most_wanted_count, "</b>"), htmltools::HTML),
    labelOptions = labelOptions(permanent = FALSE, direction = "top",
                                style = list("font-size" = "12px")),
    group = 'Sites with 3 or less MW'
  ) %>%
  addMarkers(
    data = data_yellow_star,
    ~xlong,
    ~ylat,
    icon = yellow_star,
    label = ~lapply(paste0(AWQ..Station, ": ", WaterbodyName.x, "<br>", "MW Count: ", "<b>", RBV_most_wanted_count, "</b>"), htmltools::HTML),
    labelOptions = labelOptions(permanent = FALSE, direction = "top",
                                style = list("font-size" = "12px")),
    group = 'Sites with 4+ MW'
  ) %>%
  addLegend(
    position = "bottomright",
    colors = legend_colors,
    labels = legend_labels,
    title = "Legend"
  ) %>%
  addLayersControl(
    baseGroups = c('Esri GrayCanvas (default)', 'Open Street Map', 'World Imagery'),
    overlayGroups = c('Sites with 3 or less MW', 'Sites with 4+ MW', 'Rivers'),
    options = layersControlOptions(collapsed = FALSE)
  )

# ```{r pabysite, echo=FALSE, out.width="100%"}
# pabysite <- read.csv("presence_absence_1.csv", check.names = FALSE)
# 
# pabysite$Station.ID <- as.character(pabysite$Station.ID)
# 
# datatable(head(pabysite, 59), colnames = gsub("[.]", " ", names(pabysite)), options = list(
#   pageLength = 10, lengthChange = FALSE,
#   lengthMenu = c(20)
# ))
# ```

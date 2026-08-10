
library(stringr)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(kableExtra)
library(DBI)
library(odbc)
library(keyring)
### pulling most recent AWX stations
con <- dbConnect(odbc(), 
                 Driver = "MySQL ODBC 9.0 ANSI Driver", 
                 Server = "sdc-epafiling", 
                 Database = "awqx", 
                 Trusted_Connection = "True",
                 uid = key_list("sdc-epafiling")[1,2],
                 pwd = key_get("sdc-epafiling", "readonly_user"))

# Pull full list of stations from awX
site_select <- ("SELECT * FROM awqx.stations;")
sites <- dbGetQuery(con, site_select)
# Disconnect from database 
dbDisconnect(con)


data <- read.csv("RBV_Summary_2025.csv") #this will need to be updated to 2024, generated from metrics script
data <- merge(data, sites, by = "staSeq")

# since there are multiple samples at one site, I'm only keeping the highest value per site ID
data <- data %>%
  group_by(staSeq) %>%
  slice_max(order_by = RBV_most_wanted_count, n = 1, with_ties = FALSE) %>%
  ungroup()


# adding the assessment layer 
GIS_Segment <- "C:/Users/LandryJes/Documents/RBVReport/Annual_Report_Development_2025/CT 2024 IWQR Segments Final.gdb"
River_Layer = "CT2024IWQR_River_USES_FINAL"

# Reading in the geodatabase file
Rivers <- st_read(dsn = GIS_Segment, layer = River_Layer, quiet = TRUE)
Rivers <- st_transform(Rivers, crs = 4326)

# Assigning full/ not support to color code
fullaql <- Rivers %>% 
  filter(AQUATIC_LIFE_ATTAINMENT_2024 == "Fully Supporting")

bad <- Rivers %>% 
  filter(AQUATIC_LIFE_ATTAINMENT_2024 == "Not Supporting") 

# CT boundary layer
CT <- read_sf("ct_boundary.geojson")

circle <- makeIcon(
  iconUrl = "circle marker.png", 
  iconWidth = 15, iconHeight = 15
)

yellow_star <- makeIcon(
  iconUrl = "map marker.png", 
  iconWidth = 22, iconHeight = 22
)

data$icon <- ifelse(data$RBV_most_wanted_count <= 3, "circle", "yellow_star")

data_black_circle <- data %>% filter(icon == "circle")
data_yellow_star <- data %>% filter(icon == "yellow_star")

legend_labels <- c("Sites with 3 or less MW", "Sites with 4+ MW", "Rivers supporting full AQL", "Impaired rivers for AQL")
legend_colors <- c("#0D2D6C", "#F2AB19", "#00AAE7", "darkred")

leaflet(options = leafletOptions(minZoom = 8, maxZoom = 16)) %>%
  setView(lng = -72.3246, lat = 41.69601, zoom = 8.4) %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "Esri GrayCanvas (default)") %>%
  addPolylines(data = fullaql, color = "#00AAE7", label = ~ASSESSMENT_UNIT_NAME, opacity = 1, weight = 1.5, group = "Rivers") %>%
  addPolylines(data = bad, color = "darkred", label = ~ASSESSMENT_UNIT_NAME, opacity = 1, weight = 1.5, group = "Rivers") %>%
  addPolylines(data = CT, color = "black", opacity = 1, weight = 1.5) %>%
  addMarkers(
    data = data_black_circle,
    ~xlong,
    ~ylat,
    icon = circle,
    label = ~lapply(paste0(staSeq, ": ", WaterbodyName, "<br>", "MW Count: ", "<b>", RBV_most_wanted_count, "</b>"), htmltools::HTML),
    labelOptions = labelOptions(permanent = FALSE, direction = "top",
                                style = list("font-size" = "12px")),
    group = 'Sites with 3 or less MW'
  ) %>%
  addMarkers(
    data = data_yellow_star,
    ~xlong,
    ~ylat,
    icon = yellow_star,
    label = ~lapply(paste0(staSeq, ": ", WaterbodyName, "<br>", "MW Count: ", "<b>", RBV_most_wanted_count, "</b>"), htmltools::HTML),
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
  ) %>%  
  addMarkers(
    data = data, lng = ~xlong, lat = ~ylat, label = data$WaterbodyName,
    group = data$WaterbodyName,
    icon = makeIcon( 
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
      iconWidth = 1, iconHeight = 1
    )
  ) %>%
  addSearchFeatures(
    targetGroups = data$WaterbodyName, # group should match addMarkers() group
    options = searchFeaturesOptions(
      zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
      textPlaceholder = "Search by Waterbody Name..."
    )
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

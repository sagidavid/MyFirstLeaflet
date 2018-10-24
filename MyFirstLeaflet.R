library(rgdal)
library(spdplyr)
library(sp)
library(leaflet)
library(rgeos)
library(rmapshaper)

#Import shapefile using readOGR, GDAL1_integer64_policy = TRUE to maintain numerical values
#Simplify the imported shapefile (now Large SpatialPolygonDataFrame) using ms_simplify
FullDatasetLarge <- readOGR (dsn = ".", layer = "BRMA_Merged_Attributed", GDAL1_integer64_policy = TRUE)
FullDataset <- ms_simplify(input = FullDatasetLarge, keep = 0.01)

#Create subsets of the data, breaking it to indivudual sets of year and entitlements
"SAR_2015" <- FullDataset %>% select("BRMA", "SAR_2015", "U.R", "C.I", "Region")
"SAR_2016" <- FullDataset %>% select("BRMA", "SAR_2016", "U.R", "C.I", "Region")
"SAR_2017" <- FullDataset %>% select("BRMA", "SAR_2017", "U.R", "C.I", "Region")
"SAR_2018" <- FullDataset %>% select("BRMA", "SAR_2018", "U.R", "C.I", "Region")
"B1_2015" <- FullDataset %>% select("BRMA", "X1BED_2015", "U.R", "C.I", "Region")
"B1_2016" <- FullDataset %>% select("BRMA", "X1BED_2016", "U.R", "C.I", "Region")
"B1_2017" <- FullDataset %>% select("BRMA", "X1BED_2017", "U.R", "C.I", "Region")
"B1_2018" <- FullDataset %>% select("BRMA", "X1BED_2018", "U.R", "C.I", "Region")
"B2_2015" <- FullDataset %>% select("BRMA", "X2BED_2015", "U.R", "C.I", "Region")
"B2_2016" <- FullDataset %>% select("BRMA", "X2BED_2016", "U.R", "C.I", "Region")
"B2_2017" <- FullDataset %>% select("BRMA", "X2BED_2017", "U.R", "C.I", "Region")
"B2_2018" <- FullDataset %>% select("BRMA", "X2BED_2018", "U.R", "C.I", "Region")
"B3_2015" <- FullDataset %>% select("BRMA", "X3BED_2015", "U.R", "C.I", "Region")
"B3_2016" <- FullDataset %>% select("BRMA", "X3BED_2016", "U.R", "C.I", "Region")
"B3_2017" <- FullDataset %>% select("BRMA", "X3BED_2017", "U.R", "C.I", "Region")
"B3_2018" <- FullDataset %>% select("BRMA", "X3BED_2018", "U.R", "C.I", "Region")
"B4_2015" <- FullDataset %>% select("BRMA", "X4BED_2015", "U.R", "C.I", "Region")
"B4_2016" <- FullDataset %>% select("BRMA", "X4BED_2016", "U.R", "C.I", "Region")
"B4_2017" <- FullDataset %>% select("BRMA", "X4BED_2017", "U.R", "C.I", "Region")
"B4_2018" <- FullDataset %>% select("BRMA", "X4BED_2018", "U.R", "C.I", "Region")

#Rename the Entitlement_Year attribute to RentData consistently
"Sorted_SAR_2015" <- SAR_2015 %>% rename(RentData = SAR_2015)
"Sorted_SAR_2016" <- SAR_2016 %>% rename(RentData = SAR_2016)
"Sorted_SAR_2017" <- SAR_2017 %>% rename(RentData = SAR_2017)
"Sorted_SAR_2018" <- SAR_2018 %>% rename(RentData = SAR_2018)
"Sorted_B1_2015" <- B1_2015 %>% rename(RentData = X1BED_2015)
"Sorted_B1_2016" <- B1_2016 %>% rename(RentData = X1BED_2016)
"Sorted_B1_2017" <- B1_2017 %>% rename(RentData = X1BED_2017)
"Sorted_B1_2018" <- B1_2018 %>% rename(RentData = X1BED_2018)
"Sorted_B2_2015" <- B2_2015 %>% rename(RentData = X2BED_2015)
"Sorted_B2_2016" <- B2_2016 %>% rename(RentData = X2BED_2016)
"Sorted_B2_2017" <- B2_2017 %>% rename(RentData = X2BED_2017)
"Sorted_B2_2018" <- B2_2018 %>% rename(RentData = X2BED_2018)
"Sorted_B3_2015" <- B3_2015 %>% rename(RentData = X3BED_2015)
"Sorted_B3_2016" <- B3_2016 %>% rename(RentData = X3BED_2016)
"Sorted_B3_2017" <- B3_2017 %>% rename(RentData = X3BED_2017)
"Sorted_B3_2018" <- B3_2018 %>% rename(RentData = X3BED_2018)
"Sorted_B4_2015" <- B4_2015 %>% rename(RentData = X4BED_2015)
"Sorted_B4_2016" <- B4_2016 %>% rename(RentData = X4BED_2016)
"Sorted_B4_2017" <- B4_2017 %>% rename(RentData = X4BED_2017)
"Sorted_B4_2018" <- B4_2018 %>% rename(RentData = X4BED_2018)

#Create a new SpatialPolygonDataFrame without any Rent Data and using it to filter geographies
BRMA_List <- FullDataset %>% select("BRMA", "U.R", "C.I", "Region")
Filter_Coastal <- BRMA_List %>% 
  filter(C.I == "Inland")
Filter_Inland <- BRMA_List %>% 
  filter(C.I == "Coastal")
Filter_Urban <- BRMA_List %>% 
  filter(U.R == "Rural")
Filter_Rural <- BRMA_List %>% 
  filter(U.R == "Urban")

#Copy out all the SpatialPolygonDataFrames that have been made so far into individual shapefiles
writeOGR(Sorted_SAR_2015, dsn = ".", layer = "SAR_2015", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_SAR_2016, dsn = ".", layer = "SAR_2016", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_SAR_2017, dsn = ".", layer = "SAR_2017", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_SAR_2018, dsn = ".", layer = "SAR_2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B1_2015, dsn = ".", layer = "B1_2015", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B1_2016, dsn = ".", layer = "B1_2016", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B1_2017, dsn = ".", layer = "B1_2017", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B1_2018, dsn = ".", layer = "B1_2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B2_2015, dsn = ".", layer = "B2_2015", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B2_2016, dsn = ".", layer = "B2_2016", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B2_2017, dsn = ".", layer = "B2_2017", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B2_2018, dsn = ".", layer = "B2_2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B3_2015, dsn = ".", layer = "B3_2015", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B3_2016, dsn = ".", layer = "B3_2016", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B3_2017, dsn = ".", layer = "B3_2017", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B3_2018, dsn = ".", layer = "B3_2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B4_2015, dsn = ".", layer = "B4_2015", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B4_2016, dsn = ".", layer = "B4_2016", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B4_2017, dsn = ".", layer = "B4_2017", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Sorted_B4_2018, dsn = ".", layer = "B4_2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)

writeOGR(Filter_Coastal, dsn = ".", layer = "Filter_Coastal", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Inland, dsn = ".", layer = "Filter_Inland", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Urban, dsn = ".", layer = "Filter_Urban", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Filter_Rural, dsn = ".", layer = "Filter_Rural", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#define the colour palette in preparation for the Leaflet
bins <- c(-Inf, 16, 18, 20, 22, 24, 26, 28, 30, Inf)
pal <- colorBin("RdYlGn", domain = Sorted_SAR_2015$RentData, bins = bins)

#define the hover-over labels in preparation for the Leaflet
labels <- sprintf("<strong>%s</strong><br/> %g percent",
            Sorted_SAR_2015$BRMA, Sorted_SAR_2015$RentData
  ) %>% lapply(htmltools::HTML)

#Creat the Leaflet
m <- leaflet(Sorted_SAR_2015) %>% 
  
#Set starting view with lat/long and default zoom
  setView(lat=54.5, lng=-2.5 , zoom=6) %>% 

#Add polygons
  addPolygons(
    data = Filter_Coastal,
    group = "Coastal",
    fillColor = "Black",
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0) %>% 
    
  addPolygons(
    data = Filter_Inland,
    group = "Inland",
    fillColor = "Black",
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0) %>% 
  
  addPolygons(
    data = Filter_Urban,
    group = "Urban",
    fillColor = "Black",
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0) %>% 
  
  addPolygons(
    data = Filter_Rural,
    group = "Rural",
    fillColor = "Black",
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0) %>% 
  
  addPolygons(
    data = Sorted_SAR_2015,
    group = "SAR 2015",
    fillColor = ~pal(RentData),
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0, 
    highlightOptions = highlightOptions(
      color = "white", 
      dashArray = "",
      weight = 2, 
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  
  addPolygons(
    data = Sorted_SAR_2016,
    group = "SAR 2016",
    fillColor = ~pal(RentData), 
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0, 
    highlightOptions = highlightOptions(
      color = "white", 
      dashArray = "",
      weight = 2, 
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  
  addPolygons(
    data = Sorted_SAR_2017,
    group = "SAR 2017",
    fillColor = ~pal(RentData), 
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0, 
    highlightOptions = highlightOptions(
      color = "white", 
      dashArray = "",
      weight = 2, 
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  
  addPolygons(
    data = Sorted_SAR_2018,
    group = "SAR 2018",
    fillColor = ~pal(RentData), 
    weight = 1, 
    smoothFactor = 0.5,
    dashArray = "1",
    opacity = 1.0, 
    fillOpacity = 1.0, 
    highlightOptions = highlightOptions(
      color = "white", 
      dashArray = "",
      weight = 2, 
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 

#Add Legend    
  addLegend(
    pal = pal,
    values = ~RentData,
    group = "SAR 2015",
    title = "Affordability",
    opacity = 0.7,
    position = "bottomright") %>% 
  
#Add Layer Control  
  addLayersControl(
    overlayGroups = c("Coastal", "Inland", "Urban", "Rural"),
    baseGroups = c("SAR 2015","SAR 2016", "SAR_2017", "SAR_2018"),
    options = layersControlOptions(collapsed = FALSE)
  )

#Create the Leaflet
m
---
title: "CPRA Oyster Resource Zones"
output:
  flexdashboard::flex_dashboard:
    navbar: null
    theme: yeti
    orientation: columns
    vertical_layout: fill
    logo: www/lynker_logo_white_transparent.png
    css: www/custom.css
runtime: shiny
resource_files:
- state_owned_water_bottoms_480m.tif
---

```{r setup, include = FALSE}
# Shiny & Flexdashboard libraries
library(shiny)
library(flexdashboard)

# Data libraries
library(tidyr)
# library(ggplot2)
library(dplyr)
library(leaflet)
library(leafem)
library(sf)
library(rgdal)
library(raster)
library(viridisLite)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
# library(leaflegend)
library(RColorBrewer)
library(shinyalert)

source('utils.R')
```

<style>
.navbar, [data-toggle=tab], .navbar-brand  {   background-color:#1c3a5d;   border-color:black;   color:white; }

.navbar-logo img {
    position: absolute;
    right: 6px;
}

</style>



```{r context="server"}
# local path to data
path           <- "C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/"
# path           <- "C:/Users/JessicaGarrett/Documents/repos/cpra_dashboard/"

# coord ref. system 26915
crs            <- CRS('+init=EPSG:26915')

# # extent for all rasters
ext            <- extent(c(405220, 909700, 3199570, 3416530))

# ============================
# ---- CV + OV + AOC data ----
# ============================

# Commericial viability
cv           <- readRDS("mp2023_commercial_viability.rds")

# Oyster Viability
ov           <- readRDS("mp2023_salinity_si_yr_08.rds")

# AOC
aoc          <-  readRDS("mp2023_aoc_yr_08.rds")
# ov <- raster::stack("mp2023_salinity_si_yr_08.tif")

orz          <- readRDS("mp2023_oyster_resource_zones_aoc.rds")




# ==================
# ---- Polygons ----
# ==================

# AOC
aoc_areas     <- readRDS("aoc_area_polygon.rds") %>%
                          st_transform(4326) %>%
                          st_union() %>%
                          st_as_sf() %>%
                          mutate(
                            label   = "AOC permitted areas"
                            )

# Navigable waterways
waterways      <- readRDS("navig_waterways_buffer_simple.rds") %>%
                          st_transform(4326) %>%
                          mutate(label = "Navigatable waterways")

# Coastal use permits
# cup            <- readRDS("coastal_use_permits_480m.rds")
cup            <- raster::raster("coastal_use_permits_480m.tif")

# State owned water bottoms
# sowb           <- readRDS("state_owned_water_bottoms_480m.rds")
sowb           <- raster::raster("state_owned_water_bottoms_480m.tif")

# CPRA & FWOA Projects
# cpra_projects  <-  readRDS("cpra_restoration_proj_480m_v2.rds")
cpra_projects  <-  raster::raster("cpra_restoration_proj_480m_v2.tif")


# Oyster leases
oyster_leases  <- readRDS("oyster_leases_union.rds")
# oyster_leases  <- readRDS("oyster_leases_simple.rds")


# LDH polygon
# ldh            <- readRDS("ldh_classifications_simple.rds") %>%
#                             st_transform(26915) %>%
#                             st_transform(4326)
ldh             <- readRDS("oyster_harvest_areas_final.rds") %>%
                            st_transform(26915) %>%
                            st_transform(4326) 




# depth raster
# depth <- raster::raster(paste0(path, "depth_inundation/depth_inundation_01_01_480m_resample.tif"))

# landwater
# land_sf     <- readRDS("land_polygon_simple_v4.rds") %>%   st_transform(26915) %>% st_transform(4326) %>%  mutate(label = "2023 MP Land (Year 1)")  dplyr::select(land = landwater_binary_mp2023, label, geometry)
# ========================================


# Initialize Maps
output$cvMap        <- renderLeaflet({
    cv_basemap(
      cv = cv
    )
  })

output$ovMap        <- renderLeaflet({
    ov_basemap(
      ov = ov
      )
  })

output$aocMap        <- renderLeaflet({
    aoc_basemap(
      aoc            = aoc,
      orz            = orz, 
      aoc_areas      = aoc_areas,
      waterways      = waterways,
      sowb           = sowb,
      cup            = cup,
      ldh            = ldh,
      cpra_projects  = cpra_projects,
      oyster_leases  = oyster_leases
    )
  })


# CSS + Yaml headers + colors
# 678d91
# navbar:
  # - { icon: "fa-question-circle", href: "https://github.com/anguswg-ucsb/cpra_dashboard", align: right }
#487e8c
# CSS to adjust tab font size
# .active {
#   font-size:25px;
# }
```

AOC model
=====================================
Inputs {.sidebar}
-------------------------------------
```{r}
# Download ORZ AOC Model Var/Relationships PDF
# downloadLink("downloadData", "ORZ Model Report")
wellPanel(downloadButton("downloadData", "ORZ Model Report"))

    
output$downloadData <- downloadHandler(
    filename = "ORZ AOC Model Variables & Relationships_122121.pdf",
    content = function(file) {
      file.copy(here::here("ORZ AOC Model Variables & Relationships_122121.pdf"), file)
    }
)
```

Column
-----------------------------------------------------------------------
### Commercial viability
```{r}
# Column {data-width=250}

leafletOutput("cvMap")

# ---- Observer for radio button legends CV ----
# observeEvent(input$cvMap_groups,{
#   # print(input$cvMap_groups[2])
#     cvMap <- leafletProxy("cvMap") %>%
#       clearTiles() %>% 
#       addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(pane = "Topographic"), group = "Topographic") %>%
#       clearControls()
#     
#     if (input$cvMap_groups[2] == 'Fetch SI (Shallow)') {
#       vect <- seq(0.2, 1, by = .05)
#       cvMap %>% 
#         # addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(pane = "Topographic"), group = "Topographic") %>%
#         addLegend(
#           pal       = colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F),
#           position  = "bottomleft",
#           title     = "Fetch SI (Shallow)",        values    = vect,
#           group     = "Fetch SI (Shallow)",        layerId   = "Fetch SI (Shallow)"
#           )
#     }
#     else if (input$cvMap_groups[2] == "Fetch SI (Deep)") {
#       vect <- seq(0.2, 1, by = .05)
#       cvMap %>%   
#         # addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(pane = "Topographic"), group = "Topographic") %>%
#         addLegend(
#             pal       = colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F),
#             position  = "bottomleft",
#             title     = "Fetch SI (Deep)",           values    = vect,
#             group     = "Fetch SI (Deep)",           layerId   = "Fetch SI (Deep)")
#     }
#     else if (input$cvMap_groups[2] == "Distance to Roads SI") {
#       vect <- seq(0.2, 1, by = .05)
#       cvMap %>%   
#           # addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(pane = "Topographic"), group = "Topographic") %>%
#           addLegend(
#             pal       = colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F),
#             position  = "bottomleft",
#             title     = "Distance to Roads SI",      values    = vect,
#             group     = "Distance to Roads SI",      layerId   = "Distance to Roads SI")
#               }
#     else if (input$cvMap_groups[2] == "Sediment deposition SI") {
#       vect <- seq(0.2, 1, by = .05)
#       cvMap %>%   
#           # addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(pane = "Topographic"), group = "Topographic") %>%
#           addLegend( # Sedimentation Rate CV legend
#             pal       = colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F),
#             position  = "bottomleft",
#             title     = "Sed. dep. SI",              values    = vect,
#             group     = "Sediment deposition SI",    layerId   = "Sediment deposition SI")
#         }
#     else if (input$cvMap_groups[2] == "Shallow water CV (Year 8)") {
#       vect <- seq(0.2, 1, by = .05)
#       cvMap %>%   
#           # addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(pane = "Topographic"), group = "Topographic") %>%
#           addLegend( # Shallow Water Comm. Viab legend
#             pal       = colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F),
#             position  = "topleft",
#             title     = "Shallow water CV (Year 8)", values  = vect,
#             group     = "Shallow water CV (Year 8)", layerId = "Shallow water CV (Year 8)")
#     }    
#     else if (input$cvMap_groups[2] == "Deep water CV (Year 8)") {
#       vect <- seq(0.2, 1, by = .05)
#       cvMap %>%   
#           # addProviderTiles(providers$Esri.OceanBasemap, options = providerTileOptions(pane = "Topographic"), group = "Topographic") %>%
#           addLegend(
#              pal       = colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F),
#             position  = "topleft",
#             title     = "Deep water CV (Year 8)",    values = vect,
#             group     = "Deep water CV (Year 8)",    layerId = "Deep water CV (Year 8)")
#     }
#   })
```

```{r}
useShinyalert(rmd = TRUE)

observeEvent(input$cv_info_button, {
  # if(input$my_easy_button == "frozen") {
    shinyalert(title               = " ",
               closeOnClickOutside = TRUE,
               showCancelButton    = FALSE,
               html                = TRUE,
               text                =
                HTML('
                  <table class="table">
                    <h4><strong>Commercial Viability</strong></h4>
                        <thead>
                          <tr>
                            <th scope="col">Layer</th>
                            <th scope="col">Description</th>
                            </tr>
                          </thead>
                          <tbody>
                           <tr>
                              <td><strong>Fetch</strong></td>
                              <td>Average exposure to open water. Provides a proxy for wave energy that could affect AOC operations </td>
                              </tr>
                           <tr>
                              <td><strong>Roads</strong></td>
                              <td>Straight line distance to nearest road. Provides a metric for ease of access.</td>
                           </tr>
                           <tr>
                              <td><strong>Sediment Deposition</strong></td>
                              <td> Sediment deposition rate. Provides a measure of AOC level of effort to reduce effects of sedimentation.</td>
                           </tr>
                           <tr>
                              <td><strong>Shallow water CV</strong></td>
                              <td>Index of commercial viability for AOC operations in shallow water based on fetch, road distance, and sediment deposition </td>
                           </tr>
                           <tr>
                              <td><strong>Deep water CV</strong></td>
                              <td>Index of commercial viability for AOC operations in deep water based on fetch and sediment deposition </td>
                           </tr>
                      </tbody>
                  </table>'
                  )
               )
  # }
  })
```


```{r context = "server"}
# tags$html(HTML(paste0(
#     HTML(
#       '<div class="modal fade" id="infobox1" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
#     ),
# 
#     # Header / Title
#     HTML("<strong>Meta data</strong>"),
#     HTML(
#       '</div><div class="modal-body">'
#     ),
#     HTML('
# <table class="table">
#       <thead>
#         <tr>
#           <th scope="col">Layer</th>
#           <th scope="col">Description</th>
#           </tr>
#         </thead>
#         <tbody>
#          <tr>
#             <td><strong>Fetch</strong></td>
#             <td>Average exposure to open water. Provides a proxy for wave energy that could affect AOC operations </td>
#             </tr>
#          <tr>
#             <td><strong>Roads</strong></td>
#             <td>Straight line distance to nearest road. Provides a metric for ease of access.</td>
#          </tr>
#          <tr>
#             <td><strong>Sediment Deposition</strong></td>
#             <td> Sediment deposition rate. Provides a measure of AOC level of effort to reduce effects of sedimentation.</td>
#          </tr>
#          <tr>
#             <td><strong>Shallow water CV</strong></td>
#             <td>Index of commercial viability for AOC operations in shallow water based on fetch, road distance, and sediment deposition </td>
#          </tr>
#          <tr>
#             <td><strong>Deep water CV</strong></td>
#             <td>Index of commercial viability for AOC operations in deep water based on fetch and sediment deposition </td>
#          </tr>
#     </tbody>
# </table>'
#     ),
#     # Closing divs
#     HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
#   )))
```

### Oyster viability
```{r}
# Column {data-width=250}
leafletOutput("ovMap")
# leafletOutput("testMap2")
```

```{r}
useShinyalert(rmd = TRUE)

observeEvent(input$ov_info_button, {
  # if(input$my_easy_button == "frozen") {
    shinyalert(title               = " ",
               closeOnClickOutside = TRUE,
               showCancelButton    = FALSE,
               html                = TRUE,
               text                =
                HTML('
                  <table class="table">
                    <h4><strong>Oyster Viability</strong></h4>
                        <thead>
                          <tr>
                            <th scope="col">Layer</th>
                            <th scope="col">Description</th>
                            </tr>
                          </thead>
                          <tbody>
                            <tr>
                              <td><strong>Cool month min salinity</strong></td>
                              <td>Suitability index based on cool month minimum salinity </td>
                            </tr>
                            <tr>
                              <td><strong>Warm month min salinity</strong></td>
                              <td> Suitability index based on warm month minimum salinity</td>
                          </tr>
                          <tr>
                              <td><strong>Annual mean salinity</strong></td>
                              <td> Suitability index based on annual average salinity </td>
                         </tr>
                          <tr>
                              <td><strong>SI MS</strong></td>
                              <td>Suitability index based on combination of cool and warm month minimum salinity</td>
                         </tr>
                         <tr>
                              <td><strong>SI OV</td>
                              <td>Oyster viability index based on cool and warm month minimum salinity and annual average salinity</td>
                         </tr>
                      </tbody>
                  </table>'
                  )
               )
  })
```

```{r context = "server"}
# Make maps track eachother when interacted with
observe({ # Observer to respond to zoom / pan of baseMap1 and apply to baseMap2
    coords  <- input$aocMap_center
    zoom    <- input$aocMap_zoom
    print(coords)
    print(zoom)
    if (!is.null(coords)) {
      leafletProxy("ovMap") %>%
        setView(lat = coords$lat, lng = coords$lng, zoom = zoom)
      leafletProxy("cvMap") %>%
        setView(lat = coords$lat, lng = coords$lng, zoom = zoom)
    }
})
```

Column 
-----------------------------------------------------------------------
### AOC (Commercial x Oyster viability)
```{r}
# Column {data-width=250}
leafletOutput("aocMap")
```


```{r}
useShinyalert(rmd = TRUE)

observeEvent(input$aoc_info_button, {
  # if(input$my_easy_button == "frozen") {
    shinyalert(title               = " ",
               closeOnClickOutside = TRUE,
               showCancelButton    = FALSE,
               html                = TRUE,
               text                =
                HTML('
                  <table class="table">
                    <h4><strong>AOC</strong></h4>
                        <thead>
                          <tr>
                            <th scope="col">Layer</th>
                            <th scope="col">Description</th>
                            </tr>
                          </thead>
                          <tbody>
                         <tr>
                            <td><strong>AOC Shallow</strong></td>
                            <td>AOC suitability index for shallow water operations based on oyster viability and commercial viability</td>
                         </tr>
                         <tr>
                            <td><strong>AOC Deep</strong></td>
                            <td>AOC suitability index for deep water operations based on oyster viability and commercial viability </td>
                         </tr>
                         <tr>
                            <td><strong>USACE Navigation Channels</strong></td>
                            <td>Reference layer showing navigation channels, which may create regulatory limitations on AOC operations</td>
                         </tr>
                         <tr>
                            <td><strong>State owned water bottoms</strong></td>
                            <td>Reference layer showing state owned water bottoms, where AOC operations would be allowed</td>
                         </tr>
                         <tr>
                            <td><strong>Coastal Use Permits</strong></td>
                            <td>Reference layer showing coastal use permits, which may create regulatory limits on AOC operations </td>
                         </tr>
                         <tr>
                            <td><strong>CPRA Projects</strong></td>
                            <td>Reference layer showing integrated protection projects, which may create regulatory limitations on AOC operations</td>
                         </tr>

                         <tr>
                            <td><strong>AOC permitted areas</strong></td>
                            <td>Reference layer showing current AOC permit locations (provided by LDWF)</td>
                         </tr>
                         <tr>
                            <td><strong>Oyster harvest area classifications</strong></td>
                            <td>Reference layer which reflects the frequency with which oyster harvest was permissible over the last 10 years</td>
                         </tr>
                         <tr>
                            <td><strong>Oyster leases</strong></td>
                            <td>Reference layer showing current oyster leases (provided by LDWF)</td>
                         </tr>                         
                      </tbody>
                  </table>'
                  )
               )
  })
```

<!-- ### -->
```{r}
# useShinyalert(rmd = TRUE)
# actionButton(
#   "modelRunHelpButton",
#   label = "Meta data"
#   # class = "btn-primary btn-lg",
#   # icon("question")
#   )
# 
# observeEvent(input$modelRunHelpButton, {
#   shinyalert(title               = " ",
#              closeOnClickOutside = TRUE,
#              showCancelButton    = TRUE,
#              html                = TRUE,
#              text                =
#               HTML('
#                 <table class="table">
#                   <h6><strong>Commercial Viability</strong></h6>
#                       <thead>
#                         <tr>
#                           <th scope="col">Layer</th>
#                           <th scope="col">Description</th>
#                           </tr>
#                         </thead>
#                         <tbody>
#                          <tr>
#                             <td><strong>Fetch</strong></td>
#                             <td>Average exposure to open water. Provides a proxy for wave energy that could affect AOC operations </td>
#                             </tr>
#                          <tr>
#                             <td><strong>Roads</strong></td>
#                             <td>Straight line distance to nearest road. Provides a metric for ease of access.</td>
#                          </tr>
#                          <tr>
#                             <td><strong>Sediment Deposition</strong></td>
#                             <td> Sediment deposition rate. Provides a measure of AOC level of effort to reduce effects of sedimentation.</td>
#                          </tr>
#                          <tr>
#                             <td><strong>Shallow water CV</strong></td>
#                             <td>Index of commercial viability for AOC operations in shallow water based on fetch, road distance, and sediment deposition </td>
#                          </tr>
#                          <tr>
#                             <td><strong>Deep water CV</strong></td>
#                             <td>Index of commercial viability for AOC operations in deep water based on fetch and sediment deposition </td>
#                          </tr>
#                     </tbody>
#                 </table>
#                          <table class="table">
#                           <h6><strong>Oyster Viability</strong></h6>
#                       <thead>
#                         <tr>
#                           <th scope="col">Layer</th>
#                           <th scope="col">Description</th>
#                           </tr>
#                         </thead>
#                         <tbody>
#                           <tr>
#                             <td><strong>Cool month min salinity</strong></td>
#                             <td>Suitability index based on cool month minimum salinity </td>
#                             </tr>
#                          <tr>
#                             <td><strong>Warm month min salinity</strong></td>
#                             <td> Suitability index based on warm month minimum salinity</td>
#                          </tr>
#                          <tr>
#                             <td><strong>Annual mean salinity</strong></td>
#                             <td> Suitability index based on annual average salinity </td>
#                          </tr>
#                          <tr>
#                             <td><strong>SI MS</strong></td>
#                             <td>Suitability index based on combination of cool and warm month minimum salinity</td>
#                          </tr>
#                          <tr>
#                             <td><strong>SI OV</td>
#                             <td>Oyster viability index based on cool and warm month minimum salinity and annual average salinity</td>
#                          </tr>
#                          <tr>
#                             <td><strong>AOC Shallow</strong></td>
#                             <td>AOC suitability index for shallow water operations based on oyster viability and commercial viability</td>
#                             </tr>
#                          <tr>
#                             <td><strong>AOC Deep</strong></td>
#                             <td>AOC suitability index for deep water operations based on oyster viability and commercial viability </td>
#                          </tr>
#                     </tbody>
#                 </table>
#                 <table class="table">
#                  <h6><strong>AOC</strong></h6>
#                       <thead>
#                         <tr>
#                           <th scope="col">Layer</th>
#                           <th scope="col">Description</th>
#                           </tr>
#                         </thead>
#                         <tbody>
#                          <tr>
#                             <td><strong>AOC Shallow</strong></td>
#                             <td>AOC suitability index for shallow water operations based on oyster viability and commercial viability</td>
#                             </tr>
#                          <tr>
#                             <td><strong>AOC Deep</strong></td>
#                             <td>AOC suitability index for deep water operations based on oyster viability and commercial viability </td>
#                          </tr>
#                          <tr>
#                             <td><strong>USACE Navigation Channels</strong></td>
#                             <td>Reference layer showing navigation channels, which may create regulatory limitations on AOC operations</td>
#                          </tr>
#                          <tr>
#                             <td><strong>State owned water bottoms</strong></td>
#                             <td>Reference layer showing state owned water bottoms, where AOC operations would be allowed</td>
#                          </tr>
#                          <tr>
#                             <td><strong>Coastal Use Permits</strong></td>
#                             <td>Reference layer showing coastal use permits, which may create regulatory limits on AOC operations </td>
#                          </tr>
#                          <tr>
#                             <td><strong>CPRA Projects</strong></td>
#                             <td>Reference layer showing integrated protection projects, which may create regulatory limitations on AOC operations</td>
#                          </tr>
#                     </tbody>
#                 </table>'
#                 )
#              )
# })
```
















# # --- Shiny utils ---
# basemap <- function(
#   land_sf,
#   road_buffer,
#   cpra_projects,
#   cup,
#   ldh,
#   aoc,
#   oyster_leases,
#   sal3,
#   sal10,
#   depth1,
#   wlvl_clamp,
#   fetch_cat7,
#   waterways,
#   sowb,
#   hsi_2017,  # hsi_sal3, hsi_sal10, hsi_sal3_mask, hsi_sal10_mask,  mask_open,  mask_interm,
#   pts = NULL
# ) {
#
#   # ==================================
#   # ---- COLOR PALETTES & LABELS -----
#   # ==================================
#
#   # Fetch color palette + labels
#   fetch_categories <- length(unique(raster::values(fetch_cat7))) -1
#   fetch_cols       <- data.frame(fetch_cols = c(1, 2, 3, 4, 5, 10, 20))  # fetch_cols <- data.frame(fetch_cols = 1:fetch_categories)
#   turbo_pal        <- viridisLite::turbo(n = fetch_categories, direction = -1)
#   fetch_fact_df    <- fetch_cols %>% mutate(fetch_cols = factor(fetch_cols))
#   fetch_fact       <- colorFactor(turbo_pal,   domain = fetch_fact_df$fetch_cols)
#   fetch_labels     <- c("1km", "2m", "3km", "4km", "5km", "10km", "20km")
#   # fetch_pal      <- colorNumeric(palette = turbo_pal,   domain = fetch_cols$fetch_cols, reverse = TRUE, na.color = NA)
#   # pal = colorNumeric("Blues", reverse= F, na.color = "#00000000",      domain = unique(values(fetch_rc)))
#
#   # Salinity color palette + labels
#   sal_cols         <- data.frame(numeric_cols = 1:36)
#   sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)
#
#   # Water level variability palatte + labels
#   wlvl_pal          <- colorNumeric(turbo(n = 20), domain = values(wlvl_clamp), na.color = NA, reverse = F)
#
#   # Depth factors color palette + labels
#   depth_lvl          <- data.frame(numeric_cols = 1:3)
#   depth_labels       <- c("Too shallow", "Shallow water", "Deep water")
#   depth_fact_pal     <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
#   # depth_fact_pal   <- colorNumeric(mako(n = 3), domain = values(depth1), reverse = T)
#
#   # hsi_pal          <- colorNumeric('magma', domain =values(hsi_sal3), na.color = NA, reverse = F)
#
#   # LDH factor color palette
#   factpal          <- colorFactor(c("red", "yellow", "green"),   domain = ldh$Status)
#
#   # sowb legend color palette + label
#   sowb_label       <- "State owned water bottoms"
#   sowb_pal         <- colorFactor(c("#00bfb2"),   domain = sowb_label)
#
#   # Coastal use permits legend color palette + label
#   cup_label        <- "Coastal use permits"
#   cup_pal          <- colorFactor(c("#EAC435"),   domain = cup_label)
#
#   # CPRA restoration proj legend color palette + label
#   cpra_proj_label  <- "CPRA projects"
#   cpra_proj_pal    <- colorFactor(c("green", "orange"),   domain = cpra_projects$type)
#
#   # CPRA factor color palette
#   # cpra_pal          <- colorFactor(c("red", "yellow", "green"),   domain = ldh$Status)
#
#   # Navigablewaterways legend color palette + label
#   waterways_label  <- "USACE navigation channels"
#   waterways_pal    <- colorFactor(c("dodgerblue"), domain = waterways_label)
#
#   # Mask open legend color palette + label
#   # mask_open_label          <- "Full mask (LDH open)"
#   # mask_open_pal            <- colorFactor(c("grey"),   domain = mask_open_label)
#
#   # Mask open + interm. legend color palette + label
#   # mask_interm_label        <- "Full mask (LDH open & intermed.)"
#   # mask_interm_pal          <- colorFactor(c("grey"),   domain = mask_interm_label)
#
#   # AOC active/not active factor color palette
#   aoc_pal          <- colorFactor(c("darkorange"),   domain = aoc$label)
#
#   # Oyster leases palette
#   leases_pal        <- colorFactor(c("hotpink"),   domain = oyster_leases$label)
#
#   # Land water legend color palette + label
#   land_label        <- "2023 MP Land (Year 1)"
#   land_pal          <- colorFactor(c("grey"),   domain = land_sf$label)
#
#   # HSI 2017 500m grid color + labels
#   vect <- 0:1
#   hsi_pal           <- colorNumeric(viridis(n =20), domain = vect, na.color = NA,
#                                     reverse = F)
#   hsi_pal2          <- colorNumeric(viridis(n =20), domain = values(hsi_2017), na.color = NA,
#                                     reverse = F)
#   # hsi_pal          <- colorNumeric(viridis(n =20), domain = vect, na.color = NA, reverse = T)
#   # hsi_pal2          <- colorNumeric(viridis(n =20), domain = values(hsi_2017), na.color = NA, reverse = T)
#
#   # ==================================
#   # ----------- LEAFLET MAP ----------
#   # ==================================
#   leaflet() %>%
#     addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
#     addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
#     addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat. Geo. Topographic") %>%
#     addScaleBar("bottomleft") %>%
#     addMeasure(position = "bottomright", primaryLengthUnit = "feet",
#                primaryAreaUnit = "sqmiles", activeColor = "red", completedColor = "green") %>%
#     leafem::addMouseCoordinates() %>%
#     setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
#     addPolygons(
#       data               = ldh,
#       fillColor          = ~ factpal(Status),
#       fillOpacity        = 0.2,
#       color              = ~ factpal(Status),
#       weight             = 3,
#       opacity            = 1,
#       label              = ~ Status,
#       group              = "Oyster harvest areas",
#       highlightOptions   = highlightOptions(
#         opacity      = 1,
#         weight       = 6,
#         bringToFront = TRUE
#       )
#     ) %>%
#     addLegend(
#       pal                = factpal,
#       position           = "topleft",
#       title              ='Oyster harvest areas status',
#       values             = ldh$Status,
#       group              = "Oyster harvest areas",
#       layerId            = "Oyster harvest areas") %>%
#     addPolygons(
#       data               = oyster_leases,
#       fillColor          = "hotpink",
#       fillOpacity        = 0.2,
#       color              = "hotpink",
#       weight             = 2,
#       opacity            = 1,
#       label              = ~label,
#       group              = "Oyster leases",
#       highlightOptions   = highlightOptions(
#         opacity      = 1,
#         weight       = 6,
#         bringToFront = TRUE
#       )
#     ) %>%
#     addLegend(
#       pal                = leases_pal,
#       position           = "topleft",
#       values             =  oyster_leases$label,
#       group              =  "Oyster leases",
#       layerId            =  "Oyster leases") %>%
#     addPolygons(
#       data               = aoc,
#       fillColor          = ~aoc_pal(label),
#       fillOpacity        = 0.2,
#       color              = ~aoc_pal(label),
#       highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE),
#       weight             = 3,
#       opacity            = 1,
#       label              = ~label, group = "AOC permitted areas") %>%
#     addLegend(
#       pal                = aoc_pal,
#       position           = "topleft",
#       # title            = "AOC permitted areas",
#       values             = aoc$label,
#       group              = "AOC permitted areas",
#       layerId            = "AOC permitted areas") %>%
#     addRasterImage(
#       sowb,
#       colors             = "#00bfb2",
#       opacity            = 0.7,
#       group              = "State owned water bottoms") %>% # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
#     addLegend(
#       pal                = sowb_pal,
#       position           = "topleft",
#       values             = sowb_label,
#       group              = "State owned water bottoms",
#       layerId            = "State owned water bottoms"
#     ) %>%
#     addRasterImage(
#       cup,
#       colors             = "#EAC435",
#       opacity            = 0.7,
#       group              = "Coastal Use Permits") %>%   # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
#     addLegend(
#       pal                = cup_pal,
#       position           = "topleft",
#       values             = cup_label,
#       group              = "Coastal Use Permits",
#       layerId            = "Coastal Use Permits"
#     ) %>%
#     addPolygons(
#       data               = cpra_projects,
#       fillColor          = ~cpra_proj_pal(type),
#       color              = ~cpra_proj_pal(type),
#       fillOpacity        = 0.2,
#       weight             = 3,
#       opacity            = 1,
#       label              = ~proj_name,
#       group              = "CPRA projects",
#       popup              = paste(
#         "<b>Project ID: </b> ", cpra_projects$proj_id, "<br>",
#         "<b>Project name: </b>", cpra_projects$proj_name, "<br>",
#         "<b>Structure status: </b>", cpra_projects$struc_clas, "<br>",
#         "<b>Structure class: </b>", cpra_projects$struc_stat, "<br>",
#         "<b>Construction date: </b>", cpra_projects$const_date, "<br>"),
#       highlightOptions   = highlightOptions(
#         opacity      = 1,
#         weight       = 6,
#         bringToFront = TRUE
#       )) %>%  # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
#     addLegend(
#       pal                = cpra_proj_pal,
#       position           = "topleft",
#       values             = cpra_projects$type,
#       group              = "CPRA projects",
#       layerId            = "CPRA projects"
#     ) %>%
#     # addRasterImage(
#     #   cpra_projects,
#     #   colors    = "hotpink", opacity   = 0.7,
#     #   group     = "CPRA projects") %>%   # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
#     # addLegend(
#     #   pal       = cpra_proj_pal,
#     #   position  = "topleft", values    = cpra_proj_label,
#     #   group     = "CPRA projects",  layerId   = "CPRA projects") %>%
#     addRasterImage(
#       fetch_cat7,
#       opacity            = 0.8,
#       colors             = turbo_pal,
#       group              = "Fetch") %>%
#     addLegend(
#       pal                = fetch_fact,
#       position           = "bottomright",
#       title              = "Fetch",
#       labFormat          = labelFormat(suffix = " km"),
#       group              = "Fetch",
#       layerId            = "Fetch",
#       values             = fetch_cols$fetch_cols
#     ) %>%
#     addRasterImage(
#       sal3,
#       opacity            = 0.8,
#       colors             = sal_pal,
#       group              = "2023 MP Mean annual salinity (Year 1)") %>%
#     addRasterImage(
#       sal10,
#       opacity            = 0.8,
#       colors             = sal_pal,
#       group              = "2023 MP Mean annual salinity (Year 8)") %>%
#     addLegend(
#       pal                = sal_pal,
#       title              = "2023 MP Mean annual salinity (Year 1)",
#       position           = "bottomright",
#       labFormat          = labelFormat(suffix = " g/L"),
#       group              = "2023 MP Mean annual salinity (Year 1)",
#       layerId            = "2023 MP Mean annual salinity (Year 1)",
#       values             = sal_cols$numeric_cols
#     ) %>%
#     addLegend(
#       pal                = sal_pal,
#       title              = "2023 MP Mean annual salinity (Year 8)",
#       position           = "bottomright",
#       labFormat          = labelFormat(suffix = " g/L"),
#       group              = "2023 MP Mean annual salinity (Year 8)",
#       layerId            = "2023 MP Mean annual salinity (Year 8)",
#       values             = sal_cols$numeric_cols
#     ) %>%
#     addRasterImage(
#       depth1,
#       opacity            = 0.8,
#       colors             =  brewer.pal(9, "Spectral"),  # colors = c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
#       group              = "2023 MP Depth (Year 1)"
#     ) %>%
#     addLegend(
#       pal                = depth_fact_pal,
#       title              = "2023 MP Depth (Year 1)",
#       position           = "bottomleft",
#       group              = "2023 MP Depth (Year 1)",
#       layerId            = "2023 MP Depth (Year 1)",
#       values             = depth_labels
#     ) %>%
#     addRasterImage(
#       wlvl_clamp,
#       colors             = wlvl_pal,
#       opacity            = 0.8,
#       group              = "Water level variability (2021)") %>%
#     addLegend(
#       pal                = wlvl_pal,
#       labFormat          = labelFormat(suffix = " m"),
#       title              = "Water level variability (2021)",
#       position           = "bottomleft",
#       group              = "Water level variability (2021)",
#       layerId            = "Water level variability (2021)",
#       values             = values(wlvl_clamp)) %>%
#     addRasterImage(
#       hsi_2017,
#       colors             = hsi_pal2,
#       opacity            = 0.8,
#       group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)") %>%
#     addLegend(
#       pal                = hsi_pal,
#       title              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", position  = "bottomright",
#       group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", layerId   = "2017 MP Oyster HSI (FWOA, S04, Year 3)", values = vect) %>%
#     addPolylines(
#       data               = road_buffer[4,],
#       fillColor          = 'transparent',  col   = "red", opacity  = 1, weight  = 3,
#       label              = ~road_buffer$buffer_dist[4], group = "Road buffer 20km",
#       highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#     addPolylines(
#       data               = road_buffer[3,],
#       fillColor          = 'transparent', col   = "red", opacity = 1, weight  = 3,
#       label              = ~road_buffer$buffer_dist[3],  group = "Road buffer 10km",
#       highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#     addPolylines(
#       data               = road_buffer[2,],
#       fillColor          = 'transparent',  col   = "red", opacity     = 1,  weight  = 3,
#       label              = ~road_buffer$buffer_dist[2], group = "Road buffer 5km",
#       highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#     addPolylines(
#       data               = road_buffer[1,],
#       fillColor          = 'transparent', col  = "red",opacity  = 1, weight  = 3,
#       label              = ~road_buffer$buffer_dist[1],  group = "Road buffer 2km",
#       highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#     addPolygons(
#       data               = waterways,
#       fillColor          = "dodgerblue", weight = 2, fillOpacity = 0.7, color = "white",
#       highlightOptions   = highlightOptions(color = "white", opacity = 1, weight = 4, bringToFront = TRUE),
#       label              = ~label,  group = "USACE navigation channels") %>%
#     addLegend(
#       pal                = waterways_pal,
#       position           = "topleft", values = waterways_label,
#       group              = "USACE navigation channels",
#       layerId            = "USACE navigation channels") %>%
#     addPolygons(
#       data               = land_sf,
#       fillColor          = 'white', fillOpacity =  0.4,
#       col                = "black", opacity  = 1, weight  = 1.5,
#       group              = "2023 MP Land (Year 1)", label = ~label) %>%
#     addLegend(
#       pal                = land_pal,
#       position           = "topleft", values = land_label,
#       group              = "2023 MP Land (Year 1)",
#       layerId            = "2023 MP Land (Year 1)") %>%
#     addLayersControl(
#       options = layersControlOptions(collapsed = TRUE),
#       baseGroups = c("Imagery", "Topographic", "Nat. Geo. Topographic"),
#       overlayGroups = c(
#           "2023 MP Mean annual salinity (Year 1)",
#           "2023 MP Mean annual salinity (Year 8)",
#           "Fetch",
#           "Water level variability (2021)",
#           "Road buffer 2km",
#           "Road buffer 5km",
#           "Road buffer 10km",
#           "Road buffer 20km",
#           "2023 MP Land (Year 1)",
#           "2023 MP Depth (Year 1)",
#           "Oyster harvest areas",
#           "Oyster leases",
#           "USACE navigation channels",
#           "Coastal Use Permits",
#           "CPRA projects",
#           "State owned water bottoms",
#           "AOC permitted areas",
#           "2017 MP Oyster HSI (FWOA, S04, Year 3)"
#         )) %>%
#     hideGroup(
#       c(
#           "AOC permitted areas", "2023 MP Mean annual salinity (Year 1)",
#           "2023 MP Mean annual salinity (Year 8)", "2017 MP Oyster HSI (FWOA, S04, Year 3)",
#           "2023 MP Depth (Year 1)", "Water level variability (2021)",
#           "Fetch", "Road buffer 2km",
#           "Road buffer 5km", "Road buffer 10km",
#           "Road buffer 20km",  "USACE navigation channels",
#           "CPRA projects", "Coastal Use Permits", "Oyster leases",
#           "State owned water bottoms", "2023 MP Land (Year 1)")
#       )
# }
# --- Shiny utils --
cv_basemap <- function(
  cv,
  pts = NULL
) {

  # ---- CV Info box ----
  cv_info_box <- HTML(paste0(
    HTML(
      '<div class="modal fade" id="infobox1" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
    ),

    # Header / Title
    HTML("<strong>Meta data</strong>"),
    HTML(
      '</div><div class="modal-body">'
    ),
    HTML('
<table class="table">
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
    ),
    # Closing divs
    HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
  ))

  # ---- COLOR PALETTES & LABELS -----
  # 0.2 - 1 values vector
  vect <- seq(0.2, 1, by = .05)

  # commercial viability palettes
  comm_var_pal  <- colorNumeric(turbo(n =20,  direction = -1), domain = vect, na.color = NA, reverse = F)

  # reclassified Commericial Viability layers palette
  reclass_pal   <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)

  # rc_wlvl_pal      <- colorNumeric(c( "red", "#FDE725FF", "#21908CFF"), # mako(n =20, direction = -1),  domain = vect, na.color = NA, reverse = F)
  # rc_wlvl_pal <- colorFactor(c("red", "green", "blue"),# cividis(n =3, direction = -1),  na.color = NA , domain = values(rc_wlvl3))

  # ----------- LEAFLET MAP ----------
  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    # addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    # addMeasure(position           = "bottomright", primaryLengthUnit  = "feet",   primaryAreaUnit    = "sqmiles", activeColor        = "red", completedColor     = "green") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    # # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
    addRasterImage(
      cv$si_fetch_shallow,
      # fetch_shallow_mask,
      project   = T,
      colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "Fetch SI (Shallow)",
      layerId   = "Fetch SI (Shallow)"
      ) %>%
    addRasterImage(
      # fetch_deep_mask,
      cv$si_fetch_deep,
      project   = T,
      colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "Fetch SI (Deep)",
      layerId   = "Fetch SI (Deep)") %>%
    addRasterImage(
      cv$si_roads,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Distance to Roads SI",
      layerId   = "Distance to Roads SI") %>%
    addRasterImage(
      cv$si_sed_dep,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Sediment deposition SI",
      layerId   = "Sediment deposition SI") %>%
    addRasterImage(
      cv$shallow_cv,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Shallow water CV (Year 8)",
      layerId     = "Shallow water CV (Year 8)") %>%
    addRasterImage(
      cv$deep_cv,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Deep water CV (Year 8)",
      layerId     = "Deep water CV (Year 8)") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft",
      title     = "Fetch SI (Shallow)",        values    = vect,
      group     = "Fetch SI (Shallow)",        layerId   = "Fetch SI (Shallow)") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft",
      title     = "Fetch SI (Deep)",           values    = vect,
      group     = "Fetch SI (Deep)",           layerId   = "Fetch SI (Deep)") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft",
      title     = "Distance to Roads SI",      values    = vect,
      group     = "Distance to Roads SI",      layerId   = "Distance to Roads SI") %>%
    addLegend( # Sedimentation Rate CV legend
      pal       = reclass_pal,
      position  = "bottomleft",
      title     = "Sed. dep. SI",              values    = vect,
      group     = "Sediment deposition SI",    layerId   = "Sediment deposition SI") %>%
    addLegend( # Shallow Water Comm. Viab legend
      pal       = comm_var_pal,
      position  = "topleft",
      title     = "Shallow water CV (Year 8)", values  = vect,
      group     = "Shallow water CV (Year 8)", layerId = "Shallow water CV (Year 8)") %>%
    addLegend(
      pal       = comm_var_pal,
      position  = "topleft",
      title     = "Deep water CV (Year 8)",    values = vect,
      group     = "Deep water CV (Year 8)",    layerId = "Deep water CV (Year 8)") %>%
    # addImageQuery(fetch_cv$fetch_shallow_mask, digits = 2, layerId = "Fetch SI (Shallow)") %>%
    # addImageQuery(fetch_cv$fetch_deep_mask,    digits = 2, layerId = "Fetch SI (Deep)") %>%
    # addImageQuery(road_cv$road_buffer_cv,      digits = 2, layerId = "Distance to Roads SI") %>%
    # addImageQuery(sed_dep_cv$sed_dep_si_03_03, digits = 2, layerId = "Sediment deposition SI") %>%
    # addImageQuery(sed_dep_cv$sed_dep_si_10_10, digits = 2, layerId = "Sediment deposition SI") %>%
    # addImageQuery(shallow_cv$shallow_cv_03_03, digits = 2, layerId = "Shallow water CV (Year 8)") %>%
    # addImageQuery(shallow_cv$shallow_cv_10_10, digits = 2, layerId = "Shallow water CV (Year 8)") %>%
    # addImageQuery(deep_cv$deep_cv_03_03,       digits = 2, layerId = "Deep water CV (Year 8)") %>%
    # addImageQuery(deep_cv$deep_cv_10_10,       digits = 2, layerId = "Deep water CV (Year 8)") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = T),
      baseGroups = c("Topographic", "Imagery"), # "Topographic w/ roads"
      overlayGroups = c(
        "Fetch SI (Shallow)",
        "Fetch SI (Deep)",
        "Distance to Roads SI",
        "Sediment deposition SI",
        "Shallow water CV (Year 8)",
        "Deep water CV (Year 8)"
      )
    ) %>%
    hideGroup(
      c(
        "Fetch SI (Shallow)",
        "Fetch SI (Deep)",
        "Distance to Roads SI",
        "Sediment deposition SI",
        "Shallow water CV (Year 8)",
        "Deep water CV (Year 8)")
    ) %>%
    addBootstrapDependency() %>% # Add Bootstrap to be able to use a modal
    addEasyButton(
      easyButton(
        icon     = "fa-info-circle",
        title    = "Meta data",
        position = "topright",
        id = "info_button",
        onClick = JS("
              function(btn, map) {
                Shiny.onInputChange('cv_info_button', Math.random());
              }"
        )))
}

# ---- Shiny utils ---
ov_basemap <- function(
      ov,
      pts = NULL
      )

  {

  # ---- COLOR PALETTES & LABELS -----

  # SI value domain
  vect <- seq(0, 1, by = .1)


  # SI palatte
  si_pal          <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)

  # ----------- LEAFLET MAP ----------

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addBootstrapDependency() %>% # Add Bootstrap to be able to use a modal
    # addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    #addMeasure(position="bottomright",primaryLengthUnit="feet",primaryAreaUnit="sqmiles",activeColor="red",completedColor="green") %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
    addRasterImage(
      ov$si_sal_cool,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "Cool month min salinity (Year 8)",
      layerId   = "Cool month min salinity (Year 8)") %>%
    addRasterImage(
      ov$si_sal_warm,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "Warm month min salinity (Year 8)",
      layerId   = "Warm month min salinity (Year 8)") %>%
    addRasterImage(
     ov$si_sal_avg,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "Annual mean salinity (Year 8)",
      layerId   = "Annual mean salinity (Year 8)") %>%
    addRasterImage(
      ov$si_ms,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI MS (Year 8)",
      layerId   = "SI MS (Year 8)") %>%
    addRasterImage(
      ov$si_ov,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI OV (Year 8)",
      layerId   = "SI OV (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "Cool month min salinity (Year 8)",
      group     = "Cool month min salinity (Year 8)",  layerId   = "Cool month min salinity (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "Warm month min salinity (Year 8)",
      group     = "Warm month min salinity (Year 8)",  layerId   = "Warm month min salinity (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "Annual mean salinity (Year 8)",
      group     = "Annual mean salinity (Year 8)",  layerId   = "Annual mean salinity (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI MS (Year 8)",
      group     = "SI MS (Year 8)",  layerId   = "SI MS (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI OV (Year 8)",
      group     = "SI OV (Year 8)",  layerId   = "SI OV (Year 8)") %>%
    # addImageQuery(sal_cool$salinity_min_cool_03_03, digits    = 2, position  = "bottomright",
    #               layerId   = "Cool month min salinity (Year 8)") %>%
    # addImageQuery(sal_cool$salinity_min_cool_10_10, digits    = 2, position  = "bottomright",
    #               layerId   = "Sal SI cool (Year 8)") %>%
    # addImageQuery(sal_warm$salinity_min_warm_03_03, digits    = 2, position  = "bottomright",
    #               layerId   = "Warm month min salinity (Year 8)") %>%
    # addImageQuery(sal_warm$salinity_min_warm_10_10,  digits    = 2, position  = "bottomright",
    #               layerId   = "Sal SI warm (Year 8)") %>%
    # addImageQuery(sal_avg$salinity_avg_03_03, digits    = 2, position  = "bottomright",
    #               layerId   = "Annual mean salinity (Year 8)") %>%
    # addImageQuery(sal_avg$salinity_avg_10_10, digits    = 2, position  = "bottomright",
    #               layerId   = "Sal SI avg (Year 8)") %>%
    # addImageQuery(si_ms$si_ms_mask_03_03, digits    = 2, position  = "bottomright",
    #               layerId   = "SI MS (Year 8)") %>%
    # addImageQuery(si_ms$si_ms_mask_10_10, digits    = 2, position  = "bottomright",
    #               layerId   = "SI MS (Year 8)") %>%
    # addImageQuery(si_ov$si_ov_mask_03_03, digits    = 2,  position  = "bottomright",
    #               layerId   = "SI OV (Year 8)") %>%
    # addImageQuery(si_ov$si_ov_mask_10_10, digits    = 2,  position  = "bottomright",
    #               layerId   = "SI OV (Year 8)") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c( "Topographic", "Imagery"),  # "Topographic w/ roads"
      overlayGroups = c(
        "Cool month min salinity (Year 8)",
        "Warm month min salinity (Year 8)",
        "Annual mean salinity (Year 8)",
        "SI MS (Year 8)",
        "SI OV (Year 8)"
      )
    ) %>%
    hideGroup(
      c(
        "Cool month min salinity (Year 8)",
        "Warm month min salinity (Year 8)",
        "Annual mean salinity (Year 8)",
        "SI MS (Year 8)",
        "SI OV (Year 8)"
      )
    ) %>%
    addBootstrapDependency() %>%
    addEasyButton(
      easyButton(
        icon     = "fa-info-circle",
        title    = "Meta data",
        position = "topright",
        id = "info_button",
        onClick = JS("
              function(btn, map) {
                Shiny.onInputChange('ov_info_button', Math.random());
              }"
        )))
    # addBootstrapDependency() %>%
    # addEasyButton(easyButton(
    #   icon = "fa-info-circle", title = "Meta data", position = "topright",
    #   onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
    # )) %>%
    # htmlwidgets::appendContent(ov_info_box)

}

# --- Shiny utils --
# test_basemap2 <- function(
#   pts = NULL
# )
# {
#
#
#   # ----------- LEAFLET MAP ----------
#   # leaf <- leaflet() %>%
#   #   addTiles() %>%
#   #   addEasyButton(easyButton(
#   #     icon = htmltools::span(class = "star", htmltools::HTML("&starf;")),
#   #     onClick = JS("function(btn, map){ map.setZoom(1);}")))
#   # leaf
#   #
#   # leaf <- leaflet() %>%
#   #   addTiles() %>%
#   #   addEasyButtonBar(
#   #     easyButton(
#   #       icon = htmltools::span(class = "star", htmltools::HTML("&starf;")),
#   #       onClick = JS("function(btn, map){ alert(\"Button 1\");}"))
#   #     )
#
#   leaflet() %>%
#     addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
#     addBootstrapDependency() %>%
#     addEasyButton(
#       easyButton(
#         icon     = "fa-info-circle",
#         title    = "Meta data",
#         position = "topright",
#         id = "info_button",
#         onClick = JS("
#               function(btn, map) {
#                 Shiny.onInputChange('my_easy_button', Math.random());
#               }"
#         )))
#     # addEasyButton(
#     #   easyButton(
#     #     icon     = "fa-info-circle",
#     #     title    = "Meta data",
#     #     position = "topright",
#     #     id = "info_button",
#     #     onClick = JS("
#     #           function(btn, map) {
#     #             Shiny.onInputChange('my_easy_button', 'frozen');
#     #             Shiny.onInputChange('my_easy_button', Math.random());
#     #           }"
#     #     )))
# }

    # addEasyButton(
    #   easyButton(
    #     icon     = "fa-info-circle",
    #     title    = "Meta data",
    #     position = "topright",
    #     id = "info_button",
    #     # onClick  =
    #     #   JS('Shiny.onInputChange(\"button_click\", .modal("show")'))
    #     onClick = JS("
    #      function(btn, map) {
    #             Shiny.onInputChange('my_easy_button', 'clicked');
    #           }"
    #     )
    #     # onClick  = JS("function(btn, map){ $('#infobox').modal('show');
    #     #       }")
    #     ))
    # htmlwidgets::onRender("function(btn, map){ $('#infobox').modal('show');
    #           }")
    # htmlwidgets::appendContent(info_box)
    # widgetframe::frameWidget()

# }



# library(leaflet)
# library(htmltools)
# library(htmlwidgets)
#
# bingPlugin <- htmlDependency(
#   info_box,
#   src = normalizePath("./js"),
#   script = "Bing.min.js"
# )
#
# registerPlugin <- function(map, plugin) {
#   map$dependencies <- c(map$dependencies, list(plugin))
#   map
# }
#
# leaflet() %>% setView(-122.23, 37.75, zoom = 10) %>%
#   registerPlugin(bingPlugin) %>%
#   onRender("function(el, x) {
#     var imagerySet = 'Aerial';
#     var bing = new L.BingLayer('LfO3DMI9S6GnXD7d0WGs~bq2DRVkmIAzSOFdodzZLvw~Arx8dclDxmZA0Y38tHIJlJfnMbGq5GXeYmrGOUIbS2VLFzRKCK0Yv_bAl6oe-DOc',
#          {type: imagerySet});
#      this.addLayer(bing);
#  }")


# leaflet() %>% addTiles() %>%
#   addMarkers(data=quakes,
#              clusterOptions = markerClusterOptions(),
#              clusterId = "quakesCluster") %>%
#   addEasyButton(easyButton(
#     states = list(
#       easyButtonState(
#         stateName="unfrozen-markers",
#         icon="ion-toggle",
#         title="Freeze Clusters",
#         onClick = JS("
#           function(btn, map) {
#             var clusterManager =
#               map.layerManager.getLayer('cluster', 'quakesCluster');
#             clusterManager.freezeAtZoom();
#             btn.state('frozen-markers');
#           }")
#       ),
#       easyButtonState(
#         stateName="frozen-markers",
#         icon="ion-toggle-filled",
#         title="UnFreeze Clusters",
#         onClick = JS("
#           function(btn, map) {
#             var clusterManager =
#               map.layerManager.getLayer('cluster', 'quakesCluster');
#             clusterManager.unfreeze();
#             btn.state('unfrozen-markers');
#           }")
#       )
#     )
#   ))
# --- Shiny utils --
aoc_basemap <- function(
  aoc,
  aoc_areas,
  waterways,
  sowb,
  cup,
  cpra_projects,
  ldh,
  pts = NULL
) {

  # ---- COLOR PALETTES & LABELS -----

  # AOC permitted areas polygon
  aoc_poly_pal          <- colorFactor(c("black"),   domain = aoc_areas$label)

  # Waterways legend color palette + label
  waterways_label  <- "USACE navigation channels"
  waterways_pal    <- colorFactor(c("dodgerblue"), domain = waterways_label)

  # sowb legend color palette + label
  sowb_label       <- "State owned water bottoms"
  sowb_pal         <- colorFactor(c("black"),   domain = sowb_label)

  # Coastal use permits legend color palette + label
  cup_label        <- "Coastal use permits"
  cup_pal          <- colorFactor(c("black"),   domain = cup_label)

  # Coastal use permits legend color palette + label
  cpra_label       <- "CPRA Projects"
  cpra_pal         <- colorFactor(c("black"),   domain = cpra_label)

  # LDH factor color palette
  ldh_pal          <- colorFactor(c("red", "yellow", "green"),   domain = ldh$Status)
  # SI value domain
  vect <- seq(0, 1, by = .1)

  # SI palatte
  aoc_pal          <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)

  # ----------- LEAFLET MAP ----------

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    # addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    addRasterImage(
      aoc$aoc_shallow,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC shallow (Year 8)",
      layerId   = "AOC shallow (Year 8)") %>%
    addRasterImage(
      aoc$aoc_deep,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC deep (Year 8)") %>%
    addPolygons(
      data      = aoc_areas,
      fillColor = ~aoc_poly_pal(label),
      fillOpacity = 0.2,
      color     = ~aoc_poly_pal(label),
      weight    = 3,
      opacity   = 1,
      highlightOptions = highlightOptions(
         color = "red", opacity = 1, weight = 7, bringToFront = TRUE),
      label     = ~label,
      group     = "AOC permitted areas") %>%
    addPolygons(
      data      = waterways,
      fillColor = "dodgerblue",
      weight    = 2,
      fillOpacity = 0.7,
      color     = "white",
      highlightOptions    = highlightOptions(
          color = "white", opacity = 1, weight = 4, bringToFront = TRUE),
      label     = ~label,
      group     = "USACE navigation channels") %>%
    addRasterImage(
      sowb,
      colors    = "black",
      opacity   = 0.2,
      group     = "State owned water bottoms") %>%
    addRasterImage(
      cup,
      colors    = "black",
      opacity   = 0.2,
      group     = "Coastal Use Permits") %>%
    addRasterImage(
      cpra_projects,
      colors    = "black",
      opacity   = 0.2,
      group     = "CPRA Projects") %>%
    addPolygons(
      data        = ldh,
      fillColor   = ~ldh_pal(Status), fillOpacity = 0.2, color       = ~ldh_pal(Status),
      weight      = 3,                opacity     = 1,   label       = ~Status,
      group       = "Oyster harvest areas",
      highlightOptions = highlightOptions(
        opacity = 1,
        weight = 6,
        bringToFront = TRUE)) %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      title     = "AOC shallow (Year 8)",      values    = vect,
      group     = "AOC shallow (Year 8)",      layerId   = "AOC shallow (Year 8)") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      title     = "AOC deep (Year 8)",         values    = vect,
      group     = "AOC deep (Year 8)",         layerId   = "AOC deep (Year 8)") %>%
    addLegend(
      pal       = aoc_poly_pal,
      position  = "topleft",
      title     = "AOC permitted areas",       values    = aoc_areas$label,
      group     = "AOC permitted areas",       layerId   = "AOC permitted areas") %>%
    addLegend(
      pal       = waterways_pal,
      position  = "topleft",                   values    = waterways_label,
      group     = "USACE navigation channels", layerId   = "USACE navigation channels") %>%
    addLegend(
      pal       = sowb_pal,
      position  = "topleft",                   values    = sowb_label,
      group     = "State owned water bottoms", layerId   = "State owned water bottoms") %>%
    addLegend(
      pal       = cup_pal,
      position  = "topleft",                   values    = cup_label,
      group     = "Coastal Use Permits",       layerId   = "Coastal Use Permits") %>%
    addLegend(
      pal       = cpra_pal,
      position  = "topleft",                   values    = cpra_label,
      group     = "CPRA Projects",             layerId   = "CPRA Projects") %>%
    addLegend(
      pal       = ldh_pal,
      position  = "topleft",
      title     ='Oyster harvest areas status',values    = ldh$Status,
      group     = "Oyster harvest areas",      layerId   = "Oyster harvest areas") %>%
    # addImageQuery(aoc_shallow$aoc_shallow_03_03,
    #               digits    = 2,
    #               position  = "bottomright",
    #               layerId   = "AOC shallow (Year 8)") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = T),
      baseGroups = c("Topographic", "Imagery"), #"Topographic w/ roads"),
      overlayGroups = c(
        "AOC shallow (Year 8)",
        "AOC deep (Year 8)",
        "USACE navigation channels",
        "State owned water bottoms",
        "Coastal Use Permits",
        "CPRA Projects",
        "AOC permitted areas",
        "Oyster harvest areas"
      )
    ) %>%
    hideGroup(
      c(
        "AOC shallow (Year 8)",
        "AOC deep (Year 8)",
        "USACE navigation channels",
        "State owned water bottoms",
        "Coastal Use Permits",
        "CPRA Projects",
        "AOC permitted areas",
        "Oyster harvest areas"
      )) %>%
    addEasyButton(
      easyButton(
        icon     = "fa-info-circle",
        title    = "Meta data",
        position = "topright",
        id = "info_button",
        onClick = JS("
              function(btn, map) {
                Shiny.onInputChange('aoc_info_button', Math.random());
              }"
        )))
}




# # ---- OV Info button ----
# ov_info_box <- HTML(paste0(
#   HTML(
#     '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
#   ),
#
#   # Header / Title
#   HTML("<strong>Meta data</strong>"),
#   HTML(
#     '</div><div class="modal-body">'
#   ),
#   HTML('
# <table class="table">
#       <thead>
#         <tr>
#           <th scope="col">Layer</th>
#           <th scope="col">Description</th>
#           </tr>
#         </thead>
#         <tbody>
#          <tr>
#             <td><strong>Cool month min salinity</strong></td>
#             <td>Suitability index based on cool month minimum salinity </td>
#             </tr>
#          <tr>
#             <td><strong>Warm month min salinity</strong></td>
#             <td> Suitability index based on warm month minimum salinity</td>
#          </tr>
#          <tr>
#             <td><strong>Annual mean salinity</strong></td>
#             <td> Suitability index based on annual average salinity </td>
#          </tr>
#          <tr>
#             <td><strong>SI MS</strong></td>
#             <td>Suitability index based on combination of cool and warm month minimum salinity</td>
#          </tr>
#          <tr>
#             <td><strong>SI OV</td>
#             <td>Oyster viability index based on cool and warm month minimum salinity and annual average salinity</td>
#          </tr>
#     </tbody>
# </table>'
#   ),
#   # Closing divs
#   HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
# ))
# # ---- CV Info button ----
# cv_info_box <- HTML(paste0(
#   HTML(
#     '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
#   ),
#
#   # Header / Title
#   HTML("<strong>Meta data</strong>"),
#   HTML(
#     '</div><div class="modal-body">'
#   ),
#   HTML('
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
#   ),
#   # Closing divs
#   HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
# ))
# # ---- AOC Info button ----
# aoc_info_box <- HTML(paste0(
#   HTML(
#     '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
#   ),
#
#   # Header / Title
#   HTML("<strong>Meta data</strong>"),
#   HTML(
#     '</div><div class="modal-body">'
#   ),
#   HTML('
# <table class="table">
#       <thead>
#         <tr>
#           <th scope="col">Layer</th>
#           <th scope="col">Description</th>
#           </tr>
#         </thead>
#         <tbody>
#          <tr>
#             <td><strong>AOC Shallow</strong></td>
#             <td>AOC suitability index for shallow water operations based on oyster viability and commercial viability</td>
#             </tr>
#          <tr>
#             <td><strong>AOC Deep</strong></td>
#             <td>AOC suitability index for deep water operations based on oyster viability and commercial viability </td>
#          </tr>
#          <tr>
#             <td><strong>USACE Navigation Channels</strong></td>
#             <td>Reference layer showing navigation channels, which may create regulatory limitations on AOC operations</td>
#          </tr>
#          <tr>
#             <td><strong>State owned water bottoms</strong></td>
#             <td>Reference layer showing state owned water bottoms, where AOC operations would be allowed</td>
#          </tr>
#          <tr>
#             <td><strong>Coastal Use Permits</strong></td>
#             <td>Reference layer showing coastal use permits, which may create regulatory limits on AOC operations </td>
#          </tr>
#          <tr>
#             <td><strong>CPRA Projects</strong></td>
#             <td>Reference layer showing integrated protection projects, which may create regulatory limitations on AOC operations</td>
#          </tr>
#     </tbody>
# </table>'
#   ),
#   # Closing divs
#   HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
# ))

# ---- Extra scrap code ----
# Define HTML for the infobox
# cv_info_box <- HTML(paste0(
#   HTML(
#     '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
#   ),
#
#   # Header / Title
#   HTML("Meta data"),
#   HTML(
#     '</div><div class="modal-body">'
#   ),
#
#
#   HTML('
# <p><strong>Fetch</strong> - Average exposure to open water. Provides a proxy for wave energy that could affect AOC operations </p>
# <p><strong>Roads</strong> - Line of sight distance to nearest road. Provides a metric for ease of access. </p>
# <p><strong>Sediment Depositon</strong> - Modeled sediment deposition rate. Provides a measure of how often AOC equipment may need cleaning  </p>
# <p><strong>Shallow water CV</strong> - Index of commercial viability for shallow water, combining fetch, distance and sediment deposition  </p>
# <p><strong>Deep water CV</strong> - Index of commercial viability for deep water, combining fetch and sediment deposition   </p>'),
#
#   # Closing divs
#   HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
# ))
# Define HTML for the infobox
# aoc_info_box <- HTML(paste0(
#   HTML(
#     '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
#   ),
#
#   # Header / Title
#   HTML("Meta data"),
#   HTML(
#     '</div><div class="modal-body">'
#   ),
#
#
#   HTML('
# <p><strong>AOC Shallow</strong> - AOC suitability index for shallow water operations based on oyster viability and commercial viability   </p>
# <p><strong>AOC Deep</strong> - AOC suitability index for deep water operations based on oyster viability and commercial viability   </p>
# <p><strong>USACE Navigation Channels</strong> - Reference layer showing navigation channels, which may create regulatory limitations on AOC operations</p>
# <p><strong>State owned water bottoms</strong> - Reference layer showing state owned water bottoms, where AOC operations would be allowed</p>
# <p><strong>Coastal Use Permits</strong> - Reference layer showing coastal use permits, which may create regulatory limits on AOC operations   </p>
# <p><strong>CPRA Projects</strong> - Reference layer showing integrated protection projects, which may create regulatory limitations on AOC operations</p>'),
#
#   # Closing divs
#   HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
# ))

# Define HTML for the infobox
# ov_info_box <- HTML(paste0(
#   HTML(
#     '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
#   ),
#
#   # Header / Title
#   HTML("Meta data"),
#   HTML(
#     '</div><div class="modal-body">'
#   ),
#
#
#   HTML('
# <p><strong>Cool month min salinity</strong> - Suitability index based on cool month minimum salinity </p>
# <p><strong>Warm month min salinity</strong> - Suitability index based on warm month minimum salinity </p>
# <p><strong>Annual mean salinity</strong> - Suitability index based on annual average salinity  </p>
# <p><strong>SI MS</strong> - Suitability index based on combination of cool and warm month minimum salinity  </p>
# <p><strong>SI OV</strong> - Oyster viability index based on cool and warm month minimum salinity and annual average salinity</p>'),
#
#   # Closing divs
#   HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
# ))
# # ==================================
# # ----------- LEAFLET MAP ----------
# # ==================================
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
#   addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
#   addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat. Geo. Topographic") %>%
#   addScaleBar("bottomleft") %>%
#   addMeasure(position = "bottomright", primaryLengthUnit = "feet",
#              primaryAreaUnit = "sqmiles", activeColor = "red", completedColor = "green") %>%
#   leafem::addMouseCoordinates() %>%
#   setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
#   addPolygons(
#     data             = ldh,
#     fillColor        = ~factpal(Status), fillOpacity = 0.2, color = ~factpal(Status),
#     highlightOptions = highlightOptions(opacity = 1, weight = 6, bringToFront = TRUE), weight = 3, opacity = 1,
#     label            = ~Status, group = "LDH") %>%
#   addLegend(
#     pal       = factpal,
#     position  = "topleft",
#     title     ='LDH Status',
#     # title = htmltools::tags$div('LDH Status', style = 'font-size: 16px; color: black;'),
#     values    = ldh$Status,
#     group     = "LDH",
#     layerId = "LDH") %>%
#   addPolygons(
#     data             = aoc,
#     fillColor        = ~aoc_pal(label), fillOpacity = 0.2, color = ~aoc_pal(label),
#     highlightOptions = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE), weight = 3, opacity = 1,
#     label            = ~label, group = "AOC") %>%
#   addLegend(
#     pal       = aoc_pal,
#     position  = "topleft",
#     title     = "AOC",
#     # title = htmltools::tags$div('LDH Status', style = 'font-size: 16px; color: black;'),
#     values    = aoc$label,
#     group     = "AOC",
#     layerId   = "AOC") %>%
#   # addPolygons(
#   #   data             = mask_interm,
#   #   fillColor        = "white", fillOpacity = 0.5, color = "white", weight = 1.5, opacity = 1,
#   #   highlightOptions = highlightOptions(color = "white", opacity = 1, weight = 2.5, bringToFront = TRUE),
#   #   label            = ~label, group = "Full mask (LDH open & intermed.)" )  %>%
#   # addPolygons(
#   #   data             = mask_open,
#   #   fillColor        = "white", fillOpacity = 0.5, color = "white", weight = 1.5, opacity = 1,
#   #   highlightOptions = highlightOptions(color = "white", opacity = 1, weight = 2.5, bringToFront = TRUE),
#   #   label            = ~label, group = "Full mask (LDH open)")  %>%
#   addRasterImage(
#     sowb,
#     colors    = "#00bfb2", opacity   = 0.7,
#     group     = "State owned water bottoms") %>% # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
#   addLegend(
#     pal       = sowb_pal,
#     position  = "topleft", values    = sowb_label,
#     group     = "State owned water bottoms", layerId = "State owned water bottoms") %>%
#   addRasterImage(
#     cup,
#     colors    = "#EAC435", opacity   = 0.7,
#     group     = "Coastal Use Permits") %>%   # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
#   addLegend(
#     pal       = cup_pal,
#     position  = "topleft", values    = cup_label,
#     group     = "Coastal Use Permits",  layerId   = "Coastal Use Permits") %>%
#   addRasterImage(
#     cpra_projects,
#     colors    = "hotpink", opacity   = 0.7,
#     group     = "CPRA projects") %>%   # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
#   addLegend(
#     pal       = cpra_proj_pal,
#     position  = "topleft", values    = cpra_proj_label,
#     group     = "CPRA projects",  layerId   = "CPRA projects") %>%
#   # addPolygons(
#   #     data             = cpra_proj_poly,
#   #     col              = "black",
#   #     fillColor        = "hotpink", # "#CC2936",  #  "#F45B69",
#   #     fillOpacity      = 0.7, weight = 1.5,
#   #     highlightOptions = highlightOptions(color = "hotpink", opacity = 1, weight = 2, bringToFront = TRUE),
#   #     label            = ~label, group = "CPRA projects") %>%
#   # addLegend(
#   #     pal       = cpra_proj_pal,
#   #     position  = "topleft", values = cpra_proj_label,
#   #     group     = "CPRA projects", layerId  = "CPRA projects") %>%
# # addPolygons(
# #     data              = cup,
# #     col               = "black", fillColor = "#EAC435", fillOpacity = 0.4, weight = 1.5, #"#DD7373",
# #     highlightOptions  = highlightOptions(color = "#EAC435", opacity = 1, weight = 2, bringToFront = TRUE),
# #     label             = ~label, group = "Coastal Use Permits") %>%
# # addLegend(
# #     pal       = cup_pal,
# #     position  = "topleft", values = cup_label,
# #     group     = "Coastal Use Permits", layerId  = "Coastal Use Permits") %>%
# # leaflegend::addLegendFactor(pal = factpal,  title = htmltools::tags$div('LDH Status', style = 'font-size: 16px; color: black;'),  values = ldh$Status, labelStyle = 'font-size: 12px; font-weight: bold;',   group = "LDH") %>%
# addRasterImage(
#   fetch_cat7,
#   opacity   = 0.8,
#   colors    = turbo_pal,
#   group     = "Fetch") %>%
#   addLegend(
#     pal       = fetch_fact,
#     position  = "bottomright", title  = "Fetch", labFormat = labelFormat(suffix = " km"),
#     group     = "Fetch", layerId  = "Fetch", values    = fetch_cols$fetch_cols ) %>%
#   addRasterImage(
#     sal3,
#     opacity   = 0.8,
#     colors    = sal_pal,
#     group     = "2023 MP Mean annual salinity (Year 1)") %>%
#   addRasterImage(
#     sal10,
#     opacity   = 0.8,
#     colors    = sal_pal,
#     group     = "2023 MP Mean annual salinity (Year 8)") %>%
#   addLegend(
#     pal       = sal_pal,
#     title     = "2023 MP Mean annual salinity (Year 1)", position  = "bottomright",
#     labFormat = labelFormat(suffix = " g/L"),
#     group     = "2023 MP Mean annual salinity (Year 1)",  layerId = "2023 MP Mean annual salinity (Year 1)", values = sal_cols$numeric_cols) %>%
#   addLegend(
#     pal       = sal_pal,
#     title     = "2023 MP Mean annual salinity (Year 8)", position  = "bottomright",
#     labFormat = labelFormat(suffix = " g/L"),
#     group     = "2023 MP Mean annual salinity (Year 8)", layerId = "2023 MP Mean annual salinity (Year 8)", values = sal_cols$numeric_cols) %>%
#   addRasterImage(
#     depth1,
#     opacity   = 0.8,
#     colors    =  brewer.pal(9, "Spectral"),  # colors    =  c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
#     group     = "2023 MP Depth (Year 1)") %>%
#   addLegend(
#     pal       = depth_fact_pal,
#     title     = "2023 MP Depth (Year 1)",  position  = "bottomleft",
#     group     = "2023 MP Depth (Year 1)",  layerId = "2023 MP Depth (Year 1)", values = depth_labels) %>%
#   addRasterImage(
#     wlvl_clamp,
#     colors    = wlvl_pal,
#     opacity   = 0.8,
#     group     = "Water level variability (2021)") %>%
#   addLegend(
#     pal       = wlvl_pal,
#     labFormat = labelFormat(suffix = " m"),
#     title     = "Water level variability (2021)", position  = "bottomleft",
#     group     = "Water level variability (2021)", layerId   = "Water level variability (2021)", values = values(wlvl_clamp)) %>%
#   addRasterImage(
#     hsi_2017,
#     colors    = hsi_pal,
#     opacity   = 0.8,
#     group     = "HSI 2017 500m grid") %>%
#   addLegend(
#     pal       = hsi_pal,
#     title     = "HSI 2017 500m grid", position  = "bottomleft",
#     group     = "HSI 2017 500m grid", layerId   = "HSI 2017 500m grid", values = values(hsi_2017)) %>%
#   # addRasterImage(
#   #     wlvl_clamp,
#   #     colors    = wlvl_pal,
#   #     opacity   = 0.8,
#   #     group     = "Water level variability (2021)") %>%
#   # addLegend(
#   #     pal       = wlvl_pal,
#   #     labFormat = labelFormat(suffix = " m"),
#   #     title     = "Water level variability (2021)", position  = "bottomleft",
#   #     group     = "Water level variability (2021)", layerId   = "Water level variability (2021)", values = values(wlvl_clamp)) %>%
#   # addRasterImage(
# #   depth10,
# #   colors    = depth_pal,
# #   group     = "Depth year 8") %>%
# # addLegend(
# #   pal       = depth_pal,
# #   title     = "Depth year 8", position  = "bottomleft",
# #   labFormat = labelFormat(suffix = " m"),
# #   group     = "Depth year 8",  layerId = "Depth year 8", values = depth_cols$numeric_cols) %>%
# # addRasterImage(
# #   hsi_sal3,
# #   colors    = viridisLite::magma(n = 20),
# #   # colors    = RColorBrewer::brewer.pal(9, "YlOrRd"),
# #   opacity   = 0.7,
# #   group     = "HSI Salinity year 1") %>%
# #   addLegend(
# #     pal       = hsi_pal,
# #     title     = "HSI", position  = "bottomright",
# #     group     = "HSI Salinity year 1", layerId = "HSI Salinity year 1", values = values(hsi_sal3)) %>%
# #   addRasterImage(
# #     hsi_sal10,
# #     colors    = viridisLite::magma(n = 20),
# #     # colors    = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
# #     opacity   = 0.7,
# #     group     = "HSI 2023 MP Mean annual salinity (Year 8)") %>%
# #   addLegend(
# #     pal       = hsi_pal,
# #     title     = "HSI", position  = "bottomright",
# #     group     = "HSI 2023 MP Mean annual salinity (Year 8)", layerId = "HSI 2023 MP Mean annual salinity (Year 8)", values = values(hsi_sal10)) %>%
# #   addRasterImage(
# #     hsi_sal3_mask,
# #     colors    = viridisLite::magma(n = 20),
# #     # colors    = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
# #     opacity   = 0.7,
# #     group     = "HSI Salinity full mask year 1") %>%
# #   addLegend(
# #     pal       = hsi_pal,
# #     title     = "HSI", position  = "bottomright",
# #     group     = "HSI Salinity full mask year 1", layerId = "HSI Salinity full mask year 1", values = values(hsi_sal3)) %>%
# #   addRasterImage(
# #     hsi_sal10_mask,
# #     colors    = viridisLite::magma(n = 20),
# #     # colors    = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
# #     opacity   = 0.7,
# #     group     = "HSI Salinity full mask year 8") %>%
# # addLegend(
# #   pal       = hsi_pal,
# #   title     = "HSI", position  = "bottomright",
# #   group     = "HSI Salinity full mask year 8", layerId = "HSI Salinity full mask year 8", values = values(hsi_sal10)) %>%
# # addRasterImage(fetch_rc, colors  = if (raster::is.factor(fetch_rc)) "Set1" else  "YlGnBu",group = "Fetch") %>% # addLegendNumeric( pal    = fetch_pal, values = fetch_cols$fetch_cols)
# addPolylines(
#   data              = road_buffer[4,],
#   fillColor         = 'transparent',  col   = "red", opacity  = 1, weight  = 3,
#   label             = ~road_buffer$buffer_dist[4], group = "Road buffer 20km",
#   highlightOptions  = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#   addPolylines(
#     data               = road_buffer[3,],
#     fillColor          = 'transparent', col   = "red", opacity = 1, weight  = 3,
#     label              = ~road_buffer$buffer_dist[3],  group = "Road buffer 10km",
#     highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#   addPolylines(
#     data               = road_buffer[2,],
#     fillColor          = 'transparent',  col   = "red", opacity     = 1,  weight  = 3,
#     label              = ~road_buffer$buffer_dist[2], group = "Road buffer 5km",
#     highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#   addPolylines(
#     data               = road_buffer[1,],
#     fillColor          = 'transparent', col  = "red",opacity  = 1, weight  = 3,
#     label              = ~road_buffer$buffer_dist[1],  group = "Road buffer 2km",
#     highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
#   addPolygons(
#     data                = waterways,
#     fillColor           = "dodgerblue", weight = 2, fillOpacity = 0.7, color = "white",
#     highlightOptions    = highlightOptions(color = "white", opacity = 1, weight = 4, bringToFront = TRUE),
#     label               = ~label,  group = "USACE navigation channels") %>%
#   addLegend(
#     pal       = waterways_pal,
#     position  = "topleft", values = waterways_label,
#     group     = "USACE navigation channels", layerId  = "USACE navigation channels") %>%
#   # addLegend(
#   #   pal       = mask_interm_pal,
#   #   position  = "topleft", values = mask_interm_label,
#   #   group     = "Full mask (LDH open & intermed.)",  layerId = "Full mask (LDH open & intermed.)"
#   # ) %>%
#   # addLegend(
#   #   pal       = mask_open_pal,
#   #   position  = "topleft", values = mask_open_label,
#   #   group     = "Full mask (LDH open)", layerId = "Full mask (LDH open)"
#   # ) %>%
#   addPolygons(
#     data       = land_sf,
#     fillColor  = 'white', fillOpacity =  0.3, col  = "black",
#     opacity    = 1, weight  = 1.5, label = ~label,  group  = "2023 MP Land (Year 1)") %>%
#   # addRasterImage(  waterways, colors  = "dodgerblue",  group = "USACE navigation channels") %>%
#   # addRasterImage( cpra_projects, colors  = turbo_pal, group = "CPRA projects") %>%
#   addLayersControl(
#     options = layersControlOptions(collapsed = TRUE),
#     baseGroups = c("Imagery", "Topographic", "Nat. Geo. Topographic"), overlayGroups = c(
#       "LDH", "AOC", "Salinity year 1", "2023 MP Mean annual salinity (Year 8)", "HSI 2017 500m grid", "2023 MP Depth (Year 1)", "Water level variability (2021)", "Fetch",
#       # "HSI Salinity year 1", "HSI Salinity year 8", "HSI Salinity full mask year 1", "HSI Salinity full mask year 8",
#       "Road buffer 2km", "Road buffer 5km","Road buffer 10km", "Road buffer 20km",
#       "USACE navigation channels", "CPRA projects", "Coastal Use Permits", "State owned water bottoms",
#       # "Full mask (LDH open)", "Full mask (LDH open & intermed.)",
#       "2023 MP Land (Year 1)")) %>%
#   hideGroup(
#     c("AOC", "Salinity year 1", "Salinity year 8", "HSI 2017 500m grid", "2023 MP Depth (Year 1)", "Water level variability (2021)", "Fetch",
#       # "HSI Salinity year 1", "HSI Salinity year 8", "HSI Salinity full mask year 1", "HSI Salinity full mask year 8",
#       "Road buffer 2km", "Road buffer 5km", "Road buffer 10km", "Road buffer 20km",  "USACE navigation channels",
#       "CPRA projects", "Coastal Use Permits",  "State owned water bottoms",
#       # "Full mask (LDH open)", "Full mask (LDH open & intermed.)",
#       "2023 MP Land (Year 1)")
#   )

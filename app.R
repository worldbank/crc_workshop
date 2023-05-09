library(shinythemes)
library(shinyWidgets)

# Before proceeding, set working directory to source file location
source('global_params.R')
source('cities.R')
source('text.R')

# UI elements ----------------------------------------
intro_text_style <- 'text-align: center; color: grey; line-height: 2em; padding-left: 6em; padding-right: 6em; '

sb_l <- 8   # sidebar left column width
sb_r <- 4   # sidebar right column width

sidebar_tags <- tags$head(
  tags$style(type = 'text/css',
             '.shiny-input-container {margin: 0; } ',
             '.irs-handle.single {width: 10px; height: 10px; }',
             '.irs-bar.irs-bar--single {background-color: grey; border-bottom-color: grey; border-top-color: grey; }',
             '.irs-single {display: none !important; }',
             '.irs-from {display: none !important; }',
             '.irs-to {display: none !important; }',
             '.irs-min {display: none !important; }',
             '.irs-max {display: none !important; }')
)

sidebar_disclaimer <- p(em('Some layers may take a moment to appear.',
                           style = 'font-size: 0.8em; color: darkgrey;'))

siderbar_column_header <- fluidRow(
  column(sb_l, strong('Layers', style = 'font-size: 0.8em; ')),
  column(sb_r, strong('Opacity', style = 'font-size: 0.8em; '))
)

alpha_slider <- function(slider_name, default_alpha = 0.9) {
  sliderInput(slider_name, NULL, 0, 1, default_alpha, step = 0.1, ticks = F)
}

layer_toggle <- function(box_id, box_lab, box_def = F, slider_def = 0.9) {
  slider_id <- paste0(box_id, '_alpha')
  fluidRow(
    column(sb_l, checkboxInput(box_id, box_lab, value = box_def)),
    column(sb_r, alpha_slider(slider_id, slider_def))
  )
}

city_tab <- function(city) {
  city_prefix <- city_prefixes[city_folders == city]

  tabPanel(city,
           sidebarLayout(
             # Sidebar -------------------------------
             sidebarPanel(
               width = 3,
               style = "overflow-y:scroll; max-height: 550px;",
               sidebar_tags,
               
               strong(city),
               sidebar_disclaimer,
               siderbar_column_header,
               layer_toggle(paste0(city_prefix, '_aoi'), '1. Area of interest (AOI)'),
               # layer_toggle(paste0(city_prefix, '_admin'), '2. Admin boundaries'),
               layer_toggle(paste0(city_prefix, '_sol'), '2. Economic hotspots'),
               layer_toggle(paste0(city_prefix, '_soc'), '3. Economic hotspots change'),
               layer_toggle(paste0(city_prefix, '_pop'), '4. Population'),
               layer_toggle(paste0(city_prefix, '_wsf'), '5. Built-up area change'),
               # layer_toggle(paste0(city_prefix, '_imperv'), '7. Built-up density'),
               layer_toggle(paste0(city_prefix, '_lc'), '6. Land cover'),
               layer_toggle(paste0(city_prefix, '_temp'), '7. Surface temperature'),
               layer_toggle(paste0(city_prefix, '_green'), '8. Green space'),
               layer_toggle(paste0(city_prefix, '_forest'), '9. Forest cover'),
               layer_toggle(paste0(city_prefix, '_forest_loss'), '10. Forest cover loss'),
               layer_toggle(paste0(city_prefix, '_air'), '11. Air quality'),
               layer_toggle(paste0(city_prefix, '_fu'), '12. Fluvial flooding', slider_def = 0.5),
               layer_toggle(paste0(city_prefix, '_pu'), '13. Pluvial flooding', slider_def = 0.5),
               layer_toggle(paste0(city_prefix, '_fire'), '14. Fire stations'),
               layer_toggle(paste0(city_prefix, '_health'), '15. Health facilities'),
               layer_toggle(paste0(city_prefix, '_police'), '16. Police stations'),
               layer_toggle(paste0(city_prefix, '_schools'), '17. Schools'),
               layer_toggle(paste0(city_prefix, '_ls'), '18. Landslide susceptibility'),
               fluidRow(
                 column(sb_l, checkboxInput(paste0(city_prefix, '_quake'), '19. Earthquake hazard', value = F))
               ),
               layer_toggle(paste0(city_prefix, '_road'), '20. Road network criticality'),
               br(),
               actionBttn(paste0(city_prefix, '_clear'), 'Clear layers',
                          size = 'sm', style = 'fill')
             ),
             
             # Main panel -----------------------------
             mainPanel(
               width = 9,
               leafletOutput(paste0(city_prefix, '_map'),
                             height = 550),
               br(),
               uiOutput(paste0(city_prefix, '_plot')),
               br()
             )
           )
  )
}


# UI -----------------------------------------------------
ui <- navbarPage(
  theme = shinytheme('yeti'),
  
  site_title,
  id = 'main',
  
  # Tab: Introduction -------------------------------------
  tabPanel('Introduction',
           # setBackgroundImage(src = 'home_bg.jpg'),
           fluidRow(
             column(9,
                    fluidRow(
                      column(3,
                             img(src = 'Black.png', width = '150px')),
                      column(3,
                             img(src = 'GFDRR_Primary Logo_BW-Shade-nobg.png', width = '150px',
                                 style = 'margin-top: 15px; ')),
                      column(3,
                             img(src = 'WB-WBG-horizontal-RGB-web.jpg', width = '150px',
                                 style = 'margin-top: 18px; '))
                    ),
                    hr(), br(),
                    p(p1_1, strong(p1_2), p1_3, style = intro_text_style),
                    br(), hr(), br(),
                    p(p2, style = intro_text_style),
                    br(),
                    # p(p3, style = intro_text_style),
                    # br(),
                    p(p4, style = intro_text_style),
                    br(), hr(), br(),
                    p(p5, style = intro_text_style),
                    br(),
                    p(p6, style = intro_text_style),
                    br(),
                    p(p7, style = intro_text_style),
                    br(),
                    p(p8, style = intro_text_style),
                    br(),
                    p(p9, style = intro_text_style),
                    br(),
                    p(p10, style = intro_text_style),
                    br(), hr(), br(),
                    p(p11, style = intro_text_style),
                    br(),
                    p(p12, style = intro_text_style),
                    br(), hr()
             ),
             column(2, offset = 1,
                    br(), br(), br(), br(),
                    h4('Select a city'),
                    br(), br(),
                    apply(cities_df, 1, function(city) {
                      inputId <- paste0('go_', city['prefix'])
                      actionBttn(inputId, city['city'], style  = 'minimal', color = 'success')}))
           )),
  
  # Tabs: City Profiles ---------------------------------------
  do.call(navbarMenu, c("City Profiles", lapply(cities_df$city, city_tab))),

  # Tab: Climate Projections --------------------------
  # tabPanel('Regional Climate Projections',
  #          sidebarLayout(
  #            # Sidebar -----------------------
  #            sidebarPanel(
  #              width = 3,
  #              style = "height: 550px;",
  #              sidebar_tags,
  #              
  #              sidebar_disclaimer,
  #              
  #              selectInput('wb', 'Layer',
  #                          choices = list('Select one' = '', 
  #                                         `1. Longest consecutive dry days (CDD)` = c(`1a. CDD 1990` = 'cdd_1990',
  #                                                                                     `1b. CDD 2020` = 'cdd_2020',
  #                                                                                     `1c. CDD 1990-2040` = 'cdd_2040._Change1990'),
  #                                         `2. Average heat wave magnitude index daily (HWMID)` = c(`2a. HWMID 1990` = 'hwmid_1990',
  #                                                                                                  `2b. HWMID 2020` = 'hwmid_2020',
  #                                                                                                  `2c. HWMID 1990-2040` = 'hwmid_2040._Change1990'),
  #                                         `3. Average wet bulb globe temperature (WBGT)` = c(`3a. WBGT 1990` = 'WBGT_1990',
  #                                                                                            `3b. WBGT 2020` = 'WBGT_2020',
  #                                                                                            `3c. WBGT 1990-2040` = 'WBGT_2040._Change1990'),
  #                                         `4. Annual precipitation (PRCP)` = c(`4a. PRCP 1990` = 'prcptot_1990',
  #                                                                              `4b. PRCP 2020` = 'prcptot_2020',
  #                                                                              `4c. PRCP 1990-2040` = 'prcptot_2040._Change1990'),
  #                                         `5. Annual rainfall above 95th percentile (R95P)` = c(`5a. R95P 1990` = 'r95p_1990',
  #                                                                                               `5b. R95P 2020` = 'r95p_2020',
  #                                                                                               `5c. R95P 1990-2040` = 'r95p_2040._Change1990'))),
  #              sliderInput('wb_alpha', 'Opacity', 0, 1, 0.9, step = 0.1, ticks = F),
  #              hr(),
  #              textOutput('wb_def')
  #            ),
  #            
  #            # Main panel -----------------------
  #            mainPanel(
  #              width = 9,
  #              leafletOutput('wb_map',
  #                            height = 550),
  #              br(),
  #              imageOutput('wb_plot', width = '50%'),
  #              br()
  #            )
  #          ))
)

# Server ------------------------------------------------
server <- function(input, output, session) {
  # Intro page buttons -----------------------
  # output$ni_img <- renderImage({list(src = 'nis.png')}, deleteFile = F)
  # output$no_img <- renderImage(list(src = 'novi_sad.png'), deleteFile = F)
  # output$pr_img <- renderImage(list(src = 'pristina.png'), deleteFile = F)
  # output$sa_img <- renderImage(list(src = 'sarajevo.png'), deleteFile = F)
  # output$ti_img <- renderImage(list(src = 'tirana.png'), deleteFile = F)

  # Page navigation --------------------------
  # apply(cities_df, 1, function(city) {
  #   inputId <- paste0('go_', city['prefix'])
  #   observeEvent(input[[inputId]], updateNavbarPage(session, 'main', selected = city['city']))
  # })
  # Function for quickly writing the observeEvent functions because
  # the above apply function does not appear to work
#   # apply(cities_df, 1, function(city) {
#   #   inputId <- paste0('go_', city['prefix'])
#   #   paste0("observeEvent(input$", inputId, ", updateNavbarPage(session, 'main', selected = '", city['city'], "'))")
#   # }) %>% cat(sep = "\n")
#   
  observeEvent(input$go_abi, updateNavbarPage(session, 'main', selected = 'Abidjan'))
  observeEvent(input$go_add, updateNavbarPage(session, 'main', selected = 'Addis Ababa'))
  observeEvent(input$go_ban, updateNavbarPage(session, 'main', selected = 'Banjul'))
  observeEvent(input$go_bra, updateNavbarPage(session, 'main', selected = 'Brazzaville'))
  observeEvent(input$go_buf, updateNavbarPage(session, 'main', selected = 'Buffalo City'))
  observeEvent(input$go_buj, updateNavbarPage(session, 'main', selected = 'Bujumbura'))
  observeEvent(input$go_kam, updateNavbarPage(session, 'main', selected = 'Kampala'))
  observeEvent(input$go_kig, updateNavbarPage(session, 'main', selected = 'Kigali'))
  observeEvent(input$go_kis, updateNavbarPage(session, 'main', selected = 'Kisumu'))
  observeEvent(input$go_lag, updateNavbarPage(session, 'main', selected = 'Lagos'))
  observeEvent(input$go_lib, updateNavbarPage(session, 'main', selected = 'Libreville'))
  observeEvent(input$go_mom, updateNavbarPage(session, 'main', selected = 'Mombasa'))
  observeEvent(input$go_nai, updateNavbarPage(session, 'main', selected = 'Nairobi'))
  observeEvent(input$go_nga, updateNavbarPage(session, 'main', selected = 'Ngaoundere'))
  observeEvent(input$go_por, updateNavbarPage(session, 'main', selected = 'Port Louis'))
  observeEvent(input$go_vic, updateNavbarPage(session, 'main', selected = 'Victoria'))
  
  # Basemap function -------------------------------
  basemap <- function(data) {
    leaflet(data) %>%
      addProviderTiles('Stamen.TonerLite', group = 'Default',
                       options = providerTileOptions(zIndex = -3)) %>%
      addTiles(group = 'OpenStreetMap',
               options = tileOptions(zIndex = -2)) %>%
      addProviderTiles('Esri.WorldImagery', group = 'Satellite',
                       options = providerTileOptions(zIndex = -1)) %>%
      addPolygons(fillOpacity = 0, opacity = 0) %>%
      addLayersControl(baseGroups = c('Default', 'OpenStreetMap', 'Satellite'),
                       options = layersControlOptions(collapsed = F,
                                                      autoZIndex = F))
  }
  
  # Layer toggle functions -----------------------------------
  feature_poly <- function(map_id, input_name, feature_var, feature_lab = '') {
    weight <- case_when(str_detect(input_name, 'aoi') ~ 3,
                        str_detect(input_name, 'admin') ~ 1)
    layer_group <- input_name
    feature_alpha_name <- paste0(input_name, '_alpha')
    observe({
      notif_id <- paste0(input_name, '_notif')
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group)
      } else if (is.null(feature_var)) {
        showNotification(ui = 'No information on this layer for this city',
                         duration = NULL, id = notif_id)
      } else if (str_detect(input_name, 'aoi')) {
        leafletProxy(map_id, data = feature_var) %>%
          clearGroup(group = layer_group) %>%
          addPolygons(color = '#ffbd08', weight = weight, opacity = input[[feature_alpha_name]],
                      fillColor = 'transparent', group = layer_group)
      } else if (str_detect(input_name, 'admin')) {
        leafletProxy(map_id, data = feature_var) %>%
          clearGroup(group = layer_group) %>%
          addPolygons(color = '#ffbd08', weight = weight, opacity = input[[feature_alpha_name]],
                      fillColor = 'transparent', group = layer_group, label = feature_var[[feature_lab]])
      }
    })
  }
  feature_point <- function(map_id, input_name, feature_var, feature_icon) {
    layer_group <- input_name
    feature_alpha_name <- paste0(input_name, '_alpha')
    observe({
      notif_id <- paste0(input_name, '_notif')
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group)
        removeNotification(notif_id)
      } else if (is.null(feature_var)) {
        showNotification(ui = 'No information on this layer for this city',
                         duration = NULL, id = notif_id)
      } else {
        leafletProxy(map_id, data = feature_var) %>%
          clearGroup(group = layer_group) %>%
          addMarkers(options = markerOptions(opacity = input[[feature_alpha_name]]),
                     icon = feature_icon, group = layer_group)
      }
    })
  }
  raster_discrete <- function(map_id, input_name, raster_var, raster_col, raster_val, leg_title, lab_suffix = '') {
    layer_group <- input_name
    raster_alpha_name <- paste0(input_name, '_alpha')
    leg_id <- paste0(input_name, '_leg')
    observe({
      notif_id <- paste0(input_name, '_notif')
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group) %>%
          removeControl(leg_id)
      } else if (is.null(raster_var) | length(raster_var) <= 1) {
        showNotification(ui = 'No information on this layer for this city',
                         duration = NULL, id = notif_id)
      } else {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group) %>%
          addRasterImage(raster_var, opacity = input[[raster_alpha_name]],
                         project = F, group = layer_group, colors = raster_col) %>%
          addLegend(colors = raster_col, labels = raster_val, title = leg_title, layerId = leg_id, opacity = 0.8,
                    labFormat = labelFormat(suffix = lab_suffix), position = 'bottomright')
      }
    })
  }
  raster_continuous <- function(map_id, input_name, raster_var, raster_col, raster_val, leg_title, lab_suffix = '') {
    layer_group <- input_name
    raster_alpha_name <- paste0(input_name, '_alpha')
    leg_id <- paste0(input_name, '_leg')
    observe({
      notif_id <- paste0(input_name, '_notif')
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group) %>%
          removeControl(leg_id)
      } else if (is.null(raster_var) | length(raster_var) <= 1) {
        showNotification(ui = 'No information on this layer for this city',
                         duration = NULL, id = notif_id)
      } else {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group) %>%
          addRasterImage(raster_var, opacity = input[[raster_alpha_name]],
                         project = F, group = layer_group, colors = raster_col) %>%
          addLegend(pal = raster_col, values = raster_val, title = leg_title, layerId = leg_id, opacity = 0.8, bins = 3,
                    labFormat = labelFormat(suffix = lab_suffix), position = 'bottomright')
      }
    })
  }
  quake_fn <- function(input_name, lat, lon) {
    notif_id <- paste0(input_name, '_notif')
    observe({
      if (input[[input_name]]) {
        showNotification(ui = '',
                         action = a(href = paste0('https://maps.openquake.org/map/global-seismic-hazard-map/#8/', lat, '/', lon),
                                    'View the Global Seismic Hazard Map',
                                    target = '_blank'),
                         duration = NULL, id = notif_id)
      } else {
        removeNotification(notif_id)
      }
    })
  }
  road_fn <- function(map_id, input_name, feature_var, feature_col) {
    layer_group <- input_name
    feature_alpha_name <- paste0(input_name, '_alpha')
    leg_id <- paste0(input_name, '_leg')
    observe({
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group) %>%
          removeControl(leg_id)
      } else {
        leafletProxy(map_id, data = feature_var) %>%
          clearGroup(group = layer_group) %>%
          addPolylines(color = ~feature_col(edge_centr),
                       weight = ~ifelse(highway %in% c('primary', 'trunk', 'motorway'), 3, 1),
                       opacity = input[[feature_alpha_name]], label = ~name,
                       group = layer_group) %>%
          addLegend(pal = feature_col, values = feature_var[['edge_centr']], title = 'Road criticality',
                    layerId = leg_id, opacity = 0.8, bins = 3, position = 'bottomright')
      }
    })
  }
  uncheck <- function(input_id) {
    updateCheckboxInput(session, input_id, value = F)
  }
  clear_layers <- function(city_prefix) {
    observeEvent(input[[paste0(city_prefix, '_clear')]], {lapply(all_layers[[city_prefix]], uncheck)})
  }

  # Plot functions -------------------------
  render_plot_ui <- function(output_name, input_name0, input_name1) {
    output_name0 <- paste0(input_name0, '_plot')
    output_name1 <- paste0(input_name1, '_plot')
    output[[output_name]] <- renderUI({
      if (input[[input_name0]] & input[[input_name1]]) {
        fluidRow(
          column(6, plotOutput(output_name0)),
          column(6, plotOutput(output_name1))
        )
      } else if (input[[input_name0]]) {
        plotOutput(output_name0, width = '50%')
      } else if (input[[input_name1]]) {
        plotOutput(output_name1, width = '50%')
      } else {
        NULL
      }
    })
  }
  pop_plot <- function(input_csv, city) {
    ggplot(input_csv) +
      geom_line(aes(year, pop), linewidth = 2, color = 'grey') +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            text = element_text(size = 16)) +
      labs(subtitle = paste(city, 'Population Historical Growth'))
  }
  wsf_plot <- function(input_csv, city) {
    ggplot(input_csv) +
      geom_line(aes(year, `cumulative sq km`), linewidth = 2, color = 'grey') +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            text = element_text(size = 16)) +
      scale_x_continuous(breaks = seq(1985, 2015, 10)) +
      labs(subtitle = paste(city, 'Built-Up Area Historical Growth (sq km)'))
  }

  # Tabs: City Profiles -------------------------------
  # Set up basemap ----------------------------
  # Doesn't appear to work, requiring writing each city's output individually
  # for (prefix in cities_df$prefix) {
  #   element <- paste0(prefix, "_map")
  #   map_name <- paste0(prefix, "_basemap")
  #   output[[element]] <- renderLeaflet(basemap(aoi[[prefix]])) %>% bindCache(map_name)
  # }
  # Because the for loop doesn't appear to work, a loop for writing the below objects
  # for (prefix in cities_df$prefix) {
  #   element <- paste0(prefix, "_map")
  #   map_name <- paste0(prefix, "_basemap")
  #   paste0("output$", element, " <- renderLeaflet(basemap(aoi$", prefix, ")) %>% bindCache('", map_name, "')") %>%
  #     cat(sep = "\n")
  # }
  
  output$abi_map <- renderLeaflet(basemap(aoi$abi)) %>% bindCache('abi_basemap')
  output$add_map <- renderLeaflet(basemap(aoi$add)) %>% bindCache('add_basemap')
  output$ban_map <- renderLeaflet(basemap(aoi$ban)) %>% bindCache('ban_basemap')
  output$bra_map <- renderLeaflet(basemap(aoi$bra)) %>% bindCache('bra_basemap')
  output$buf_map <- renderLeaflet(basemap(aoi$buf)) %>% bindCache('buf_basemap')
  output$buj_map <- renderLeaflet(basemap(aoi$buj)) %>% bindCache('buj_basemap')
  output$kam_map <- renderLeaflet(basemap(aoi$kam)) %>% bindCache('kam_basemap')
  output$kig_map <- renderLeaflet(basemap(aoi$kig)) %>% bindCache('kig_basemap')
  output$kis_map <- renderLeaflet(basemap(aoi$kis)) %>% bindCache('kis_basemap')
  output$lag_map <- renderLeaflet(basemap(aoi$lag)) %>% bindCache('lag_basemap')
  output$lib_map <- renderLeaflet(basemap(aoi$lib)) %>% bindCache('lib_basemap')
  output$mom_map <- renderLeaflet(basemap(aoi$mom)) %>% bindCache('mom_basemap')
  output$nai_map <- renderLeaflet(basemap(aoi$nai)) %>% bindCache('nai_basemap')
  output$nga_map <- renderLeaflet(basemap(aoi$nga)) %>% bindCache('nga_basemap')
  output$por_map <- renderLeaflet(basemap(aoi$por)) %>% bindCache('por_basemap')
  output$vic_map <- renderLeaflet(basemap(aoi$vic)) %>% bindCache('vic_basemap')

  
  
  # Toggle layers ------------------------------
  # # All cities
  lapply(cities_df$prefix, function(prefix) {
    map_name <- paste0(prefix, "_map")
    feature_poly(map_name, paste0(prefix, '_aoi'), aoi[[prefix]])
    # feature_poly(map_name, paste0(prefix, '_admin'), admin[[prefix]], 'name')
    raster_continuous(map_name, paste0(prefix, '_sol'), sol[[prefix]], sol_col[[prefix]], sol_val[[prefix]], 'Economic hotspots')
    raster_continuous(map_name, paste0(prefix, '_soc'), soc[[prefix]], soc_col[[prefix]], soc_val[[prefix]], 'Economic hotspots change')
    raster_continuous(map_name, paste0(prefix, '_pop'), pop[[prefix]], pop_col[[prefix]], pop_val[[prefix]], 'Population')
    raster_discrete(map_name, paste0(prefix, '_wsf'), wsf[[prefix]], wsf_col[[prefix]], wsf_val[[prefix]], 'Built-up area')
    raster_continuous(map_name, paste0(prefix, '_temp'), temp[[prefix]], temp_col[[prefix]], temp_val[[prefix]], 'Temperature', '°C')
    raster_discrete(map_name, paste0(prefix, '_green'), green[[prefix]], green_col[[prefix]], green_val[[prefix]], 'Green space')
    raster_discrete(map_name, paste0(prefix, '_forest'), forest[[prefix]], forest_col[[prefix]], forest_val[[prefix]], 'Forest cover')
    raster_discrete(map_name, paste0(prefix, '_forest_loss'), forest_loss[[prefix]], forest_loss_col[[prefix]], forest_loss_val[[prefix]], 'Forest cover loss')
    raster_continuous(map_name, paste0(prefix, '_air'), air[[prefix]], air_col[[prefix]], air_val[[prefix]], 'PM2.5 concentration', 'μg/m3')
    raster_discrete(map_name, paste0(prefix, '_fu'), fu[[prefix]], fu_col[[prefix]], fu_val[[prefix]], 'Fluvial flood')
    raster_discrete(map_name, paste0(prefix, '_pu'), pu[[prefix]], pu_col[[prefix]], pu_val[[prefix]], 'Pluvial flood')
    feature_point(map_name, paste0(prefix, '_fire'), fire[[prefix]], fire_icon)
    feature_point(map_name, paste0(prefix, '_health'), health[[prefix]], health_icon)
    feature_point(map_name, paste0(prefix, '_police'), police[[prefix]], police_icon)
    feature_point(map_name, paste0(prefix, '_schools'), schools[[prefix]], schools_icon)
    raster_discrete(map_name, paste0(prefix, '_ls'), ls[[prefix]], ls_col[[prefix]], ls_val[[prefix]], 'Landslide susceptibility')
    raster_discrete(map_name, paste0(prefix, '_lc'), lc[[prefix]], lc_col[[prefix]], lc_val[[prefix]], 'Land cover')
    road_fn(map_name, paste0(prefix, '_road'), road[[prefix]], road_col[[prefix]])
    quake_fn(paste0(prefix, '_quake'), '44.542', '18.641')
  })
  

  # Clear layers -----------------------------
  lapply(city_prefixes, clear_layers)
  # observeEvent(input$sa_clear, {lapply(all_layers$sa, uncheck)})

  # Plot --------------------------------
  lapply(city_prefixes, {
    function(prefix) render_plot_ui(paste0(prefix, '_plot'), paste0(prefix, '_pop'), paste0(prefix, '_wsf'))
  })
  apply(cities_df, 1, function(city) {
    prefix <- city["prefix"]
    pop_id <- paste0(prefix, "_pop_plot")
    wsf_id <- paste0(prefix, "_wsf_plot")
    output[[pop_id]] <- renderPlot(pop_plot(pop_csv[[prefix]], city['city'])) %>% bindCache(paste0(prefix, '_pop'))
    output[[wsf_id]] <- renderPlot(wsf_plot(wsf_csv[[prefix]], city['city'])) %>% bindCache(paste0(prefix, '_wsf'))
  })

  # Tab: Climate Projections ------------------------------
  # Show variable definition ------------------------
  # output$wb_def <- renderText({
  #   if (str_detect(input$wb, 'cdd')) {
  #     'Consecutive Dry Days (CDD) is defined as the maximum number of consecutive days per year when daily precipitation is under 1mm per day. The indicator lists the length of a single longest period and not the number or frequency of such periods.'
  #   } else if (str_detect(input$wb, 'hwmid')) {
  #     'The heat wave magnitude index daily (HWMId) merges the duration (days) and the intensity (daily maximum temperature) of prolonged extreme temperature events into a single numerical index.'
  #   } else if (str_detect(input$wb, 'WBGT')) {
  #     'Wet Bulb Globe Temperature (WBGT) represents the cooling capacity of the human body through perspiration. This indicator calculates the weighted mean of a function of temperature, relative humidity, and pressure.'
  #   } else if (str_detect(input$wb, 'prcptot')) {
  #     'Annual total precipitation shows total precipitation (rainfall and snowfall) per year in mm. It shows what areas receive the most or least rain but does not show in what season and how intensely precipitation is accumulated over the year, e.g., light rain every day or intense rainfall in a few weeks per year.'
  #   } else if (str_detect(input$wb, 'r95p')) {
  #     'Annual rainfall above the 95th percentile documents millimeters of rainfall accumulated during days with very heavy rainfall, which is defined as as much or more rain than on 95 percent of the days in the reference period from 1981 to 2010.'
  #   }
  # }) %>%
  #   bindCache(input$wb, 'def')

  # Set up basemap -------------------------
  # output$wb_map <- renderLeaflet({
  #   leaflet(wb_cities) %>%
  #     addProviderTiles('Stamen.TonerLite', group = 'Default',
  #                      options = providerTileOptions(zIndex = -3)) %>%
  #     addTiles(group = 'OpenStreetMap',
  #              options = tileOptions(zIndex = -2)) %>%
  #     addProviderTiles('Esri.WorldImagery', group = 'Satellite',
  #                      options = providerTileOptions(zIndex = -1)) %>%
  #     addLayersControl(baseGroups = c('Default', 'OpenStreetMap', 'Satellite'),
  #                      options = layersControlOptions(collapsed = F,
  #                                                     autoZIndex = F)) %>%
  #     addCircleMarkers(fillColor = 'darkgrey', fillOpacity = 0.8, stroke = F, label = ~city, radius = 5)
  # })

  # Toggle layers ----------------------------
  # wb_raster_map <- reactive({
  #   req(input$wb != '')
  #   var <- str_split(input$wb, '_', 2)[[1]][1]
  #   time <- str_split(input$wb, '_', 2)[[1]][2]
  #   wb_rasters[[var]][[time]]
  # }) %>%
  #   bindCache(input$wb)
  #
  # wb_pal <- reactive({
  #   req(input$wb != '')
  #   if (str_detect(input$wb, 'cdd')) {
  #     'viridis'
  #   } else if (str_detect(input$wb, 'hwmid')) {
  #     'Reds'
  #   } else if (str_detect(input$wb, 'WBGT')) {
  #     'Spectral'
  #   } else if (str_detect(input$wb, 'prcptot')) {
  #     'Blues'
  #   } else if (str_detect(input$wb, 'r95p')) {
  #     'BuPu'
  #   }
  # }) %>%
  #   bindCache(input$wb, 'palette')

  # wb_raster_col_reverse <- reactive({
  #   req(input$wb != '')
  #   if (str_detect(input$wb, 'WBGT')) {
  #     T
  #   } else {
  #     F
  #   }
  # }) %>%
  #   bindCache(input$wb, 'reverse')
  #
  # wb_raster_col <- reactive({
  #   req(input$wb != '')
  #   colorNumeric(wb_pal(), values(wb_raster_map()), na.color = 'transparent',
  #                reverse = wb_raster_col_reverse())
  # }) %>%
  #   bindCache(input$wb, 'color')

  # wb_leg_title <- reactive({
  #   req(input$wb != '')
  #   if (str_detect(input$wb, 'cdd')) {
  #     'Longest consecutive dry days'
  #   } else if (str_detect(input$wb, 'hwmid')) {
  #     'Average heat wave magnitude index daily'
  #   } else if (str_detect(input$wb, 'WBGT')) {
  #     'Average wet bulb globe temperature'
  #   } else if (str_detect(input$wb, 'prcptot')) {
  #     'Annual precipitation'
  #   } else if (str_detect(input$wb, 'r95p')) {
  #     'Annual rainfall above 95th percentile'
  #   }
  # }) %>%
  #   bindCache(input$wb, 'leg_title')

  # wb_lab_suffix <- reactive({
  #   req(input$wb != '')
  #   if (str_detect(input$wb, 'WBGT')) {
  #     '°C'
  #   } else if (str_detect(input$wb, 'prcptot') | str_detect(input$wb, 'r95p')) {
  #     'mm'
  #   } else {
  #     ''
  #   }
  # })

  # observe({
  #   req(input$wb != '')
  #   leafletProxy('wb_map') %>%
  #     clearGroup(group = 'wb_raster') %>%
  #     removeControl('wb_leg') %>%
  #     addRasterImage(wb_raster_map(), opacity = input$wb_alpha,
  #                    project = F, group = 'wb_raster', colors = wb_raster_col()) %>%
  #     addLegend(pal = wb_raster_col(), values = values(wb_raster_map()), title = wb_leg_title(), layerId = 'wb_leg',
  #               opacity = 0.9, bins = 3,
  #               labFormat = labelFormat(suffix = wb_lab_suffix()),
  #               position = 'bottomright')
  # })

  # Show plot --------------------------------
#   wb_plot_file <- reactive({
#     paste0('www/wb_plots/', input$wb, '.png')
#   }) %>%
#     bindCache(input$wb, 'plot')
#
#   output$wb_plot <- renderImage({
#     list(src = wb_plot_file(),
#          width = 512)
#   }, deleteFile = F)
}

# Run app ---------------------------------------------------
shinyApp(ui, server)
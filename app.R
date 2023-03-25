library(shinythemes)
library(shinyWidgets)

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
  city_prefix <- case_when(city == 'Sarajevo' ~ 'sa',
                           city == 'Tuzla' ~ 'tu')
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
  
  'Climate-Resilient Cities Workshop in Bosnia and Herzegovina',
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
                    # actionBttn('go_ni', 'Nis', style = 'minimal', color = 'success'),
                    # br(), br(),
                    # actionBttn('go_no', 'Novi Sad', style = 'minimal', color = 'success'),
                    # br(), br(),
                    # actionBttn('go_pr', 'Pristina', style = 'minimal', color = 'success'),
                    # br(), br(),
                    actionBttn('go_sa', 'Sarajevo', style = 'minimal', color = 'success'),
                    br(), br(),
                    actionBttn('go_tu', 'Tuzla', style = 'minimal', color = 'success'))
           )),
  
  # Tabs: City Profiles ---------------------------------------
  navbarMenu('City Profiles',
             # city_tab('Nis'),
             # city_tab('Novi Sad'),
             # city_tab('Pristina'),
             city_tab('Sarajevo'),
             city_tab('Tuzla')
  )
  
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
  observeEvent(input$go_sa, updateNavbarPage(session, 'main', selected = 'Sarajevo'))
  observeEvent(input$go_tu, updateNavbarPage(session, 'main', selected = 'Tuzla'))
  
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
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group)
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
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group)
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
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group) %>%
          removeControl(leg_id)
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
      if (!input[[input_name]]) {
        leafletProxy(map_id) %>%
          clearGroup(group = layer_group) %>%
          removeControl(leg_id)
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
  quake <- function(input_name, lat, lon) {
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
  road <- function(map_id, input_name, feature_var, feature_col) {
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
  output$sa_map <- renderLeaflet(basemap(aoi$sa)) %>% bindCache('sa_basemap')
  output$tu_map <- renderLeaflet(basemap(aoi$tu)) %>% bindCache('tu_basemap')
  
  # Toggle layers ------------------------------
  
  # Sarajevo
  feature_poly('sa_map', 'sa_aoi', aoi$sa)
  # feature_poly('sa_map', 'sa_admin', admin$sa, 'name_en')
  raster_continuous('sa_map', 'sa_sol', sol$sa, sol_col$sa, sol_val$sa, 'Economic hotspots')
  raster_continuous('sa_map', 'sa_soc', soc$sa, soc_col$sa, soc_val$sa, 'Economic hotspots change')
  raster_continuous('sa_map', 'sa_pop', pop$sa, pop_col$sa, pop_val$sa, 'Population')
  raster_discrete('sa_map', 'sa_wsf', wsf$sa, wsf_col$sa, wsf_val$sa, 'Built-up area')
  # raster_continuous('sa_map', 'sa_imperv', imperv$sa, imperv_col$sa, imperv_val$sa, 'Imperviousness', '%')
  raster_continuous('sa_map', 'sa_temp', temp$sa, temp_col$sa, temp_val$sa, 'Temperature', '°C')
  raster_discrete('sa_map', 'sa_green', green$sa, green_col$sa, green_val$sa, 'Green space')
  raster_discrete('sa_map', 'sa_forest', forest$sa, forest_col$sa, forest_val$sa, 'Forest cover')
  raster_discrete('sa_map', 'sa_forest_loss', forest_loss$sa, forest_loss_col$sa, forest_loss_val$sa, 'Forest cover loss')
  raster_continuous('sa_map', 'sa_air', air$sa, air_col$sa, air_val$sa, 'PM2.5 concentration', 'μg/m3')
  raster_discrete('sa_map', 'sa_fu', fu$sa, fu_col$sa, fu_val$sa, 'Fluvial flood')
  raster_discrete('sa_map', 'sa_pu', pu$sa, pu_col$sa, pu_val$sa, 'Pluvial flood')
  feature_point('sa_map', 'sa_fire', fire$sa, fire_icon)
  feature_point('sa_map', 'sa_health', health$sa, health_icon)
  feature_point('sa_map', 'sa_police', police$sa, police_icon)
  feature_point('sa_map', 'sa_schools', schools$sa, schools_icon)
  raster_discrete('sa_map', 'sa_ls', ls$sa, ls_col$sa, ls_val$sa, 'Landslide susceptibility')
  raster_discrete('sa_map', 'sa_lc', lc$sa, lc_col$sa, lc_val$sa, 'Land cover')
  # road('sa_map', 'sa_road', road$sa, road_col$sa)
  quake('sa_quake', '43.894', '18.383')
  observe({
    if (!input$sa_road) {
      leafletProxy('sa_map') %>%
        clearGroup(group = 'sa_road') %>%
        removeControl('sa_road_leg')
    } else {
      leafletProxy('sa_map', data = road_sa) %>%
        clearGroup(group = 'sa_road') %>%
        addPolylines(color = ~road_col$sa(edge_centr), 
                     weight = ~ifelse(highway %in% c('primary', 'trunk', 'motorway'), 3, 1), 
                     opacity = input$sa_road_alpha, label = ~name,
                     group = 'sa_road') %>%
        addLegend(pal = road_col$sa, values = road_sa$edge_centr, title = 'Road criticality',
                  layerId = 'sa_road_leg', opacity = 0.8, bins = 3, position = 'bottomright')
    }
  })
  
  # Tuzla
  feature_poly('tu_map', 'tu_aoi', aoi$tu)
  # feature_poly('tu_map', 'tu_admin', admin$tu, 'name')
  raster_continuous('tu_map', 'tu_sol', sol$tu, sol_col$tu, sol_val$tu, 'Economic hotspots')
  raster_continuous('tu_map', 'tu_soc', soc$tu, soc_col$tu, soc_val$tu, 'Economic hotspots change')
  raster_continuous('tu_map', 'tu_pop', pop$tu, pop_col$tu, pop_val$tu, 'Population')
  raster_discrete('tu_map', 'tu_wsf', wsf$tu, wsf_col$tu, wsf_val$tu, 'Built-up area')
  # raster_continuous('tu_map', 'tu_imperv', imperv$ti, imperv_col$ti, imperv_val$ti, 'Imperviousness', '%')
  raster_continuous('tu_map', 'tu_temp', temp$tu, temp_col$tu, temp_val$tu, 'Temperature', '°C')
  raster_discrete('tu_map', 'tu_green', green$tu, green_col$tu, green_val$tu, 'Green space')
  raster_discrete('tu_map', 'tu_forest', forest$tu, forest_col$tu, forest_val$tu, 'Forest cover')
  raster_discrete('tu_map', 'tu_forest_loss', forest_loss$tu, forest_loss_col$tu, forest_loss_val$tu, 'Forest cover loss')
  raster_continuous('tu_map', 'tu_air', air$tu, air_col$tu, air_val$tu, 'PM2.5 concentration', 'μg/m3')
  raster_discrete('tu_map', 'tu_fu', fu$tu, fu_col$tu, fu_val$tu, 'Fluvial flood')
  raster_discrete('tu_map', 'tu_pu', pu$tu, pu_col$tu, pu_val$tu, 'Pluvial flood')
  feature_point('tu_map', 'tu_fire', fire$tu, fire_icon)
  feature_point('tu_map', 'tu_health', health$tu, health_icon)
  feature_point('tu_map', 'tu_police', police$tu, police_icon)
  feature_point('tu_map', 'tu_schools', schools$tu, schools_icon)
  raster_discrete('tu_map', 'tu_ls', ls$tu, ls_col$tu, ls_val$tu, 'Landslide susceptibility')
  raster_discrete('tu_map', 'tu_lc', lc$tu, lc_col$tu, lc_val$tu, 'Land cover')
  # road('ti_map', 'ti_road', road$ti, road_col$ti)
  quake('tu_quake', '44.542', '18.641')
  observe({
    if (!input$tu_road) {
      leafletProxy('tu_map') %>%
        clearGroup(group = 'tu_road') %>%
        removeControl('tu_road_leg')
    } else {
      leafletProxy('tu_map', data = road_tu) %>%
        clearGroup(group = 'tu_road') %>%
        addPolylines(color = ~road_col$tu(edge_centr), 
                     weight = ~ifelse(highway %in% c('primary', 'trunk', 'motorway'), 3, 1), 
                     opacity = input$tu_road_alpha, label = ~name,
                     group = 'tu_road') %>%
        addLegend(pal = road_col$tu, values = road_tu$edge_centr, title = 'Road criticality',
                  layerId = 'tu_road_leg', opacity = 0.8, bins = 3, position = 'bottomright')
    }
  })
  
  
  # Clear layers -----------------------------
  lapply(city_prefixes, clear_layers)
  # observeEvent(input$sa_clear, {lapply(all_layers$sa, uncheck)})
  
  # Plot --------------------------------
  lapply(city_prefixes, {
    function(prefix) render_plot_ui(paste0(prefix, '_plot'), paste0(prefix, '_pop'), paste0(prefix, '_wsf'))
  })
  # output$ni_pop_plot <- renderPlot(pop_plot(pop_csv$ni, 'Nis')) %>% bindCache('ni_pop')
  # output$no_pop_plot <- renderPlot(pop_plot(pop_csv$no, 'Novi Sad')) %>% bindCache('no_pop')
  # output$pr_pop_plot <- renderPlot(pop_plot(pop_csv$pr, 'Pristina')) %>% bindCache('pr_pop')
  output$sa_pop_plot <- renderPlot(pop_plot(pop_csv$sa, 'Sarajevo')) %>% bindCache('sa_pop')
  output$tu_pop_plot <- renderPlot(pop_plot(pop_csv$tu, 'Tuzla')) %>% bindCache('tu_pop')
  # output$ni_wsf_plot <- renderPlot(wsf_plot(wsf_csv$ni, 'Nis')) %>% bindCache('ni_wsf')
  # output$no_wsf_plot <- renderPlot(wsf_plot(wsf_csv$no, 'Novi Sad')) %>% bindCache('no_wsf')
  # output$pr_wsf_plot <- renderPlot(wsf_plot(wsf_csv$pr, 'Pristina')) %>% bindCache('pr_wsf')
  output$sa_wsf_plot <- renderPlot(wsf_plot(wsf_csv$sa, 'Sarajevo')) %>% bindCache('sa_wsf')
  output$tu_wsf_plot <- renderPlot(wsf_plot(wsf_csv$tu, 'Tuzla')) %>% bindCache('tu_wsf')
  
  
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

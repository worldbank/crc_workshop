# Read data -------------------------------------------------
# city data folders
city_folder <- list(sa = 'Sarajevo', tu = 'Tuzla')

# city prefixes
city_prefixes <- c('sa', 'tu')

# data lists
aoi <- list()
# admin <- list()
road <- list()
fire <- list()
health <- list()
police <- list()
schools <- list()
sol <- list()
soc <- list()
pop <- list()
wsf <- list()
# imperv <- list()
temp <- list()
green <- list()
forest <- list()
forest_loss <- list()
air <- list()
fu <- list()
pu <- list()
ls <- list()
lc <- list()
pop_csv <- list()
wsf_csv <- list()

road_sa <- sf::st_read('Sarajevo/edges', 'edges')
road_tu <- sf::st_read('Tuzla/edges', 'edges')
road <- list(sa = road_sa, tu = road_tu)

# Read data
for (city in city_prefixes) {
  city_name_nospace <- gsub(' ', '_', city_folder[[city]])
  city_name_lower <- tolower(city_name_nospace)
  
  # features
  aoi[[city]] <- sf::st_read(paste0(city_folder[[city]], '/AOI'), paste0(city_name_nospace, '_AOI'))
  # admin[[city]] <- st_read(paste0(city_folder[[city]], '/admin'), paste0(city_name_nospace, '_admin'))
  # road[[city]] <- st_read(paste0(city_folder[[city]], '/edges'), 'edges')
  fire[[city]] <- sf::st_read(paste0(city_folder[[city]], '/osm_infrastructure'), 'osm_fire')
  health[[city]] <- sf::st_read(paste0(city_folder[[city]], '/osm_infrastructure'), 'osm_health')
  police[[city]] <- sf::st_read(paste0(city_folder[[city]], '/osm_infrastructure'), 'osm_police')
  schools[[city]] <- sf::st_read(paste0(city_folder[[city]], '/osm_infrastructure'), 'osm_schools')

  # rasters
  sol[[city]] <- read_raster(city_folder[[city]], paste0(city_name_lower, '_VIIRS_sol_14_21'))
  soc[[city]] <- read_raster(city_folder[[city]], paste0(city_name_lower, '_VIIRS_soc_14_21'))
  pop[[city]] <- read_raster(city_folder[[city]], paste0('01_population_', city_name_lower, '_AOI'))
  wsf[[city]] <- read_raster(city_folder[[city]], paste0('02_urban_change_WSF_4326_', city_name_lower, '_AOI'))
  # imperv[[city]] <- read_raster(city_folder[[city]], paste0('13_imperviousness_', city_name_lower, '_AOI'))
  temp[[city]] <- read_raster(city_folder[[city]], paste0(city_name_lower, '_Summer'))
  green[[city]] <- read_raster(city_folder[[city]], paste0(city_name_lower, '_GreenSpaces'))
  forest[[city]] <- read_raster(city_folder[[city]], paste0(city_name_lower, '_ForestCoverLoss'), 2)
  forest_loss[[city]] <- read_raster(city_folder[[city]], paste0(city_name_lower, '_ForestCoverLoss'), 1)
  air[[city]] <- read_raster(city_folder[[city]], paste0('07_air_quality_', city_name_lower, '_AOI'))
  fu[[city]] <- read_raster(city_folder[[city]], 'class_all_fu')
  pu[[city]] <- read_raster(city_folder[[city]], 'class_all_pu')
  ls[[city]] <- read_raster(city_folder[[city]], paste0('11_landslides_', city_name_lower, '_AOI'))
  lc[[city]] <- read_raster(city_folder[[city]], paste0(city_name_lower, '_lc'))
  
  # tabular
  pop_csv[[city]] <- read_csv(paste0(city_folder[[city]], '/pop.csv'))
  wsf_csv[[city]] <- read_csv(paste0(city_folder[[city]], '/02_urban_change_stats_', city_name_lower, '_AOI.csv'))
}


# Colors, values, and icons ----------------------------------------------
## road
road_val <- list()
road_col <- list()
## SOL
sol_val <- list()
sol_col <- list()
## SOC
soc_val <- list()
soc_col <- list()
## population
pop_val <- list()
pop_col <- list()
## wsf
wsf_val <- list()
wsf_col <- list()
## imperviousness
# imperv_val <- list()
# imperv_col <- list()
## temperature
temp_val <- list()
temp_col <- list()
## green
green_val <- list()
green_col <- list()
## forest
forest_val <- list()
forest_col <- list()
## forest loss
forest_loss_val <- list()
forest_loss_col <- list()
## air quality
air_val <- list()
air_col <- list()
## fluvial
fu_val <- list()
fu_col <- list()
## pluvial
pu_val <- list()
pu_col <- list()
## landslide
ls_val <- list()
ls_col <- list()
## land cover
lc_val <- list()
lc_col <- list()

for (city in city_prefixes) {
  road_val[[city]] <- road[[city]]$edge_centr
  road_col[[city]] <- colorNumeric(c('#bfd2ff', '#789eff', '#006eff', '#993a53', '#e60000'), road_val[[city]])
  sol_val[[city]] <- values(sol[[city]])
  sol_col[[city]] <- colorNumeric('PuOr', sol_val[[city]], na.color = 'transparent', reverse = T)
  soc_val[[city]] <- values(soc[[city]])
  soc_col[[city]] <- colorNumeric('PuOr', soc_val[[city]], na.color = 'transparent', reverse = T)
  pop_val[[city]] <- values(pop[[city]])
  pop_col[[city]] <- colorNumeric('YlOrRd', pop_val[[city]], na.color = 'transparent')
  wsf_val[[city]] <- c('', 'Pre 1985', '1986-1995', '1996-2005', '2006-2015')
  wsf_col[[city]] <- c('transparent', '#ffffbe', '#ffaa00', '#e60000', '#290800')
  # imperv_val[[city]] <- values(imperv[[city]])
  # imperv_col[[city]] <- colorNumeric('RdPu', imperv_val[[city]], na.color = 'transparent')
  temp_val[[city]] <- values(temp[[city]])
  temp_col[[city]] <- colorNumeric('Spectral', temp_val[[city]], na.color = 'transparent', reverse = T)
  green_val[[city]] <- c('', 'Green spaces')
  green_col[[city]] <- c('transparent', '#08ff90')
  forest_val[[city]] <- c('', 'Forest cover')
  forest_col[[city]] <- c('transparent', 'green')
  forest_loss_val[[city]] <- c('', 'Forest cover loss')
  forest_loss_col[[city]] <- c('transparent', 'red')
  air_val[[city]] <- values(air[[city]])
  air_col[[city]] <- colorNumeric('YlOrBr', air_val[[city]], na.color = 'transparent')
  fu_val[[city]] <- c('', 'Fluvial flood zone')
  fu_col[[city]] <- c('transparent', '#446589')
  pu_val[[city]] <- c('', 'Pluvial flood zone')
  pu_col[[city]] <- c('transparent', '#446589')
  ls_val[[city]] <- c('', 'Very low', 'Low', 'Medium', 'High', 'Very high')
  ls_col[[city]] <- c('transparent', '#edf8fb', '#b3cde3', '#8c96c6', '#8856a7', '#810f7c')
  lc_val[[city]] <- c('Tree cover', 'Grassland', 'Cropland', 'Built-up', 
                      'Bare', 'Water', 'Herbaceous wetland')
  lc_col[[city]] <- c('#397e48', '#88af52', '#e49634', '#c4281b',
                      '#a59b8f', '#429be4', '#7c85c9')
}
lc_val$pr <- c('', 'Tree cover', 'Grassland', 'Cropland', 'Built-up',
               'Bare', 'Herbaceous wetland')
lc_col$pr <- c('transparent', '#397e48', '#88af52', '#e49634', '#c4281b',
                    '#a59b8f', '#7c85c9')


# Layers list ------------------------------------
all_layers <- list()
all_layers_suffix <- c('_aoi', #'_admin', 
                       '_sol', '_soc', '_pop', '_wsf', #'_imperv', 
                       '_temp', '_green', '_forest', '_forest_loss', '_air', '_fu', '_pu',
                       '_fire', '_health', '_police', '_schools', '_ls', '_quake', '_road', '_lc')

for (city in city_prefixes) {
  all_layers[[city]] <- paste0(city, all_layers_suffix)
}

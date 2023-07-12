# Read data -------------------------------------------------
# city data folders
# USER INPUT: List all cities to be included
cities <- list(
  'Abidjan, Côte d\'Ivoire',
  'Addis Ababa, Ethiopia',
  'Banjul, Gambia',
  'Brazzaville, Republic of Congo',
  'Buffalo City, South Africa',
  'Bujumbura, Burundi',
  'Kampala, Uganda',
  'Kigali, Rwanda',
  'Kisumu, Kenya',
  'Lagos, Nigeria',
  'Libreville, Gabon',
  'Mombasa, Kenya',
  'Nairobi, Kenya',
  'Ngaoundere, Cameroon',
  'Port Louis, Mauritius',
  'Victoria, Seychelles'
)

# USER CHECK: Verify total count of cities is accurate
length(cities)

cities_df <- data.frame(
  city = str_extract(cities, "^[^,]*",),
  country = str_extract(cities, "[^,]*$",),
  city_and_country = unlist(cities),
  prefix = substr(cities, 1, 3) %>% tolower()) %>%
  mutate(string = str_replace_all(city, " ", "_") %>% tolower())

# Create list of 
# city_prefixes <- substr(cities, 1, 3) %>% tolower()
city_prefixes <- cities_df$prefix
if (length(city_prefixes) != length(unique(city_prefixes))) stop("Prefixes are not unique.")

# city_folders <- cities %>% 
#   str_extract("^[^,]*",) %>%
#   setNames(city_prefixes) %>%
#   as.list()
city_folders <- cities_df$city %>% setNames(cities_df$prefix)

# data lists
aoi <- list()
coords <- list()
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
road <- lapply(city_folders, function(city) {
  dir <- paste0(city, '/edges')
  sf::st_read(dir, 'edges')
})

# Function for reading rasters with fuzzy names
fuzzy_read <- function(city, fuzzy_string) {
  file <- list.files(paste0(city_folder)) %>% subset(str_detect(., fuzzy_string)) %>%
    str_extract("^[^\\.]*") %>% unique()
  if (length(file) > 1) warning(paste("Too many", fuzzy_string, "files in", city_folder))
  if (length(file) < 1) warning(paste("No", fuzzy_string, "file in", city_folder))
  if (length(file) == 1) {
    raster <- suppressMessages(read_raster(city_folder, file))
    return(raster)
  } else {
    return(NA)
  }
}

# Read data
# lapply(city_prefixes, function(city) {
for (city in city_prefixes) {
  
  city_folder <- city_folders[[city]]
  city_name_nospace <- gsub(' ', '_', city_folder)
  city_name_lower <- tolower(city_name_nospace)

  # features
  aoi[[city]] <- sf::st_read(paste0(city_folder, '/AOI'), paste0(city_name_nospace, '_AOI')) %>%
    sf::st_transform(crs = 4326)
  coords[[city]] <- aoi[[city]] %>% st_geometry() %>% st_centroid() %>%
    st_coordinates() %>% as.vector() %>% round(3) %>% as.character() %>% setNames(c("long", "lat"))
  # admin[[city]] <- st_read(paste0(city_folder, '/admin'), paste0(city_name_nospace, '_admin'))
  # road[[city]] <- st_read(paste0(city_folder, '/edges'), 'edges') # Already read this above
  if (file.exists(paste0(city_folder, '/osm_infrastructure/osm_fire.shp'))) fire[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_fire')
  health[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_health')
  police[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_police')
  schools[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_schools')
  
  # rasters
  sol[[city]] <- fuzzy_read(city, "avg_rad_sum")
  soc[[city]] <- fuzzy_read(city, "linfit")
  pop[[city]] <- fuzzy_read(city, "01_population")
  wsf[[city]] <- fuzzy_read(city, "WSFevolution_reclass")
  # imperv[[city]] <- read_raster(city_folder, paste0('13_imperviousness_', city_name_lower, '_AOI'))
  temp[[city]] <- fuzzy_read(city, "_Summer")
  green[[city]] <- fuzzy_read(city, "GreenSpaces")
  forest[[city]] <- fuzzy_read(city, "_CurrentForestCover")
  forest_loss[[city]] <- fuzzy_read(city, "_Deforestation")
  air[[city]] <- fuzzy_read(city, "air_quality")
  fu[[city]] <- fuzzy_read(city, "FU_1000")
  pu[[city]] <- fuzzy_read(city, "PU_1000")
  ls[[city]] <- fuzzy_read(city, "landslide")
  lc[[city]] <- fuzzy_read(city, "03_landcover")
    
  # # tabular
  pop_csv[[city]] <- read_csv(paste0(city_folder, '/pop.csv'))
  wsf_csv[[city]] <- read_csv(paste0(city_folder, '/', city_name_nospace, '_built_up_stats.csv'))
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
  sol_val[[city]] <- if (length(sol[[city]]) == 1) 0 else values(sol[[city]])
  sol_col[[city]] <- colorNumeric('PuOr', sol_val[[city]], na.color = 'transparent', reverse = T)
  soc_val[[city]] <- if (length(soc[[city]]) == 1) 0 else values(soc[[city]])
  soc_col[[city]] <- colorNumeric('PuOr', soc_val[[city]], na.color = 'transparent', reverse = T)
  pop_val[[city]] <- if (length(pop[[city]]) == 1) 0 else values(pop[[city]])
  pop_col[[city]] <- colorNumeric('YlOrRd', pop_val[[city]], na.color = 'transparent')
  wsf_val[[city]] <- c('', 'Pre 1985', '1986-1995', '1996-2005', '2006-2015')
  wsf_col[[city]] <- c('transparent', '#ffffbe', '#ffaa00', '#e60000', '#290800')
  # imperv_val[[city]] <- values(imperv[[city]])
  # imperv_col[[city]] <- colorNumeric('RdPu', imperv_val[[city]], na.color = 'transparent')
  temp_val[[city]] <- if (length(temp[[city]]) == 1) 0 else values(temp[[city]])
  temp_col[[city]] <- colorNumeric('Spectral', temp_val[[city]], na.color = 'transparent', reverse = T)
  green_val[[city]] <- c('', 'Green spaces')
  green_col[[city]] <- c('transparent', '#08ff90')
  forest_val[[city]] <- c('', 'Forest cover')
  forest_col[[city]] <- c('transparent', 'green')
  forest_loss_val[[city]] <- c('', 'Forest cover loss')
  forest_loss_col[[city]] <- c('transparent', 'red')
  air_val[[city]] <- if (length(air[[city]]) == 1) 0 else values(air[[city]])
  air_col[[city]] <- colorNumeric('YlOrBr', air_val[[city]], na.color = 'transparent')
  fu_val[[city]] <- c('', 'Fluvial flood zone')
  fu_col[[city]] <- c('transparent', '#446589')
  pu_val[[city]] <- c('', 'Pluvial flood zone')
  pu_col[[city]] <- c('transparent', '#446589')
  ls_val[[city]] <- c('', 'Very low', 'Low', 'Medium', 'High', 'Very high')
  ls_col[[city]] <- c('transparent', '#edf8fb', '#b3cde3', '#8c96c6', '#8856a7', '#810f7c')
  lc_val[[city]] <- c(
    # 'No data' = 0,
    'Tree cover' = 10,
    'Shrubland' = 20,
    'Grassland' = 30,
    'Cropland' = 40,
    'Built-up' = 50,
    'Bare/sparse vegetation' = 60,
    'Snow and ice' = 70,
    'Permanent water bodies' = 80,
    'Herbaceous wetland' = 90,
    'Mangroves' = 95)
  lc_col[[city]] <- colorFactor(
    palette = c(
      '0'  = 'transparent', # 'No data'
      '10' = '#277242', # 'Tree cover'
      '20' = '#DEBA59', # 'Shrubland'
      '30' = '#79A54F', # 'Grassland'
      '40' = '#E88B39', # 'Cropland'
      '50' = '#C62421', # 'Built-up'
      '60' = '#9C9085', # 'Bare/sparse vegetation
      '70' = '#DDDDDD', # Snow and ice
      '80' = '#2491D7', # 'Permanent water bodies'
      '90' = '#707CBA', # 'Herbaceous wetland'
      '95' = '#00C86E'), # Not sure what class this should be, maybe mangroves?
    levels = c(10*0:9, 95),
    na.color = '#00000000')
}

# Layers list ------------------------------------
all_layers <- list()
all_layers_suffix <- c('_aoi', #'_admin', 
                       '_sol', '_soc', '_pop', '_wsf', #'_imperv', 
                       '_temp', '_green', '_forest', '_forest_loss', '_air', '_fu', '_pu',
                       '_fire', '_health', '_police', '_schools', '_ls', '_quake', '_road', '_lc')

for (city in city_prefixes) {
  all_layers[[city]] <- paste0(city, all_layers_suffix)
}
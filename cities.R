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
    return(read_raster(city_folder, file))
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
  lc_val[[city]] <- c('', 'Tree cover', 'Grassland', 'Cropland', 'Built-up', 
                      'Bare', 'Water', 'Herbaceous wetland')
  lc_col[[city]] <- c('transparent', '#397e48', '#88af52', '#e49634', '#c4281b',
                                     '#a59b8f', '#429be4', '#7c85c9')
  # Colors from City Scan
  # lc_col[[city]] <- colorFactor(palette = c(
  #   '0' = "transparent",
  #   '10' = "#4B7C4E",
  #   '20' = "#DAC36A",
  #   '30' = "#90AE5E",
  #   '40' = "#D99949",
  #   '50' = "#B43728",
  #   '60' = "#A39B91",
  #   '70' = "#E1E1E1",
  #   '80' = "#5B99DA",
  #   '90' = "#7F86C0",
  #   '100' = "#6ECC7E",
  #   '110' = "#F6E6A9"), levels = 10*0:9)
}
# lc_val$pr <- c(0 = '', 'Tree cover', 'Grassland', 'Cropland', 'Built-up',
#                'Bare', 'Herbaceous wetland')
# lc_col$pr <- c('transparent', '#397e48', '#88af52', '#e49634', '#c4281b',
#                     '#a59b8f', '#7c85c9')


# Layers list ------------------------------------
all_layers <- list()
all_layers_suffix <- c('_aoi', #'_admin', 
                       '_sol', '_soc', '_pop', '_wsf', #'_imperv', 
                       '_temp', '_green', '_forest', '_forest_loss', '_air', '_fu', '_pu',
                       '_fire', '_health', '_police', '_schools', '_ls', '_quake', '_road', '_lc')

for (city in city_prefixes) {
  all_layers[[city]] <- paste0(city, all_layers_suffix)
}


# This is what the fuzzy_read function is replicating
# # features
# aoi[[city]] <- sf::st_read(paste0(city_folder, '/AOI'), paste0(city_name_nospace, '_AOI'))
# # admin[[city]] <- st_read(paste0(city_folder, '/admin'), paste0(city_name_nospace, '_admin'))
# # road[[city]] <- st_read(paste0(city_folder, '/edges'), 'edges')
# if (file.exists(paste0(city_folder, '/osm_infrastructure/osm_fire.shp'))) fire[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_fire')
# health[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_health')
# police[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_police')
# schools[[city]] <- sf::st_read(paste0(city_folder, '/osm_infrastructure'), 'osm_schools')
# 
# # rasters
# sol_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "avg_rad_sum")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(sol_file) > 1) warning(paste("Too many sol files in", city_folder))
# if (length(sol_file) < 1) warning(paste("No sol file in", city_folder))
# if (length(sol_file) == 1) {
#   sol[[city]] <- read_raster(city_folder, sol_file) 
#   } else {
#     sol[[city]] <- NA
#   }
# soc_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "linfit")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(soc_file) > 1) warning(paste("Too many soc files in", city_folder))
# if (length(soc_file) < 1) warning(paste("No soc file in", city_folder))
# if (length(soc_file) == 1) {
#   soc[[city]] <- read_raster(city_folder, soc_file) 
#   } else {
#     soc[[city]] <- NA
#   }
# pop_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "01_population")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(pop_file) > 1) warning(paste("Too many pop files in", city_folder))
# if (length(pop_file) < 1) warning(paste("No pop file in", city_folder))
# if (length(pop_file) == 1) {
#   pop[[city]] <- read_raster(city_folder, pop_file) 
#   } else {
#     pop[[city]] <- NA
#   }
# wsf_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "WSFevolution_reclass")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(wsf_file) > 1) warning(paste("Too many wsf files in", city_folder))
# if (length(wsf_file) < 1) warning(paste("No wsf file in", city_folder))
# if (length(wsf_file) == 1) {
#   wsf[[city]] <- read_raster(city_folder, wsf_file) 
#   } else {
#     wsf[[city]] <- NA
#   }
# # imperv[[city]] <- read_raster(city_folder, paste0('13_imperviousness_', city_name_lower, '_AOI'))
# temp_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "_Summer")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(temp_file) > 1) warning(paste("Too many temp files in", city_folder))
# if (length(temp_file) < 1) warning(paste("No temp file in", city_folder))
# if (length(temp_file) == 1) {
#   temp[[city]] <- read_raster(city_folder, temp_file) 
#   } else {
#     temp[[city]] <- NA
#   }
# green_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "GreenSpaces")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(green_file) > 1) warning(paste("Too many green file in", city_folder))
# if (length(green_file) < 1) warning(paste("No green file in", city_folder))
# if (length(green_file) == 1) {
#   green[[city]] <- read_raster(city_folder, green_file) 
#   } else {
#     green[[city]] <- NA
#   }
# forest_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "_CurrentForestCover")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(forest_file) > 1) warning(paste("Too many forest files in", city_folder))
# if (length(forest_file) < 1) warning(paste("No many forest file in", city_folder))
# if (length(forest_file) == 1) {
#   forest[[city]] <- read_raster(city_folder, forest_file) 
#   } else {
#     forest[[city]] <- NA
#   }
# forest_loss_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "_Deforestation")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(forest_loss_file) > 1) warning(paste("Too many forest_loss files in", city_folder))
# if (length(forest_loss_file) < 1) warning(paste("No forest_loss file in", city_folder))
# if (length(forest_loss_file) == 1) {
#   forest_loss[[city]] <- read_raster(city_folder, forest_loss_file) 
#   } else {
#     forest_loss[[city]] <- NA
#   }
# air_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "air_quality")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(air_file) > 1) warning(paste("Too many air files in", city_folder))
# if (length(air_file) < 1) warning(paste("No air file in", city_folder))
# if (length(air_file) == 1) {
#   air[[city]] <- read_raster(city_folder, air_file) 
#   } else {
#     air[[city]] <- NA
#   }
# fu_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "FU_1000")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(fu_file) > 1) warning(paste("Too many fu files in", city_folder))
# if (length(fu_file) < 1) warning(paste("No fu file in", city_folder))
# if (length(fu_file) == 1) {
#   fu[[city]] <- read_raster(city_folder, fu_file) 
#   } else {
#     fu[[city]] <- NA
#   }
# pu_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "PU_1000")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(pu_file) > 1) warning(paste("Too many pu files in", city_folder))
# if (length(pu_file) < 1) warning(paste("No pu file in", city_folder))
# if (length(pu_file) == 1) {
#   pu[[city]] <- read_raster(city_folder, pu_file) 
#   } else {
#     pu[[city]] <- NA
#   }
# landslide_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "landslide")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(landslide_file) > 1) warning(paste("Too many landslide files in", city_folder))
# if (length(landslide_file) < 1) warning(paste("No landslide file in", city_folder))
# if (length(landslide_file) == 1) {
#   ls[[city]] <- read_raster(city_folder, landslide_file) 
#   } else {
#     ls[[city]] <- NA
#   }
# lc_file <- list.files(paste0(city_folder)) %>% subset(str_detect(., "03_landcover")) %>%
#   str_extract("^[^\\.]*") %>% unique()
# if (length(lc_file) > 1) warning(paste("Too many lc files in", city_folder))
# if (length(lc_file) < 1) warning(paste("No lc file in", city_folder))
# if (length(lc_file) == 1) {
#   lc[[city]] <- read_raster(city_folder, lc_file)
#   } else {
#     lc[[city]] <- NA
#   }
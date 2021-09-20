pacman::p_load(rio,
               tidyverse,
               magrittr,
               sf,
               tmap,
               raster,
               fasterize,
               tictoc,
               future,
               furrr,
               haven,
               stargazer,
               fixest,
               zoo,
               knitr,
               magick,
               stringi,
               broom,
               hrbrthemes,
               readr,
               nngeo)

# ---------------------------------------------------------
# Load master data
# ---------------------------------------------------------
# ----- Capital data
caps <- rio::import("1_temporary data/capital_data.dta") 

capitals <- caps %>%  
  filter(!is.na(cap_lat)) %>% 
  st_as_sf(.,
           coords = c("cap_lon", "cap_lat"),
           crs = 4326) %>% 
  dplyr::mutate(row_id = row_number())

continent_vector <- as.character(c("Antarctica"))

# ----- load map data
ne <- st_read(dsn = "NE/ne_10m_admin_0_map_units",
              layer = "ne_10m_admin_0_map_units",
              crs = 4326) %>% 
  filter(CONTINENT %!in% continent_vector) %>% 
  dplyr::select(c(SOVEREIGNT,
                  GEOUNIT,
                  SUBUNIT,
                  TYPE,
                  ADMIN,
                  GU_A3,
                  SUBREGION,
                  REGION_WB,
                  REGION_UN,
                  CONTINENT,
                  ADM0_A3,
                  ISO_A3)) %>% 
  dplyr::mutate(poly_id = row_number())

# ---------------------------------------------------------
# Match cities to geographic features
# ---------------------------------------------------------
tic()
geo_data <- st_join(capitals, ne, join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  dplyr::select(c(state, year,
                  SOVEREIGNT,
                  GEOUNIT,
                  SUBUNIT,
                  TYPE,
                  ADMIN,
                  GU_A3,
                  SUBREGION,
                  REGION_WB,
                  REGION_UN,
                  CONTINENT,
                  ADM0_A3,
                  ISO_A3))
toc()

# add geodata to the capital data
caps <- caps %>% 
  tidylog::left_join(.,
                     geo_data,
                     by = c("state", "year"))

names(caps)

# rename variables in capital data
caps <- caps %>% 
  dplyr::rename(cap_SUBREGION = SUBREGION,
                cap_REGION_WB = REGION_WB,
                cap_REGION_UN = REGION_UN,
                cap_CONTINENT = CONTINENT,
                cap_SOVEREIGNT = SOVEREIGNT,
                cap_SUBUNIT = SUBUNIT,
                cap_TYPE = TYPE,
                cap_ADMIN = ADMIN,
                cap_GU_A3 = GU_A3,
                cap_ADM0_A3 = ADM0_A3,
                cap_ISO_A3 = ISO_A3,
                cap_GEOUNIT = GEOUNIT)

# export data
haven::write_dta(data = caps,
                 path = "1_temporary data/capitals.dta",
                 version = 14)

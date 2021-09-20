pacman::p_load(tmap,
               rgdal,
               raster,
               geosphere,
               rgeos,
               readr,
               dplyr,
               sf,
               lwgeom,
               nngeo,
               progress,
               tictoc,
               future,
               data.table,
               furrr,
               purrr,
               units,
               smoothr,
               stringr,
               xml2,
               readr,
               haven)

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
# load cities data
cities <- rio::import("2_data in stages/8_city_populations.dta") %>% 
  as.data.frame() %>% 
  mutate_all(na_if,"") %>% # set "" as NA (from stata to R)
  filter(!is.na(longitude) & !is.na(latitude)) %>% # remove cities without coordinates
  dplyr::select(c(city,
                  country,
                  latitude,
                  longitude,
                  year,
                  city_id)) %>% # select needed variables
  distinct(., city_id, .keep_all = T) # keep only unique cities because coordinates are fixed over time

continent_vector <- as.character(c("Antarctica"))

# load map data
ne <- st_read(dsn = "NE/ne_10m_admin_0_map_units",
              layer = "ne_10m_admin_0_map_units",
              crs = 4326) %>% 
  filter(CONTINENT %!in% continent_vector) %>% # remove antarctica
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

tmap_mode("view")
tm_shape(ne) +
  tm_polygons(col = "red", alpha = 0.3)

# ---------------------------------------------------------
# Match cities to geographic features
# ---------------------------------------------------------
# convert to sf (spatial features)
cit_sf <- cities %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>%
  dplyr::mutate(row_id = row_number())

# use st_join to link each city to nearest feature
# it would ideally be possible to use 'exact joins' but because some coastal cities are marginally 
# outside a given polygon, that is not feasible
tic()
geo_data <- st_join(cit_sf, ne, join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  dplyr::select(c(city, country, city_id,
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

# save city-level codings, will be merged onto later
haven::write_dta(data = geo_data,
                 path = "1_temporary data/city_geodata.dta",
                 version = 14)




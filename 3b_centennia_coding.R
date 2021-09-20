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
# define a CRS for the spatial projection
set_crs <- 3035

# define a vector of years: 10yr intervals from 1000 to 2000
year_sec <- seq(1000, 2000, 10)

# ----- Eurasia data
eurasia_union <- st_read(dsn = "5_centennia/eurasia_map_union",
                         layer = "eurasia_map_union",
                         crs = 4326) %>% 
  st_transform(.,
               crs = set_crs)

# ----- Centennia data
centennia <- st_read(dsn = "5_centennia/centennia_full",
                     layer = "centennia_full",
                     crs = 4326) %>% 
  st_transform(.,
               crs = set_crs)

# ----- Cities data
cities_master <- rio::import("2_data in stages/8_city_populations.dta") %>% 
  as.data.frame() %>% 
  mutate_all(na_if,"") %>% # set "" as NA (from stata to R)
  filter(!is.na(longitude) & !is.na(latitude)) %>% # remove cities without coordinates
  dplyr::select(c(city,
                    country,
                    latitude,
                    longitude,
                    year,
                    city_id)) # select needed variables

cities_master <- cities_master %>% 
  dplyr::mutate(id_yr = paste(city_id,
                              year,
                              sep = "_")) # create a city-year indicator


# create a temporary dataset with only unique cities
temp <- cities_master %>% 
  distinct(., city_id, .keep_all = T) %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(.,
               crs = set_crs)


# using the unique dataset, keep only cities in the Eurasian region that Centennia covers
temp <- st_intersection(temp,
                        eurasia_union)


# convert to dataframe, no need for the spatial features anymore
temp <- temp %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
class(temp)

# remove cities from the master data that are not contained in the Eurasian region
cities_master <- cities_master %>% 
  filter(city_id %in% temp$city_id)

# convert master data to spatial dataset
cities <- cities_master %>% 
  st_as_sf(.,
           coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(.,
               crs = set_crs)

# clean up
rm(temp, eurasia_union, cities_master)

# ---------------------------------------------------------
# Split up data
# ---------------------------------------------------------
# keep only maps in the 1000-2000 period
cent_purrr <- centennia %>% 
  filter(year %in% year_sec)

# split it into a list of 101, one for each t (time period, i.e. decade)
cent_list <- split(cent_purrr,
                   f = cent_purrr$year)
rm(cent_purrr)

# do the same for the cities
cit_purrr <- cities %>% 
  filter(year %in% year_sec)
cit_list <- split(cit_purrr,
                  f = cit_purrr$year)
rm(cit_purrr)

# clean up
rm(cities, centennia)

# ---------------------------------------------------------
# Define function: Extract allegience codings
# ---------------------------------------------------------
# define a distance buffer: how far (km) from a polygon is a city allowed to be and still be matched to it?
# we need it to be > 0 because of coastal cities
dist_buffer <- 5

# define a function that matches cities to the digitized maps (= the polygons)
# below, I apply this function to the two lists created above: cent_list + cit_list
# this way, it links the maps from the centennia list from year 1000 (1st element of list) with the corresponding
# cities from the cities list (1st element of list)
# it does so in two iterations. We could do with only the second but that would take considerably longer

func_centennia_coding <- function(cent, cities, state_df1,
                                  cities_temp, state_df2,
                                  state_df) { 
  
  # drop the year variable from the centennia dataset (list element)
  cent <- cent %>% 
    dplyr::select(-year)
  
  # join cities to polygons using exact match
  state_df1 <- st_join(cities, cent, join = st_intersects) %>% 
    as.data.frame() %>% 
    # select necessary variables
    dplyr::select(c(city_id, year, state, id_yr)) %>% 
    # remove unmatched cities
    filter(!is.na(state)) %>%
    # remove duplicates (former error, should be fine now. It was due to errors with the Centennia data)
    distinct(.,
             id_yr,
             .keep_all = T)
  
  # create a temporary dataset with all the cities that were NOT matched above
  cities_temp <- cities %>%
    filter(id_yr %!in% state_df1$id_yr)

  # among these cities, try again to join them to a polygon that is no more than 5km away. E.g. coastal cities
  state_df2 <- st_join(cities_temp, cent, join = st_nn, k = 1, maxdist = dist_buffer*1000,
                         progress = F) %>%
    as.data.frame() %>%
    dplyr::select(c(city_id, year, state, id_yr)) %>%
    distinct(.,
             id_yr,
             .keep_all = T)
  
  # combine the two outputs in one file
  state_df <- rbind(state_df1, state_df2)
  
  # this is our result
  return(state_df)
  
}

# ---------------------------------------------------------
# Apply function
# ---------------------------------------------------------
# define number of cores in computer we want to use. To speed it up, set it to availableCores() - 1
# then, however, close ALL other applications on your computer
no_cores <- availableCores() - 3
no_cores

# start a parallel processing session (much faster than a for loop)
# depending on Mac/Windows you might need to work around with multisession/multicore/multiprocess
# see: https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
# check your activity monitor, there should be several processes running
plan(multisession, workers = no_cores)

# apply the function defined above on the two lists of equal length
# this creates a 3rd list which is our output
tic()
coding_list <- furrr::future_map2(cent_list,
                                  cit_list,
                                  func_centennia_coding,
                                  .progress = T,
                                  .options = furrr_options(seed = TRUE,
                                                           scheduling = Inf))
toc()
future::plan(sequential)

# ---------------------------------------------------------
# Unpack data from list
# ---------------------------------------------------------
coding_data <- bind_rows(coding_list, .id = "year_id") %>% 
  dplyr::select(-year_id) %>% 
  filter(!is.na(state)) %>% # remove cities not matched to a state polygon
  dplyr::mutate(cent = as.character(state)) %>% 
  dplyr::select(c(city_id, year, cent, id_yr))

# check for duplicated observations at the city-year level
any(duplicated(coding_data$id_yr))

# drop city-year variable
coding_data <- coding_data %>% 
  dplyr::select(-id_yr)

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
haven::write_dta(data = coding_data,
                 path = "1_temporary data/centennia_coding_data.dta",
                 version = 14)


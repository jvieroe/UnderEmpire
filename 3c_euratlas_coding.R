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

# ===================================================================
# Prepare Euratlas
# ===================================================================
# ----- Eurasia data
eurasia_union <- st_read(dsn = "5_centennia/eurasia_map_union",
                         layer = "eurasia_map_union",
                         crs = 4326)

# ----- Euratlas data
# define a vector of years: 100yr intervals from 0 to 900
year_sec <- seq(0, 900, 100)

# create an empty list
eura_list <- list()

# load Euratlas data
for (i in seq_along(year_sec)) {
  
  dsn_path <- paste0("Euratlas Nussli/Euratlas Nussli/data/",
                     year_sec[i],
                     "/utf8")
  
  print(dsn_path)
  
  df <- st_read(dsn = dsn_path,
                layer = "sovereign_dependent_states") %>% 
    st_transform(crs = 4326)
  
  # crop with Eurasia data
  df <- st_intersection(df,
                        eurasia_union)
  
  # save it in the empty list
  eura_list[[i]] <- df
  
}

# unpack data from our list
euratlas <- bind_rows(eura_list)
rm(dsn_path, df)

# clean up data
euratlas <- euratlas %>% 
  dplyr::select(-c(FID))

# plot it, try year 900
tmap_mode("view")
tm_shape(euratlas[euratlas$year == 900,]) +
  tm_polygons(col = "red", alpha = 0.3)

rm(eura_list)
rm(eurasia_union)

# ===================================================================
# Code cities using Euratlas
# ===================================================================

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
# define a CRS for the spatial projection
set_crs <- 3035

# ----- Eurasia data
eurasia_union <- st_read(dsn = "5_centennia/eurasia_map_union",
                         layer = "eurasia_map_union",
                         crs = 4326) %>% 
  st_transform(.,
               crs = set_crs)

# transform CRS of euratlas data loaded above
euratlas <- euratlas %>% 
  st_transform(.,
               crs = set_crs)

# load cities data
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

# create city-year indicator
cities_master <- cities_master %>% 
  dplyr::mutate(id_yr = paste(city_id,
                              year,
                              sep = "_"))

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
rm(cities_master, temp, eurasia_union)

# ---------------------------------------------------------
# Split up data
# ---------------------------------------------------------
# define year sequence
year_sec <- seq(0, 900, 100)

# filter euratlas data to that defined period
eura_purrr <- euratlas %>% 
  filter(year %in% year_sec)

# split it into a list
eura_list <- split(eura_purrr,
                   f = eura_purrr$year)
rm(eura_purrr)

# do the same for the cities
cit_purrr <- cities %>% 
  filter(year %in% year_sec)
cit_list <- split(cit_purrr,
                  f = cit_purrr$year)
rm(cit_purrr)

rm(cities, euratlas)

# ---------------------------------------------------------
# Define function: Extract allegience codings
# ---------------------------------------------------------
# define a distance buffer: how far (km) from a polygon is a city allowed to be and still be matched to it?
# we need it to be > 0 because of coastal cities
dist_buffer <- 5

# define a function that matches cities to the digitized maps (= the polygons)
# below, I apply this function to the two lists created above: eura_list + cit_list
# this way, it links the maps from the euratlas list from year 0 (1st element of list) with the corresponding
# cities from the cities list (1st element of list)
# it does so in two iterations. We could do with only the second but that would take considerably longer
func_euratlas_coding <- function(eura, cities, state_df1,
                                 cities_temp, state_df2,
                                 state_df) { 
  
  # drop the year variable from the euratlas dataset (list element)
  eura <- eura %>% 
    dplyr::select(-year)
  
  # join cities to polygons using exact match
  state_df1 <- st_join(cities, eura, join = st_intersects) %>% 
    as.data.frame() %>% 
    # select necessary variables
    dplyr::select(c(city_id, year, id_yr,
                    owner_id, holder_id, short_name, sname_o, lname_o, variants_o, sname_h, lname_h, variants_h)) %>% 
    # remove unmatched cities
    filter(!is.na(short_name)) %>% 
    # remove duplicates (former error, should be fine now)
    distinct(.,
             id_yr,
             .keep_all = T)
  
  # create a temporary dataset with all the cities that were NOT matched above
  cities_temp <- cities %>% 
    filter(id_yr %!in% state_df1$id_yr)
  
  # among these cities, try again to join them to a polygon that is no more than 5km away. E.g. coastal cities
  state_df2 <- st_join(cities_temp, eura, join = st_nn, k = 1, maxdist = dist_buffer*1000,
                       progress = F) %>% 
    as.data.frame() %>%
    dplyr::select(c(city_id, year, id_yr,
                    owner_id, holder_id, short_name, sname_o, lname_o, variants_o, sname_h, lname_h, variants_h)) %>% 
    distinct(.,
             id_yr,
             .keep_all = T)
  
  # combine the two outputs in one file
  state_df <- rbind(state_df1, state_df2)
  
  # this is the output
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
coding_list <- furrr::future_map2(eura_list,
                                  cit_list,
                                  func_euratlas_coding,
                                  .progress = T,
                                  .options = furrr_options(seed = TRUE,
                                                           scheduling = Inf))
toc()
future::plan(sequential)

# ---------------------------------------------------------
# Unpack data
# ---------------------------------------------------------
coding_data <- bind_rows(coding_list, .id = "year_id") %>% 
  dplyr::select(-year_id) %>% 
  filter(!is.na(short_name))

# check for duplicated observations at the city-year level
any(duplicated(coding_data$id_yr))

# drop city-year variable
coding_data <- coding_data %>% 
  dplyr::select(-id_yr)

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
haven::write_dta(data = coding_data,
                 path = "1_temporary data/euratlas_coding_data.dta",
                 version = 14)




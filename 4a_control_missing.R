cities_import <- load_cities_func()

cities <- cities_import %>%  
  mutate_all(na_if,"") %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% # & needs_origins_coding == 0
  dplyr::select(c(city_id, city, country, latitude, longitude, year, city_pop_i,
                  pol_control, pol_control_i,
                  gap_1, gap_2,
                  GEOUNIT, CONTINENT, SUBREGION, REGION_UN, REGION_WB)) %>% 
  #filter(is.na(pol_control_i)) %>% 
  dplyr::mutate(coding_column = "") %>% 
  arrange(country, city, year) %>% 
  filter(SUBREGION != "Eastern Europe" & SUBREGION != "Northern Africa" & SUBREGION != "Northern Europe" & 
           SUBREGION != "Southern Europe" & SUBREGION != "Western Asia" & SUBREGION != "Western Europe" & 
           SUBREGION != "Southern Asia") %>% 
  filter(country != "china") %>% 
  group_by(city_id) %>% 
  mutate(max_pop = max(city_pop_i, na.rm = F)) %>% 
  filter(max_pop > 10000) %>% 
  dplyr::select(-max_pop)

largest_cities <- cities %>%  
  mutate(city = factor(city),
         country = factor(country)) %>%
  group_by(city_id) %>%
  slice(which.max(city_pop_i)) %>% 
  dplyr::select(c(city, country, city_pop_i, city_id)) %>% 
  arrange(country, city, desc(city_pop_i)) %>% 
  group_by(country) %>% 
  mutate(n_cit = n(),
         rank = row_number()) %>% 
  filter(rank <= 25) %>% 
  as.data.frame()

cities_code <- cities[cities$city_id %in% largest_cities$city_id,] %>% 
  as.data.frame() %>% 
  dplyr::select(c(city, country, latitude, longitude, year, pol_control_i)) %>% 
  mutate(pol_control = ifelse(is.na(pol_control_i), "", pol_control_i),
         allegience = "",
         notes = "",
         source = "") %>% 
  dplyr::select(-pol_control_i) %>% 
  arrange(country, city, year)

missing <- cities_code %>% filter(pol_control == "")

countries <- unique(cities_code$country)
countries

library(xlsx)

for (i in 1:length(countries)) {
  
  temp <- cities_code[cities_code$country == countries[i],]
  
  temp <- temp %>% arrange(city, year)
  
  write.xlsx(temp, paste("C:/Users/fmg720/Dropbox/Mille/code_control/code_",countries[i],".xlsx", sep = ""),
             sheetName = "Sheet1",
             col.names = TRUE,
             row.names = FALSE,
             append = TRUE)
  
}



# ==============================================
# Append data
# ==============================================
setwd("C:/Users/fmg720/Dropbox/Mille/code_control")
rm(list=ls())

file_list <- list.files()
file_list

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- rio::import(file)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset  <- rio::import(file)
    dataset <- rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

export_data <- dataset %>% 
  #filter(!is.na(pol_control) & !is.na(allegience)) %>% 
  dplyr::select(c(city, country, year, pol_control, allegience, notes, source))



# 
# setwd("C:/Users/fmg720/Dropbox/Under Empire/Data and code")
# # =====================================
# cities <- rio::import("2_data in stages/9_anatem.dta") %>% 
#   mutate_all(na_if,"") %>% 
#   dplyr::select(c(city, country, latitude, longitude, year, pol_control)) %>% 
#   dplyr::mutate(coding_column = "") %>% 
#   filter(country == "argentina") %>% 
#   mutate(iden = paste(city, year, sep = "_"))
# 
# check <- cities %>% 
#   left_join(., export_data, by = "iden") %>% 
#   filter(!is.na(pol_control.y) & is.na(pol_control.x)) %>% 
#   arrange(city.x, year.x) %>% 
#   dplyr::select(c(city.x, country.x, year.x, pol_control.y))

centennia <- st_read(dsn = "C:/Users/fmg720/Dropbox/Centennia/centennia_clean",
                     layer = "centennia_clean",
                     crs = 4326)

cent <- centennia %>% 
  as.data.frame() %>% 
  dplyr::select(-c(geometry, id)) %>% 
  arrange(state, year) %>% 
  mutate(unit_year = paste(state, year, sep = "_"))

capitals <- rio::import("1_temporary data/centennia_capitals_merge.dta") %>% 
  mutate(unit_year = paste(pol_control, year, sep = "_")) %>% 
  dplyr::select(-year)

cent <- cent %>% 
  left_join(., capitals, by = "unit_year") %>% 
  mutate(pol_control = ifelse(is.na(pol_control), "", pol_control),
         capital = ifelse(is.na(capital), "", capital),
         country = ifelse(is.na(country), "", country),)


# cent2 <- cent %>% 
#   filter(state %!in% capitals$pol_control)
# cent3 <- cent %>% 
#   filter(state %in% capitals$pol_control)
  
library(xlsx)
write.xlsx(cent, "C:/Users/fmg720/Dropbox/Centennia/centennia_capitals.xlsx",
           sheetName = "Sheet1", 
           col.names = TRUE,
           row.names = TRUE,
           append = TRUE)



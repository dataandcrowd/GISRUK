library(openair)
library(tidyverse)
options(warn = -1)

london_meta %>% 
  filter(latitude > 51) %>% 
  ggplot(aes(longitude, latitude, colour = site_type)) +
  geom_point()

london_meta <- importMeta(source = "kcl", all = T) %>% 
  filter(la_id %in% c(1:33),
         is.na(ClosingDate),
         !is.na(latitude))



unique(london_meta$site_type)

london_meta %>% filter(site_type %in% c("Roadside", "Kerbside")) -> london_road
london_meta %>% filter(site_type %in% c("Suburban", "Urban Background", "Industrial")) -> london_back

#importKCL("BG2", year = 2019, pollutant = "no2")

no2_road <- lapply(1:nrow(london_road), function(i){
importKCL(london_road$code[i], year = 2019) 
})

no2_road[lengths(no2_road) != 0] -> no2_road_new


no2_road_new %>% 
  reduce(left_join, by = c("date", "no2", "site", "code"))



df %>% 
  as_tibble
  rename(timestamp = date)
  mutate(date = lubridate::as_date(timestamp),
         hour = lubridate::hour(timestamp), 
         dn = case_when(hour >= 8 & hour <= 17 ~ "Work", 
                        TRUE ~ "Home")) 



no2_back <- lapply(1:nrow(london_back), function(i){
  importKCL(london_back$code[i], year = 2019)
})


no2_road[[20]] %>% mutate(hour = lubridate::hour(date), dn = case_when(hour >= 8 & hour <= 17 ~ "Work", TRUE ~ "Home")) %>% View()


aq %>% 
  mutate(Date = dmy(Date),
         datehms = ymd_hms(paste0(Date, Time)),
         hours = as.character(Time),
         no2 = as.numeric(no2)) %>% 
  select(-pm2.5) -> aq_clean

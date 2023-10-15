#Function to transform metadata to dataframe for Problem 2

transform_metadata_to_df <- function(metadata) {
  metadata[[1]] %>% 
    map(as_tibble) %>%
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = "")) %>% 
    mutate(latestData = as.Date(latestData, tz = "Europe/Berlin")) %>%
    mutate(location = map(location, unlist)) %>% 
    mutate(lat = map_dbl(location, "latLon.lat"), lon = map_dbl(location, "latLon.lon")) %>%
    select(-location)
}

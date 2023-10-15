#Function to transform metadata to dataframe for Problem 2

transform_metadata_to_df <- function(metadata) {
  metadata[[1]] %>% 
    map(as_tibble) %>%
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = "")) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>%
    mutate(location = map(location, unlist)) %>% 
    mutate(lat = map_dbl(location, "latLon.lat"), lon = map_dbl(location, "latLon.lon")) %>%
    select(-location)
}


#Function to return date variable with given offset
to_iso8601 <<- function(dateVariable, offset) {
  dateWithOffset <- dateVariable + days(offset)
  dateWithOffsetISO <- iso8601(dateWithOffset)
  dateZ <- paste(dateWithOffsetISO,"Z", sep ="")
  return(dateZ)
}
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)



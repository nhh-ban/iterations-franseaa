#Creating function for task 4b

vol_qry <- function(id, from, to) {
  q <- glue::glue(
    '{
      trafficData(trafficRegistrationPointId: "!id!") {
        volume {
          byHour(from: "!from!", to: "!to!") {
            edges {
              node {
                from
                to
                total {
                  volumeNumbers {
                    volume
                  }
                }
              }
            }
          }
        }
      }
    }', .open = "!", .close = "!")
  return(q)
}

check <-GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

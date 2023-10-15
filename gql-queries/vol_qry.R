#Creating function for task 4b

vol_qry <- function(id, from, to) {
  q <- glue::glue(
  "{
    trafficData(trafficRegistrationPointId: &id&) {
      volume {
        byHour(from: &from&, to: &to&) {
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
  }", .open = "&", .close = "&")
  return(q)
}

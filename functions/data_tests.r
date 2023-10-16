# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-
  function(df) { #Defining function to test for column names
    
    #Creating list of expected column names from funciton to create dataframe
    expected_colnames <- c("id", "name", "latestData", "lat", "lon") 
    
    #If col. names in dataframe are equal to the expected colnames
    if (all(colnames(df) == expected_colnames) == TRUE) { 
      print("PASS: Data has the correct columns")
    } else{ #If they are not as expeted: FAIL test. 
      print("FAIL: Columns do not match the correct specification")
    }
  }

test_stations_metadata_nrows <-
  function(df) { #Creating function to test for number of rows in dataframe
    
    min_expected_rows <- 5000 #Minimum expected rows in dataframe
    max_expected_rows <- 10000 #Maximum expected rows in dataframe 
    
    #If the dataframe have number of rows in the given interval above. 
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else { #If the number of rows are not in the interval: FAIL test
      print("FAIL: Data has suspiciously many rows")
    }
  }

test_stations_metadata_coltypes <-
  function(df) { #Creating function to test for types in the different col. values
    #Creating vector of the expected column types 
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    #If the col. types are equal to the expected: 
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{ #Else: FAIL test. 
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
test_stations_metadata_nmissing <-
  function(df) { #Creating function to test if there is too much missing values
    max_miss_vals <- 200 #Limit for missing values
    
    #If the number of missing values are below the max given limit
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else { #If above the max limit: FAIL
      print("FAIL: Too many missing values in data set")
    }
  }

test_stations_metadata_latestdata_timezone <-
  function(df) { #Creating function to test for correct timezone
    
    #If the dataframe has the correct given timezone
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else { #Another timezone: FAIL test
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }


test_stations_metadata <- 
  function(df){ #Creating one function that includes all functions above 
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }






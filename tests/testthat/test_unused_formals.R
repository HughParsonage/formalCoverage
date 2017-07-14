context("Unused formals")

test_that("add3 detected", {
  add3 <- function(x, y, z = 0) {
    x + y
  }
  expect_equal(unused_formals(add3), "z")
})

test_that("get_current_weather", {
  get_current_weather <- function(station_name,
                                  latlon = NULL,
                                  raw = FALSE,
                                  emit_latlon_msg = TRUE,
                                  as.data.table = FALSE) {
    if (missing(station_name) && is.null(latlon)) {
      stop("One of 'station_name' or 'latlon' must be provided.")
    }
    
    if (!missing(station_name)) {
      if (!is.null(latlon)) {
        latlon <- NULL
        warning("Both station_name and latlon provided. Ignoring latlon.")
      }
      stopifnot(is.character(station_name),
                length(station_name) == 1)
      
      # CRAN NOTE avoidance
      name <- NULL
      
      station_name <- toupper(station_name)
      
      # If there's an exact match, use it; else, attempt partial match.
      if (station_name %in% JSONurl_latlon_by_station_name[["name"]]) {
        the_station_name <- station_name
      } else {
        likely_stations <- agrep(pattern = station_name,
                                 x = JSONurl_latlon_by_station_name[["name"]],
                                 value = TRUE)
        
        if (length(likely_stations) == 0) {
          stop("No station found.")
        }
        
        the_station_name <- likely_stations[1]
        if (length(likely_stations) > 1) {
          # Likely common use case
          # (otherwise defaults to KURNELL RADAR which does not provide observations)
          if (toupper(station_name) == "SYDNEY" && 'SYDNEY (OBSERVATORY HILL)' %in% likely_stations) {
            likely_stations <- c('SYDNEY (OBSERVATORY HILL)',
                                 setdiff(likely_stations,
                                         'SYDNEY (OBSERVATORY HILL)'))
            the_station_name <- 'SYDNEY (OBSERVATORY HILL)'
          }
          
          warning("Multiple stations match station_name. ",
                  "Using\n\tstation_name = '",
                  the_station_name,
                  "'\n\nDid you mean any of the following?\n",
                  paste0("\tstation_name = '",
                         likely_stations[-1],
                         "'",
                         collapse = "\n"))
        }
      }
      
      json_url <-
        JSONurl_latlon_by_station_name[name == the_station_name][["url"]]
      
    } else {
      # We have established latlon is not NULL
      if (length(latlon) != 2 || !is.numeric(latlon)) {
        stop("latlon must be a length-2 numeric vector.")
      }
      
      lat <- latlon[1]
      lon <- latlon[2]
      
      # CRAN NOTE avoidance: names of JSONurl_latlon_by_station_name
      Lat <- Lon <- NULL
      
      station_nrst_latlon <-
        JSONurl_latlon_by_station_name %>%
        # Lat Lon are in JSON
        .[which.min(haversine_distance(lat, lon, Lat, Lon))]
      
      on.exit(
        message(
          "Using station_name = '",
          station_nrst_latlon$name,
          "', at latitude = ",
          station_nrst_latlon$Lat,
          ", ",
          "longitude = ",
          station_nrst_latlon$Lon
        )
      )
      
      json_url <- station_nrst_latlon[["url"]]
    }
    if (isTRUE(httr::http_error(json_url))) {
      stop("A station was matched but a corresponding JSON file was not found at bom.gov.au.")
    }
    
    observations.json <-
      jsonlite::fromJSON(txt = json_url)
    
    if ("observations" %notin% names(observations.json) ||
        "data" %notin% names(observations.json$observations)) {
      stop("A station was matched but the JSON returned by bom.gov.au was not in expected form.")
    }
    
    # Columns which are meant to be numeric
    double_cols <-
      c("lat",
        "lon",
        "apparent_t",
        "cloud_base_m",
        "cloud_oktas",
        "rain_trace")
    # (i.e. not raw)
    cook <- function(DT, as.DT) {
      if (!is.data.table(DT)) {
        setDT(DT)
      }
      
      DTnoms <- names(DT)
      
      # CRAN NOTE avoidance
      local_date_time_full <- NULL
      if ("local_date_time_full" %chin% DTnoms) {
        DT[, local_date_time_full := as.POSIXct(
          local_date_time_full,
          origin = "1970-1-1",
          format = "%Y%m%d%H%M%OS",
          tz = ""
        )]
      }
      
      aifstime_utc <- NULL
      if ("aifstime_utc" %chin% DTnoms) {
        DT[, aifstime_utc := as.POSIXct(aifstime_utc,
                                        origin = "1970-1-1",
                                        format = "%Y%m%d%H%M%OS",
                                        tz = "GMT")]
      }
      
      for (j in which(DTnoms %chin% double_cols)) {
        set(DT, j = j, value = force_double(DT[[j]]))
      }
      
      if (!as.DT) {
        DT <- as.data.frame(DT)
      }
      
      DT[]
    }
    
    out <-
      observations.json %>%
      use_series("observations") %>%
      use_series("data")
    
    if (as.data.table) {
      setDT(out)
    }
    
    if (raw) {
      return(out)
    } else {
      return(cook(out, as.DT = as.data.table))
    }
  }
  
  expect_equal(unused_formals(get_current_weather), "emit_latlon_msg")
})

test_that("Should work on these functions", {
  dplyr_mutate <- function (.data, ...) {
    mutate_(.data, .dots = lazyeval::lazy_dots(...))
  }
  
  ggplot2_ggplot <- function (data = NULL, mapping = aes(), ..., environment = parent.frame()) {
    UseMethod("ggplot")
  }
  
  expect_null(unused_formals(dplyr_mutate))
  expect_null(unused_formals(ggplot2_ggplot))
})

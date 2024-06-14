## Creating the API connection 

# libraries ----
library(tidyverse)
library(RCurl)
library(jsonlite)

## table of CCAA options

red_regions <- tribble(~name, ~id,
                       "Andalucía", 4,
                       "Aragón", 5,
                       "Cantabria", 6,
                       "Castilla la Mancha", 7,
                       "Castilla y León", 8,
                       "Cataluña", 9,
                       "País Vasco", 10,
                       "Asturias", 11,
                       "Ceuta", 8744,
                       "Melilla", 8745,
                       "Madrid", 13,
                       "Navarra", 14,
                       "Valencia", 15,
                       "Extremadura", 16,
                       "Galicia", 17,
                       "Islas Baleares", 8743,
                       "Islas Canarias", 8742,
                       "La Rioja", 20,
                       "Murcia", 21
)

## save the variables ----

# inspired by Pablo Herrera at
# https://www.energychisquared.com/post/cómo-conseguir-datos-de-esios-con-su-api/

token.key <- "e8b975f02fb9c7865331361679409925547e18e8e86f72166c105eadf5d0c149"
httpheader <- paste0("Authorization: Token token = '", token.key, "'")

## next steps ----

#' issue (a) this API only allows 24 months at one pull for monthly data
#' issue (b) this API only allows one geolocation at one pull 
#' next step (A) look for data to pair with this
#' what is the target variable?
#' what are the expalantory variables? 
#'   i) demographic data for each month and ccaa 
#'  ii) google search data for renewable energy 
#' iii) survey data from pew, gallup \
#' 

## loop through each ccaa

all_tbl.dat <- tibble()
i <-  0
for (i in 1:nrow(red_regions)) {
  y <- red_regions[i, 1]
  x <- red_regions[i, 2]
  url <- paste0("https://apidatos.ree.es/es/datos/demanda/evolucion?",
                "start_date=2021-01-01T00:00&end_date=2022-12-31T23:59&", 
                "time_trunc=month&", 
                "geo_trunc=electric_system&", 
                "geo_limit=ccaa&geo_ids=", 
                x)
  
  rawdata <- getURI(url, httpheader = httpheader) 
  dat <- fromJSON(txt= rawdata)
  
  names <- dat$included$attributes$title
  dat$included$attributes$type
  values <- dat$included$attributes$values
  
  # convert to tibble
  
  tbl.dat <- tribble(~name, ~value, ~p, ~date)
  
  z <- length(dat$included$type) # get number of electricity types 
  for (i in 1:z) {
    name <- names[i]
    d <- values[[i]]
    
    new.data <- d
    new.data$name <- name
    
    tbl.dat = rbind(new.data, tbl.dat )
  }
  
  ## convert date
  
  glimpse(tbl.dat)
  tbl.dat <- tbl.dat |> 
    mutate(date = lubridate::as_date(datetime),
           ccaa = y$name
    )
  
  all_tbl.dat <- rbind(all_tbl.dat, tbl.dat)
    
    Sys.sleep(1)
}

# loop through dates going back to 2014 ---- 

pull_data <- function(start_date, end_date, category = "demanda", widget = "evolucion") {
  
  all_tbl.dat <- tibble()
  i <-  0
  for (i in 1:nrow(red_regions)) {
    
    
    y <- red_regions[i, 1]
    x <- red_regions[i, 2]
    url <- paste0("https://apidatos.ree.es/es/datos/",
                  category, "/", widget, "?", 
                  "start_date=", start_date,
                  "&end_date=", end_date,
                  "&time_trunc=month&", 
                  "geo_trunc=electric_system&", 
                  "geo_limit=ccaa&geo_ids=", x
            )
    
    rawdata <- getURI(url, httpheader = httpheader) 
    dat <- fromJSON(txt= rawdata)
    
    names <- dat$included$attributes$title
    dat$included$attributes$type
    values <- dat$included$attributes$values
    
    # convert to tibble ----
    
    tbl.dat <- tribble(~name, ~value, ~p, ~date)
    
    z <- length(dat$included$type) # get number of electricity types 
    for (i in 1:z) {
      name <- names[i]
      d <- values[[i]]
      
      new.data <- d
      new.data$name <- name
      
      tbl.dat = rbind(new.data, tbl.dat )
    }
    
    ## convert date
    
    glimpse(tbl.dat)
    tbl.dat <- tbl.dat |> 
      mutate(date = lubridate::as_date(datetime),
             ccaa = y$name
      )
    
    all_tbl.dat <- rbind(all_tbl.dat, tbl.dat)
    
    Sys.sleep(1)
  }
  
  return(all_tbl.dat)
}

# PULL ALL YEARS: ----

## generation done before ----
Y14.15 <- pull_data(start_date = "2014-01-01T00:00", end_date = "2015-12-31T23:59")
Y16.17 <- pull_data(start_date = "2016-01-01T00:00", end_date = "2017-12-31T23:59")
Y18.19 <- pull_data(start_date = "2018-01-01T00:00", end_date = "2019-12-31T23:59")
Y20.21 <- pull_data(start_date = "2020-01-01T00:00", end_date = "2021-12-31T23:59")
Y22.23 <- pull_data(start_date = "2022-01-01T00:00", end_date = "2023-12-31T23:59")

wind_data <- rbind(Y14.15, Y16.17, Y18.19, Y20.21, Y22.23) |> 
  filter(name == "Eólica" & name == "Generación total")

red_data <- rbind(Y14.15, Y16.17, Y18.19, Y20.21, Y22.23) |> 
  filter(name == "Eólica" | name == "Generación total")

write_csv(red_data, file = "red_data.csv")

## consumption 
Y14.15 <- pull_data(start_date = "2014-01-01T00:00", end_date = "2015-12-31T23:59")
Y16.17 <- pull_data(start_date = "2016-01-01T00:00", end_date = "2017-12-31T23:59")
Y18.19 <- pull_data(start_date = "2018-01-01T00:00", end_date = "2019-12-31T23:59")
Y20.21 <- pull_data(start_date = "2020-01-01T00:00", end_date = "2021-12-31T23:59")
Y22.23 <- pull_data(start_date = "2022-01-01T00:00", end_date = "2023-12-31T23:59")


consumo_data <- rbind(Y14.15, Y16.17, Y18.19, Y20.21, Y22.23) 

write_csv(consumo_data, file = "consumo_data.csv")

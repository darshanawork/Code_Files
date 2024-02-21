# Author: Darshana Anandan

# Guided Project: New York Solar Resource Data

# Description: Extracting the solar resource data for New York City in JSON 
##  format and converting it into a dataframe.

# Link to API documentation: 
## https://developer.nrel.gov/docs/solar/solar-resource-v1/

# ==============================================================================
# Clean the environment

rm(list = ls())

# ==============================================================================
# Import libraries

# install.packages("httr")
# install.packages("dplyr")
library("httr")
library("dplyr")

# ==============================================================================
# Setting up the parameters to query the API
## API URL
api_url <- "https://developer.nrel.gov/api/solar/solar_resource/v1"

## Use the link(https://api.data.gov/signup/) to generate key
key <- ""

## The required parameters to request NYC solar data
parameters_list <- list(format = "json", api_key = key, lat = 41, lon = -75 )

# ==============================================================================
# Extracting data

## API requests
response <- GET(api_url, query = parameters_list)
response

## Checking the status of the request
status <- status_code(response)
status

## Checking the API response format
response_type <- http_type(response)
response_type

## Extracting content
content <- content(response, "text")
content

## Parsing API response content to R object
json_lists <- jsonlite::fromJSON(content)
str(json_lists)

# ==============================================================================
# Building a dataframe from a complex list
# Approach 1

## Extracting key variables form the complex list
outputs <- json_lists$outputs
avg_dni <- json_lists$outputs$avg_dni$monthly
avg_ghi <- json_lists$outputs$avg_ghi$monthly
avg_lat_tilt <- json_lists$outputs$avg_lat_tilt$monthly 

## Combining the monthly vectors into a dataframe
dataframe_1 <- tibble::tibble("month" = month.abb,
                            "avg_dni" = avg_dni,
                            "avg_ghi" = avg_ghi,
                            "avg_lat_tilt" = avg_lat_tilt)
dataframe_1

# Approach 2

# Simplifying the list
outputs_list <- unlist(outputs)
outputs_list

## Restructuring the new list into matrix
outputs_matrix <- matrix(outputs_list, nrow = 13)
outputs_matrix

## Removing the annual values from the matrix
outputs_matrix <- outputs_matrix[-1,]
outputs_matrix

## Converting the matrix into a dataframe
dataframe_2 <- as.data.frame(outputs_matrix)
dataframe_2

# ==============================================================================
# Creating a custom function to extract data

nrel_api_json_get_df <- function(endpoint, queries = list()) {
  
  ## Preparing the URL
  url <- modify_url("https://developer.nrel.gov", path = endpoint)
  
  ## API requests
  response <- GET(url, query = queries)
  
  ## Tracking errors
  if (http_error(response)) {
    print(status_code(response))
    print(http_status(response))
    stop("Something went wrong", call. = FALSE)
  }
  
  if (http_type(response) != "application/json") {
    stop("API did not return JSON", call. = FALSE)
  }
  
  ## Extracting content
  json_text <- content(response, "text")
  
  ## Converting content into Dataframe
  pre_data <- jsonlite::fromJSON(json_text)
  dataframe <- tibble::tibble(
    "month" = month.abb,
    "avg_dni" = as.numeric(pre_data$outputs$avg_dni$monthly),
    "avg_ghi" = as.numeric(pre_data$outputs$avg_ghi$monthly),
    "avg_lat_tilt" = as.numeric(pre_data$outputs$avg_lat_tilt$monthly))
  
  ## Return the dataframe  
  dataframe
}

## Using the custom built function to extract
dataframe_3 <- nrel_api_json_get_df("api/solar/solar_resource/v1.json", 
                                    parameters_list)
dataframe_3

# ==============================================================================
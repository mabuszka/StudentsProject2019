#setwd("~/R/StudentsProject2019/KonopackaPiotrowski")

#load source files where functions are defined. load_sources is defined in data_functions.R
source("data_functions.R")
load_sources(".", "functions")

#load libraries that may be useful
libs <- list("conflicted", "dplyr", "lubridate", "tidyr", "stringr", "data.table", "ggplot2")
load_libraries(libs)

#load data; WHY head(data, 3) inside load_as_dataframe doesn't work? 
data <- load_as_dataframe("data/athlete_events.csv")

#end of script
cat("OK")

  


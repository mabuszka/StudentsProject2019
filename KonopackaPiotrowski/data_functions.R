#needs data.table library. Loads data and removes duplicated rows.
load_as_dataframe <- function(path){
  data <- fread(file = path, 
                data.table = FALSE,
                blank.lines.skip = TRUE,
                check.names = TRUE)
#remove duplicated rows
  rows1 <- dim(data)[1]
  data <- distinct(data)
  rows2 <- dim(data)[1]
  cat("Removed duplicated rows:", rows1 - rows2, "\n")
  head(data, 3)
  return (data)
}

#installs all listed libraries (and required packages) at once
load_libraries <- function(libs){
  install.packages("easypackages")
  library("easypackages")
  packages(libs)
  libraries(libs)
}

#loads all source files which names end with given word from path directory
load_sources <- function(path, word){
  library(tidyr)
  files <- list.files(path = path,
                      include.dirs = FALSE,
                      full.names = FALSE)
  inds <- ends_with(match = "functions.R", vars = files)
  good <- files[inds]
  sapply(good, source)
}


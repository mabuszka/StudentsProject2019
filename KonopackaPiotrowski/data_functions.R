#needs data.table library
load_as_dataframe <- function(path){
  data <- fread(file = path, 
                data.table = FALSE,
                blank.lines.skip = TRUE,
                check.names = TRUE)
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
  files <- list.files(path = path,
                      include.dirs = FALSE,
                      full.names = FALSE)
  inds <- ends_with(match = "functions.R", vars = files)
  good <- files[inds]
  sapply(good, source)
}
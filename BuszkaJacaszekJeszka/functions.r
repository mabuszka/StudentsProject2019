library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)

##returns data_frame that contain all files, which name started with "StreamingHistory".
Streaming_History_df <- function(folder_path){
  file_paths<- list.files(folder_path,"StreamingHistory")
  read_files <- lapply(file_paths, jsonlite::fromJSON)
  bind_rows(read_files)
}

### functions to be used on streaming_history 

##returns streaming_history with changed names of the columns for consistency
names_change <- function(streaming_history, column_names){
  names(streaming_history) <- column_names
  streaming_history
}

##returns streaming_history with end_time column changed from character to date
end_time_as_date <- function(streaming_history) {
  streaming_history[["end_time"]] <- ymd_hm(streaming_history[["end_time"]])
  streaming_history
  
}

##returns streaming_history with the ms_played changed into duration
##must be used after end_time_as_date
ms_played_ <- function(streaming_history){
  streaming_history[["ms_played"]] <- dmilliseconds(streaming_history[["ms_played"]])
  streaming_history
}

##returns streaming_history with added start_time column using end_time and ms_played
##must be used after ms_played_as_duration
add_start_time <- function(streaming_history){

    start_time <- streaming_history[["end_time"]] - streaming_history[["ms_played"]]
    streaming_history <- cbind(streaming_history, start_time)
    streaming_history

}

add_start_time <- function(streaming_history){
  
  start_time <- streaming_history[["end_time"]] - streaming_history[["ms_played"]]
  streaming_history <- cbind(streaming_history, start_time)
  streaming_history
  
}
add_start_time(x)



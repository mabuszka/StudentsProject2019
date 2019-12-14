library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
##returns dataframe with streaming history data from spotify
Streaming_History_df <- function(folder_path){
  file_paths<- list.files(folder_path,"StreamingHistory")
  read_files <- lapply(file_paths, jsonlite::fromJSON)
  bind_rows(read_files)
}

##changes names 
names_change <- function(streaming_history, column_names = c("end_time", "artist_name", "track_name", "s_played")){
  names(streaming_history) <- column_names
  streaming_history
}

##returns streaming history with end time changed to date
#mutate(streaming_history, end_time = ymd_hm(end_time))

##returns streaming_history with end_time column changed from character to date
#mutate(streaming_history, s_played = dmilliseconds((s_played)))

##returns streaming_history with added start_time column using end_time and ms_played
add_start_time <- function(streaming_history){
  
  start_time <- streaming_history[["end_time"]] - streaming_history[["s_played"]]
  streaming_history <- cbind(streaming_history, start_time)
  streaming_history
  
}

##returns streaming history with added "skipped" column [true or false]
add_skipped <- function(streaming_history){
  skipped <- (streaming_history[["s_played"]] < duration(10, "seconds"))
  streaming_history <- cbind(streaming_history, skipped)
  streaming_history
}

##returns streaming history with added weekdays column
add_weekday <- function(streaming_history){
  weekday <- wday(streaming_history[["start_time"]], label = TRUE)
  streaming_history <- cbind(streaming_history, weekday)
  streaming_history
}

#### creating and preparing dataframe
Streaming_History_Complete <- function(folder_path){
  Streaming_History_df(folder_path) %>%
    names_change() %>% 
    mutate(end_time = ymd_hm(end_time)) %>% 
    mutate(s_played = dmilliseconds((s_played))) %>%
    add_start_time() %>%
    add_skipped() %>%
    add_weekday()
  
}


## functions to be used on streaming history complete

#how many songs were skipped in given time period, as a number or as percentage
how_many_skipped <- function(streaming_history, start_date, end_date, as_percentage = FALSE){
  filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == TRUE)  
  if(as_percentage) {
    return (paste(round((nrow(filtered)/nrow(streaming_history)) * 100, digits = 3), "%", sep = ""))
  }
  
  return(nrow(filtered))
}

#how long you listened to spotify in given time period, as a duration or as a percentage 
how_long_listened <- function(streaming_history, start_date, end_date, as_percentage = FALSE){
  filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date))
  suma <- sum(filtered[["s_played"]])
  seconds_in_period <-as.numeric(difftime(end_date,start_date, units = "secs"))
  if (as_percentage) return(paste(round(suma/seconds_in_period * 100, digits = 3), "%", sep = ""))
  return (as.duration(suma))
}

#which songs were played the most times in given time period
most_played_track <- function(streaming_history, start_date, end_date, how_many = 10){
  df <-filter(data, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == FALSE) %>% 
    group_by(track_name) %>% 
    summarise(number = n())
  df <- df[order(-df[["number"]]),]
  df[1:how_many,]
}

#which songs were skipped the most times in given time period
most_skipped_track <- function(streaming_history, start_date, end_date, how_many = 10){
  df <-filter(data, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == TRUE) %>% 
    group_by(track_name) %>% 
    summarise(number = n())
  df <- df[order(-df[["number"]]),]
  df[1:how_many,]
}

#which artists were played the most times in given time period
most_played_track <- function(streaming_history, start_date, end_date, how_many = 10){
  df <-filter(data, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == FALSE) %>% 
    group_by(track_name) %>% 
    summarise(number = n())
  df <- df[order(-df[["number"]]),]
  df[1:how_many,]
}

#which atrists were skipped the most times in given time period
most_skipped_track <- function(streaming_history, start_date, end_date, how_many = 10){
  df <-filter(data, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == TRUE) %>% 
    group_by(track_name) %>% 
    summarise(number = n())
  df <- df[order(-df[["number"]]),]
  df[1:how_many,]
}
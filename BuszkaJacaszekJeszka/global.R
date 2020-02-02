library(shiny)
library(DT)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyr)
library(shinythemes)
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

### creating dataframe with date, devices and country 
Search_Queries_df <- function(folder_path){
  file_path <-list.files(folder_path,"SearchQueries")
  select(jsonlite::fromJSON(file_path),1:3) %>%
    mutate(date = ymd(date))
}



### creating dataframe with names of playlists, string containing song names separated 
#by ";;;" and artist names separated by ";;;"
Playlist_df_function <- function(folder_path){
  file_path <-list.files(folder_path,"Playlist")
  a <- jsonlite::fromJSON(file_path)
  b <- select(a[[1]], name, items)
  num_of_rows <- dim(b)[1]
  vec_num_of_rows <- 1:num_of_rows
  df <- data.frame(b$name,NA, NA)
  colnames(df) <- c("Playlist names","Song names","Artist names")
  for (i in vec_num_of_rows){
    df[i,2] <- paste(b[i,2][[1]]$track$trackName, collapse = ";;;")
    df[i,3] <- paste(b[i,2][[1]]$track$artistName, collapse = ";;;")
    df[,1] <- as.character(df[,1])
  }
  
  return(df)
}

# creating dataframe similar to Streaming_History_Complete, but this one has additional 
#column which has string of playlists that including that song, separated by ;
Playlist_df_Str_his <- function(folder_path){
  Str_his_df <- Streaming_History_Complete(folder_path)
  Playlist_df <- Playlist_df_function(folder_path)
  In_which_playlist <- select(data.frame(1:dim(Str_his_df)[1],NA),2)
  colnames(In_which_playlist) <- c("In_playlists")
  for (i in 1:dim(Playlist_df)[1]){
    splited_song_names <- strsplit(Playlist_df[i,2],";;;")
    
    splited_artist_names <- strsplit(Playlist_df[i,3],";;;")
    
    for (j in 1:dim(Str_his_df)[1]){
      for(k in 1:length(splited_artist_names[[1]])){
        if(Str_his_df[j,2]==splited_artist_names[[1]][k] & Str_his_df[j,3]==splited_song_names[[1]][k] )
        {
          if(is.na(In_which_playlist[j,1]==TRUE)){
            In_which_playlist[j,1] <- Playlist_df[i,1]
            break
          }
          else {In_which_playlist[j,1] <- paste(In_which_playlist[j,1], ";",Playlist_df[i,1])
          break
          }
        }
        
        
        
      }
    }
  }
  return(cbind(Str_his_df,In_which_playlist))
  
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


create_summary_table <- function(streaming_history, start_date, end_date, as_percentage){
  filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date))
  hm_songs_played <- nrow(filtered)
  hm_different_tracks <- length(unique(filtered[,3]))
  hm_skipped <- how_many_skipped(streaming_history, start_date, end_date, as_percentage)
  hm_different_artists <- length(unique(filtered[,2]))
  skip <- length(unique(filtered[,7]))
  hl_listened <- as.character(how_long_listened(streaming_history, start_date, end_date, as_percentage))
  
  Characteristic <- c("Songs played in total","Different songs played","Songs skipped","Different artists played","How long you listened to Spotify")
  Value <- c( hm_songs_played, hm_different_tracks, hm_skipped, hm_different_artists, hl_listened)
  
  summary_table <- data.frame(Characteristic, Value)
  summary_table
  
 
  
}

#which songs/artist were played/skipped the most/least times in given time period
most_played_skipped_track_artist <- function(streaming_history, start_date, end_date, how_many = 10, is_skipped,track_or_artist ){
  df <-filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == is_skipped) %>% 
    group_by(eval(as.name(track_or_artist))) %>% 
    summarise(number = n())
  df <- df[order(-df[["number"]]),]
  if(track_or_artist == "track_name"){
  colnames(df) <- c("track name","number")} else{colnames(df) <- c("artist name","number")}
  df[1:how_many,]
}
# 
# #which songs were skipped the most times in given time period
# most_skipped_track <- function(streaming_history, start_date, end_date, how_many = 10){
#   df <-filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == TRUE) %>% 
#     group_by(track_name) %>% 
#     summarise(number = n())
#   df <- df[order(-df[["number"]]),]
#   df[1:how_many,]
# }
# 
# #which artists were played the most times in given time period
# most_played_artist <- function(streaming_history, start_date, end_date, how_many = 10){
#   df <-filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == FALSE) %>% 
#     group_by(artist_name) %>% 
#     summarise(number = n())
#   df <- df[order(-df[["number"]]),]
#   df[1:how_many,]
# }
# 
# #which atrists were skipped the most times in given time period
# most_skipped_artist <- function(streaming_history, start_date, end_date, how_many = 10){
#   df <-filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == TRUE) %>% 
#     group_by(artist_name) %>% 
#     summarise(number = n())
#   df <- df[order(-df[["number"]]),]
#   df[1:how_many,]
# }

library(ggplot2)
library(dplyr)
library(lubridate)
###functions to be used on streaming history

##visualizes number of songs played in given time period at different hours 
number_of_songs_listened_by_hour <- function(streaming_history, start_date, end_date, 
                                             by_weekday = FALSE, dont_show_skipped = FALSE){
  filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date)) 
  if (dont_show_skipped) filtered <- filter(filtered, skipped == FALSE)
  vis <- ggplot(filtered, aes(x = hour(start_time)))+
    theme(panel.background = element_rect(fill = "moccasin"))+
    geom_bar(fill = "sienna4")+
    xlab("Hour")+
    ylab("Songs listened")
  
  if (by_weekday){
    vis <- vis+
      facet_wrap(~weekday)
    # +
    #   scale_x_discrete(limits = seq(from = 0, to = 22, by = 2))
  }
  else {
    vis <- vis+
      scale_x_discrete(limits = 0:23)
  }
  vis
}

number_of_songs_listened_by_hour(Streaming_History,"2019-09-10", "2019-10-09",FALSE, FALSE )

# number_of_songs_listened_by_weekday(Streaming_History, "2019-06-10", "2019-10-09")
##visualizes number of songs listened by weekday in given time period
number_of_songs_listened_by_weekday <- function(streaming_history, start_date, end_date, dont_show_skipped = TRUE){
  filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date)) 
  if (dont_show_skipped) filtered <- filter(filtered, skipped == FALSE)
  vis <- ggplot(filtered, aes(x = weekday))+
    geom_bar(fill = "sienna4")+
    xlab("Weekday")+
    theme(axis.text.x = element_text(angle = 0), panel.background = element_rect(fill = "moccasin"))+
    ylab("Songs listened")
  vis
}

##visualizes number of songs skipped in given time period, has two vesiorns - bar and point

number_of_skipped_songs <- function(streaming_history, start_date, end_date, by = "day", type = "bar"){
  filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == TRUE) %>% 
    mutate(end_time = floor_date(end_time, by))
  vis <- ggplot(filtered, aes(x = end_time ))+
    theme(panel.background = element_rect(fill = "moccasin"), axis.text.x = element_text(angle = 0))+
    ylab("Songs skipped")+
    xlab("Date")
  if (type == "point") vis <- vis + geom_point(stat = "count", color = "sienna4")
  if (type == "point" & by == "week") vis <- vis + geom_point(stat = "count", color = "sienna4", size = 4)
  if (type == "point" & by == "month") vis <- vis + geom_point(stat = "count", color = "sienna4", size = 7)
  if (type == "point" & by == "year") vis <- vis + geom_point(stat = "count", color = "sienna4", size = 12)
  if (type == "bar") vis <- vis + geom_bar(fill = "sienna4")
  vis
} 

number_of_skipped_songs(Streaming_History, "2019-07-10", "2019-10-18", by = "day", "bar")

### function to be used on search Queries:

##
platform_used <- function(search_queries, start_date, end_date){
  filter(search_queries, date >= ymd(start_date), date <= ymd(end_date)) %>% 
    ggplot(aes(x = platform, fill = platform))+
    geom_bar(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "moccasin"), axis.text.x = element_text(angle = 0))+
    xlab("Platform used")+
    ylab("How many searches")+
    scale_fill_brewer(palette="Accent")
  
  
}

##
country <- function(search_queries, start_date, end_date){
  filter(search_queries, date >= ymd(start_date), date <= ymd(end_date)) %>% 
    ggplot(aes(x = country, fill = country))+
    geom_bar(show.legend = FALSE)+
    theme(panel.background = element_rect(fill = "moccasin"), axis.text.x = element_text(angle = 0))+
    xlab("Country")+
    ylab("How many searches")+
    scale_fill_brewer(palette="Accent")
}

platform_used_by_date(search_queries, "2019-10-10", "2019-10-18")

##
platform_used_by_date<- function(search_queries, start_date, end_date){
  filter(search_queries, date >= ymd(start_date), date <= ymd(end_date)) %>%
    ggplot(aes(x = date, color = platform))+
    geom_point(stat = "count", size = 5)+
    theme(panel.background = element_rect(fill = "moccasin"),legend.key = element_rect(fill = "moccasin"), axis.text.x = element_text(angle = 0))+
    xlab("Date")+
    ylab("How many searches")+
    scale_color_brewer(palette="Accent")
}

##
country_by_date <- function(search_queries, start_date, end_date){
  filter(search_queries, date >= ymd(start_date), date <= ymd(end_date)) %>%
    ggplot(aes(x = date, color = country))+
    geom_point(stat = "count", size = 5)+
    theme(panel.background = element_rect(fill = "moccasin"),legend.key = element_rect(fill = "moccasin"))+
    xlab("Date")+
    ylab("How many searches")+
    scale_color_brewer(palette="Accent")
}
# ##visualizes number of songs played in given time period at different hours 
# number_of_songs_listened_by_hour <- function(streaming_history, start_date, end_date, 
#                                              by_weekday = FALSE, dont_show_skipped = TRUE){
#   filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date)) 
#   if (dont_show_skipped) filtered <- filter(filtered, skipped == FALSE)
#   vis <- ggplot(filtered, aes(x = hour(start_time)))+
#     geom_bar()+
#     scale_x_discrete(limits = 0:24)+
#     xlab("Hour")+
#     ylab("Songs listened")
#   
#   if (by_weekday){
#     vis <- vis+
#       facet_wrap(~weekday)
#   }
#   vis
# }
# 
# ##visualizes number of songs listened by weekday in given time period
# number_of_songs_listened_by_weekday <- function(streaming_history, start_date, end_date, dont_show_skipped = TRUE){
#   filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date)) 
#   if (dont_show_skipped) filtered <- filter(filtered, skipped == FALSE)
#   vis <- ggplot(filtered, aes(x = weekday))+
#     geom_bar()+
#     xlab("Weekday")+
#     theme(axis.text.x = element_text(angle = 270))+
#     ylab("Songs listened")
#   vis
# }
# 
# ##visualizes number of songs skipped in given time period, has two vesiorns - bar and point
# 
# number_of_skipped_songs <- function(streaming_history, start_date, end_date, by = "day", type = "bar"){
#   filtered <- filter(streaming_history, start_time >= ymd(start_date), start_time <= ymd(end_date), skipped == TRUE) %>% 
#     mutate(end_time = floor_date(end_time, by))
#   vis <- ggplot(filtered, aes(x = end_time ))+
#     ylab("Songs skipped")+
#     xlab("Date")
#   if (type == "point") vis <- vis + geom_point(stat = "count")
#   if (type == "bar") vis <- vis + geom_bar()
#   vis
# } 
# 
# ### function to be used on search Queries:
# 
# ##
# platform_used <- function(search_queries, start_date, end_date){
#   filter(search_queries, date >= ymd(start_date), date <= ymd(end_date)) %>% 
#     ggplot(aes(x = platform, fill = platform))+
#     geom_bar(show.legend = FALSE)+
#     xlab("Platform used")+
#     ylab("How many searches")
#     
#     
# }
# 
# ##
# platform_used_by_date<- function(search_queries, start_date, end_date){
#   filter(search_queries, date >= ymd(start_date), date <= ymd(end_date)) %>%
#     ggplot(aes(x = date, color = platform))+
#     geom_point(stat = "count")+
#     xlab("Date")+
#     ylab("How many searches")
# }
# 
# ##
# country_by_date <- function(search_queries, start_date, end_date){
#   filter(search_queries, date >= ymd(start_date), date <= ymd(end_date)) %>%
#     ggplot(aes(x = date, color = country))+
#     geom_point(stat = "count" )+
#     xlab("Date")+
#     ylab("How many searches")
# }
# 
Streaming_History <- Streaming_History_Complete(".")
search_queries <- Search_Queries_df(".")
# 
# 

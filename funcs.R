# If the file results.RDS does not yet exist, create the file by obtaing results through the API.
get_marathon_results <- function()
{
  if(!file.exists('data/result.RDS'))
  {
    source('get_marathon_results_from_api.R')
  }
  df_runners <- readRDS('data/result.RDS')
}


# Read the map from RDS or from Google Maps API.
get_map <- function()
{
  # If RDS with map data already exists, just load RDS.
  if(file.exists('data/rdam_map.RDS'))
  {
    rdam_map <- readRDS('data/rdam_map.RDS')
  }
  else # Otherwise, read from API using ggmapstyles and store RDS.
  {
    register_google_from_json()
    rdam_map <- get_snazzymap(center = c(4.48,51.90),
                              zoom = 12,
                              mapRef = "https://snazzymaps.com/style/72543/assassins-creed-iv")
    saveRDS(rdam_map,'data/rdam_map.RDS')
  }
  return(rdam_map)
}


# Register google API key from data/key.json
register_google_from_json <- function()
{
  register_google(fromJSON(file='data/key.json')$key)
}


# Function that reads the track data from tcx, and returns a data.frame with lon, lat, alt and (cumulative) distance columns.
get_track <- function()
{
  # Read the track data
  doc = xmlParse("track/Marathon Rotterdam 2018.tcx")
  nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns")
  df_track  <- plyr::ldply(nodes, as.data.frame(xmlToList))
  df_track  <- setNames(df_track, c('time', 'lat', 'lon', 'alt', 'distance'))
  df_track$time <- NULL
  for (column in colnames(df_track))
  {
    df_track[[column]] <- as.numeric(as.character(df_track[[column]]))
  }
  
  # Slight modifications to the track, otherwise there is a large piece of overlapping trail,
  # which reduces the clarity of the visualization.
  df_track$lon[df_track$distance>39000] = df_track$lon[df_track$distance>39000] + 0.0019
  df_track$lat[df_track$distance>39000] = df_track$lat[df_track$distance>39000] - 0.0014
  df_track$lon[df_track$distance>39000 & df_track$distance<40220] = df_track$lon[df_track$distance>39000 & df_track$distance<40220] - 0.00015
  df_track$lon[df_track$distance>41400 & df_track$distance<42200] = df_track$lon[df_track$distance>41400 & df_track$distance<42200] - 0.00025
  df_track$lat[df_track$distance>41400 & df_track$distance<42200] = df_track$lat[df_track$distance>41400 & df_track$distance<42200] + 0.0002

  return(df_track)
}


# Get the split times for each runner. Output is a list of vectors, 
# where each entry in the list contains the split times for one runner.
get_split_times_per_runner_and_remove_incomplete_runners <- function(df_runners,removeIncomplete=T){
  split_cols = colnames(df_runners)[grepl('_time',colnames(df_runners))]
  if (removeIncomplete)
      df_runners = df_runners[which(rowSums(is.na(df_runners[,split_cols,with=F]))<1),]
  split_times = lapply(1:nrow(df_runners), function(x) {c(0,as.numeric(as.vector(df_runners[x,split_cols,with=F])))*60*24})
  return(list('df_runners' = df_runners, 'split_times' = split_times))
}


# Get the split distances, equal for all runners.
get_split_dists <- function(df_runners)
{
  split_dists = as.character(as.vector(df_runners[1,colnames(df_runners)[grepl('split',colnames(df_runners))],with=F]))[c(T,F)]
  split_dists = gsub('k','',split_dists)
  split_dists = c(0,as.numeric(gsub('Finish','42.68',split_dists)))*1000
  return(split_dists)
}

# Create a plot of Rotterdam, with the marathon track and the current position of the runners.
create_plot <- function(df_position, df_track, minute, n_finished)
{
  td <- seconds_to_period(minute*60)
  suppressMessages(p <- ggmap(rdam_map,extent = "panel") +
    geom_path(data = df_track, color='white',alpha=0.9, size = 6, lineend = "round") + 
    geom_point(data=df_position,aes(x=lon,y=lat),size=1.5,color='#EE7600',alpha=0.7) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = "bold", 
                                    size = (30),
                                    margin = margin(t = 20, r = 0, b = 10, l = 0)),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = (22),
                                       margin = margin(t = 10, r = 0, b = 25, l = 0))
    ) +
    labs(title='Rotterdam Marathon 2018',subtitle='A time-lapse of all runners') +
    annotate("text", x = 4.44, y = 51.95, label = sprintf('%02d:%02d', td@hour, minute(td)),color='white',size=16,hjust = 0) +
    annotate("text", x = 4.44, y = 51.9427, label = paste0('Finished: ', n_finished),color='white',size=10,hjust = 0) +
    scale_x_continuous(limits = c(4.466-0.03, 4.546+0.03), expand = c(0, 0)) +
    scale_y_continuous(limits = c(51.87-0.01, 51.95+0.01), expand = c(0, 0)))
  return(p)
}

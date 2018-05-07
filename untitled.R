

# Load libraries
library(ggmap)
library(ggmapstyles)
library(data.table)
library(rjson)
library(XML)
library(data.table)
library(lubridate)
library(chron)
source('funcs.R')

# Create the appropriate directories that are ignored in gitignore.
dir.create('images',showWarnings = F)
dir.create('images/magick',showWarnings = F)
dir.create('output',showWarnings = F)

# Read the base data; the results of the marathon, the track coordinates, and the map of Rotterdam.
df_runners = get_marathon_results()
df_track = get_track()
rdam_map <- get_map()
tot_dist = max(df_track$distance)

# Add some noise per runner, so they do not all run on the same straight line.
set.seed(1)
df_runners$lat_noise = runif(nrow(df_runners),-0.0008,0.0008)
df_runners$lon_noise = runif(nrow(df_runners),-0.0008,0.0008)

# Split times and dists
X <- get_split_times_per_runner_and_remove_incomplete_runners (df_runners)
df_runners <- X[['df_runners']]
split_times <- X[['split_times']]
split_dists <- get_split_dists(df_runners)

split_dists[length(split_dists)]=42200

split_speed = lapply(split_times,function(x) {as.list(diff(split_dists)/diff(x) /1000*60)} )
df_split_speeds = rbindlist(split_speed)
df_split_speeds$gender = df_runners$gender
df_split_speeds$bib = df_runners$bib
x = melt(df_split_speeds,id.vars=c('bib'))

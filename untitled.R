library(data.table)
library(data.table)
library(lubridate)
library(chron)
source('funcs.R')

# Read the results of the marathon.
df_runners = get_marathon_results()

# Split times and dists
X <- get_split_times_per_runner_and_remove_incomplete_runners(df_runners)
df_runners <- X[['df_runners']]
split_times <- X[['split_times']]
split_dists <- get_split_dists(df_runners)
split_dists[length(split_dists)]=42200
split_names <-  sapply(tail(split_dists,-1)/1000, function(x) paste0(x,'k'))

# Calculate average speed during each split
df_split_speeds = rbindlist(lapply(split_times,function(x) {as.list(diff(split_dists)/diff(x)/1000*60)}))
colnames(df_split_speeds) <- split_names
df_split_speeds$gender = df_runners$gender
df_split_speeds$bib = df_runners$bib
df_split_speeds_long = melt(df_split_speeds,id.vars=c('bib','gender'),variable.name='split')
library(dplyr)
df_split_speeds_long = df_split_speeds_long %>% 
  left_join(data.frame(split = split_names, distance = diff(split_dists)))
df_gender_speed = df_split_speeds_long %>% 
  group_by(bib) %>% mutate(relative_speed = value/weighted.mean(value,distance)) %>%
  group_by(gender, split) %>% summarize(avg_relative_speed = mean(relative_speed)) %>%
  mutate(split = factor(split,split_names)) %>%
  mutate(split_numeric = as.numeric(as.character(gsub('k','',split))))



library(plotly)
library(ggplot2)
ggplot(data=df_gender_speed,aes(x=split_numeric,y=avg_relative_speed,group=gender)) + geom_line(aes(colour=gender))


factor_levels <- levels(df_gender_speed$gender)
palette <- structure(c('red','blue'), names=factor_levels)

plot_ly(df_gender_speed,
        x = ~split_numeric, 
        y = ~avg_relative_speed, 
        color = ~gender,
        colors=palette,
        hoverinfo="text", 
        text = ~paste0(round(avg_relative_speed*100,1),'%')
) %>%
  add_lines(line = list(width = 2)
  ) %>%
  layout(title=paste0("Speed per interval relative to the average speed"),
         xaxis = list(
           title = "",
           showlegend = FALSE,
           showgrid = FALSE,
           zeroline = TRUE,
           showline = FALSE,
           showticklabels=TRUE,
           tickmode = 'array',
           dtick=5),
         yaxis = list(title="",
                      tickformat="%",
                      showticklabels = TRUE,
                      showgrid = FALSE,
                      zeroline = TRUE,
                      showline = FALSE)
  )  %>% config(displaylogo= FALSE,collaborate=FALSE)
 



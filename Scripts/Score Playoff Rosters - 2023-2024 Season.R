# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(data.table)
library(nflreadr)
library(openxlsx)

roster_dir <- "Output/Compiled Rosters/"
roster_file <- "Playoff Fantasy Rosters for 2023-2024, Compiled 2024-01-21 060708.csv"

rosters <- fread(file = paste0(roster_dir,roster_file))
rosters[,team_abbr:=NULL]

rosters[,position_code:=ifelse(position_code=="D","Defense",position_code)]

if("Fantasy Owner Email" %in% names(rosters)){
  rosters[,`Fantasy Owner Email`:=NULL] # remove PII before further joins
}

stats_dir <- "Output/NFL Stats/"
stats_file_players <- "stats_2023_REG_POST_gen2024-01-21 052832.csv"
stats_players <- fread(file = paste0(stats_dir,stats_file_players))
stats_players[,player_id:=ifelse(position=="Defense",team_abbr,player_id)]

scored_rosters <- merge.data.table(stats_players[season_type == "Post"], rosters, by = "player_id", all.y = TRUE, allow.cartesian=TRUE)

scored_rosters <- scored_rosters[stat_type=="fantasy_points"]

output_file <- paste0(
  "Output/Scored Rosters/NFL Fantasy Scores for 2023-2024 as of ",
  str_remove_all(Sys.time(), ":"),
  ".csv"
)

fwrite(scored_rosters, output_file)


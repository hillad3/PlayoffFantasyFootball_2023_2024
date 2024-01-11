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
roster_file <- "Playoff Fantasy Roster for 2023-2024, Compiled 2024-01-10 012321.csv"

roster_full <- fread(file = paste0(roster_dir,roster_file))

# drop unnecessary columns because they are in the stats data already
roster_full[,`Team Abbr.`:=NULL]
roster_full[,`Position Group`:=NULL]
roster_full[,`Selection`:=NULL]
roster_full[,`Check 1 - Selection is Unique`:=NULL]
roster_full[,`Check 2 - Team is Unique`:=NULL]

setnames(
  roster_full,
  old = c(
    "Fantasy Owner",
    "Fantasy Team Name",
    "Automation Mapping",
    "Roster",
    "Position Code",
    "Position Type"
  ),
  new = c(
    "fantasy_owner",
    "fantasy_team_name",
    "player_id",
    "roster",
    "position_code",
    "position_type"
  )
)

roster_full[,position_code:=ifelse(position_code=="D","Defense",position_code)]

roster_full <- roster_full[, fantasy_owner_initials:=lapply(
    fantasy_owner,
    function(x){
      if(str_detect(x," ")){
        paste0(str_sub(x,1,1),".",str_sub(x,str_locate(x," ")[,1]+1,str_locate(x," ")[,1]+1),".")
      } else {
        paste0(str_sub(x,1,3),".")
      }
    }
  )
]
roster_full[,fantasy_owner_initials:=unlist(fantasy_owner_initials)]
roster_full[,fantasy_owner:=NULL]

stats_dir <- "Output/NFL Stats/"
stats_file_players <- "player_stats_2023_REG_POST_gen2024-01-11 011954.csv"
stats_players <- fread(file = paste0(stats_dir,stats_file_players))
stats_players[,player_id:=ifelse(position=="Defense",team_abbr,player_id)]

stats_players <- stats_players[season_type == "REG"]

scored_roster <- merge.data.table(stats_players, roster_full, by = "player_id", all.y = TRUE, allow.cartesian=TRUE)

# scored_roster <- scored_roster[stat_type == "fantasy_points"]
# scored_roster <- scored_roster[,.(fantasy_points=sum(stat_values)), 
#                                by = .(fantasy_owner_initials,
#                                       fantasy_team_name,
#                                       player_id,
#                                       player_name,
#                                       team_abbr)]
# setorder(scored_roster, cols = fantasy_team_name, -fantasy_points)
# scored_roster[,total_fantasy_points:=cumsum(fantasy_points),by=fantasy_team_name]


output_file <- paste0(
  "Output/Scored Rosters/Test NFL Fantasy Scores for 2023-2024 as of ",
  str_remove_all(Sys.time(), ":"),
  ".csv"
)


fwrite(stats_players, output_file)


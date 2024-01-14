# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(data.table)
library(logger)
logger::log_layout(layout_glue_colors)

season <- "2023-2024"

# get a list of files
directory_files <- dir("Data/Individual Rosters")

# get a list of excel files
roster_files <- directory_files[str_detect(directory_files, "xlsx$|csv$")]

# exclude specific types of files
roster_files <- roster_files[!str_detect(roster_files, "^Compiled Roster, Gen")]
excluded_files <- c("Team Name 1.xlsx", "Team Name 2.xlsx", "Team Name 3.xlsx", "Playoff Fantasy Roster Check.xlsx")
roster_files <- roster_files[!(roster_files %in% excluded_files)]

if(any(str_detect(roster_files, "xlsx$"))){
  log_error("There are .xlsx files still remaining in the directory. Either convert to csv or move old file to archive folder.")
  stop("Stopping")
}

# ## option for debugging reading in files
# for(r in roster_files){
#   print(roster_files[[1]])
#   fread(paste0(paste0("Data/Individual Rosters/",roster_files[[r]])))
# }

rosters <- lapply(paste0("Data/Individual Rosters/",roster_files), fread)

team_names <- vector(mode="list",length = length(rosters))
errors <- 0
for(r in seq_along(rosters)){
  print(roster_files[[r]])
  if(any(duplicated(rosters[[r]]$`Team Abbr`))){
    log_error("1: There are duplicated NFL team names within a single roster")
    errors <- errors + 1
  }
  if(any(duplicated(rosters[[r]]$`Automation Mapping`))){
    log_error("2: There are duplicated automation mapping codes within a single roster")
    errors <- errors + 1
  }
  if(!all(c("K","QB1","QB2","QB3","RB1","RB2","RB3","TE1","TE2","WR1","WR2","WR3","D") %in% rosters[[r]]$`Position Code`)){
    log_error("3: There are missing position types within single roster")
    errors <- errors + 1
  }
  if(length(rosters[[r]]$`Position Code`[rosters[[r]]$`Position Code` %in% c("RB4","WR4","TE3")])!=1L){
    log_error("4: There are missing position types within a single roster")
    errors <- errors + 1
  }
  if(dim(rosters[[r]])[1]!=14L){
    log_error("5: There are differing number of rows in the data.table")
    errors <- errors + 1
  }
  if(dim(rosters[[r]])[2]!=12L){
    log_error("6: There are differing number of columns in the data.table")
    errors <- errors + 1
  }
  if (any(is.na(rosters[[r]] |> unlist()))) {
    log_error("7: There are NAs in the table")
    errors <- errors + 1
  }
  if(unique(rosters[[r]]$`Fantasy Team Name`) %in% team_names){
    log_error("8: This team name has already been used.")
    errors <- errors + 1
  } else {
    team_names[[r]] <- unique(rosters[[r]]$`Fantasy Team Name`)
  }
}
if(errors>0){
  stop("There are errors that need to be fixed to continue")
}

rosters <- rbindlist(rosters)

rosters[,source_file := rep(roster_files, each=14)] # this should fail if not perfectly rectangular



# drop unnecessary data. these details will be brought in from tte stats
rosters[,`Position Group`:=NULL]
rosters[,`Selection`:=NULL]
# rosters[,`Fantasy Owner Email`:=NULL]
rosters[,`Check 1 - Selection is Unique`:=NULL]
rosters[,`Check 2 - Team is Unique`:=NULL]

setnames(
  rosters,
  old = c(
    "Fantasy Owner",
    "Fantasy Team Name",
    "Automation Mapping",
    "Roster",
    "Position Code",
    "Position Type",
    "Team Abbr."
  ),
  new = c(
    "fantasy_owner",
    "fantasy_team_name",
    "player_id",
    "roster",
    "position_code",
    "position_type",
    "team_abbr"
  )
)

# create initials and drop names to remove PII
rosters <- rosters[, fantasy_owner_initials:=lapply(
  fantasy_owner,
  function(x){
    if(str_detect(x," ")){
      paste0(str_sub(x,1,1),".",str_sub(x,str_locate(x," ")[,1]+1,str_locate(x," ")[,1]+1),".")
    } else {
      paste0(str_sub(x,1,3),".")
    }
  }
)]
rosters[,fantasy_owner_initials:=unlist(fantasy_owner_initials)]
rosters[,fantasy_team_and_initials:=paste0(fantasy_team_name," (",fantasy_owner_initials,")")]
rosters[,fantasy_owner:=NULL]
rosters[,fantasy_owner_initials:=NULL]
rosters[,`Fantasy Owner Email`:=NULL] # optional

# TODO count to ensure that team names are unique and total to 14 (to avoid duplicate names from multiple people)
tmp <- rosters[,.(fantasy_team_and_initials)][,.(n=.N), by=.(fantasy_team_and_initials)]

if(any(tmp$n!=14L)){
  stop("There is a team that does not have exactly 14 players on the roster")
}

fwrite(
  rosters,
  file = paste0(
    "Output/Compiled Rosters/Playoff Fantasy Rosters for ",
    season,
    ", Compiled ",
    str_remove_all(Sys.time(), ":"),
    ".csv"
  ),
)


# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

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
excluded_files <- c("Team Name 1.xlsx", "Team Name 2.xlsx", "Team Name 3.xlsx")
roster_files <- roster_files[!(roster_files %in% excluded_files)]

if(any(str_detect(roster_files, "xlsx$"))){
  log_error("There are .xlsx files still remaining in the directory. Either convert to csv or move old file to archive folder.")
  stop("Stopping")
}

rosters <- lapply(paste0("Data/Individual Rosters/",roster_files), fread)

check_validity <- function(dt){
  if(any(duplicated(dt$`Team Abbr`))){
    log_error("There are duplicated NFL team names within a single roster")
    stop()
  }
  if(any(duplicated(dt$`Automation Mapping`))){
    log_error("There are duplicated automation mapping codes within a single roster")
    stop()
  }
  if(!all(c("K","QB1","QB2","QB3","RB1","RB2","RB3","TE1","TE2","WR1","WR2","WR3","D") %in% dt$`Position Code`)){
    log_error("There are missing position types within single roster")
    stop()
  }
  if(length(dt$`Position Code`[dt$`Position Code` %in% c("RB4","WR4","TE3")])!=1L){
    log_error("There are missing position types within single roster")
    stop()
  }
  if(dim(dt)[1]!=14L){
    log_error("There are differing number of rows in the data.table")
    stop()
  }
  if(dim(dt)[2]!=12L){
    log_error("There are differing number of columns in the data.table")
    stop()
  }
}

lapply(rosters, check_validity)

rosters <- rbindlist(rosters)

rosters[,source_file := rep(roster_files, each=14)] # this should file if not perfectly rectangular

if (any(is.na(rosters |> unlist()))) {
  log_warn("There are NAs in the table")
  print(dt)
}

# TODO count to ensure that team names are unique and total to 14 (to avoid duplicate names from multiple people)
# TODO take care of special characters in the Participant Info fields

# drop email column before consolidating
rosters[,`Fantasy Owner Email`:=NULL]

fwrite(
  rosters,
  file = paste0(
    "Output/Compiled Rosters/Playoff Fantasy Roster for ",
    season,
    ", Compiled ",
    str_remove_all(Sys.time(), ":"),
    ".csv"
  ),
)


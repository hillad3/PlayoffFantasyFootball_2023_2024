# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(openxlsx)
library(logger)
logger::log_layout(layout_glue_colors)

# get a list of files
directory_files <- dir("Data/Individual Rosters")

# get a list of excel files
roster_files <- directory_files[str_detect(directory_files, "xlsx$")]

# remove compiled files, although these should not be present
roster_files <- roster_files[!str_detect(roster_files, "^Compiled Roster, Gen")]

rosters <- lapply(paste0("Data/Individual Rosters/",roster_files), readxl::read_excel, sheet = "Team Roster", skip = 5)

rosters <- bind_rows(rosters)

rosters <- rosters %>%
  mutate(file_name = unlist(lapply(roster_files, rep, 14)))

issues_to_fix <- list()

if (any(is.na(rosters$`Fantasy Owner`) |
  is.na(rosters$`Fantasy Owner Email`) |
  is.na(rosters$`Fantasy Team Name`))) {
  log_warn("There are blanks in the Fantasy details column")
  issues_to_fix[["blank_fantasy_details"]] <- rosters %>%
    filter(is.na(rosters$`Fantasy Owner`) |
      is.na(rosters$`Fantasy Owner Email`) |
      is.na(rosters$`Fantasy Team Name`)) %>%
    distinct(file_name, `Fantasy Owner`, `Fantasy Owner Email`, `Fantasy Team Name`)
}

if (any(is.na(rosters$`Check 1 - Selection is Unique`))) {
  log_warn("There are blanks in the roster")
  issues_to_fix[["blank_roster_spot"]] <- rosters %>%
    filter(is.na(rosters$`Check 1 - Selection is Unique`)) %>%
    select(file_name, Roster, `Automation Mapping`)
}

if (any(!(rosters$`Check 1 - Selection is Unique`[!is.na(rosters$`Check 1 - Selection is Unique`)]))) {
  log_warn("There are FALSEs in the Check 1 column for unique Players")
  issues_to_fix[["non_unique_player"]] <- rosters %>%
    filter(!is.na(`Check 1 - Selection is Unique`)) %>%
    filter(!`Check 1 - Selection is Unique`) %>%
    select(file_name, Roster, `Automation Mapping`)
}

if (any(!(rosters$`Check 2 - Team is Unique`[!is.na(rosters$`Check 2 - Team is Unique`)]))) {
  log_warn("There are FALSEs in the Check 2 column for unique Teams")
  issues_to_fix[["non_unique_team"]] <- rosters %>%
    filter(!is.na(`Check 2 - Team is Unique`)) %>%
    filter(!`Check 2 - Team is Unique`) %>%
    select(file_name, Roster, `Automation Mapping`, `Team Abbr.`)
}

if(length(issues_to_fix)==0L){
  rm(issues_to_fix)
}

if (!exists("issues_to_fix")) {
  write.xlsx(
    rosters,
    file = paste0("Output/Compiled Rosters/Full Fantasy Roster, Compiled ", str_remove_all(Sys.time(), ":"), ".xlsx"),
    sheets = list("Compiled Roster")
  )
} else {
  log_error("Address issues_to_fix and re-run script to write output to file. Or execute write.xlsx() manually.")
}

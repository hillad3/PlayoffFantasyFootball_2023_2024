# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(nflreadr)
library(logger)
logger::log_layout(layout_glue_colors)

# list the names of teams in the playoffs in quotes
# all teams are listed below as a default
playoff_teams <- c(
  "ARI",
  "ATL",
  "BAL",
  "BUF",
  "CAR",
  "CHI",
  "CIN",
  "CLE",
  "DAL",
  "DEN",
  "DET",
  "GB",
  "HOU",
  "IND",
  "JAX",
  "KC",
  "LA",
  "LAC",
  "LV",
  "MIA",
  "MIN",
  "NE",
  "NO",
  "NYG",
  "NYJ",
  "OAK",
  "PHI",
  "PIT",
  "SD",
  "SEA",
  "SF",
  "STL",
  "TB",
  "TEN",
  "WAS"
)

# check to make sure 14 playoff teams have been selected
if (length(playoff_teams) != 14L) {
  log_error("playoff_teams should contain exactly 14 teams, which is currently not true.")
}

players <- nflreadr::load_players() %>%
  as_tibble() %>%
  filter(status == "ACT") %>%
  filter(team_abbr %in% playoff_teams) %>%
  filter(position_group %in% c("QB", "RB", "WR", "TE", "SPEC")) %>%
  filter(position %in% c("QB", "RB", "FB", "WR", "TE", "K", "P")) %>%
  select(
    position_group,
    position,
    team_abbr,
    display_name,
    gsis_id,
    status,
    status_short_description,
    jersey_number,
  ) %>%
  arrange(
    position_group,
    team_abbr,
    display_name,
  ) %>%
  mutate(
    selector_id = paste(position_group,team_abbr,display_name, sep = "_")
  )

# create a FLEX position_group
players <- bind_rows(
  players,
  players %>%
    filter(position_group %in% c("RB", "WR", "TE")) %>%
    mutate(position_group = "FLEX")
)

# this chunk of code creates a wider dataframe for easy pasting into excel
longest_position <- max(unlist(lapply(split(players$position_group, players$position_group), length)))

same_length <- function(df, target_length = longest_position){

  names(df) <- paste0(names(df),"_",unique(df$position_group))

  if(dim(df)[1] == target_length){
    return (df)
  } else {
    rows_to_add <- target_length - dim(df)[1]
    column_names <- names(df)
    empty_list <- vector(mode = "list", length(column_names))
    names(empty_list) <- column_names

    for(i in seq_along(column_names)){
      if(is.character(df[[1,i]])){
        empty_list[[i]] <- rep(NA_character_, rows_to_add)
      } else if (is.integer(df[[1,i]])){
        empty_list[[i]] <- rep(NA_integer_, rows_to_add)
      } else {
        stop("The data type of this column is not covered")
      }
    }
    df <- bind_rows(
      df,
      bind_cols(empty_list)
    )
    return (df)
  }

}

players_wide <- split(players, players$position_group)

players_wide <- lapply(players_wide, same_length)

players_wide <- bind_cols(players_wide)


## paste the dataframe into memory so it can be pasted into the excel file "Playoff Fantasy Football - Signup Roster 2023.xlsx"
# clipr::write_clip(players_wide)

teams <- nflreadr::load_teams() %>%
  as_tibble() %>%
  filter(team_abbr %in% playoff_teams) %>%
  select(team_abbr, team_name) %>%
  mutate(selector_id = paste0(team_abbr,"_",team_name))
names(teams) <- paste0(names(teams),"_D")

## paste the dataframe into memory so it can be pasted into the excel file
# clipr::write_clip(teams)
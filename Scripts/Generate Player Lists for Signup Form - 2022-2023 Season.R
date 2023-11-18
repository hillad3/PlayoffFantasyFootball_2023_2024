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
  "BAL",
  "BUF",
  "CIN",
  "DAL",
  "JAX",
  "KC",
  "LAC",
  "MIA",
  "MIN",
  "NYG",
  "PHI",
  "SEA",
  "SF",
  "TB"
)

# check to make sure 14 playoff teams have been selected
if (length(playoff_teams) != 14L) {
  log_error("playoff_teams should contain exactly 14 teams, which is currently not true.")
}

players <- nflreadr::load_players() %>%
  as_tibble() %>%
  mutate(
    # these are cases to resolve to align to the 2022-2023 season
    team_abbr = case_when(
      display_name == "Brett Maher" & gsis_id == "00-0030332" ~ "DAL",
      display_name == "Dalton Schultz" & gsis_id == "00-0034383" ~ "DAL",
      display_name == "Dalvin Cook" & gsis_id == "00-0033893" ~ "MIN",
      display_name == "Ezekiel Elliott" & gsis_id == "00-0033045" ~ "DAL",
      display_name == "Leonard Fournette" & gsis_id == "00-0033856" ~ "TB",
      display_name == "Marvin Jones" & gsis_id == "00-0029293" ~ "JAX",
      display_name == "Mike Gesicki" & gsis_id == "00-0034829" ~ "MIA",
      display_name == "Miles Sanders" & gsis_id == "00-0035243" ~ "PHI",
      display_name == "Riley Patterson" & gsis_id == "00-0036816" ~ "JAX",
      .default = team_abbr
    )
  ) %>% 
  filter(status != "RET") %>%
  filter(team_abbr %in% playoff_teams) %>%
  filter(position_group %in% c("QB", "RB", "WR", "TE", "SPEC")) %>%
  filter(position %in% c("QB", "RB", "FB", "WR", "TE", "K")) %>%
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


## paste the dataframe into memory so it can be pasted into the excel file Form
# "Playoff Fantasy Football - 2023-2024 Signup Roster.xlsx"
# Note: excel password is abc123. it isn't intended to keep the KGB out, just the average idiot

# clipr::write_clip(players_wide)

teams <- nflreadr::load_teams() %>%
  as_tibble() %>%
  filter(team_abbr %in% playoff_teams) %>%
  select(team_abbr, team_name) %>%
  mutate(selector_id = paste0(team_abbr,"_",team_name))

names(teams) <- paste0(names(teams),"_D")

## paste the dataframe into memory so it can be pasted into the Form 
# "Playoff Fantasy Football - 2023-2024 Signup Roster.xlsx"

# clipr::write_clip(teams)
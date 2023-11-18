# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(nflreadr)
library(openxlsx)

# update inputs as required
ff_season <- 2022L
ff_season_type <- "POST"
# if ff_week is NULL then it will do all available data. 
# Note that playoff weeks are a continuation of regular season weeks
ff_week = NULL
data_dir <- "Output/Compiled Rosters/"
roster_file <- "Full Fantasy Roster, 2022-2023 Season, Compiled 2023-11-18 0001.xlsx"
sheet_name <- "Compiled Roster"

team_mapping <- readxl::read_excel(
  path = paste0(data_dir,roster_file),
  sheet = sheet_name
) %>%
  select(
		fantasy_owner = `Fantasy Owner`,
		fantasy_team_name = `Fantasy Team Name`,
		player_id = `Automation Mapping`,
		team_abbr = `Team Abbr.`,
		position_type = `Position Type`,
		position_group = `Position Group`
  )

def_mapping <- team_mapping %>%
  filter(position_type == "Defense / Special teams") %>%
  select(
    team = team_abbr,
    fantasy_team_name
  )

off_mapping <- team_mapping %>%
  filter(position_type != "Defense / Special teams") %>%
  select(
    player_id,
    fantasy_team_name
  )



# import play-by-play dataset
ps <- nflreadr::load_player_stats(seasons = ff_season) %>%
  as_tibble() %>%
  filter(season_type == ff_season_type)

if(!is.null(ff_week)){
  ps <- ps %>% filter(week == ff_week)
}

ps <- ps %>%
  select(
    week,
    player_id,
		player = player_name,
		team = recent_team,
		passing_yards,
		passing_tds,
		interceptions,
		sacks,
		sack_fumbles,
		sack_fumbles_lost,
		sack_fumbles_lost,
		passing_2pt_conversions,
		rushing_yards,
		rushing_tds,
		rushing_fumbles,
		rushing_fumbles_lost,
		rushing_2pt_conversions,
		receiving_yards,
		receiving_tds,
		receiving_fumbles,
		receiving_fumbles_lost,
		receiving_2pt_conversions,
	) %>%
  mutate(week = as.integer(week))

ps <- ps %>%
  pivot_longer(
    cols = c(
      passing_yards,
      passing_tds,
      interceptions,
      sacks,
      sack_fumbles,
      sack_fumbles_lost,
      sack_fumbles_lost,
      passing_2pt_conversions,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_2pt_conversions,
      receiving_yards,
      receiving_tds,
      receiving_fumbles,
      receiving_fumbles_lost,
      receiving_2pt_conversions,
    ),
    names_to = "value_label",
    values_to = "value"
  )



# import play-by-play dataset
pbp <- nflfastR::load_pbp(seasons = ff_season) %>%
  as_tibble() %>%
  filter(season_type == ff_season_type)

if(!is.null(ff_week)){
  pbp <- pbp %>% filter(week == ff_week)
}

pbp <- pbp %>%
  select(
    game_id,
    game_date,
    week,
    home_team,
    away_team,
    home_score,
    away_score,
    posteam,
    defteam,
    play_type,
    time,
    desc,
    fixed_drive_result,
    touchdown,
    pass_touchdown,
    rush_touchdown,
    return_touchdown,
    yards_gained,
    rushing_yards,
    passing_yards,
    return_yards,
    return_team,
    interception,
    interception_player_name,
    interception_player_id,
    fumble,
    fumble_lost,
    fumble_recovery_1_team,
    passer_player_name,
    passer_player_id,
    receiver_player_name,
    receiver_player_id,
    rusher_player_name,
    rusher_player_id,
    td_player_name,
    td_player_id,
    kicker_player_name,
    kicker_player_id,
    kickoff_returner_player_name,
    kickoff_returner_player_id,
    punt_returner_player_name,
    punt_returner_player_id,
    fumbled_1_player_name,
    fumbled_1_player_id,
    fumble_recovery_1_player_name,
    fumble_recovery_1_player_id,
    sack,
    safety,
    two_point_conv_result,
    two_point_attempt,
    extra_point_result,
    extra_point_attempt,
    field_goal_result,
    field_goal_attempt,
    kick_distance,
    blocked_player_name,
    blocked_player_id
  ) %>%
  mutate(week = as.integer(str_sub(game_id,6,7)))






# player
if(TRUE){

  ps <- ps %>%
    mutate(
      ff_points = case_when(
        value_label == "passing_yards" & value >= 400 ~ as.integer(value/50) + 2L,
        value_label == "passing_yards" & value < 400 ~ as.integer(value/50),
        value_label == "rushing_yards" & value >= 200 ~ as.integer(value/10L) + 2L,
        value_label == "rushing_yards" & value < 200 ~ as.integer(value/10L),
        value_label %in% c("passing_tds", "rushing_tds","receiving_tds") ~ value * 6L,
        value_label %in% c("passing_2pt_conversions", "rushing_2pt_conversions","receiving_2pt_conversions") ~ value * 2L,
        value_label == "interceptions" ~ value * -2L,
        value_label %in% c("sack_fumbles", "rushing_fumbles", "receiving_fumbles") ~ value * -2L,
        .default = 0L
      )
    ) %>%
    filter(abs(ff_points) >= 1e-7)

  # create a list to hold each unique fantasy football points dataset
  player <- list()

  # offensive bonus for touchdown with pass over 40 yards for qb
  player[["pass_td_40yds_qb"]] <- pbp %>%
    filter(pass_touchdown == 1L & passing_yards >= 40) %>%
    group_by(week, team = posteam, player = passer_player_name, player_id = passer_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * 2L))

  # offensive bonus for touchdown with pass over 40 yards for receiver
  player[["pass_td_40yds_receiver"]] <- pbp %>%
    filter(pass_touchdown == 1L & passing_yards >= 40) %>%
    group_by(week, team = posteam, player = receiver_player_name, player_id = receiver_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * 2L))

  # offensive bonus for touchdown with rush over 40 yards for qb
  player[["run_td_40yds"]] <- pbp %>%
    filter(rush_touchdown == 1L & rushing_yards >= 40) %>%
    group_by(week, team = posteam, player = rusher_player_name, player_id = rusher_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * 2L))

  # player bonus for returning a td
  # only for normal possession plays by the opposite team (ie. pass or rush)
  # in a kickoff, the receiving team is listed as the posteam
  # in a punt, the receiving team is listed as the defteam
  player[["return_td_40yds"]] <- bind_rows(
    pbp %>%
      filter(play_type == "kickoff" & !is.na(kickoff_returner_player_name) & return_touchdown == 1L & return_yards >= 40) %>%
      group_by(
        game_id,
        team = posteam,
        player = kickoff_returner_player_name,
        player_id = kickoff_returner_player_id
      ) %>%
      reframe(value = n()),
    pbp %>%
      filter(play_type == "punt" & !is.na(punt_returner_player_name) & return_touchdown == 1L & return_yards >= 40) %>%
      group_by(
        game_id,
        team = defteam,
        player = punt_returner_player_name,
        player_id = punt_returner_player_id
      ) %>%
      reframe(value = n())
  ) %>%
    group_by(
      game_id,
      team,
      player,
      player_id
    ) %>%
    reframe(value = sum(value)) %>%
    mutate(
      ff_points = as.integer(value * 2L)
    )


  # offensive bonus for extra point
  player[["extra_point"]] <- pbp %>%
    filter(extra_point_result == "good") %>%
    group_by(week, team = posteam, player = kicker_player_name, player_id = kicker_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * 1L))

  # offensive penalty for extra point missed (could be blocked or missed)
  player[["extra_point_missed"]] <- pbp %>%
    filter(extra_point_result != "good" & extra_point_attempt == 1L) %>%
    group_by(week, team = posteam, player = kicker_player_name, player_id = kicker_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * -1L))

  # offensive bonus for FG <= 39 yards
  player[["fg_upto39"]] <- pbp %>%
    filter(field_goal_result == "made" & kick_distance <= 39) %>%
    group_by(week, team = posteam, player = kicker_player_name, player_id = kicker_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * 3L) )

  # offensive bonus for FG > 39 and <= 49 yards
  player[["fg_upto49"]] <- pbp %>%
    filter(field_goal_result == "made" & kick_distance > 39 & kick_distance <= 49) %>%
    group_by(week, team = posteam, player = kicker_player_name, player_id = kicker_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * 4L))

  # offensive bonus for FG > 49 yards
  player[["fg_over49"]] <- pbp %>%
    filter(field_goal_result == "made" & kick_distance > 49) %>%
    group_by(week, team = posteam, player = kicker_player_name, player_id = kicker_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * 5L))

  # offensive penalty for FG missed (could be blocked or missed)
  player[["fg_missed"]] <- pbp %>%
    filter(field_goal_result != "made" & field_goal_attempt == 1L) %>%
    group_by(week, team = posteam, player = kicker_player_name, player_id = kicker_player_id) %>%
    reframe(value = n()) %>%
    mutate(ff_points = as.integer(value * -1L))

}

player <- bind_rows(player, .id = "value_label")%>%
  select(week, team, player, player_id, value_label, value, ff_points) %>%
  mutate(position_type = "player") %>%
  bind_rows(ps %>% mutate(position_type = "player"))

player_ff_teams <- player %>%
  right_join(off_mapping, by = c("player_id"), relationship = "many-to-many") %>%
  filter(!is.na(week))


# def
if(TRUE){

  def <- list()

  # defensive bonus for sacks
  def[["sack"]] <- pbp %>%
    filter(sack == 1L) %>%
    group_by(week, team = defteam) %>%
    reframe(value = sum(sack)) %>%
    mutate(
      ff_points = as.integer(value * 1L)
    )

  # defensive bonus for safeties
  def[["safety"]] <- pbp %>%
    filter(safety == 1L) %>%
    group_by(week, team = defteam) %>%
    reframe(value = sum(safety)) %>%
    mutate(
      ff_points = as.integer(value * 1L)
    )

  # defensive bonus for fumble recovery
  def[["fumble_recovery"]] <- pbp %>%
    filter(fumble == 1L & fumble_lost == 1L) %>%
    group_by(week, team = defteam) %>%
    reframe(value = sum(fumble)) %>%
    mutate(
      ff_points = as.integer(value * 2L)
    )

  # defensive bonus for interceptions
  def[["interception"]] <- pbp %>%
    filter(interception == 1L) %>%
    group_by(week, team = defteam) %>%
    reframe(value = sum(interception)) %>%
    mutate(
      ff_points = as.integer(value * 2L)
    )

  # def bonus for blocks on punt, fg or extra point
  def[["block"]] <- pbp %>%
    filter(!is.na(blocked_player_name)) %>%
    mutate(block = 1L) %>%
    group_by(week, team = defteam) %>%
    reframe(value = sum(block)) %>%
    mutate(
      ff_points = as.integer(value * 2L)
    )

  # def bonus for def td for any reason or cause (block, fumble, interception, etc)
  # only for normal possession plays by the opposite team (ie. pass or rush)
  def[["def_td"]] <- pbp %>%
    filter(return_touchdown == 1L & play_type %in% c("pass", "run")) %>%
    group_by(week, team = defteam) %>%
    reframe(value = sum(return_touchdown)) %>%
    mutate(
      ff_points = as.integer(value * 6L)
    )

  # special teams bonus for a return td
  # in a kickoff, the kicking team is listed as the defteam
  # in a punt, the receiving team is listed as the defteam
  def[["kick_return_td"]] <- bind_rows(
    pbp %>%
      filter(return_touchdown == 1L & play_type %in% c("kickoff")) %>%
      group_by(week, team = posteam) %>%
      reframe(value = sum(touchdown)),
    pbp %>%
      filter(return_touchdown == 1L & play_type %in% c("punt")) %>%
      group_by(week, team = defteam) %>%
      reframe(value = sum(touchdown))
  ) %>%
    mutate(
      ff_points = as.integer(value * 6L)
    )

  # calculate points allowed for each team
  def[["def_points_allowed"]] <- bind_rows(
    pbp %>% distinct(week, team = home_team, value = away_score),
    pbp %>% distinct(week, team = away_team, value = home_score)
  ) %>%
    mutate(
      ff_points = case_when(
        value == 0L ~ 10L,
        value >= 1L & value <= 6 ~ 7L,
        value >= 7L & value <= 13 ~ 4L,
        value >= 14L & value <= 21 ~ 1L,
        value >= 22L & value <= 27 ~ -1L,
        value >= 28L & value <= 34 ~ -4L,
        value >= 35L & value <= 45 ~ -7L,
        value >= 46L ~ -10L,
        .default = 0L
      )
    )

}

def <- bind_rows(def, .id = "value_label") %>%
  select(week, team, value_label, value, ff_points) %>%
  mutate(position_type = "Defense / Special teams")

def_ff_teams <- def %>%
  right_join(def_mapping, by = c("team"), relationship = "many-to-many") %>%
  filter(!is.na(week))


# combined all data with all players and teams
all <- bind_rows(
  player,
  def
)

# combined all data with only FF matches
all_ff_teams <- bind_rows(
  player_ff_teams,
  def_ff_teams
)

if(is.null(ff_week)){
  output_file <- paste0(
    "Output/Scored Rosters/NFL Playoff Scoring for ",
    ff_season,"-",ff_season+1," ",ff_season_type," Season as of ",
    str_remove_all(Sys.time(),":"),".xlsx")
} else {
  output_file <- paste0(
    "Output/Scored Rosters/NFL Playoff Scoring for ",
    ff_season,"-",ff_season+1," ",ff_season_type," Season Week ",
    ff_week," as of ", str_remove_all(Sys.time(),":"),".xlsx")
}

write.xlsx(
  x = list(all, all_ff_teams),
  sheetName = list("Player Data", "Fantasy Team Detailed Scores"),
  file = output_file
)

# clipr::write_clip(all)
# clipr::write_clip(all_ff_teams)


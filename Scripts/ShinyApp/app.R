# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(tidyverse)
library(shiny)
library(data.table)
library(DT)
library(shinyjs)
library(shinythemes)

season_year <- 2022L
season_type <- c("REG","POST")
season_teams <- c(
  "ARI","ATL","BAL","BUF","CAR",
  "CHI","CIN","CLE","DAL","DEN",
  "DET","GB","HOU","IND","JAX",
  "KC","LA","LAC","LV","MIA",
  "MIN","NE","NO","NYG","NYJ",
  "PHI","PIT","SEA","SF","TB",
  "TEN","WAS"
)


get_team_info <- function(season_year_int = season_year){
  # create data.table for NFL teams
  dt <- data.table::as.data.table(nflreadr::load_teams(current = TRUE))
  dt[,team_name_w_abbr := paste0(team_name, ' (', team_abbr, ')')]
  dt <- dt[,.(team_abbr, team_name, team_name_w_abbr, team_conf, team_division, team_logo_espn)]
  return(dt)
}

# create data.table for NFL teams
dt_nfl_teams <- get_team_info()

dt_roster <- data.table::as.data.table(nflreadr::load_rosters(season_year))
dt_roster[,player_name:=paste0(str_sub(first_name,1,1),".",last_name)]
dt_roster <- unique(dt_roster[,.(position, player_id = gsis_id, player_name, team_abbr = team)])

get_pbp <- function(season_year_int = season_year,
                    season_type_char = season_type, 
                    season_teams_list = season_teams){
  dt <- data.table::as.data.table(nflfastR::load_pbp(seasons = season_year_int))
  dt <- dt[season_type %in% season_type_char]
  dt[,season_type := if_else(season_type=="REG","Regular", if_else(season_type=="POST","Post","Error"))]
  dt <- dt[home_team %in% season_teams_list | away_team %in% season_teams_list]
  cols <- c(
    'game_id',
    'game_date',
    'week',
    'season_type',
    'home_team',
    'away_team',
    'home_score',
    'away_score',
    'posteam',
    'defteam',
    'play_type',
    'time',
    'desc',
    'fixed_drive_result',
    'touchdown',
    'pass_touchdown',
    'rush_touchdown',
    'return_touchdown',
    'yards_gained',
    'rushing_yards',
    'passing_yards',
    'return_yards',
    'return_team',
    'interception',
    'interception_player_name',
    'interception_player_id',
    'fumble',
    'fumble_lost',
    'fumble_recovery_1_team',
    'passer_player_name',
    'passer_player_id',
    'receiver_player_name',
    'receiver_player_id',
    'rusher_player_name',
    'rusher_player_id',
    'td_player_name',
    'td_player_id',
    'kicker_player_name',
    'kicker_player_id',
    'kickoff_returner_player_name',
    'kickoff_returner_player_id',
    'punt_returner_player_name',
    'punt_returner_player_id',
    'fumbled_1_player_name',
    'fumbled_1_player_id',
    'fumble_recovery_1_player_name',
    'fumble_recovery_1_player_id',
    'sack',
    'sack_player_name',
    'sack_player_id',
    'half_sack_1_player_name',
    'half_sack_1_player_id',
    'half_sack_2_player_name',
    'half_sack_2_player_id',
    'safety',
    'safety_player_name',
    'safety_player_id',
    'two_point_conv_result',
    'two_point_attempt',
    'extra_point_result',
    'extra_point_attempt',
    'field_goal_result',
    'field_goal_attempt',
    'kick_distance',
    'blocked_player_name',
    'blocked_player_id'
  )
  dt[, .SD, .SDcol=cols]
}

get_bonus_stats <- function(pbp_dt, 
                            player_data = dt_roster,
                            team_data = dt_nfl_teams){
  # create a list to hold each unique fantasy football points dataset
  player <- list()
  
  # offensive bonus for touchdown with pass over 40 yards for qb
  player[["40yd_pass_td_qb_bonus"]] <- pbp_dt[
    pass_touchdown == 1L & passing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = passer_player_name, player_id = passer_player_id),
    list(stat_label = "40yd_pass_td_qb_bonus", football_value = .N, fantasy_points=.N*2L)
  ]
  
  player[["40yd_pass_td_receiver_bonus"]] <- pbp_dt[
    pass_touchdown == 1L & passing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = receiver_player_name, player_id = receiver_player_id),
    list(stat_label = "40yd_pass_td_receiver_bonus", football_value = .N, fantasy_points=.N*2L)
  ]
  
  # offensive bonus for touchdown with rush over 40 yards for qb
  player[["40yd_rush_td_bonus"]] <- pbp_dt[
    rush_touchdown == 1L & rushing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = rusher_player_name, player_id = rusher_player_id),
    list(stat_label = "40yd_rush_td_bonus", football_value = .N, fantasy_points=.N*2L)
  ]
  
  # player bonus for returning a td
  # only for normal possession plays by the opposite team (ie. pass or rush)
  # in a kickoff, the receiving team is listed as the posteam
  # in a punt, the receiving team is listed as the defteam
  tmp <- rbindlist(list(
    pbp_dt[
      play_type == "kickoff" & !is.na(kickoff_returner_player_name) & return_touchdown == 1L & return_yards >= 40, 
      by = .(week,
             season_type, 
             team_abbr = posteam,
             player = kickoff_returner_player_name,
             player_id = kickoff_returner_player_id),
      list(stat_label = "40yd_return_td_bonus", football_value = .N, fantasy_points=.N*2L)
    ],
    pbp_dt[
      play_type == "punt" & !is.na(punt_returner_player_name) & return_touchdown == 1L & return_yards >= 40, 
      by = .(week,
             season_type, 
             team_abbr = defteam,
             player = punt_returner_player_name,
             player_id = punt_returner_player_id),
      list(stat_label = "40yd_return_td_bonus", football_value = .N, fantasy_points=.N*2L)
    ]
  )) 
  player[["40yd_return_td_bonus"]] <- tmp[,by = .(week,season_type,team_abbr,player,player_id,stat_label),
      list(football_value = sum(football_value), fantasy_points = sum(fantasy_points))]
  
  player <- rbindlist(player)
  
  player <- merge(player, player_data[,.(player_id, player_name, team_abbr, position)], all.x = TRUE, by = c("player_id", "team_abbr"))
  
  player <- merge(player, team_data[,.(team_abbr, team_conf, team_division)], all.x = TRUE, by = c("team_abbr"))
  
  player <-
    player[, .(
      team_abbr,
      team_conf,
      team_division,
      position,
      week,
      season_type, 
      player_id,
      player_name,
      stat_label,
      football_value,
      fantasy_points
    )]
  
  setorder(player, cols = week, position)
  
  return(player)

}

get_defense_stats <- function(pbp_dt, team_data = dt_nfl_teams){
  # create a list to hold each unique fantasy football points dataset
  def <- list()
  
  ## defensive bonus for sacks
  # If you want to exclude sack where the QB got back to the line of scrimmage, then add filter 
  # condition of yards_gained < 0L. There are some instances where the sack_player is not recorded 
  # but there was still a sack recorded (seemingly if a fumble happens in the same play)
  def[["def_sack"]] <- pbp_dt[
    sack == 1L,
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_sack", football_value = .N, fantasy_points = .N*1L)
  ]
  
  # defensive bonus for safeties
  def[["def_safety"]] <- pbp_dt[
    safety == 1L & !is.na(safety_player_id),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_safety", football_value = .N, fantasy_points = .N*1L)
  ]
  
  # defensive bonus for fumble recovery
  def[["def_fumble_recovery"]] <- pbp_dt[
    fumble == 1L & fumble_lost == 1L & play_type != "punt",
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_fumble_recovery", football_value = .N, fantasy_points = .N*2L)
  ]
  
  # defensive bonus for fumble recovery for a punt
  # punts start with the receiving team listed as defteam, so those may need special consideration
  def[["def_fumble_recovery_punt"]] <- pbp_dt[
    fumble == 1L & fumble_lost == 1L & play_type == "punt",
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_fumble_recovery_punt", football_value = .N, fantasy_points = .N*2L)
  ]
  
  # defensive bonus for interceptions
  def[["def_interception"]] <- pbp_dt[
    interception == 1L,
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_interception", football_value = .N, fantasy_points = .N*2L)
  ]
  
  # def bonus for blocks on punt, fg or extra point
  def[["def_block"]] <- pbp_dt[
    !is.na(blocked_player_name),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_block", football_value = .N, fantasy_points = .N*2L)
  ]
  
  # def bonus for def td for any reason or cause (block, fumble, interception, etc)
  # only for normal possession plays by the opposite team (ie. pass or rush)
  def[["def_td"]] <- pbp_dt[
    return_touchdown == 1L & play_type %in% c("pass", "run"),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_td", football_value = .N, fantasy_points = .N*6L)
  ]
  
  # special teams bonus for a return td
  # in a kickoff, the kicking team is listed as the defteam
  def[["def_kickoff_return_td"]] <- pbp_dt[
    return_touchdown == 1L & play_type %in% c("kickoff"),
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_kickoff_return_td", football_value = .N, fantasy_points = .N*6L)
  ]
  
  # special teams bonus for a return td
  # in a punt, the receiving team is listed as the defteam
  def[["def_punt_return_td"]] <- pbp_dt[
    return_touchdown == 1L & play_type %in% c("punt"),
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_punt_return_td", football_value = .N, fantasy_points = .N*6L)
  ]
  
  # calculate points allowed for each team
  tmp <- rbindlist(list(
    unique(pbp_dt[,.(week, season_type, team_abbr = home_team, football_value = away_score)]),
    unique(pbp_dt[,.(week, season_type, team_abbr = away_team, football_value = home_score)])
  ))
  tmp[, stat_label := "def_points_allowed"]
  tmp <- tmp[,.(week, season_type, team_abbr, stat_label, football_value)]
  def[["def_points_allowed"]] <- tmp[, 
    fantasy_points := case_when(
      football_value == 0L ~ 10L,
      football_value >= 1L & football_value <= 6 ~ 7L,
      football_value >= 7L & football_value <= 13 ~ 4L,
      football_value >= 14L & football_value <= 17 ~ 1L,
      football_value >= 18L & football_value <= 21 ~ 0L,
      football_value >= 22L & football_value <= 27 ~ -1L,
      football_value >= 28L & football_value <= 34 ~ -4L,
      football_value >= 35L & football_value <= 45 ~ -7L,
      football_value >= 46L ~ -10L,
      .default = 0L
    )
  ]
  
  def <- rbindlist(def)
  def[, position:="Defense"]
  def[, player_id:="N/A"]
  
  def[,.('position','week','player_id','team_abbr')]
  
  def <- merge(def, team_data[,.(team_abbr, player_name = team_name, team_conf, team_division)], all.x = TRUE)
  
  def <-
    def[, .(
      team_abbr,
      team_conf,
      team_division,
      position,
      week,
      season_type, 
      player_id,
      player_name,
      stat_label,
      football_value,
      fantasy_points
    )]
  
  setorder(def, cols = week, position)
  
  return(def)
  
}


get_player_stats <- function(stat_type_char, # either 'offense' or 'kicking'
                             season_year_int = season_year, 
                             season_type_char = season_type, 
                             season_teams_list = season_teams,
                             team_data = dt_nfl_teams){
  # create data.table for players, which is a combination of the offensive scorers plus kickers
  dt <- data.table::as.data.table(nflreadr::load_player_stats(seasons = season_year_int, stat_type = stat_type_char))
  dt <- dt[season_type %in% season_type_char]
  dt[,season_type := if_else(season_type=="REG","Regular", if_else(season_type=="POST","Post", "Error"))]
  
  if(stat_type_char == 'offense'){setnames(dt, old=c('recent_team'), new=c('team_abbr'))} 
  if(stat_type_char == 'kicking'){setnames(dt, old=c('team'), new=c('team_abbr'))}
  dt <- dt[team_abbr %in% season_teams_list]
  
  if(stat_type_char=='offense'){
    dt <- dt[position %in% c('QB', 'RB', 'FB', 'WR', 'TE')]
    dt[,position := if_else(position == 'FB', 'RB', position)]
  }
  if(stat_type_char=='kicking'){dt[,position := 'K']}# position is not in the original dataset
  
  if(stat_type_char=='offense'){
    # consolidate fumbles lost and 2pt conversions into one statistic
    dt[,fumbles_lost := sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost]
    dt[,two_pt_conversions := passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions]
  }
  if(stat_type_char=='kicking'){dt[,fg_made_50_ := fg_made_50_59 + fg_made_60_]}
  
  standard_cols <- c(
    'position',
    'week',
    'season_type',
    'player_id',
    'player_name',
    'team_abbr'
  )
  
  if(stat_type_char=='offense'){
    stat_cols <- c(
      'passing_yards',
      'passing_tds',
      'rushing_yards',
      'rushing_tds',
      'receiving_yards',
      'receiving_tds',
      'interceptions',
      'sacks',
      'fumbles_lost',
      'two_pt_conversions'
    ) 
  } else if(stat_type_char=='kicking'){
    stat_cols <- c(
      'fg_made',
      'fg_made_40_49',
      'fg_made_50_',
      'fg_missed',
      'fg_blocked',
      'pat_made',
      'pat_missed'
    ) 
  }
  dt <- dt[, .SD, .SDcols = c(standard_cols, stat_cols)] # order cols
  
  # change data types to double prior to melting
  dt[,c(stat_cols) := lapply(.SD, as.numeric), .SDcols=stat_cols]
  
  # melt into long format
  dt <- melt(
    dt,
    id.vars = standard_cols,
    measure.vars = stat_cols,
    variable.factor = FALSE,
    variable.name = 'stat_label',
    value.name = 'football_value'
  )
  
  # calculate fantasy football points
  dt[,fantasy_points := case_when(
    stat_label == 'passing_yards' & football_value >= 400 ~ as.integer(football_value/50) + 2L,
    stat_label == 'passing_yards' & football_value < 400 ~ as.integer(football_value/50),
    stat_label == 'rushing_yards' & football_value >= 200 ~ as.integer(football_value/10L) + 2L,
    stat_label == 'rushing_yards' & football_value < 200 ~ as.integer(football_value/10L),
    stat_label == 'receiving_yards' & football_value >= 200 ~ as.integer(football_value/10L) + 2L,
    stat_label == 'receiving_yards' & football_value < 200 ~ as.integer(football_value/10L),
    stat_label %in% c('passing_tds', 'rushing_tds','receiving_tds') ~ as.integer(football_value) * 6L,
    stat_label %in% c('passing_2pt_conversions', 'rushing_2pt_conversions','receiving_2pt_conversions') ~ as.integer(football_value) * 2L,
    stat_label == 'interceptions' ~ as.integer(football_value) * -2L,
    stat_label %in% c('sack_fumbles_lost', 'rushing_fumbles_lost', 'receiving_fumbles_lost') ~ as.integer(football_value) * -2L,
    stat_label == 'fg_made' ~ as.integer(football_value) * 3L,
    stat_label == 'fg_made_40_49' ~ as.integer(football_value) * 1L, # this is a bonus
    stat_label == 'fg_made_50_' ~ as.integer(football_value) * 2L, # this is a bonus
    stat_label == 'fg_missed' ~ as.integer(football_value) * -1L,
    stat_label == 'pat_made' ~ as.integer(football_value) * 1L,
    stat_label == 'pat_missed' ~ as.integer(football_value) * -1L,
    .default = 0L
  )
  ]
  
  dt <- merge(dt, team_data[,.(team_abbr, team_conf, team_division)], all.x = TRUE)
  
  dt <-
    dt[, .(
      team_abbr,
      team_conf,
      team_division,
      position,
      week,
      season_type,
      player_id,
      player_name,
      stat_label,
      football_value,
      fantasy_points
    )]
  
  return(dt)
  
}


get_combined_stats <- function(teams = dt_nfl_teams, playoff_teams = season_teams){
  
  # bind rows
  dt <- rbindlist(list(
    get_player_stats(stat_type_char='offense'), 
    get_player_stats(stat_type_char='kicking'),
    get_bonus_stats(get_pbp()),
    get_defense_stats(get_pbp())
  ))
  
  # reapply playoff teams filter since defensive / special teams are scored for both home and away teams depending in play type
  dt <- dt[team_abbr %in% playoff_teams]
  
  # create the lookup_string used in the dashboard filters
  dt[,lookup_string := paste0(position,', ',team_abbr,': ',player_name,' (',team_division,', ID: ',player_id,')')]
  
  # sort columns
  setorder(dt, cols = position, player_name, week)
  
  # arrange columns
  setcolorder(
    dt,
    c(
      'position',
      'week',
      'season_type',
      'lookup_string',
      'player_id',
      'player_name',
      'team_abbr',
      'team_conf',
      'team_division'
    )
  )
}

get_shiny_stats <- function(dt, pos, type, summarized_boolean, long_format_boolean){
  
  if(!(pos %in% c("K","QB","RB","TE","WR","Defense"))){
    print(paste0(pos, " is not a valid position"))
  }
  
  dt <- dt[position == pos]
  dt <- dt[season_type == type]
  
  if(summarized_boolean){
    
    grouping_by <- c(
      'season_type',
      'position',
      'lookup_string',
      'player_id',
      'player_name',
      'team_abbr',
      'team_conf',
      'team_division',
      'stat_label'
    )
    
    dt <- dt[, week:=NULL]
    
    dt <- dt[, by = grouping_by, 
             .(football_value = sum(football_value), fantasy_points = sum(fantasy_points))
    ]
    
    dt[, stat_label := paste0("total_",stat_label)]
  }
  
  if(long_format_boolean){
    # do nothing
  } else {
    
    # cast wider
    if(summarized_boolean){
      # does not include the `week` variable when summarized
      dt <- dcast(
        dt,
        season_type + position + lookup_string + player_id + player_name + team_abbr + team_conf + team_division ~ stat_label,
        value.var = c('football_value', 'fantasy_points'),
        fill = 0
      )   
    } else {
      dt <- dcast(
        dt,
        season_type + position + week + lookup_string + player_id + player_name + team_abbr + team_conf + team_division ~ stat_label,
        value.var = c('football_value', 'fantasy_points'),
        fill = 0
      )
    }
  }
  
  return(dt)
  
}

order_cols <- function(dt){
  
  master_order <- c(
    'position',
    'lookup_string',
    'week',
    'season_type',
    'player_id',
    'player_name',
    'team_abbr',
    'team_conf',
    'team_division',
    'passing_yards',
    'passing_tds',
    'rushing_yards',
    'rushing_tds',
    'receiving_yards',
    'receiving_tds',
    'interceptions',
    'sacks',
    'fumbles_lost',
    'two_pt_conversions',
    'fg_made',
    'fg_made_40_49',
    'fg_made_50_',
    'fg_missed',
    'fg_missed_list',
    'fg_blocked',
    'fg_blocked_list',
    'pat_made',
    'pat_missed',
    'stat_label',
    'football_value',
    'fantasy_points',
    'football_value_total_passing_tds',
    'fantasy_points_total_passing_tds',
    'football_value_total_receiving_tds',
    'fantasy_points_total_receiving_tds',
    'football_value_total_rushing_tds',
    'fantasy_points_total_rushing_tds',
    'football_value_total_passing_yards',
    'fantasy_points_total_passing_yards',
    'football_value_total_receiving_yards',
    'fantasy_points_total_receiving_yards',
    'football_value_total_rushing_yards',
    'fantasy_points_total_rushing_yards',
    'football_value_total_fumbles_lost',
    'fantasy_points_total_fumbles_lost',
    'football_value_total_interceptions',
    'fantasy_points_total_interceptions',
    'football_value_total_sacks',
    'fantasy_points_total_sacks',
    'football_value_total_two_pt_conversions',
    'fantasy_points_total_two_pt_conversions',
    'football_value_total_fg_made',
    'fantasy_points_total_fg_made',
    'football_value_total_fg_made_40_49',
    'fantasy_points_total_fg_made_40_49',
    'football_value_total_fg_made_50_',
    'fantasy_points_total_fg_made_50_',
    'football_value_total_fg_missed',
    'fantasy_points_total_fg_missed',
    'football_value_total_fg_blocked',
    'fantasy_points_total_fg_blocked',
    'football_value_total_pat_made',
    'fantasy_points_total_pat_made',
    'football_value_total_pat_missed',
    'fantasy_points_total_pat_missed',
    'football_value_passing_tds',
    'fantasy_points_passing_tds',
    'football_value_receiving_tds',
    'fantasy_points_receiving_tds',
    'football_value_rushing_tds',
    'fantasy_points_rushing_tds',
    'football_value_passing_yards',
    'fantasy_points_passing_yards',
    'football_value_rushing_yards',
    'fantasy_points_rushing_yards',
    'football_value_receiving_yards',
    'fantasy_points_receiving_yards',
    'football_value_fumbles_lost',
    'fantasy_points_fumbles_lost',
    'football_value_interceptions',
    'fantasy_points_interceptions',
    'football_value_sacks',
    'fantasy_points_sacks',
    'football_value_two_pt_conversions',
    'fantasy_points_two_pt_conversions',
    'football_value_fg_made',
    'fantasy_points_fg_made',
    'football_value_fg_made_40_49',
    'fantasy_points_fg_made_40_49',
    'football_value_fg_made_50_',
    'fantasy_points_fg_made_50_',
    'football_value_fg_missed',
    'fantasy_points_fg_missed',
    'football_value_fg_blocked',
    'fantasy_points_fg_blocked',
    'football_value_pat_made',
    'fantasy_points_pat_made',
    'football_value_pat_missed',
    'fantasy_points_pat_missed',
    'football_value_total_40yd_pass_td_qb_bonus',
    'fantasy_points_total_40yd_pass_td_qb_bonus',
    'football_value_total_def_block',
    'fantasy_points_total_def_block',
    'football_value_total_def_points_allowed',
    'fantasy_points_total_def_points_allowed',
    'football_value_total_def_td',
    'fantasy_points_total_def_td',
    'football_value_total_def_fumble_recovery',
    'fantasy_points_total_def_fumble_recovery',
    'football_value_total_def_fumble_recovery_punt',
    'fantasy_points_total_def_fumble_recovery_punt',
    'football_value_total_def_interception',
    'fantasy_points_total_def_interception',
    'football_value_total_def_kickoff_return_td',
    'fantasy_points_total_def_kickoff_return_td',
    'football_value_total_def_punt_return_td',
    'fantasy_points_total_def_punt_return_td',
    'football_value_total_def_sack',
    'fantasy_points_total_def_sack',
    'football_value_total_def_safety',
    'fantasy_points_total_def_safety',
    'football_value_total_40yd_pass_td_receiver_bonus',
    'fantasy_points_total_40yd_pass_td_receiver_bonus',
		'football_value_total_40yd_return_td_bonus',
		'fantasy_points_total_40yd_return_td_bonus',
		'football_value_40yd_return_td_bonus',
		'fantasy_points_40yd_return_td_bonus',
		'football_value_40yd_rush_td_bonus',
		'fantasy_points_40yd_rush_td_bonus',
		'football_value_40yd_pass_td_receiver_bonus',
		'fantasy_points_40yd_pass_td_receiver_bonus',
		'football_value_total_40yd_rush_td_bonus',
		'fantasy_points_total_40yd_rush_td_bonus',
		'football_value_40yd_pass_td_qb_bonus',
		'fantasy_points_40yd_pass_td_qb_bonus',
		'football_value_def_block',
		'fantasy_points_def_block',
		'football_value_def_fumble_recovery',
		'fantasy_points_def_fumble_recovery',
		'football_value_def_fumble_recovery_punt',
		'fantasy_points_def_fumble_recovery_punt',
		'football_value_def_interception',
		'fantasy_points_def_interception',
		'football_value_def_kickoff_return_td',
		'fantasy_points_def_kickoff_return_td',
		'football_value_def_points_allowed',
		'fantasy_points_def_points_allowed',
		'football_value_def_punt_return_td',
		'fantasy_points_def_punt_return_td',
		'football_value_def_sack',
		'fantasy_points_def_sack',
		'football_value_def_safety',
		'fantasy_points_def_safety', 
		'football_value_def_td',
		'fantasy_points_def_td'
  )
  
  if(any(duplicated(master_order))){
    print("There are duplicated columns in the master_order")
    print(paste0(master_order[duplicated(master_order)], collapse = "; "))
  }  
  
  found_order <- names(dt)
  
  unmapped_cols <- found_order[!(found_order %in% master_order)]
  
  if(length(unmapped_cols)){
    print("There are unmapped columns in the dataset")
    print(paste0(unmapped_cols, collapse = "; "))
  }
  
  preferred_order <- master_order[master_order %in% found_order]
  
  return(dt[,..preferred_order])
  
}

count_positions <- function(x){
  position_counts <- c(NULL)
  position_tmp <- c(NULL)
  for(a in x){
    if(a %in% c("K","Defense")){
      position_counts <- c(position_counts,a)
    } else if((a == "RB" & sum(position_tmp==a)==3L) |
              (a == "TE" & sum(position_tmp==a)==2L) |
              (a == "WR" & sum(position_tmp==a)==3L)
    ){
      position_tmp <- c(position_tmp,a)
      position_counts <- c(position_counts,paste0("FLEX (",a,")"))
    } else {
      position_counts <- c(position_counts,paste0(a,sum(position_tmp==a)+1L))
      position_tmp <- c(position_tmp,a)
    }
  }
  return(position_counts)
}

# TODO currently I do not have functionality set up for team points calculated on pbp data
# create data.table for play-by-play data for scoring defensive points for each team
# dt_nfl_team_stats <- data.table::as.data.table(nflfastR::load_pbp(seasons = season_year))
# dt_nfl_team_stats[season_type %in% season_type]

# create data.table for players, which is a combination of the offensive scorers plus kickers
dt_nfl_player_stats <- get_combined_stats()

# remove zero value statistics
dt_nfl_player_stats <- dt_nfl_player_stats[abs(fantasy_points) >= 1e-7 | abs(football_value) >= 1e-7]

# get a list of unique players for the lookup
dt_lookup <- unique(get_combined_stats()[,.(position, lookup_string, team_abbr)], by=c('lookup_string'))
dt_lookup <- setorder(dt_lookup, cols = position, team_abbr, lookup_string)


dt_nfl_teams[,position:="Defense"]
dt_nfl_teams[,lookup_string:=paste0(position,", ",team_abbr," (",team_division,")")]

team_lookupstring_position <- rbindlist(list(dt_lookup, dt_nfl_teams[,.(position, lookup_string, team_abbr)]))

roster_choices <- team_lookupstring_position %>% distinct(lookup_string) %>% as.list()
def_teams_choices <- dt_nfl_teams %>% distinct(team_abbr) %>% as.list()


ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("sandstone"),
  # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/bootstrap.min.css")),
  titlePanel("Playoff Fantasy Football"),
  tabsetPanel(
    tabPanel(
      "How to Play",
      fluidPage(
        tags$h2("Game Overview"),
        tags$p("Playoff Fantasy Football is an elimination based version of Fantasy Football:"),
        tags$ul(
          tags$li("Each contestant will create a diversified roster prior to the start of playoffs."),
          tags$ul(
            tags$li("Your roster must include one player from each of the 14 teams in the playoffs."),
            tags$li("Your roster must include:"),
              tags$ul(
                tags$li("1 Kicker (K)"),
                tags$li("3 Quaterbacks (QB)"),
                tags$li("3 Running Backs (RB)"),
                tags$li("3 Wide Receivers (WR)"),
                tags$li("2 Tight Ends (TE)"),
                tags$li("1 Flex Position (either RB, WR or TE)"),
                tags$li("1 Defense / Special Teams.")
              ),
          ),
          tags$li("The roster will be locked from changes after submission to the Commissioner."),
          tags$ul(
            tags$li("Rosters must be submitted, valid, and paid for by kickoff of the first wildcard game (1pm Saturday, January 13th, 2023)."),
            tags$li("Late rosters will not be accepted."),
            tags$li("Multiple rosters are allowed per Owner, as long as each are paid for.")
          ),
          tags$li("Each week, as teams are eliminated from the playoffs, so does the pool of potential players on your roster who can score points."),
          tags$ul(
            tags$li("Therefore, your overall roster success is as dependent on each player's longevity in the playoffs as much as it is on the player's performance.")
          ),
          tags$li("Fantasy scoring is calculated based on each player's performance during a game."),
          tags$li("The types of statistics converted into Fantasy points is consistent with typical scoring rules (see details below)"),
          tags$li("Points are cumulative throughout the playoffs (including wildcard games and Super Bowl)."),
          tags$li("The person with the most points at the end of the playoffs wins the grand prize."),
          tags$li("Prizes will be awarded to the top 5 scoring entries."),
          tags$li("Prize purse will be announced after wildcard playoff weekend, since prize purse is dependent on the number of entries."),
          tags$li("If you think you're going to win, spread the word: The more participants, the larger the prizes."),
          tags$li("If you think you're going to lose, spread the word: Imagine the commaraderie of shared experience!"),
          tags$li("The Commissioner will (probably) provide weekly updates on Fantasy Team standings throughout the contest. Final summary of scoring and standings will be provided.")
        ),
        tags$h2("How To Use this Dashboard"),
        tags$p("You can use this dashboard to explore player statistics and create your roster:"),
        tags$ul(
          tags$li("Regular season statistics are available on the 'Explore 2023 Stats' tab, which may help provide insights on each player you should prioritize. Statistics are available in 'football values' and in 'fantasy points'."),
          tags$li("Use the 'Select Roster' tab on this dashboard to start creating your roster."),
          tags$li("Add players to your roster based on the combination you think will score the most points by the end of the Superbowl."),
          tags$li("When a player is added to your roster, the team associated with that player (and any of its remaining players) will be removed from your next possible selections. For example: if you pick Jalen Hurts as one of your quarterbacks, you no longer be able to select an Eagles player on your roster."),
          tags$li("When you've satisified the maximum number of positions on your roster, any player associated with that poisiton will be removed from your next possible selection. For example: if you pick Jalen Hurts as your third (and last) quarterback, you no longer be able to select a quarterback."),
          tags$li("As needed, you can remove players from your team, which will release that Team and/or Position as a next possible selection."),
          tags$li("You must include your Name, Email and Fantasy Team Name in the Participant Information Box. Don't forget to confirm that you've paid the Commish."),
          tags$li("The roster can only be downloaded after all parameters have been satisfied (that is, a completed roster of 14 players and the Participant Information box is filled in with valid information)."),
          tags$li("You must still email the commissioner your roster downloaded from this website. This website does not save your roster.", style="color:red; font-weight:bold;"),
        ),
        tags$h2("Alternate Roster in Excel"),
        tags$p("The email sent to you by the Commissioner should contain an Excel file that is equivalent to this dashboard. If you prefer, you can complete that roster template and email the Excel file back to the Commissioner. I don't know why you would do this, but technically it is possible."),
        tags$h2("Scoring"),
        tags$h4("Passing"),
        tags$ul(
          tags$li("TD Pass = 6 points"),
          tags$li("Every 50 passing yards = 1 point"),
          tags$li("400+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Passing TD Bonus = 2 points"),
          tags$li("2pt Passing Conversion = 2 points"),
          tags$li("Interception Thrown = -2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        tags$h4("Rushing"),
        tags$ul(
          tags$li("TD Rush = 6 points"),
          tags$li("Every 10 rushing yards = 1 point"),
          tags$li("200+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Passing TD Bonus = 2 points"),
          tags$li("2pt rushing Conversion = 2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        tags$h4("Receiving"),
        tags$ul(
          tags$li("TD Receiving = 6 points"),
          tags$li("Every 10 receiving yards = 1 point"),
          tags$li("200+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Receiving TD Bonus = 2 points"),
          tags$li("2pt rushing Conversion = 2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        tags$h4("Kicking"),
        tags$ul(
          tags$li("PAT Made = 1 point"),
          tags$li("PAT Missed = -1 point"),
          tags$li("FG Made = 3 points"),
          tags$li("FG Made (40-49 yards) Bonus = 1 point"),
          tags$li("FG Made (50+ yards) Bonus = 2 points"),
          tags$li("FG Missed = -1 point"),
        ),
        tags$h4("Defense / Special Teams"),
        tags$ul(
          tags$li("Each Sack = 1 point"),
          tags$li("Each Interception = 2 points"),
          tags$li("Each Safety = 2 points"),
          tags$li("Each Fumble Recovery = 2 points"),
          tags$li("Each Blocked Punt/PAT/FG = 2 points"),
          tags$li("Interception Return TD = 6 points"),
          tags$li("Fumble Return TD = 6 points"),
          tags$li("Kickoff Return TD = 6 points"),
          tags$li("Punt Return TD = 6 points"),
          tags$li("Blocked Punt or FG Return TD = 6 points"),
          tags$li("0 Points Allowed = 10 points"),
          tags$li("1-6 Points Allowed = 7 points"),
          tags$li("7-13 Points Allowed = 4 points"),
          tags$li("14-17 Points Allowed = 1 points"),
          tags$li("18-21 Points Allowed = 0 points"),
          tags$li("22-27 Points Allowed = -1 points"),
          tags$li("28-34 Points Allowed = -4 points"),
          tags$li("35-45 Points Allowed = -7 points"),
          tags$li("46+ Points Allowed = -10 points"),
        )
      )
    ),
    tabPanel(
      "Select Roster",
      actionButton(
        inputId = "toggleRosterSelector", 
        label = "Toggle Roster Selector",
        icon = icon("bars"),
        style = "margin-top:3px; margin-bottom:3px"
      ),
      sidebarLayout(
        div(id = "rosterSelector",
          sidebarPanel(
            selectizeInput(
              inputId = "roster_selections_made",
              label = "Select Player or Defensive Team",
              choices = roster_choices,
              options = list(maxItems = 1)
            ),
            actionButton(
              inputId = "add_player",
              label = "Add to Roster",
              icon = icon("add"),
              style="color: white; background-color: #0086b3; border-color: #2e6da4"
            ),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "roster_slots_remaining_text"),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "positions_available_text"),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "teams_available_text"),
            tags$h1("", style = 'margin:100px'),
            selectizeInput(
              inputId = "roster_selections_removed",
              label = "Remove Player or Defensive Team",
              choices = NULL,
              options = list(maxItems = 1),
            ),
            actionButton(
              inputId = "remove_player",
              label = "Remove",
              icon = icon("trash", lib = "glyphicon"),
              style="color: white; background-color: gray; border-color: black"
            ),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "positions_on_roster_text"),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "teams_on_roster_text"),
            tags$p("", style='margin-bottom:25px'),
            fluidPage(
              tags$p("Participant Information", style='font-weight:bold; margin-bottom:0px;', inline=TRUE),
              tags$p("* required", style = "color:red; margin-top:3px", inline=TRUE),
              textInput("fantasy_owner_name", label = "Name *", placeholder = "Dick Butkus"),
              textInput("fantasy_owner_email", label = "Email *", placeholder = "myemail@gmail.com"),
              textInput("fantasy_team_name", label = "Fantasy Team Name *", placeholder = "Unique Team Name"),
              checkboxInput("paid_checkbox", label = "I have paid the Commish because I am not a delinquent *"),
              tags$p("Note: Fantasy Team Name will be displayed in rankings", style='margin-top:20px'),
              style = 'background-color:#ffffc2; border-style:solid; border-color:black;'
            ),
            tags$p("", style='margin-bottom:20px'),
            downloadButton(
              outputId = "download_roster", 
              label = "Download Roster",
              style = "color: white; background-color: #F62817;"
            ),
            tags$p("Don't forget to email your roster to the Commish!"),
            width = 4
          )
        ),
        mainPanel(
          fluidRow(
            tags$h3("Current Roster"),
            DTOutput(outputId = "players_on_roster_DT"),
            style="margin-left:2px"
          ),
          fluidRow(
            tags$h3("Valid Player Selections Remaining", style="margin-top:100px"),
            DTOutput(outputId = "players_remaining_DT"),
            style="margin-left:2px"
          )
        )
      )
    ),
    tabPanel(
      "Explore Stats",
      actionButton(
        inputId = "toggleFilterOptions", 
        label = "Toggle Filter Options",
        icon = icon("bars"),
        style = "margin-top:3px; margin-bottom:3px"
      ),
      sidebarLayout(
        div(id = "filterOptions",
          sidebarPanel(
            # this is a single select way to provide positions for the DT table
            selectInput(
              inputId = "selected_position",
              label = "Inspect a Position:",
              choices = list("QB", "RB", "WR", "TE", "K", "Defense"),
              selected = "QB"
            ),
            selectInput(
              inputId = "reg_or_post",
              label = "Regular or Post Season:",
              choices = list("Regular","Post"),
              selected = "Regular"
            ),
            selectInput(
              inputId = "stat_type",
              label = "Statistic Type:",
              choices = list("Football Values", "Fantasy Points", "Both"),
              selected = "Football Value"
            ),
            tags$p("Inspect Team(s)", style = "font-weight:bold; margin-top:40px"),
            actionButton("select_all_teams", label="All", inline=TRUE),
            actionButton("deselect_all_teams", label="None", inline=TRUE),
            checkboxGroupInput(
              "selected_teams",
              label = "",
              choiceNames = as.list(dt_nfl_teams$team_name_w_abbr),
              choiceValues = as.list(dt_nfl_teams$team_abbr),
              selected = as.list(dt_nfl_teams$team_abbr)
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(paste0(season_year," Season Totals"), br(), DTOutput("statistics_season")),
            tabPanel(paste0(season_year," by Week"), br(), DTOutput("statistics_weekly"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ## this section is for stats exploration
  observeEvent(input$toggleFilterOptions, {
    shinyjs::toggle(id = "filterOptions")
  })
  
  
  stats <- reactive({
    data.table(
      'pos' = input$selected_position,
      'type' = input$reg_or_post
    )
  })

  output$statistics_weekly <- renderDT({
    player_stats <- get_shiny_stats(
      dt_nfl_player_stats, 
      stats()$pos, 
      stats()$type,
      summarized_boolean = FALSE, 
      long_format_boolean = FALSE
    ) 
    
    player_stats <- order_cols(player_stats[team_abbr %in% input$selected_teams])
    if(input$stat_type == "Football Values"){
      cols <- c('position','lookup_string', 'week', 'player_id', 'player_name',
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^football_value")])
      return(player_stats[, .SD, .SDcols = cols])
    } else if (input$stat_type == "Fantasy Points"){
      cols <- c('position','lookup_string', 'week', 'player_id', 'player_name',
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^fantasy_points")])
      return(player_stats[, .SD, .SDcols = cols])
    } else {
      return(player_stats)
    }
  })
  
  output$statistics_season <- renderDT({
    player_stats <- get_shiny_stats(
      dt_nfl_player_stats, 
      stats()$pos,
      stats()$type,
      summarized_boolean = TRUE, 
      long_format_boolean = FALSE
    ) %>% order_cols()
    
    player_stats <- order_cols(player_stats[team_abbr %in% input$selected_teams])
    if(input$stat_type == "Football Values"){
      cols <- c('position','lookup_string', 'player_id', 'player_name', 
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^football_value")])
      return(player_stats[, .SD, .SDcols = cols])
    } else if (input$stat_type == "Fantasy Points"){
      cols <- c('position','lookup_string', 'player_id', 'player_name', 
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^fantasy_points")])
      return(player_stats[, .SD, .SDcols = cols])
    } else {
      return(player_stats)
    }
  })
  
  observeEvent(
    input$select_all_teams, {
    updateCheckboxGroupInput(
      session,
      "selected_teams",
      label = "",
      choiceNames = as.list(dt_nfl_teams$team_name_w_abbr),
      choiceValues = as.list(dt_nfl_teams$team_abbr),
      selected = as.list(dt_nfl_teams$team_abbr)
    )
  })
  
  observeEvent(
    input$deselect_all_teams, {
    updateCheckboxGroupInput(
      session,
      "selected_teams",
      label = "",
      choiceNames = as.list(dt_nfl_teams$team_name_w_abbr),
      choiceValues = as.list(dt_nfl_teams$team_abbr),
      selected = NULL
    )
  })
  
  
  ## this section is for Roster Selection
  
  observeEvent(input$toggleRosterSelector, {
    shinyjs::toggle(id = "rosterSelector")
  })
  
  roster <- reactiveValues(players = c(NULL))
  
  observeEvent(input$add_player,{
      roster$players <- c(roster$players, input$roster_selections_made) %>% sort()
  })
  
  observeEvent(input$remove_player,{
    roster$players <- roster$players[!(roster$players %in% input$roster_selections_removed)]
  })
  
  roster_slots_remaining <- reactive({
    14-length(roster$players)
  }) 
  
  roster_full <- reactive({
    if(length(roster$players) == 14L){
      TRUE
    } else {
      FALSE
    }
  })
  
  output$roster_slots_remaining_text <- renderText({
      paste0("Roster slot(s) remaining: ", roster_slots_remaining(), " of 14")
  })

  
  # keep track of teams selected on the roster
  teams_on_roster <- reactive({
    team_lookupstring_position[lookup_string %in% roster$players, team_abbr] %>% 
      unique() %>% 
      sort()
  })
  output$teams_on_roster_text <- renderText({
    if(is_empty(teams_on_roster())){
      "Teams on roster: None"
    } else {
      paste0("Teams on roster: ", paste0(teams_on_roster(), collapse = ",  "))
    }
  })
  
  # keep track of unselected teams
  teams_available <- reactive({
    team_lookupstring_position[!(team_abbr %in% teams_on_roster()), team_abbr] %>% unique() %>% sort()
  }) 
  output$teams_available_text <- renderText({
    paste0("Teams remaining: ", paste0(teams_available() %>% unlist(), collapse = ",  "))
  })
  
  # keep track of positions on the roster
  positions_selected <- reactive({
    team_lookupstring_position[lookup_string %in% roster$players, position]
  })
  output$positions_on_roster_text <- renderText({
    if(is_empty(positions_selected())){
      "Positions Filled: None"
    } else {
      paste0("Positions Filled: ", paste0(count_positions(positions_selected()) %>% unlist(), collapse = ",  "))
    }
  })
  
  output$positions_available_text <- renderText({
    if(length(positions_selected())==14L){
      "Positions Remaining: None"
    } else {
      all_positions <- c("K","QB1","QB2","QB3","RB1","RB2","RB3","TE1","TE2","WR1","WR2","WR3","FLEX","Defense")
      current_positions <- count_positions(positions_selected())
      current_positions <- str_remove(current_positions," .[:alpha:]{2}.")
      remaining_positions <- all_positions[!(all_positions %in% current_positions)]
      paste0("Positions Remaining: ", paste0(remaining_positions %>% unlist(), collapse = ",  "))
    }
  })
  
  players_remaining <- reactive({

    players_remaining <- team_lookupstring_position %>%
      filter(!(team_abbr %in% teams_on_roster()))
    
    if(length(positions_selected()[positions_selected() == "Defense"])>=1L){
      players_remaining <- players_remaining %>%
        filter(position != "Defense")
    }   
    if(length(positions_selected()[positions_selected() == "K"])>=1L){
      players_remaining <- players_remaining %>%
        filter(position != "K")
    }
    if(length(positions_selected()[positions_selected() == "QB"])>=3L){
      players_remaining <- players_remaining %>%
        filter(position != "QB")
    }
    # for RB, TE and WR, need to consider the flex position when filtering
    if((length(positions_selected()[positions_selected() == "RB"])==3L & 
       (length(positions_selected()[positions_selected() == "TE"])==3L |
        length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "RB"])>=4L)){
      players_remaining <- players_remaining %>%
        filter(position != "RB")
    }
    if((length(positions_selected()[positions_selected() == "TE"])==2L & 
        (length(positions_selected()[positions_selected() == "RB"])==4L |
         length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "TE"])>=3L)){
      players_remaining <- players_remaining %>%
        filter(position != "TE")
    }
    if((length(positions_selected()[positions_selected() == "WR"])==3L & 
        (length(positions_selected()[positions_selected() == "TE"])==3L |
         length(positions_selected()[positions_selected() == "RB"])==4L) )|
       (length(positions_selected()[positions_selected() == "WR"])>=4L)){
      players_remaining <- players_remaining %>%
        filter(position != "WR")
    }
    
    players_remaining <- players_remaining %>% select(position, team_abbr, lookup_string)
    
  })
  
  output$players_on_roster_DT <- renderDT({
    if(is_empty(roster$players)){
      DT::datatable(
        data.table(lookup_string = "Roster is empty"), 
        options = list(pageLength = 25)
      )
    } else {
      DT::datatable(
        team_lookupstring_position[lookup_string %in% roster$players, 
                                   .(position, team_abbr, lookup_string)],
        options = list(pageLength = 25)
      )
    }
  })
  
  output$players_remaining_DT <- renderDT({players_remaining()})
  
  observeEvent(
    input$add_player,{
    updateSelectizeInput(
      session,
      inputId = "roster_selections_made",
      choices = players_remaining()$lookup_string
    )
      
    updateSelectizeInput(
      session,
      inputId = "roster_selections_removed",
      choices = roster$players
    )
  })
  
  observeEvent(
    input$remove_player,{
      updateSelectizeInput(
        session,
        inputId = "roster_selections_made",
        choices = players_remaining()$lookup_string
      )
      
      updateSelectizeInput(
        session,
        inputId = "roster_selections_removed",
        choices = roster$players
      )
    })
  
  observeEvent(
    roster_full(),
    {
      if(roster_full()) {
        shinyjs::disable("add_player")
        
      } else {
        shinyjs::enable("add_player")
      }
    }
  )
  


  # reactive boolean for activating download button
  participant_info <- reactive({
    fantasy_owner_name <- input$fantasy_owner_name
    fantasy_owner_email <- input$fantasy_owner_email
    fantasy_team_name <- input$fantasy_team_name
    paid <- input$paid_checkbox
    data.table("fantasy_owner_name" = fantasy_owner_name, 
      "fantasy_owner_email" = fantasy_owner_email, 
      "fantasy_team_name" = fantasy_team_name,
      "paid_checkbox" = paid)
  })
  
  download_btn_status <- reactive({
    all(
      participant_info()$fantasy_owner_name!="",
      str_detect(participant_info()$fantasy_owner_email,"[:graph:]{3,}@[:alnum:]{1,}\\.[:alnum:]{2,}"),
      participant_info()$fantasy_team_name!="",
      participant_info()$paid,
      length(positions_selected()) == 14L
    )
  })

  observeEvent(
    download_btn_status(),
    {
      if(download_btn_status()) {
        shinyjs::enable("download_roster")

      } else {
        shinyjs::disable("download_roster")
      }
    }
  )
  
  # create final roster for downloadHandler
  roster_data <- reactive({
    team_lookupstring_position %>%
      filter(lookup_string %in% roster$players) %>%
      select(position, team_abbr, lookup_string) %>% 
      mutate(
        `Fantasy Owner` = rep(participant_info()$fantasy_owner_name,14),
        `Fantasy Owner Email` = rep(participant_info()$fantasy_owner_email,14),
        `Fantasy Team Name` = rep(participant_info()$fantasy_team_name,14),
        `Roster` = 1:14,
        `Position Type` = if_else(position == "Defense", "Defense / Special teams", "Player"),
        `Automation Mapping` = if_else(
          position == "Defense", 
          team_abbr, 
          str_remove(str_remove(lookup_string, "^.*, ID: "),"\\)")
        ),
        `Check 1 - Selection is Unique` = TRUE,
        `Check 2 - Team is Unique` = TRUE
      ) %>% 
      group_by(
        position
      ) %>% 
      mutate(
        `Position Code` = if_else(position %in% c("QB","WR","TE","RB"), paste0(position,1:n()), 
                          if_else(position == "Defense", "D", position))
      ) %>% 
      ungroup() %>% 
      rename(
        `Position Group` = position,
        `Team Abbr.` = team_abbr,
        `Selection` = lookup_string
      ) %>%
      mutate(
        `Position Group` = case_when(
          `Position Code` == "K" ~ "SPEC", 
          `Position Code` %in% c("RB4","WR4","TE3") ~ "FLEX", 
          `Position Code` == "D" ~ "D", 
          .default = `Position Group`)
      ) %>% 
      select(
        `Fantasy Owner`,
        `Fantasy Owner Email`,
        `Fantasy Team Name`,
        `Automation Mapping`,
        `Roster`,
        `Position Type`,
        `Position Code`,
        `Position Group`,
        `Team Abbr.`,
        `Selection`,
        `Check 1 - Selection is Unique`,
        `Check 2 - Team is Unique`,
        everything()
      )
  })
  
  output$download_roster <- downloadHandler(
    filename = function() {
      paste0('Playoff Fantasy Roster ',Sys.time(), '.csv')
    },
    content = function(file) {
      write.csv(roster_data(), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)


# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(tidyverse)
library(shiny)
library(data.table)
library(DT)
library(shinyjs)
library(shinythemes)

playoff_year <- 2023L
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


get_team_info <- function(season_year_int = playoff_year){
  # create data.table for NFL teams
  dt <- data.table::as.data.table(nflreadr::load_teams(current = TRUE))
  dt[,team_name_w_abbr := paste0(team_name, ' (', team_abbr, ')')]
  dt <- dt[,.(team_abbr, team_name, team_name_w_abbr, team_conf, team_division, team_logo_espn)]
  dt[,position:="Defense"]
  dt[,lookup_string:=paste0(position,", ",team_abbr," (",team_division,")")]
  return(dt)
}

# create data.table for NFL teams
dt_nfl_teams <- get_team_info()

dt_roster <- data.table::as.data.table(nflreadr::load_rosters(playoff_year))
dt_roster[,player_name:=paste0(str_sub(first_name,1,1),".",last_name)]
dt_roster <- unique(dt_roster[,.(position, player_id = gsis_id, player_name, team_abbr = team)])
dt_roster <- dt_roster[,position:=if_else(position=="FB","RB",position)]
dt_roster <- dt_roster[position %in% c('QB', 'RB', 'WR', 'TE','K')]
dt_roster <- dt_roster[!is.na(position)]
dt_roster <- merge(dt_roster, dt_nfl_teams[,.(team_abbr, team_conf, team_division)], all.x = TRUE, by = c("team_abbr"))
dt_roster[,lookup_string:=paste0(position,', ',team_abbr,': ',player_name,' (',team_division,', ID: ',player_id,')')]

get_pbp <- function(season_year_int = playoff_year,
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
    list(stat_label = "40yd_pass_td_qb_bonus", football_values = .N, fantasy_points=.N*2L)
  ]
  
  player[["40yd_pass_td_receiver_bonus"]] <- pbp_dt[
    pass_touchdown == 1L & passing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = receiver_player_name, player_id = receiver_player_id),
    list(stat_label = "40yd_pass_td_receiver_bonus", football_values = .N, fantasy_points=.N*2L)
  ]
  
  # offensive bonus for touchdown with rush over 40 yards for qb
  player[["40yd_rush_td_bonus"]] <- pbp_dt[
    rush_touchdown == 1L & rushing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = rusher_player_name, player_id = rusher_player_id),
    list(stat_label = "40yd_rush_td_bonus", football_values = .N, fantasy_points=.N*2L)
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
      list(stat_label = "40yd_return_td_bonus", football_values = .N, fantasy_points=.N*2L)
    ],
    pbp_dt[
      play_type == "punt" & !is.na(punt_returner_player_name) & return_touchdown == 1L & return_yards >= 40, 
      by = .(week,
             season_type, 
             team_abbr = defteam,
             player = punt_returner_player_name,
             player_id = punt_returner_player_id),
      list(stat_label = "40yd_return_td_bonus", football_values = .N, fantasy_points=.N*2L)
    ]
  )) 
  player[["40yd_return_td_bonus"]] <- tmp[,by = .(week,season_type,team_abbr,player,player_id,stat_label),
      list(football_values = sum(football_values), fantasy_points = sum(fantasy_points))]
  
  player <- rbindlist(player)
  
  player <- merge(player, player_data[,.(player_id, player_name, team_abbr, position)], all.x = TRUE, by = c("player_id", "team_abbr"))
  
  if(any(is.na(player$position))){
    print(paste0("There were ", length(player$position[is.na(player$position)]), " rows removed because of NAs in position"))
    player <- player[!is.na(position)]
  }

  # rename Fullback to Running Back
  player <- rbindlist(list(player[position=="FB",position:="RB"],player[position!="FB"]))
  
  if(any(!(player$position %in% c('QB', 'RB', 'WR', 'TE')))){
    print(paste0("There were ", dim(player[!(position %in% c('QB', 'RB', 'FB', 'WR', 'TE'))])[1], 
                 " rows removed because of position is out of scope"))
    player <- player[position %in% c('QB', 'RB', 'WR', 'TE')]
  }
  
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
      football_values,
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
    list(stat_label="def_sack", football_values = .N, fantasy_points = .N*1L)
  ]
  
  # defensive bonus for safeties
  def[["def_safety"]] <- pbp_dt[
    safety == 1L & !is.na(safety_player_id),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_safety", football_values = .N, fantasy_points = .N*1L)
  ]
  
  # defensive bonus for fumble recovery
  def[["def_fumble_recovery"]] <- pbp_dt[
    fumble == 1L & fumble_lost == 1L & play_type != "punt",
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_fumble_recovery", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # defensive bonus for fumble recovery for a punt
  # punts start with the receiving team listed as defteam, so those may need special consideration
  def[["def_fumble_recovery_punt"]] <- pbp_dt[
    fumble == 1L & fumble_lost == 1L & play_type == "punt",
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_fumble_recovery_punt", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # defensive bonus for interceptions
  def[["def_interception"]] <- pbp_dt[
    interception == 1L,
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_interception", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # def bonus for blocks on punt, fg or extra point
  def[["def_block"]] <- pbp_dt[
    !is.na(blocked_player_name),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_block", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # def bonus for def td for any reason or cause (block, fumble, interception, etc)
  # only for normal possession plays by the opposite team (ie. pass or rush)
  def[["def_td"]] <- pbp_dt[
    return_touchdown == 1L & play_type %in% c("pass", "run"),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_td", football_values = .N, fantasy_points = .N*6L)
  ]
  
  # special teams bonus for a return td
  # in a kickoff, the kicking team is listed as the defteam
  def[["def_kickoff_return_td"]] <- pbp_dt[
    return_touchdown == 1L & play_type %in% c("kickoff"),
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_kickoff_return_td", football_values = .N, fantasy_points = .N*6L)
  ]
  
  # special teams bonus for a return td
  # in a punt, the receiving team is listed as the defteam
  def[["def_punt_return_td"]] <- pbp_dt[
    return_touchdown == 1L & play_type %in% c("punt"),
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_punt_return_td", football_values = .N, fantasy_points = .N*6L)
  ]
  
  # calculate points allowed for each team
  tmp <- rbindlist(list(
    unique(pbp_dt[,.(week, season_type, team_abbr = home_team, football_values = away_score)]),
    unique(pbp_dt[,.(week, season_type, team_abbr = away_team, football_values = home_score)])
  ))
  tmp[, stat_label := "def_points_allowed"]
  tmp <- tmp[,.(week, season_type, team_abbr, stat_label, football_values)]
  def[["def_points_allowed"]] <- tmp[, 
    fantasy_points := case_when(
      football_values == 0L ~ 10L,
      football_values >= 1L & football_values <= 6 ~ 7L,
      football_values >= 7L & football_values <= 13 ~ 4L,
      football_values >= 14L & football_values <= 17 ~ 1L,
      football_values >= 18L & football_values <= 21 ~ 0L,
      football_values >= 22L & football_values <= 27 ~ -1L,
      football_values >= 28L & football_values <= 34 ~ -4L,
      football_values >= 35L & football_values <= 45 ~ -7L,
      football_values >= 46L ~ -10L,
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
      football_values,
      fantasy_points
    )]
  
  setorder(def, cols = week, position)
  
  return(def)
  
}


get_player_stats <- function(player_type_char, # either 'offense' or 'kicking'
                             season_year_int = playoff_year, 
                             season_type_char = season_type, 
                             season_teams_list = season_teams,
                             team_data = dt_nfl_teams){
  # create data.table for players, which is a combination of the offensive scorers plus kickers
  dt <- data.table::as.data.table(nflreadr::load_player_stats(seasons = season_year_int, stat_type = player_type_char))
  dt <- dt[season_type %in% season_type_char]
  dt[,season_type := if_else(season_type=="REG","Regular", if_else(season_type=="POST","Post", "Error"))]
  
  if(player_type_char == 'offense'){setnames(dt, old=c('recent_team'), new=c('team_abbr'))} 
  if(player_type_char == 'kicking'){setnames(dt, old=c('team'), new=c('team_abbr'))}
  dt <- dt[team_abbr %in% season_teams_list]
  
  if(player_type_char=='offense'){
    dt <- dt[position %in% c('QB', 'RB', 'FB', 'WR', 'TE')]
    dt[,position := if_else(position == 'FB', 'RB', position)]
  }
  if(player_type_char=='kicking'){dt[,position := 'K']}# position is not in the original dataset
  
  if(player_type_char=='kicking'){dt[,fg_made_50_ := fg_made_50_59 + fg_made_60_]}
  
  standard_cols <- c(
    'position',
    'week',
    'season_type',
    'player_id',
    'player_name',
    'team_abbr'
  )
  
  if(player_type_char=='offense'){
    stat_cols <- c(
      'passing_yards',
      'passing_tds',
      'rushing_yards',
      'rushing_tds',
      'receiving_yards',
      'receiving_tds',
      'interceptions',
      'sack_fumbles_lost',
      'rushing_fumbles_lost',
      'receiving_fumbles_lost',
      'passing_2pt_conversions',
      'rushing_2pt_conversions',
      'receiving_2pt_conversions'
    ) 
  } else if(player_type_char=='kicking'){
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
    value.name = 'football_values'
  )
  
  # calculate fantasy football points
  dt[,fantasy_points := case_when(
    stat_label == 'passing_yards' & football_values >= 400 ~ as.integer(football_values/50) + 2L,
    stat_label == 'passing_yards' & football_values < 400 ~ as.integer(football_values/50),
    stat_label == 'rushing_yards' & football_values >= 200 ~ as.integer(football_values/10L) + 2L,
    stat_label == 'rushing_yards' & football_values < 200 ~ as.integer(football_values/10L),
    stat_label == 'receiving_yards' & football_values >= 200 ~ as.integer(football_values/10L) + 2L,
    stat_label == 'receiving_yards' & football_values < 200 ~ as.integer(football_values/10L),
    stat_label %in% c('passing_tds', 'rushing_tds','receiving_tds') ~ as.integer(football_values) * 6L,
    stat_label %in% c('passing_2pt_conversions', 'rushing_2pt_conversions','receiving_2pt_conversions') ~ as.integer(football_values) * 2L,
    stat_label == 'interceptions' ~ as.integer(football_values) * -2L,
    stat_label %in% c('sack_fumbles_lost', 'rushing_fumbles_lost', 'receiving_fumbles_lost') ~ as.integer(football_values) * -2L,
    stat_label == 'fg_made' ~ as.integer(football_values) * 3L,
    stat_label == 'fg_made_40_49' ~ as.integer(football_values) * 1L, # this is a bonus
    stat_label == 'fg_made_50_' ~ as.integer(football_values) * 2L, # this is a bonus
    stat_label == 'fg_missed' ~ as.integer(football_values) * -1L,
    stat_label == 'pat_made' ~ as.integer(football_values) * 1L,
    stat_label == 'pat_missed' ~ as.integer(football_values) * -1L,
    .default = 0L
  )]
  
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
      football_values,
      fantasy_points
    )]
  
  return(dt)
  
}


get_combined_stats <- function(teams = dt_nfl_teams, playoff_teams = season_teams){
  
  # bind rows
  dt <- rbindlist(list(
    get_player_stats(player_type_char='offense'), 
    get_player_stats(player_type_char='kicking'),
    get_bonus_stats(get_pbp()),
    get_defense_stats(get_pbp())
  ))
  
  # reapply playoff teams filter since defensive / special teams are scored for both home and away teams depending in play type
  dt <- dt[team_abbr %in% playoff_teams]
  
  # stack stat_types into long format
  tmp1 <- dt[,.(
    team_abbr,
    team_conf,
    team_division,
    position,
    week,
    season_type,
    player_id,
    player_name,
    stat_label,
    football_values
  )]
  setnames(tmp1, old = c("football_values"), new = c("stat_values"))
  tmp1[,stat_type:="football_values"]
  
  tmp2 <- dt[,.(
    team_abbr,
    team_conf,
    team_division,
    position,
    week,
    season_type,
    player_id,
    player_name,
    stat_label,
    fantasy_points
  )]
  setnames(tmp2, old = c("fantasy_points"), new = c("stat_values"))
  tmp2[,stat_type:="fantasy_points"]
  
  dt <- rbindlist(list(tmp1, tmp2))
  
  # create the lookup_string that will be used in the dashboard filters
  dt <- rbindlist(list(
    dt[position!="Defense",lookup_string := paste0(position,', ',team_abbr,': ',player_name,' (',team_division,', ID: ',player_id,')')],
    dt[position=="Defense",lookup_string:=paste0(position,", ",team_abbr," (",team_division,")")]
  ))
  
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
      'team_division',
      'stat_type',
      'stat_label'
    )
  )
}

order_cols <- function(dt, pos){
  
  basic_order <- c(
    'position',
    'lookup_string',
    'week',
    'season_type',
    'player_id',
    'player_name',
    'team_abbr',
    'team_conf',
    'team_division',
    'stat_type',
    'stat_label',
    'stat_values'
  )
  
  qb_order <- c(
    'passing_tds__fantasy_points',
    'passing_tds__football_values',
    'passing_yards__fantasy_points',
    'passing_yards__football_values',
    'rushing_tds__fantasy_points',
    'rushing_tds__football_values',
    'rushing_yards__fantasy_points',
    'rushing_yards__football_values',
    'interceptions__fantasy_points',
    'interceptions__football_values',
    'sack_fumbles_lost__fantasy_points',
    'sack_fumbles_lost__football_values',
    'rushing_fumbles_lost__fantasy_points',
    'rushing_fumbles_lost__football_values',
    'passing_2pt_conversions__fantasy_points',
    'passing_2pt_conversions__football_values',
    'receiving_tds__fantasy_points',
    'receiving_tds__football_values',
    'receiving_yards__fantasy_points',
    'receiving_yards__football_values',
		'receiving_2pt_conversions__fantasy_points',
		'receiving_2pt_conversions__football_values',
		'receiving_fumbles_lost__fantasy_points',
		'receiving_fumbles_lost__football_values',
		'rushing_2pt_conversions__fantasy_points',
		'rushing_2pt_conversions__football_values',
    '40yd_pass_td_qb_bonus__fantasy_points',
    '40yd_pass_td_qb_bonus__football_values',
    '40yd_pass_td_receiver_bonus__fantasy_points',
    '40yd_pass_td_receiver_bonus__football_values',
    '40yd_rush_td_bonus__fantasy_points',
    '40yd_rush_td_bonus__football_values'
  )

  rb_order <- c(
    'rushing_tds__fantasy_points',
    'rushing_tds__football_values',
    'rushing_yards__fantasy_points',
    'rushing_yards__football_values',
    'rushing_2pt_conversions__fantasy_points',
    'rushing_2pt_conversions__football_values',
    'rushing_fumbles_lost__fantasy_points',
    'rushing_fumbles_lost__football_values',
    'passing_tds__fantasy_points',
    'passing_tds__football_values',
    'receiving_tds__fantasy_points',
    'receiving_tds__football_values',
    'passing_yards__fantasy_points',
    'passing_yards__football_values',
    'receiving_yards__fantasy_points',
    'receiving_yards__football_values',    
    'passing_2pt_conversions__fantasy_points',
    'passing_2pt_conversions__football_values',
    'receiving_2pt_conversions__fantasy_points',
    'receiving_2pt_conversions__football_values',
    'receiving_fumbles_lost__fantasy_points',
    'receiving_fumbles_lost__football_values',
    'sack_fumbles_lost__fantasy_points',
    'sack_fumbles_lost__football_values',
    'interceptions__fantasy_points',
    'interceptions__football_values',
    '40yd_pass_td_qb_bonus__fantasy_points',
    '40yd_pass_td_qb_bonus__football_values',
    '40yd_pass_td_receiver_bonus__fantasy_points',
    '40yd_pass_td_receiver_bonus__football_values',
    '40yd_rush_td_bonus__fantasy_points',
    '40yd_rush_td_bonus__football_values',
    '40yd_return_td_bonus__fantasy_points',
    '40yd_return_td_bonus__football_values'
  )

  te_order <- c(
    'receiving_tds__fantasy_points',
    'receiving_tds__football_values',
    'receiving_yards__fantasy_points',
    'receiving_yards__football_values',
    'receiving_2pt_conversions__fantasy_points',
    'receiving_2pt_conversions__football_values',
    'receiving_fumbles_lost__fantasy_points',
    'receiving_fumbles_lost__football_values',
    'passing_tds__fantasy_points',
    'passing_tds__football_values',
    'rushing_tds__fantasy_points',
    'rushing_tds__football_values',
    'passing_yards__fantasy_points',
    'passing_yards__football_values',
    'rushing_yards__fantasy_points',
    'rushing_yards__football_values',    
    'passing_2pt_conversions__fantasy_points',
    'passing_2pt_conversions__football_values',
    'rushing_2pt_conversions__fantasy_points',
    'rushing_2pt_conversions__football_values',
    'rushing_fumbles_lost__fantasy_points',
    'rushing_fumbles_lost__football_values',
    'sack_fumbles_lost__fantasy_points',
    'sack_fumbles_lost__football_values',
    'interceptions__fantasy_points',
    'interceptions__football_values',
    '40yd_pass_td_qb_bonus__fantasy_points',
    '40yd_pass_td_qb_bonus__football_values',
    '40yd_pass_td_receiver_bonus__fantasy_points',
    '40yd_pass_td_receiver_bonus__football_values',
    '40yd_rush_td_bonus__fantasy_points',
    '40yd_rush_td_bonus__football_values',
    '40yd_return_td_bonus__fantasy_points',
    '40yd_return_td_bonus__football_values'
  )
  
  wr_order <- c(
    'receiving_tds__fantasy_points',
    'receiving_tds__football_values',
    'receiving_yards__fantasy_points',
    'receiving_yards__football_values',
    'receiving_fumbles_lost__fantasy_points',
    'receiving_fumbles_lost__football_values',
    'receiving_2pt_conversions__fantasy_points',
    'receiving_2pt_conversions__football_values',
    'passing_tds__fantasy_points',
    'passing_tds__football_values',
    'rushing_tds__fantasy_points',
    'rushing_tds__football_values',
    'passing_yards__fantasy_points',
    'passing_yards__football_values',
    'rushing_yards__fantasy_points',
    'rushing_yards__football_values',
    'passing_2pt_conversions__fantasy_points',
    'passing_2pt_conversions__football_values',
    'rushing_2pt_conversions__fantasy_points',
    'rushing_2pt_conversions__football_values',
    'rushing_fumbles_lost__fantasy_points',
    'rushing_fumbles_lost__football_values',
    'sack_fumbles_lost__fantasy_points',
    'sack_fumbles_lost__football_values',
    'interceptions__fantasy_points',
    'interceptions__football_values',
    '40yd_pass_td_qb_bonus__fantasy_points',
    '40yd_pass_td_qb_bonus__football_values',
    '40yd_pass_td_receiver_bonus__fantasy_points',
    '40yd_pass_td_receiver_bonus__football_values',
    '40yd_rush_td_bonus__fantasy_points',
    '40yd_rush_td_bonus__football_values',
    '40yd_return_td_bonus__fantasy_points',
    '40yd_return_td_bonus__football_values'
  )
  
  k_order <- c(
    'fg_made__fantasy_points',
    'fg_made__football_values',
    'pat_made__fantasy_points',
    'pat_made__football_values',
    'fg_missed__fantasy_points',
    'fg_missed__football_values',
    'fg_made_40_49__fantasy_points',
    'fg_made_40_49__football_values',
    'fg_made_50___fantasy_points',
    'fg_made_50___football_values',
    'fg_blocked__fantasy_points',
    'fg_blocked__football_values',
    'pat_missed__fantasy_points',
    'pat_missed__football_values'
  )
  
  defense_order <- c(
    'def_points_allowed__fantasy_points',
    'def_points_allowed__football_values',
    'def_td__fantasy_points',
    'def_td__football_values',
    'def_kickoff_return_td__fantasy_points',
    'def_kickoff_return_td__football_values',
    'def_punt_return_td__fantasy_points',
    'def_punt_return_td__football_values',
    'def_sack__fantasy_points',
    'def_sack__football_values',
    'def_fumble_recovery__fantasy_points',
    'def_fumble_recovery__football_values',
    'def_fumble_recovery_punt__fantasy_points',
    'def_fumble_recovery_punt__football_values',
    'def_interception__fantasy_points',
    'def_interception__football_values',
    'def_safety__fantasy_points',
    'def_safety__football_values',
    'def_block__fantasy_points',
    'def_block__football_values'
  )
    
  if(pos == "QB"){
    master_order <- c(basic_order, qb_order)
  } else if(pos == "RB"){
    master_order <- c(basic_order, rb_order)
  } else if(pos == "TE"){
    master_order <- c(basic_order, te_order)
  } else if(pos == "WR"){
    master_order <- c(basic_order, wr_order)
  } else if(pos == "K"){
    master_order <- c(basic_order, k_order)
  } else if(pos == "Defense"){
    master_order <- c(basic_order, defense_order)
  } else {
    print("Error: pos did not evaluate to a valid value")
  }
  
  if(any(duplicated(master_order))){
    print(paste0("There are duplicated columns in the ", str_lower(pos),"_order"))
    print(paste0(master_order[duplicated(master_order)], collapse = "; "))
  }  
  
  found_order <- names(dt)
  
  unmapped_cols <- found_order[!(found_order %in% master_order)] |> sort()
  
  if(length(unmapped_cols)){
    print(paste0("There are unmapped ", pos, " columns in the dataset"))
    print(paste0(unmapped_cols, collapse = "; "))
  }
  
  preferred_order <- master_order[master_order %in% found_order]
  
  return(dt[,..preferred_order])
  
}

sort_cols <- function(dt, pos, stat_type){
  
  if(pos == "QB" & stat_type %in% c("Fantasy Points", "Both")){
    setorder(dt, -passing_tds__fantasy_points)
  } else if(pos == "QB" & stat_type == "Football Values"){
    setorder(dt, -passing_tds__football_values)
  } else if(pos == "RB" & stat_type %in% c("Fantasy Points", "Both")){
    setorder(dt, -rushing_tds__fantasy_points)
  } else if(pos == "RB" & stat_type == "Football Values"){
    setorder(dt, -rushing_tds__football_values)
  } else if(pos == "WR" & stat_type %in% c("Fantasy Points", "Both")){
    setorder(dt, -receiving_tds__fantasy_points)
  } else if(pos == "WR" & stat_type == "Football Values"){
    setorder(dt, -receiving_tds__football_values)
  } else if(pos == "TE" & stat_type %in% c("Fantasy Points", "Both")){
    setorder(dt, -receiving_tds__fantasy_points)
  } else if(pos == "TE" & stat_type == "Football Values"){
    setorder(dt, -receiving_tds__football_values)
  } else if(pos == "K" & stat_type %in% c("Fantasy Points", "Both")){
    setorder(dt, -fg_made__fantasy_points)
  } else if(pos == "K" & stat_type == "Football Values"){
    setorder(dt, -fg_made__football_values)
  } else if(pos == "Defense" & stat_type %in% c("Fantasy Points", "Both")){
    setorder(dt, -def_points_allowed__fantasy_points)
  } else if(pos == "Defense" & stat_type == "Football Values"){
    setorder(dt, -def_points_allowed__football_values)
  }
  
  return(dt)
  
}

update_app_stats <- function(dt, 
                             pos, 
                             reg_or_post, 
                             stat_type, 
                             stat_teams, 
                             is_summed_stat, 
                             is_wide_table){
  
  if(!(pos %in% c("K","QB","RB","TE","WR","Defense"))){
    print(paste0(pos, " is not a valid position"))
  }
  
  dt <- dt[position == pos]
  dt <- dt[season_type == reg_or_post]
  dt <- dt[team_abbr %in% stat_teams]
  
  # this returns an empty data.table if there are no stats, which avoids an error when casting
  if(dim(dt)[1]==0L){
    return(dt)
  }
  
  if(stat_type=="Football Values"){
    dt <- dt[stat_type=="football_values"]
  } else if (stat_type=="Fantasy Points"){
    dt <- dt[stat_type=="fantasy_points"]
  }
  
  if(is_summed_stat){
    
    dt <- dt[, week:=NULL]
    
    grouping_by <- c(
      'position',
      'season_type',
      'lookup_string',
      'player_id',
      'player_name',
      'team_abbr',
      'team_conf',
      'team_division',
      'stat_type',
      'stat_label'
    )
    
    dt <- dt[, by = grouping_by, .(stat_values = sum(stat_values))]
    
  }
  
  dt[,stat_label := paste0(stat_label,"__",stat_type)]
  
  if(!is_summed_stat && !is_wide_table){
    
    col_order <- c(
      'position',
      'lookup_string',
      'week',
      'season_type',
      'player_id',
      'player_name',
      'team_abbr',
      'team_conf',
      'team_division',
      'stat_type',
      'stat_label',
      'stat_values'
    )
    
    dt <- dt[,.SD, .SDcols = col_order]
    setorder(dt, position, player_id, week)
    
  } else if(is_summed_stat && !is_wide_table){
    
    col_order <- c(
      'position',
      'lookup_string',
      'season_type',
      'player_id',
      'player_name',
      'team_abbr',
      'team_conf',
      'team_division',
      'stat_type',
      'stat_label',
      'stat_values'
    )
    
    dt <- dt[,.SD, .SDcols = col_order]
    setorder(dt, position, -stat_values)
    
  } else if(is_summed_stat && is_wide_table){
    # does not include the `week` variable when summarized
    # value.var is unspecified since football_values and fantasy_points may or may not be presents
    dt <- dcast(
      dt,
      position + season_type + lookup_string + player_id + player_name + team_abbr + team_conf + team_division ~ stat_label,
      value.var = c("stat_values"),
      fill = 0
    )
    dt <- order_cols(dt, pos)
    dt <- sort_cols(dt, pos, stat_type)
  } else if(!is_summed_stat && is_wide_table) {
    dt <- dcast(
      dt,
      position + week + season_type + lookup_string + player_id + player_name + team_abbr + team_conf + team_division ~ stat_label,
      value.var = c("stat_values"),
      fun.aggregate = sum, # this is required, otherwise it will report the length instead of totaling where duplicate stats exist
      fill = 0
    )
    dt <- order_cols(dt, pos)
    dt <- sort_cols(dt, pos, stat_type)
  }
  
  return(dt)
  
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

## create master data tables
# create data.table for players, which is a combination of the offensive scorers plus kickers
dt_nfl_player_stats <- get_combined_stats()

# remove zero value statistics
# TODO this may or may not be a good idea for the stats but increases load time
dt_nfl_player_stats <- dt_nfl_player_stats[abs(stat_values) >= 1e-7]

# get a list of unique players and teams for the lookup
team_lookupstring_position <- rbindlist(list(
  setorder(dt_roster[,.(position, lookup_string, team_abbr)], lookup_string),
  dt_nfl_teams[,.(position, lookup_string, team_abbr)]
))


ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.freelancer.css"),
    tags$title("Playoff Fantasy Football League")
  ),
  tags$h1("Playoff Fantasy Football League", style = "text-align:center"),
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
          tags$li("Regular season statistics are available on the 'Explore Stats' tab, which may help provide insights on each player you should prioritize. Statistics are available in 'football values' and in 'fantasy points'."),
          tags$li("Use the 'Build Roster' tab on this dashboard to start creating your roster."),
          tags$li("Add players to your roster based on the combination you think will score the most points by the end of the Superbowl."),
          tags$li("When a player is added to your roster, the team associated with that player (and any of its remaining players) will be removed from your next possible selections. For example: if you pick Jalen Hurts as one of your quarterbacks, you no longer be able to select an Eagles player on your roster."),
          tags$li("When you've satisified the maximum number of positions on your roster, any player associated with that position will be removed from your next possible selection. For example: if you pick Jalen Hurts as your third (and last) quarterback, you no longer be able to select a quarterback."),
          tags$li("As needed, you can remove players from your team, which will release that Team and/or Position as a next possible selection."),
          tags$li("You must include your Name, Email and Fantasy Team Name in the Participant Information Box. Don't forget to confirm that you've paid the Commish."),
          tags$li("The roster can only be downloaded after all parameters have been satisfied (that is, a completed roster of 14 players and the Participant Information box is filled in with valid information)."),
          tags$li("You must still email the commissioner your roster downloaded from this website. This website does not save your roster.", style="color:red; font-weight:bold;"),
        ),
        # tags$h2("Alternate Roster in Excel"),
        # tags$p("The email sent to you by the Commissioner should contain an Excel file that is equivalent to this dashboard. If you prefer, you can complete that roster template and email the Excel file back to the Commissioner. I don't know why you would do this, but technically it is possible."),
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
      "Build Roster",
      br(),
      actionButton(
        inputId = "toggleRosterSelector", 
        label = "Roster Selector Menu",
        icon = icon("bars"),
        style = "margin-bottom:10px"
      ),
      sidebarLayout(
        div(id = "rosterSelector",
          sidebarPanel(
            selectizeInput(
              inputId = "roster_selections_made",
              label = "Select Player or Defensive Team",
              choices = unique(team_lookupstring_position[,.(lookup_string)]) |> as.list(),
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
              tags$p("", style="margin:8px"),
              tags$span("Participant Information", style='font-weight:bold; font-size:16px; margin-right: 3px'),
              tags$span("* required", style = "color:red;"),
              tags$p("", style="margin:8px"),
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
            width = 3
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
      br(),
      actionButton(
        inputId = "toggleFilterMenu", 
        label = "Filter Menu",
        icon = icon("bars"),
        style = "margin-right:25px;",
        inline = TRUE
      ),
      shinyWidgets::materialSwitch(
        inputId = "pivot_data",
        label = "Pivot Data",
        inline = TRUE
      ),
      sidebarLayout(
        div(id = "filterMenu",
          sidebarPanel(
            width = 2,
            selectInput(
              inputId = "selected_position",
              label = "Position:",
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
              choices = list("Fantasy Points", "Football Values", "Both"),
              selected = "Fantasy Points"
            ),
            tags$p("Inspect Team(s)", style = "font-weight:bold; margin-top:40px"),
            actionButton("select_all_teams", label="All", inline=TRUE),
            actionButton("deselect_all_teams", label="None", inline=TRUE),
            checkboxGroupInput(
              "selected_teams",
              label = "",
              choiceNames = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_name_w_abbr]),
              choiceValues = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_abbr]),
              selected = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_abbr])
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(paste0(playoff_year," Season Totals"), br(), DTOutput("statistics_season")),
            tabPanel(paste0(playoff_year," by Week"), br(), DTOutput("statistics_weekly"))
          )
        )
      )
    )#,tabPanel(
    #   "Fantasy Results",
    #   id = "fantasyResultsPage",
    #   sidebarLayout(
    #     sidebarPanel(
    #       selectInput(
    #         inputId = "fantasy_team",
    #         label = "Fantasy Team:",
    #         choices = list("TBD"),
    #         selected = "TBD"
    #       ),
    #     ),
    #     mainPanel(
    #       h1("Coming soon... maybe", style="font-family:Arial")
    #     )
    #   )
    # )
  )
)

server <- function(input, output, session) {
  
  ## this section is for Roster Selection
  observeEvent(input$toggleRosterSelector, {
    shinyjs::toggle(id = "rosterSelector")
  })
  
  roster <- reactiveValues(players = c(NULL))
  
  observeEvent(input$add_player,{
    roster$players <- c(roster$players, input$roster_selections_made) |> sort()
  })
  
  observeEvent(input$remove_player,{
    roster$players <- roster$players[!(roster$players %in% input$roster_selections_removed)]
  })
  
  roster_reactive <- reactive({
    remaining <- 14-length(roster$players)
    roster_full <- ifelse(length(roster$players) == 14L,TRUE,FALSE)
    data.table(
      'slots_remaining' = remaining,
      'roster_full' = roster_full
    )
  }) 
  
  output$roster_slots_remaining_text <- renderText({
      paste0("Roster slot(s) remaining: ", roster_reactive()$slots_remaining, " of 14")
  })

  
  # keep track of teams selected on the roster
  teams_on_roster <- reactive({
    unique(team_lookupstring_position[lookup_string %in% roster$players, team_abbr])
  })
  
  output$teams_on_roster_text <- renderText({
    if(is_empty(teams_on_roster())){
      "Teams on roster: None"
    } else {
      paste0("Teams on roster: ", paste0(teams_on_roster() |> unlist(), collapse = ",  "))
    }
  })
  
  # keep track of unselected teams
  teams_available <- reactive({
    team_lookupstring_position[!(team_abbr %in% teams_on_roster()), team_abbr] |> unique() |> sort()
  }) 
  output$teams_available_text <- renderText({
    paste0("Teams remaining: ", paste0(teams_available() |> unlist(), collapse = ",  "))
  })
  
  # keep track of positions on the roster
  positions_selected <- reactive({
    team_lookupstring_position[lookup_string %in% roster$players, position]
  })
  output$positions_on_roster_text <- renderText({
    if(is_empty(positions_selected())){
      "Positions Filled: None"
    } else {
      paste0("Positions Filled: ", paste0(count_positions(positions_selected()) |> unlist(), collapse = ",  "))
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
      paste0("Positions Remaining: ", paste0(remaining_positions |> unlist(), collapse = ",  "))
    }
  })
  
  players_remaining <- reactive({

    players_remaining <- team_lookupstring_position[!(team_abbr %in% teams_on_roster())]
    
    if("Defense" %in% positions_selected()){
      players_remaining <- players_remaining[position != "Defense"]
    }   
    if("K" %in% positions_selected()){
      players_remaining <- players_remaining[position != "K"]
    }
    if(length(positions_selected()[positions_selected() == "QB"])>=3L){
      players_remaining <- players_remaining[position != "QB"]
    }
    # for RB, TE and WR, need to consider the flex position when filtering
    if((length(positions_selected()[positions_selected() == "RB"])==3L & 
       (length(positions_selected()[positions_selected() == "TE"])==3L |
        length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "RB"])>=4L)){
      players_remaining <- players_remaining[position != "RB"]
    }
    if((length(positions_selected()[positions_selected() == "TE"])==2L & 
        (length(positions_selected()[positions_selected() == "RB"])==4L |
         length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "TE"])>=3L)){
      players_remaining <- players_remaining[position != "TE"]
    }
    if((length(positions_selected()[positions_selected() == "WR"])==3L & 
        (length(positions_selected()[positions_selected() == "TE"])==3L |
         length(positions_selected()[positions_selected() == "RB"])==4L) )|
       (length(positions_selected()[positions_selected() == "WR"])>=4L)){
      players_remaining <- players_remaining[position != "WR"]
    }
    
    players_remaining
    
  })
  
  output$players_remaining_DT <- renderDT({players_remaining()})
  
  output$players_on_roster_DT <- renderDT({
    if(is_empty(roster$players)){
      DT::datatable(
        data.table(lookup_string = "Roster is empty"), 
        options = list(pageLength = 25)
      )
    } else {
      DT::datatable(
        team_lookupstring_position[lookup_string %in% roster$players],
        options = list(pageLength = 25)
      )
    }
  })
  
  
  observeEvent(
    input$add_player,{
    updateSelectizeInput(
      session,
      inputId = "roster_selections_made",
      choices = players_remaining()[,.(lookup_string)] |> as.list()
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
    roster_reactive()$roster_full,
    {
      if(roster_reactive()$roster_full) {
        shinyjs::disable("add_player")
        
      } else {
        shinyjs::enable("add_player")
      }
    }
  )
  


  # reactive boolean for activating download button
  participant_reactive <- reactive({
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
      participant_reactive()$fantasy_owner_name!="",
      str_detect(participant_reactive()$fantasy_owner_email,"[:graph:]{3,}@[:alnum:]{1,}\\.[:alnum:]{2,}"),
      participant_reactive()$fantasy_team_name!="",
      participant_reactive()$paid,
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
    team_lookupstring_position |>
      filter(lookup_string %in% roster$players) |>
      select(position, team_abbr, lookup_string) |> 
      mutate(
        `Fantasy Owner` = rep(participant_reactive()$fantasy_owner_name,14),
        `Fantasy Owner Email` = rep(participant_reactive()$fantasy_owner_email,14),
        `Fantasy Team Name` = rep(participant_reactive()$fantasy_team_name,14),
        `Roster` = 1:14,
        `Position Type` = if_else(position == "Defense", "Defense / Special teams", "Player"),
        `Automation Mapping` = if_else(
          position == "Defense", 
          team_abbr, 
          str_remove(str_remove(lookup_string, "^.*, ID: "),"\\)")
        ),
        `Check 1 - Selection is Unique` = TRUE,
        `Check 2 - Team is Unique` = TRUE
      ) |> 
      group_by(
        position
      ) |> 
      mutate(
        `Position Code` = if_else(position %in% c("QB","WR","TE","RB"), paste0(position,1:n()), 
                          if_else(position == "Defense", "D", position))
      ) |> 
      ungroup() |> 
      rename(
        `Position Group` = position,
        `Team Abbr.` = team_abbr,
        `Selection` = lookup_string
      ) |>
      mutate(
        `Position Group` = case_when(
          `Position Code` == "K" ~ "SPEC", 
          `Position Code` %in% c("RB4","WR4","TE3") ~ "FLEX", 
          `Position Code` == "D" ~ "D", 
          .default = `Position Group`)
      ) |> 
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
  
  ## this section is for stats exploration
  observeEvent(input$toggleFilterMenu, {
    shinyjs::toggle(id = "filterMenu")
  })
  
  stat_teams <- reactive({input$selected_teams})
  
  stat_params <- reactive({
    data.table(
      'pos' = input$selected_position,
      'season_type' = input$reg_or_post,
      'stat_type' = input$stat_type,
      'pivot_data' = input$pivot_data
    )
    
  })
  
  output$statistics_weekly <- renderDT({
    
    if(is_empty(dt)){
      DT::datatable(
        data.table(lookup_string = "No data available")
      )
    } else {
      update_app_stats(
        dt_nfl_player_stats, 
        stat_params()$pos, 
        stat_params()$season_type,
        stat_params()$stat_type,
        stat_teams(),
        is_summed_stat = FALSE, 
        is_wide_table = stat_params()$pivot_data
      )
    }
  })
  
  output$statistics_season <- renderDT({
    
    if(is_empty(dt)){
      DT::datatable(
        data.table(lookup_string = "No data available")
      )
    } else {
      update_app_stats(
        dt_nfl_player_stats, 
        stat_params()$pos, 
        stat_params()$season_type,
        stat_params()$stat_type,
        stat_teams(),
        is_summed_stat = TRUE, 
        is_wide_table = stat_params()$pivot_data
      )
    }
  })
  
  observeEvent(
    input$select_all_teams, {
      updateCheckboxGroupInput(
        session,
        "selected_teams",
        label = "",
        choiceNames = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_name_w_abbr]),
        choiceValues = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_abbr]),
        selected = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_abbr])
      )
    })
  
  observeEvent(
    input$deselect_all_teams, {
      updateCheckboxGroupInput(
        session,
        "selected_teams",
        label = "",
        choiceNames = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_name_w_abbr]),
        choiceValues = as.list(dt_nfl_teams[team_abbr %in% season_teams, team_abbr]),
        selected = NULL
      )
    })
  
  # this section is for the fantasy results section
  
}


shinyApp(ui, server)


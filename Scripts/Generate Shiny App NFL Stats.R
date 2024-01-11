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
playoff_teams <- c("BAL","BUF","KC","HOU","CLE","MIA","PIT","SF","DAL","DET","TB","PHI","LA","GB")


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
dt_roster <- merge.data.table(dt_roster, dt_nfl_teams[,.(team_abbr, team_conf, team_division)], all.x = TRUE, by = c("team_abbr"))
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
  
  player <- merge.data.table(player, player_data[,.(player_id, player_name, team_abbr, position)], all.x = TRUE, by = c("player_id", "team_abbr"))
  
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
  
  player <- merge.data.table(player, team_data[,.(team_abbr, team_conf, team_division)], all.x = TRUE, by = c("team_abbr"))
  
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
  
  def <- merge.data.table(def, team_data[,.(team_abbr, player_name = team_name, team_conf, team_division)], all.x = TRUE)
  
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
  
  dt <- merge.data.table(dt, team_data[,.(team_abbr, team_conf, team_division)], all.x = TRUE)
  
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
dt_nfl_player_stats <- dt_nfl_player_stats[team_abbr %in% playoff_teams]

# get a list of unique players and teams for the lookup
team_lookupstring_position <- rbindlist(list(
  setorder(dt_roster[team_abbr %in% playoff_teams,.(position, lookup_string, team_abbr)], lookup_string),
  dt_nfl_teams[team_abbr %in% playoff_teams,.(position, lookup_string, team_abbr)]
))

dir <- "./Output/NFL Stats/"
fwrite(
  dt_nfl_player_stats,
  file = paste0(
    dir,
    "player_stats_",
    playoff_year,"_",
    paste0(season_type, collapse = "_"),"_gen",
    str_remove_all(Sys.time(), ":"),".csv"
  )
)


fwrite(
  dt_nfl_teams,
  file = paste0(
    dir,
    "nfl_teams_",
    playoff_year,
    "_",
    paste0(season_type, collapse = "_"),
    "_gen",
    str_remove_all(Sys.time(), ":"),
    ".csv"
  )
)

fwrite(
  dt_roster,
  file = paste0(
    dir,
    "nfl_rosters_",
    playoff_year,
    "_",
    paste0(season_type, collapse = "_"),
    "_gen",
    str_remove_all(Sys.time(), ":"),
    ".csv"
  )
)

fwrite(
  team_lookupstring_position,
  file = paste0(
    dir,
    "lookups_",
    playoff_year,
    "_",
    paste0(season_type, collapse = "_"),
    "_gen",
    str_remove_all(Sys.time(), ":"),
    ".csv"
  )
)




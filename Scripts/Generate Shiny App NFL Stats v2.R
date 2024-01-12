# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(tidyverse)
library(data.table)

season_int <- 2023L
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


get_team_info <- function(.season_int = season_int){
  dt <- data.table::as.data.table(nflreadr::load_teams(current = TRUE))
  dt[,team_name_w_abbr := paste0(team_name, ' (', team_abbr, ')')]
  dt <- dt[,.(team_abbr, team_name, team_name_w_abbr, team_conf, team_division, team_logo_espn)]
  dt[,position:="Defense"]
  dt[,lookup_string:=paste0(position,", ",team_abbr," (",team_division,")")]
  return(dt)
}
dt_team_info <- get_team_info()

get_rosters <- function(.season_int = season_int){
  dt <- data.table::as.data.table(nflreadr::load_rosters(.season_int))
  dt[,player_name:=full_name]
  dt <- unique(dt[,.(position, player_id = gsis_id, player_name, team_abbr = team)])
  dt[,position:=ifelse(position=="FB","RB",position)]
  # this provides a space to add in specific edge players (such as punters that sometimes kick fieldgoals)
  dt <- dt[position %in% c('QB', 'RB', 'WR', 'TE','K') | player_name %in% c("Jamie Gillan")]
  dt[,position:= ifelse(position=="P","K",position)]
  dt <- dt[!is.na(position)]
  dt <- merge.data.table(dt, dt_team_info[,.(team_abbr, team_conf, team_division)], all.x = TRUE, by = c("team_abbr"))
  dt[,lookup_string := paste0(position,', ',team_abbr,': ',player_name,' (',team_division,', ID: ',player_id,')')]
  dt
  return(dt)
}
dt_rosters <- get_rosters()

get_pbp <- function(.season_int = season_int,
                    .season_type = season_type){
  dt <- data.table::as.data.table(nflfastR::load_pbp(seasons = .season_int))
  dt <- dt[season_type %in% .season_type]
  dt[,season_type := ifelse(season_type=="REG","Regular", ifelse(season_type=="POST","Post","Error"))]
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

get_bonus_stats <- function(dt = get_pbp(), # use get_pbp()
                            dt_rosters_ = dt_rosters,
                            dt_team_info_ = dt_team_info){
  # create a list to hold each unique fantasy football points dataset
  bonus <- list()
  
  # offensive bonus for touchdown with pass over 40 yards for qb
  bonus[["40yd_pass_td_qb_bonus"]] <- dt[
    pass_touchdown == 1L & passing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = passer_player_name, player_id = passer_player_id),
    list(stat_label = "40yd_pass_td_qb_bonus", football_values = .N, fantasy_points=.N*2L)
  ]
  
  bonus[["40yd_pass_td_receiver_bonus"]] <- dt[
    pass_touchdown == 1L & passing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = receiver_player_name, player_id = receiver_player_id),
    list(stat_label = "40yd_pass_td_receiver_bonus", football_values = .N, fantasy_points=.N*2L)
  ]
  
  # offensive bonus for touchdown with rush over 40 yards for qb
  bonus[["40yd_rush_td_bonus"]] <- dt[
    rush_touchdown == 1L & rushing_yards >= 40, 
    by = .(week, season_type, team_abbr = posteam, player = rusher_player_name, player_id = rusher_player_id),
    list(stat_label = "40yd_rush_td_bonus", football_values = .N, fantasy_points=.N*2L)
  ]
  
  # player bonus for returning a td
  # only for normal possession plays by the opposite team (ie. pass or rush)
  # in a kickoff, the receiving team is listed as the posteam
  # in a punt, the receiving team is listed as the defteam
  tmp <- rbindlist(list(
    dt[
      play_type == "kickoff" & !is.na(kickoff_returner_player_name) & return_touchdown == 1L & return_yards >= 40, 
      by = .(week,
             season_type, 
             team_abbr = posteam,
             player = kickoff_returner_player_name,
             player_id = kickoff_returner_player_id),
      list(stat_label = "40yd_return_td_bonus", football_values = .N, fantasy_points=.N*2L)
    ],
    dt[
      play_type == "punt" & !is.na(punt_returner_player_name) & return_touchdown == 1L & return_yards >= 40, 
      by = .(week,
             season_type, 
             team_abbr = defteam,
             player = punt_returner_player_name,
             player_id = punt_returner_player_id),
      list(stat_label = "40yd_return_td_bonus", football_values = .N, fantasy_points=.N*2L)
    ]
  )) 
  bonus[["40yd_return_td_bonus"]] <- tmp[,by = .(week,season_type,team_abbr,player,player_id,stat_label),
      list(football_values = sum(football_values), fantasy_points = sum(fantasy_points))]
  
  bonus <- rbindlist(bonus)
  
  bonus <- merge.data.table(bonus, dt_rosters_[,.(player_id, player_name, team_abbr, position)], all.x = TRUE, by = c("player_id", "team_abbr"))
  
  if(any(is.na(bonus$position))){
    print(paste0("There were ", length(bonus$position[is.na(bonus$position)]), " rows removed because of NAs in position"))
    bonus <- bonus[!is.na(position)]
  }

  # rename Fullback to Running Back
  bonus <- rbindlist(list(bonus[position=="FB",position:="RB"],bonus[position!="FB"]))
  
  if(any(!(bonus$position %in% c('QB', 'RB', 'WR', 'TE')))){
    print(paste0("There were ", dim(bonus[!(position %in% c('QB', 'RB', 'FB', 'WR', 'TE'))])[1], 
                 " rows removed because of position is out of scope"))
    bonus <- bonus[position %in% c('QB', 'RB', 'WR', 'TE')]
  }
  
  bonus <- merge.data.table(bonus, dt_team_info_[,.(team_abbr, team_conf, team_division)], all.x = TRUE, by = c("team_abbr"))
  
  bonus <-
    bonus[, .(
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
  
  setorder(bonus, cols = week, position)
  
  return(bonus)

}

get_defense_stats <- function(dt, # use get_pbp() 
                              dt_team_info_ = dt_team_info){
  # create a list to hold each unique fantasy football points dataset
  def <- list()
  
  ## defensive bonus for sacks
  # If you want to exclude sack where the QB got back to the line of scrimmage, then add filter 
  # condition of yards_gained < 0L. There are some instances where the sack_player is not recorded 
  # but there was still a sack recorded (seemingly if a fumble happens in the same play)
  def[["def_sack"]] <- dt[
    sack == 1L,
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_sack", football_values = .N, fantasy_points = .N*1L)
  ]
  
  # defensive bonus for safeties
  def[["def_safety"]] <- dt[
    safety == 1L & !is.na(safety_player_id),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_safety", football_values = .N, fantasy_points = .N*1L)
  ]
  
  # defensive bonus for fumble recovery
  def[["def_fumble_recovery"]] <- dt[
    fumble == 1L & fumble_lost == 1L & play_type != "punt",
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_fumble_recovery", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # defensive bonus for fumble recovery for a punt
  # punts start with the receiving team listed as defteam, so those may need special consideration
  def[["def_fumble_recovery_punt"]] <- dt[
    fumble == 1L & fumble_lost == 1L & play_type == "punt",
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_fumble_recovery_punt", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # defensive bonus for interceptions
  def[["def_interception"]] <- dt[
    interception == 1L,
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_interception", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # def bonus for blocks on punt, fg or extra point
  def[["def_block"]] <- dt[
    !is.na(blocked_player_name),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_block", football_values = .N, fantasy_points = .N*2L)
  ]
  
  # def bonus for def td for any reason or cause (block, fumble, interception, etc)
  # only for normal possession plays by the opposite team (ie. pass or rush)
  def[["def_td"]] <- dt[
    return_touchdown == 1L & play_type %in% c("pass", "run"),
    by = .(week, season_type, team_abbr = defteam),
    list(stat_label="def_td", football_values = .N, fantasy_points = .N*6L)
  ]
  
  # special teams bonus for a return td
  # in a kickoff, the kicking team is listed as the defteam
  def[["def_kickoff_return_td"]] <- dt[
    return_touchdown == 1L & play_type %in% c("kickoff"),
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_kickoff_return_td", football_values = .N, fantasy_points = .N*6L)
  ]
  
  # special teams bonus for a return td
  # in a punt, the receiving team is listed as the defteam
  def[["def_punt_return_td"]] <- dt[
    return_touchdown == 1L & play_type %in% c("punt"),
    by = .(week, season_type, team_abbr = posteam),
    list(stat_label="def_punt_return_td", football_values = .N, fantasy_points = .N*6L)
  ]
  
  # calculate points allowed for each team
  tmp <- rbindlist(list(
    unique(dt[,.(week, season_type, team_abbr = home_team, football_values = away_score)]),
    unique(dt[,.(week, season_type, team_abbr = away_team, football_values = home_score)])
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
  
  def <- merge.data.table(def, dt_team_info_[,.(team_abbr, player_name = team_name, team_conf, team_division)], all.x = TRUE)
  
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
                             .season_int = season_int, 
                             .season_type = season_type, 
                             dt_team_info_ = dt_team_info){
  # create data.table for players, which is a combination of the offensive scorers plus kickers
  dt <- data.table::as.data.table(nflreadr::load_player_stats(seasons = .season_int, stat_type = player_type_char))
  dt <- dt[season_type %in% .season_type]
  dt[,season_type := ifelse(season_type=="REG","Regular", ifelse(season_type=="POST","Post", "Error"))]
  if(player_type_char == 'offense'){setnames(dt, old=c('recent_team'), new=c('team_abbr'))} 
  if(player_type_char == 'kicking'){setnames(dt, old=c('team'), new=c('team_abbr'))}
  
  if(player_type_char=='offense'){
    dt <- dt[position %in% c('QB', 'RB', 'FB', 'WR', 'TE')]
    dt[,position := ifelse(position == 'FB', 'RB', position)]
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
  
  dt <- merge.data.table(dt, dt_team_info_[,.(team_abbr, team_conf, team_division)], all.x = TRUE)
  
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

combine_stats <- function(dt_rosters_ = dt_rosters){
  
  # bind rows
  dt <- rbindlist(list(
    get_player_stats(player_type_char='offense'), 
    get_player_stats(player_type_char='kicking'),
    get_bonus_stats(get_pbp()),
    get_defense_stats(get_pbp())
  ))
  
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
  
  # drop the player name from stats, since there are potential discrepancies 
  # depending on what first initial they were born with vs what they use professional
  # also note that lookup_string joind from dt_rosters will bring in the current team, not necessarily
  # the team that the player is on when the stat occurred in the past
  dt <- merge.data.table(
    dt, 
    dt_rosters_[,.(player_id, player_name, lookup_string)], 
    all.x=TRUE, 
    by = c("player_id")
  )
  
  if(any(is.na(unique(dt[position!="Defense"][,.(player_name.y)])))){
    tmp <- unique((dt[position!="Defense"][,.(player_name.x,player_name.y,player_id)]))
    stop("There are N/As in the player name")
  } else {
    dt[,player_name:=ifelse(position=="Defense",player_name.x,player_name.y)]
    dt[,player_name.x:=NULL]
    dt[,player_name.y:=NULL]
  }
  
  # check for valid player_ids
  tmp1 <- unlist(unique(dt[position!="Defense"][,.(player_id)]))
  tmp2 <- unique(dt_rosters_$player_id)
  if(!all(tmp1 %in% tmp2)){
    print("There are lookup_strings unique to dt_stats, which shouldn't happen")
    tmp3 <- anti_join(tmp1, tmp2)
    print(tmp3)
  }
  
  # create the lookup_string that will be used in the dashboard filters
  dt[,lookup_string:=ifelse(position=="Defense",paste0(position,", ",team_abbr," (",team_division,")"),lookup_string)]
  
  if(any(is.na(dt$lookup_string))){
    stop("NAs in lookup_string")
  }
  
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

## create master data tables
# create data.table for players, which is a combination of the offensive scorers plus kickers
dt_stats <- combine_stats()

# remove zero value statistics
# TODO this may or may not be a good idea for the stats but makes data set smaller for web loading
dt_stats <- dt_stats[abs(stat_values) >= 1e-7]
dt_stats <- dt_stats[team_abbr %in% playoff_teams]

# get a list of unique players and teams for the lookup
team_lookupstring_position <- rbindlist(list(
  setorder(dt_rosters[team_abbr %in% playoff_teams][,.(position, lookup_string, team_abbr)], lookup_string),
  dt_team_info[team_abbr %in% playoff_teams,.(position, lookup_string, team_abbr)]
))


dir <- "./Output/NFL Stats/"
fwrite(
  dt_stats,
  file = paste0(
    dir,
    "stats_",
    season_int,"_",
    paste0(season_type, collapse = "_"),"_gen",
    str_remove_all(Sys.time(), ":"),".csv"
  )
)


fwrite(
  dt_team_info,
  file = paste0(
    dir,
    "team_info_",
    season_int,
    "_",
    paste0(season_type, collapse = "_"),
    "_gen",
    str_remove_all(Sys.time(), ":"),
    ".csv"
  )
)

fwrite(
  dt_rosters,
  file = paste0(
    dir,
    "rosters_",
    season_int,
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
    season_int,
    "_",
    paste0(season_type, collapse = "_"),
    "_gen",
    str_remove_all(Sys.time(), ":"),
    ".csv"
  )
)




require(tidyverse)
require(data.table)

get_team_names <- function(){
  # create data.table for NFL teams
  dt <- data.table::as.data.table(nflreadr::load_teams(current = TRUE))
  dt[,team_name_w_abbr := paste0(team_name, ' (', team_abbr, ')')]
  dt <- dt[,.(team_abbr, team_name, team_name_w_abbr, team_conf, team_division, team_logo_espn)]
  return(dt)
}

get_offensive_player_stats <- function(season_year_int = season_year, season_type_char = season_type){
  # create data.table for players, which is a combination of the offensive scorers plus kickers
  dt <- data.table::as.data.table(nflreadr::load_player_stats(seasons = season_year_int, stat_type = 'offense'))
  dt <- dt[season_type %in% season_type_char]
  
  setnames(dt, old=c('recent_team'), new=c('team_abbr'))
  
  # Full Backs are considered running backs for the analysis
  dt <- dt[position %in% c('QB', 'RB', 'FB', 'WR', 'TE')]
  dt[,position := if_else(position == 'FB', 'RB', position)]
  
  # consolidate fumbles lost and 2pt conversions into one statistic
  dt[,fumbles_lost := sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost]
  dt[,two_pt_conversions := passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions]
  
  # order columns
  dt <- dt[
    , .(
      position,
      week,
      player_id,
      player_name,
      team_abbr,
      passing_yards,
      passing_tds,
      rushing_yards,
      rushing_tds,
      receiving_yards,
      receiving_tds,
      interceptions,
      sacks,
      fumbles_lost,
      two_pt_conversions
    )
  ]
  
  # change data types to double prior to melting
  vars <- c(
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
  dt[,c(vars) := lapply(.SD, as.numeric), .SDcols=vars]
  
  # melt into long format
  dt <- melt(
    dt,
    id.vars = c('position',
                'week',
                'player_id',
                'player_name',
                'team_abbr'),
    measure.vars = c(
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
    ),
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
    .default = 0L
  )
  ]
  
}

get_kicker_player_stats <- function(season_year_int = season_year, season_type_char = season_type){
  
  dt <- data.table::as.data.table(nflreadr::load_player_stats(seasons = season_year_int, stat_type = 'kicking'))
  dt <- dt[season_type %in% season_type_char]
  
  setnames(dt, old=c('team'), new=c('team_abbr'))
  
  # position is not in the original dataset
  dt[,position := 'K']
  
  # consolidate variables for fantasy scoring
  dt[,fg_made_50_ := fg_made_50_59 + fg_made_60_]
  
  # order columns
  dt <- dt[
    , .(
      position,
      week,
      player_id,
      player_name,
      team_abbr,
      fg_made,
      fg_made_40_49,
      fg_made_50_,
      fg_missed,
      fg_missed_list,
      fg_blocked,
      fg_blocked_list,
      pat_made,
      pat_missed
    )
  ]
  
  # change data types to double prior to melting
  vars <- c(
    'fg_made',
    'fg_made_40_49',
    'fg_made_50_',
    'fg_missed',
    'fg_blocked',
    'pat_made',
    'pat_missed'
  )
  dt[,c(vars) := lapply(.SD, as.numeric), .SDcols=vars]
  
  # melt data into long format
  dt <- melt(
    dt,
    id.vars = c('position',
                'week',
                'player_id',
                'player_name',
                'team_abbr'),
    measure.vars = c(
      'fg_made',
      'fg_made_40_49',
      'fg_made_50_',
      'fg_missed',
      'fg_blocked',
      'pat_made',
      'pat_missed'
    ),
    variable.factor = FALSE,
    variable.name = 'stat_label',
    value.name = 'football_value'
  )
  
  # calculate fantasy points
  dt[, fantasy_points := case_when(
    stat_label == 'fg_made' ~ as.integer(football_value) * 3L,
    stat_label == 'fg_made_40_49' ~ as.integer(football_value) * 1L, # this is a bonus
    stat_label == 'fg_made_50_' ~ as.integer(football_value) * 2L, # this is a bonus
    stat_label == 'fg_missed' ~ as.integer(football_value) * -1L,
    stat_label == 'pat_made' ~ as.integer(football_value) * 1L,
    stat_label == 'pat_missed' ~ as.integer(football_value) * -1L,
    .default = 0L
  )]
  
}

get_player_stats <- function(teams = dt_nfl_teams){
  
  # bind rows
  dt <- rbindlist(list(get_offensive_player_stats(), get_kicker_player_stats()))
  
  # join in team conf and division
  dt <- merge(dt, teams[,.(team_abbr, team_conf, team_division)], all.x = TRUE)
  
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
      'lookup_string',
      'player_id',
      'player_name',
      'team_abbr',
      'team_conf',
      'team_division'
    )
  )
}

get_position_stats <- function(dt, pos, summarized_boolean, long_format_boolean){
  
  if(!(pos %in% c("K","QB","RB","TE","WR"))){
    print(paste0(pos, " is not a valid position"))
  }
  
  dt <- dt[position == pos]
  
  if(summarized_boolean){

    grouping_by <- c(
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
    return(dt)
  } else {
    
    if(summarized_boolean){
      # does not include the `week` variable when summarized
      dt <- dcast(
        dt,
        position + lookup_string + player_id + player_name + team_abbr + team_conf + team_division ~ stat_label,
        value.var = c('football_value', 'fantasy_points'),
        fill = 0
      )
      return(dt)      
    } else {
      dt <- dcast(
        dt,
        position + week + lookup_string + player_id + player_name + team_abbr + team_conf + team_division ~ stat_label,
        value.var = c('football_value', 'fantasy_points'),
        fill = 0
      )
      return(dt)
    }
  }
  
}

order_cols <- function(dt){
  
  master_order <- c(
    'position',
    'lookup_string',
    'week',
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
    'football_value_total_fumbles_lost',
		'football_value_total_interceptions',
		'football_value_total_passing_tds',
		'football_value_total_passing_yards',
		'football_value_total_receiving_tds',
		'football_value_total_receiving_yards',
		'football_value_total_rushing_tds',
		'football_value_total_rushing_yards',
		'football_value_total_sacks',
		'football_value_total_two_pt_conversions',
		'football_value_total_fg_blocked',
		'football_value_total_fg_made',
		'football_value_total_fg_made_40_49',
		'football_value_total_fg_made_50_',
		'football_value_total_fg_missed',
		'football_value_total_pat_made',
		'football_value_total_pat_missed',
		'football_value_fumbles_lost',
		'football_value_interceptions',
		'football_value_passing_tds',
		'football_value_passing_yards',
		'football_value_receiving_tds',
		'football_value_receiving_yards',
		'football_value_rushing_tds',
		'football_value_rushing_yards',
		'football_value_sacks',
		'football_value_two_pt_conversions',
		'football_value_fg_blocked',
		'football_value_fg_made',
		'football_value_fg_made_40_49',
		'football_value_fg_made_50_',
		'football_value_fg_missed',
		'football_value_pat_made',
		'football_value_pat_missed',
		'fantasy_points_total_fumbles_lost',
		'fantasy_points_total_interceptions',
		'fantasy_points_total_passing_tds',
		'fantasy_points_total_passing_yards',
		'fantasy_points_total_receiving_tds',
		'fantasy_points_total_receiving_yards',
		'fantasy_points_total_rushing_tds',
		'fantasy_points_total_rushing_yards',
		'fantasy_points_total_sacks',
		'fantasy_points_total_two_pt_conversions',
		'fantasy_points_total_fg_blocked',
		'fantasy_points_total_fg_made',
		'fantasy_points_total_fg_made_40_49',
		'fantasy_points_total_fg_made_50_',
		'fantasy_points_total_fg_missed',
		'fantasy_points_total_pat_made',
		'fantasy_points_total_pat_missed',
		'fantasy_points_fumbles_lost',
		'fantasy_points_interceptions',
		'fantasy_points_passing_tds',
		'fantasy_points_passing_yards',
		'fantasy_points_receiving_tds',
		'fantasy_points_receiving_yards',
		'fantasy_points_rushing_tds',
		'fantasy_points_rushing_yards',
		'fantasy_points_sacks', 
		'fantasy_points_two_pt_conversions',
		'fantasy_points_fg_blocked',
		'fantasy_points_fg_made',
		'fantasy_points_fg_made_40_49',
		'fantasy_points_fg_made_50_',
		'fantasy_points_fg_missed',
		'fantasy_points_pat_made', 
		'fantasy_points_pat_missed'
  )
  
  found_order <- names(dt)
  
  unmapped_cols <- found_order[!(found_order %in% master_order)]
  
  if(length(unmapped_cols)){
    print("There are unmapped columns in the dataset")
    print(paste0(unmapped_cols, collapse = "; "))
  }
  
  preferred_order <- master_order[master_order %in% found_order]
  
  return(dt[,..preferred_order])
  
}

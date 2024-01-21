# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(tidyverse)
library(shiny)
library(data.table)
library(DT)
library(shinyjs)
library(shinythemes)
library(plotly)
library(lpSolve)

source("helperFuncs.R")
source("perfectLineupMod.R")
source("howToPlayFunc.R")
source("rosterBuilderMod.R")
source("nflPlayerStatsMod.R")
source("fantasyResultsByRosterMod.R")
source("additionalAnalysisMod.R")


playoff_year <- 2023L
# season_type <- c("REG","POST")
season_teams <- c(
  "ARI","ATL","BAL","BUF","CAR",
  "CHI","CIN","CLE","DAL","DEN",
  "DET","GB","HOU","IND","JAX",
  "KC","LA","LAC","LV","MIA",
  "MIN","NE","NO","NYG","NYJ",
  "PHI","PIT","SEA","SF","TB",
  "TEN","WAS"
)

playoff_teams <- c("BAL","BUF","CLE","DAL","DET","GB","HOU","KC","LA","MIA","PHI","PIT","SF","TB")


dt_team_info <- fread(get_last_csv("team_info"))

dt_nfl_rosters <- fread(get_last_csv("rosters"))

dt_stats <- fread(get_last_csv("stats"))

team_lookupstring_position <- fread(get_last_csv("lookups"))

dt_scores <- fread(get_last_csv("NFL Fantasy Scores"))

# this filtering was previously performed in the pipeline
# dt_scores <- dt_scores[stat_type == "fantasy_points"] 

dt_fantasy_rosters <- fread(get_last_csv("Playoff Fantasy"))
if("Fantasy Owner Email" %in% names(dt_fantasy_rosters)){
  dt_fantasy_rosters[,`Fantasy Owner Email`:=NULL]
}

# add in player_name to data
dt_fantasy_rosters <- merge(
  dt_fantasy_rosters, 
  unique(dt_stats[,player_id:=ifelse(position=="Defense",team_abbr,player_id)][,.(player_id,player_name)]),
  by = "player_id",
  all.x = TRUE
)
dt_fantasy_rosters[,position_code:=ifelse(position_code=="D","Defense",position_code)]
dt_fantasy_rosters[,position_code:=factor(position_code, c("QB1","QB2","QB3","RB1","RB2","RB3","RB4","WR1","WR2","WR3","WR4","TE1","TE2","TE3","K","Defense"))]
setorder(dt_fantasy_rosters, fantasy_team_and_initials,position_code,player_name)

summary_by_team <- dt_fantasy_rosters |> 
  distinct(fantasy_team_and_initials) |> 
  left_join(
    dt_scores |> 
      group_by(fantasy_team_and_initials, week) |> 
      reframe(fantasy_points = sum(stat_values)),
    by = c("fantasy_team_and_initials")
  ) |> 
  pivot_wider(names_from = week, names_prefix = "week_", values_from = fantasy_points, values_fill = 0) |> 
  mutate(
    fantasy_points = rowSums(across(starts_with("week")))
  ) |> 
  arrange(-fantasy_points) |> 
  mutate(rank = 1:n()) |> 
  as.data.table()

summary_by_team_and_player <- dt_fantasy_rosters |> 
  select(fantasy_team_and_initials, team_abbr, position_code, player_name, player_id) |> 
  left_join(
    dt_stats |> 
      filter(season_type=="Post" & stat_type=="fantasy_points") |> 
      group_by(player_id, week) |> 
      reframe(fantasy_points = sum(stat_values)) |> 
      pivot_wider(names_from = week, names_prefix = "week_", values_from = fantasy_points),
    by = c("player_id"),
    relationship = "many-to-one"
  ) |> 
  mutate(
    across(starts_with("week"), ~ifelse(is.na(.x),0,.x)),
    fantasy_team_and_initials = 
      factor(fantasy_team_and_initials, 
      levels=fct_inorder(summary_by_team$fantasy_team_and_initials)
    )
  ) |> 
  mutate(fantasy_points = rowSums(across(starts_with("week")))) |> 
  arrange(fantasy_team_and_initials, position_code) |> 
  as.data.table()

# add in rank to fantasy rosters
dt_fantasy_rosters <- dt_fantasy_rosters |> 
  left_join(summary_by_team |> 
              select(fantasy_team_and_initials, rank), 
            by = c("fantasy_team_and_initials"))


# dt_treemap <- bind_rows(
#   dt_scores |>
#     filter(abs(stat_values) >= 0) |> 
#     mutate(
#       position = case_when(
#         str_detect(position_code,"QB") ~ "QB",
#         str_detect(position_code,"RB") ~ "RB",
#         str_detect(position_code,"WR") ~ "WR",
#         str_detect(position_code,"TE") ~ "TE",
#         str_detect(position_code,"K") ~ "K",
#         str_detect(position_code,"Defense") ~ "D"
#       )
#     ) |> 
#     distinct(position) |> 
#     mutate(ids = position,
#            parents = "") |> 
#     rename(labels = position) |> 
#     mutate(values = 1),
#   dt_scores |>
#     filter(abs(stat_values) >= 0) |> 
#     mutate(
#       position = case_when(
#         str_detect(position_code,"QB") ~ "QB",
#         str_detect(position_code,"RB") ~ "RB",
#         str_detect(position_code,"WR") ~ "WR",
#         str_detect(position_code,"TE") ~ "TE",
#         str_detect(position_code,"K") ~ "K",
#         str_detect(position_code,"Defense") ~ "D"
#       )
#     ) |>
#     distinct(position, team_abbr) |>
#     unite("ids", c(position, team_abbr), remove = FALSE, sep = "-") |>
#     mutate(parents = position) |> 
#     rename(labels = team_abbr) |> 
#     mutate(values = 1),
#   dt_scores |>
#     filter(abs(stat_values) >= 0) |> 
#     mutate(
#       position = case_when(
#         str_detect(position_code,"QB") ~ "QB",
#         str_detect(position_code,"RB") ~ "RB",
#         str_detect(position_code,"WR") ~ "WR",
#         str_detect(position_code,"TE") ~ "TE",
#         str_detect(position_code,"K") ~ "K",
#         str_detect(position_code,"Defense") ~ "D"
#       )
#     ) |>
#     distinct(position, team_abbr, player_name) |>
#     unite("ids", c(team_abbr, player_name), remove = FALSE, sep = "-") |>
#     unite("parents", c(position, team_abbr), remove = FALSE, sep = "-") |>
#     rename(labels = player_name) |> 
#     mutate(values = 1),
#   dt_scores |>
#     filter(abs(stat_values) >= 0) |> 
#     mutate(
#       position = case_when(
#         str_detect(position_code,"QB") ~ "QB",
#         str_detect(position_code,"RB") ~ "RB",
#         str_detect(position_code,"WR") ~ "WR",
#         str_detect(position_code,"TE") ~ "TE",
#         str_detect(position_code,"K") ~ "K",
#         str_detect(position_code,"Defense") ~ "D"
#       )
#     ) |>
#     group_by(team_abbr, player_name, fantasy_team_and_initials) |>
#     reframe(values = sum(stat_values)) |>
#     unite("ids", c(player_name, fantasy_team_and_initials), remove = FALSE, sep = "-") |>
#     unite("parents", c(team_abbr, player_name), remove = FALSE, sep = "-") |>
#     rename(labels = fantasy_team_and_initials)
# )
# 
# tm_overall <- plot_ly(
#   dt_treemap,
#   type="treemap",
#   labels = ~labels,
#   parents = ~parents,
#   ids = ~ids,
#   values = ~values
# )
# tm_overall

# dt <- dt_stats[season_type=="Post" & stat_type=="fantasy_points"]
# dt <- dt[,.(values = sum(stat_values)), by = .(position, stat_label)]
# dt <- dt[,parent1:=""]
# dt <- rbindlist(list(
#   dt[,values:= 1][,.(parent = parent1, ids = position, labels = position, values)],
#   dt[,labels:= stat_label][,ids:=paste0(position,"-",stat_label)][,parent:= position][,.(parent,ids,labels,values)]
# ))
# 
# tm_overall <- plot_ly(
#   dt,
#   type="treemap",
#   labels = ~labels,
#   parents = ~parent,
#   ids = ~labels,
#   values = ~values
# )
# 
# tm_overall



ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.freelancer.css"),
    tags$title("Playoff Fantasy Football League")
  ),
  tags$h1("Playoff Fantasy Football League", style = "text-align:center"),
  tabsetPanel(
    tabPanel(
      "Fantasy Results",
      br(),
      tabsetPanel(
        type = "pills",
        tabPanel(
          "By Roster",
          fantasyResultsbyRosterUI("by_roster", summary_by_team)
        ),
        tabPanel(
          "Perfect Lineup",
          perfectLineupUI("perf", dt_stats)
        ),
        tabPanel(
          "Additional Analysis",
          additionalAnalysisUI("a_a")
        ) 
      )
    ),
    tabPanel(
      "NFL Player Stats",
      nflPlayerStatsUI("nfl_ps", dt_team_info, playoff_teams, playoff_year)
    ),
    tabPanel(
      "How to Play",
      fluidPage(howToPlayUIonly())
    ),
    # uncomment this code when needed for creating rosters
    # tabPanel(
    #   "Build Roster",
    #   buildRosterUI("b_r", team_lookupstring_position)
    # )
  )
)

server <- function(input, output, session) {
  
  # explore stats tab
  nflPlayerStatsServer("nfl_ps", dt_stats, dt_team_info, playoff_teams)
  
  # by roster sub-tab
  fantasyResultsbyRosterServer("by_roster", summary_by_team, summary_by_team_and_player)
  
  # perfect_lineup sub-tab
  perfectLineupServer("perf", dt_stats, playoff_teams)
  
  # additional analysis sub-tab
  additionalAnalysisServer("a_a", dt_fantasy_rosters)
  
  # # this section is for Roster Selection; uncomment to make active
  # buildRosterServer("b_r", team_lookupstring_position)
  
}


shinyApp(ui, server)


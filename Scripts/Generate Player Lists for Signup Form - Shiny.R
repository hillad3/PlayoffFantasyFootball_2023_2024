# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(shiny)
library(DT)

nfl_teams <- nflreadr::load_teams(current = TRUE) %>%
  as_tibble() %>%
  select(team_abbr, team_name, team_conf, team_division, team_logo_espn) %>%
  mutate(team_name_w_abbr = paste0(team_name, " (", team_abbr, ")"))

offensive_player_stats <- nflreadr::load_player_stats(seasons = 2023L, stat_type = "offense") %>%
  as_tibble() %>%
  filter(position %in% c("QB", "RB", "FB", "WR", "TE")) %>%
  mutate(position = if_else(position == "FB", "RB", position)) %>%
  left_join(nfl_teams %>% select(team_abbr, team_conf, team_division), by = c("recent_team" = "team_abbr")) %>% 
  select(
    week,
    player_id,
    player = player_name,
    team_abbr = recent_team,
    team_conf,
    team_division,
    position,
    passing_yards,
    passing_tds,
    rushing_yards,
    rushing_tds,
    receiving_yards,
    receiving_tds,
    interceptions,
    sacks,
    sack_fumbles,
    sack_fumbles_lost,
    rushing_fumbles,
    rushing_fumbles_lost,
    receiving_fumbles,
    receiving_fumbles_lost,
    passing_2pt_conversions,
    rushing_2pt_conversions,
    receiving_2pt_conversions,
  ) %>%
  mutate(week = as.integer(week))

qb_player_stats <- offensive_player_stats %>% 
  filter(position %in% c("QB")) %>%
  select(
    week,
    player_id,
    player,
    team_abbr,
    team_conf,
    team_division,
    passing_yards,
    passing_tds,
    rushing_yards,
    rushing_tds,
    receiving_yards,
    receiving_tds,
    interceptions,
    sacks,
    sack_fumbles,
    sack_fumbles_lost,
    rushing_fumbles,
    rushing_fumbles_lost,
    receiving_fumbles,
    receiving_fumbles_lost,
    passing_2pt_conversions,
    rushing_2pt_conversions,
    receiving_2pt_conversions,
  ) %>%
  mutate(week = as.integer(week)) %>% 
  arrange(desc(passing_yards))

qb_player_season_stats <- qb_player_stats %>% 
  group_by(player_id, player, team_abbr, team_conf, team_division) %>% 
  reframe(
    games_played = n(),
    total_passing_yards = sum(passing_yards),
    avg_passing_yards_per_game = mean(passing_yards, na.rm = TRUE) %>% round(0),
    total_passing_tds = sum(passing_tds),
    avg_passing_tds_per_game = mean(passing_tds, na.rm = TRUE) %>% round(2),
    total_rushing_yards = sum(rushing_yards),
    avg_rushing_yards_per_game = mean(rushing_yards, na.rm = TRUE) %>% round(0),
    total_rushing_tds = sum(rushing_tds),
    avg_rushing_tds_per_game = mean(rushing_tds, na.rm = TRUE) %>% round(2),
    total_receiving_yards = sum(receiving_yards),
    avg_receiving_yards_per_game = mean(receiving_yards, na.rm = TRUE) %>% round(0),
    total_receiving_tds = sum(receiving_tds),
    avg_receiving_tds_per_game = mean(receiving_tds, na.rm = TRUE) %>% round(2),
  ) %>% 
  arrange(desc(total_passing_yards))

rb_player_stats <- offensive_player_stats %>% 
  filter(position %in% c("RB")) %>%
  select(
    week,
    player_id,
    player,
    team_abbr,
    team_conf,
    team_division,
    rushing_yards,
    rushing_tds,
    receiving_yards,
    receiving_tds,
    rushing_fumbles,
    rushing_fumbles_lost,
    receiving_fumbles,
    receiving_fumbles_lost,
    passing_2pt_conversions,
    rushing_2pt_conversions,
    receiving_2pt_conversions,
  ) %>%
  mutate(week = as.integer(week)) %>% 
  arrange(desc(rushing_yards))

rb_player_season_stats <- rb_player_stats %>% 
  group_by(player_id, player, team_abbr, team_conf, team_division) %>% 
  reframe(
    games_played = n(),
    total_rushing_yards = sum(rushing_yards),
    avg_rushing_yards_per_game = mean(rushing_yards, na.rm = TRUE) %>% round(0),
    total_rushing_tds = sum(rushing_tds),
    avg_rushing_tds_per_game = mean(rushing_tds, na.rm = TRUE) %>% round(2),
    total_receiving_yards = sum(receiving_yards),
    avg_receiving_yards_per_game = mean(receiving_yards, na.rm = TRUE) %>% round(0),
    total_receiving_tds = sum(receiving_tds),
    avg_receiving_tds_per_game = mean(receiving_tds, na.rm = TRUE) %>% round(2),
  ) %>% 
  arrange(desc(total_rushing_yards))

wr_player_stats <- offensive_player_stats %>% 
  filter(position %in% c("WR")) %>%
  select(
    week,
    player_id,
    player,
    team_abbr,
    team_conf,
    team_division,
    receiving_yards,
    receiving_tds,
    receiving_fumbles,
    receiving_fumbles_lost,
    rushing_yards,
    rushing_tds,
    passing_2pt_conversions,
    rushing_2pt_conversions,
    receiving_2pt_conversions,
  ) %>%
  mutate(week = as.integer(week)) %>% 
  arrange(desc(receiving_yards))

wr_player_season_stats <- wr_player_stats %>% 
  group_by(player_id, player, team_abbr, team_conf, team_division) %>% 
  reframe(
    games_played = n(),
    total_receiving_yards = sum(receiving_yards),
    avg_receiving_yards_per_game = mean(receiving_yards, na.rm = TRUE) %>% round(0),
    total_receiving_tds = sum(receiving_tds),
    avg_receiving_tds_per_game = mean(receiving_tds, na.rm = TRUE) %>% round(2),
    total_rushing_yards = sum(rushing_yards),
    avg_rushing_yards_per_game = mean(rushing_yards, na.rm = TRUE) %>% round(0),
    total_rushing_tds = sum(rushing_tds),
    avg_rushing_tds_per_game = mean(rushing_tds, na.rm = TRUE) %>% round(2),
  ) %>% 
  arrange(desc(total_receiving_yards))

te_player_stats <- offensive_player_stats %>% 
  filter(position %in% c("TE")) %>%
  select(
    week,
    player_id,
    player,
    team_abbr,
    team_conf,
    team_division,
    receiving_yards,
    receiving_tds,
    receiving_fumbles,
    receiving_fumbles_lost,
    passing_2pt_conversions,
    rushing_2pt_conversions,
    receiving_2pt_conversions,
  ) %>%
  mutate(week = as.integer(week)) %>% 
  arrange(desc(receiving_yards))

te_player_season_stats <- te_player_stats %>% 
  group_by(player_id, player, team_abbr, team_conf, team_division) %>% 
  reframe(
    games_played = n(),
    total_receiving_yards = sum(receiving_yards),
    avg_receiving_yards_per_game = mean(receiving_yards, na.rm = TRUE) %>% round(0),
    total_receiving_tds = sum(receiving_tds),
    avg_receiving_tds_per_game = mean(receiving_tds, na.rm = TRUE) %>% round(2),
  ) %>% 
  arrange(desc(total_receiving_yards))

kicking_player_stats <- nflreadr::load_player_stats(seasons = 2023L, stat_type = "kicking") %>%
  as_tibble() %>%
  left_join(nfl_teams %>% select(team_abbr, team_conf, team_division), by = c("team" = "team_abbr")) %>% 
  select(
    week,
    player_id,
    player = player_name,
    team_abbr = team,
    team_conf,
    team_division,
    everything()
  ) %>%
  mutate(week = as.integer(week)) %>% 
  arrange(desc(fg_made))

kicking_player_season_stats <- kicking_player_stats %>% 
  group_by(player_id, player, team_abbr, team_conf, team_division) %>% 
  reframe(
    games_played = n(),
    total_fg_made = sum(fg_made),
    max_fg_made = max(fg_long),
    total_pat_made = sum(pat_made),
  ) %>% 
  arrange(desc(total_fg_made))

ui <- fluidPage(
  titlePanel("Playoff Fantasy Football"),
  sidebarLayout(
    sidebarPanel(

      # this is a single select way to provide positions for the DT table
      selectInput(
        inputId = "selected_position",
        label = "Choose Position:",
        choices = list("QB", "RB", "WR", "TE", "K"),
        selected = "QB"
      ),

      # # this is an alternative multi-select way to provide positions
      # checkboxGroupInput(
      #   "selected_positions",
      #   "Choose Position(s):",
      #   choiceNames = list("Quarterback (QB)", "Running Back (RB)", "Wide Receiver (WR)", "Tightend (TE)", "Kicker (K)"),
      #   choiceValues = list("QB", "RB", "WR", "TE", "K"),
      #   selected = list("QB", "RB", "WR", "TE", "K")
      # ),

      checkboxGroupInput(
        "selected_teams",
        "Choose Teams:",
        choiceNames = as.list(nfl_teams$team_name_w_abbr),
        choiceValues = as.list(nfl_teams$team_abbr),
        selected = as.list(nfl_teams$team_abbr)
      ),
      
      width = 2
    ),
    mainPanel(tabsetPanel(
      tabPanel("Statistics", tabsetPanel(
        p(),
        tabPanel("Season Stats", DTOutput("statistics_season")),
        tabPanel("Weekly Stats", DTOutput("statistics_weekly")) 
      )),
      tabPanel("Create Fantasy Roster")
    ))
  )
)

server <- function(input, output) {
  # list the names of teams in the playoffs in quotes
  # all teams are listed below as a default

  x <- reactive({
    input$selected_position
  })

  output$statistics_weekly <- renderDT({
    if (x() == "K") {
      kicking_player_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "QB") {
      qb_player_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "RB") {
      rb_player_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "WR") {
      wr_player_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "TE") {
      te_player_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } 
  })
  
  output$statistics_season <- renderDT({
    if (x() == "K") {
      kicking_player_season_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "QB") {
      qb_player_season_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "RB") {
      rb_player_season_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "WR") {
      wr_player_season_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } else if (x() == "TE") {
      te_player_season_stats %>%
        filter(team_abbr %in% input$selected_teams)
    } 
  })
}


shinyApp(ui, server)

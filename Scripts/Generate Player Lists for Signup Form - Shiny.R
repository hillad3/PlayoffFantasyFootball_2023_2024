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

nfl_player_stats <- bind_rows(
  nflreadr::load_player_stats(seasons = 2023L, stat_type = "offense") %>%
    as_tibble() %>%
    filter(position %in% c("QB", "RB", "FB", "WR", "TE")) %>%
    mutate(
      position = if_else(position == "FB", "RB", position),
      fumbles_lost = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost,
      two_pt_conversions = passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions,
    ) %>% 
    rename(team_abbr = recent_team),
  nflreadr::load_player_stats(seasons = 2023L, stat_type = "kicking") %>%
    as_tibble() %>%
    rename(team_abbr = team) %>% 
    mutate(position = "K")
) %>%
  left_join(
    nfl_teams %>% select(team_abbr, team_conf, team_division), by = c("team_abbr")
  ) %>% 
  mutate(
    week = as.integer(week),
    player_concat = paste0(position,": ",player_id,", ",player_name," (",team_abbr,"; ",team_division,")")
  ) %>% 
  select(
    week,
    player_concat,
    player_id,
    player_name,
    team_abbr,
    team_conf,
    team_division,
    position,
    everything()
  ) %>% 
  arrange(position, player_name, week)

selectable_players <- nfl_player_stats$player_concat

offensive_player_stats <- nfl_player_stats %>% 
  filter(position %in% c("QB", "RB", "FB", "WR", "TE")) %>%
  select(
    week,
    player_id,
    player_name,
    team_abbr,
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
    fumbles_lost,
    two_pt_conversions,
  )

qb_player_stats <- nfl_player_stats %>% 
  filter(position %in% c("QB")) %>%
  select(
    week,
    player_id,
    player_name,
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
    fumbles_lost,
    two_pt_conversions,
  ) %>%
  arrange(desc(passing_yards))

qb_player_season_stats <- qb_player_stats %>% 
  group_by(player_id, player_name, team_abbr, team_conf, team_division) %>% 
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

rb_player_stats <- nfl_player_stats %>% 
  filter(position %in% c("RB")) %>%
  select(
    week,
    player_id,
    player_name,
    team_abbr,
    team_conf,
    team_division,
    rushing_yards,
    rushing_tds,
    receiving_yards,
    receiving_tds,
    fumbles_lost,
    two_pt_conversions,
  ) %>%
  arrange(desc(rushing_yards))

rb_player_season_stats <- rb_player_stats %>% 
  group_by(player_id, player_name, team_abbr, team_conf, team_division) %>% 
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

wr_player_stats <- nfl_player_stats %>% 
  filter(position %in% c("WR")) %>%
  select(
    week,
    player_id,
    player_name,
    team_abbr,
    team_conf,
    team_division,
    receiving_yards,
    receiving_tds,
    rushing_yards,
    rushing_tds,
    fumbles_lost,
    two_pt_conversions,
  ) %>%
  arrange(desc(receiving_yards))

wr_player_season_stats <- wr_player_stats %>% 
  group_by(player_id, player_name, team_abbr, team_conf, team_division) %>% 
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

te_player_stats <- nfl_player_stats %>% 
  filter(position %in% c("TE")) %>%
  select(
    week,
    player_id,
    player_name,
    team_abbr,
    team_conf,
    team_division,
    receiving_yards,
    receiving_tds,
    fumbles_lost,
    two_pt_conversions,
  ) %>%
  arrange(desc(receiving_yards))

te_player_season_stats <- te_player_stats %>% 
  group_by(player_id, player_name, team_abbr, team_conf, team_division) %>% 
  reframe(
    games_played = n(),
    total_receiving_yards = sum(receiving_yards),
    avg_receiving_yards_per_game = mean(receiving_yards, na.rm = TRUE) %>% round(0),
    total_receiving_tds = sum(receiving_tds),
    avg_receiving_tds_per_game = mean(receiving_tds, na.rm = TRUE) %>% round(2),
  ) %>% 
  arrange(desc(total_receiving_yards))

kicking_player_stats <- nfl_player_stats %>% 
  select(
    week,
    player_id,
    player_name,
    team_abbr,
    team_conf,
    team_division,
    fg_made,
    fg_missed,
    fg_long,
    pat_made,
    pat_missed
  ) %>%
  arrange(desc(fg_made))

kicking_player_season_stats <- kicking_player_stats %>% 
  group_by(player_id, player_name, team_abbr, team_conf, team_division) %>% 
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
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Statistics",
          tabsetPanel(
            p(),
            tabPanel("2023 Season Totals", DTOutput("statistics_season")),
            tabPanel("By Week", DTOutput("statistics_weekly"))
          )
        ),
        tabPanel(
          "Create Fantasy Roster",
          sidebarLayout(
            sidebarPanel(
              width = 5,
              selectizeInput(
                inputId = "selected_players",
                label = "Choose Players:",
                choices = as.list(selectable_players),
                multiple = TRUE,
                options = list(maxItems = 13)
              )
            ),
            mainPanel(
              
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
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
  
  observe({
    updateSelectizeInput(
      session,
      "selected_players",
      choices = selectable_players,
      options = list(maxItems = 13),
      selected = input$selected_players
    )
  })
  
}


shinyApp(ui, server)

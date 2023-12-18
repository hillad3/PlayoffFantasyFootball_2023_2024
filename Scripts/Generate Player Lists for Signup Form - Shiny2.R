# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(shiny)
library(data.table)

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
    player_lookup = paste0(position,": ",player_id,", ",player_name," (",team_abbr,"; ",team_division,")")
  ) %>% 
  select(
    position,
    week,
    player_lookup,
    player_id,
    player_name,
    team_abbr,
    team_conf,
    team_division,
    everything()
  ) %>% 
  arrange(position, player_name, week)

offensive_player_stats <- nfl_player_stats %>% 
  filter(position %in% c("QB", "RB", "FB", "WR", "TE")) %>%
  select(
    position,
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
  )

qb_player_stats <- nfl_player_stats %>% 
  filter(position %in% c("QB")) %>%
  select(
    position,
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
  group_by(position, player_id, player_name, team_abbr, team_conf, team_division) %>% 
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
    position,
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
  group_by(position, player_id, player_name, team_abbr, team_conf, team_division) %>% 
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
    position,
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
  group_by(position, player_id, player_name, team_abbr, team_conf, team_division) %>% 
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
    position,
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
  group_by(position, player_id, player_name, team_abbr, team_conf, team_division) %>% 
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
    position,
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
  group_by(position, player_id, player_name, team_abbr, team_conf, team_division) %>% 
  reframe(
    games_played = n(),
    total_fg_made = sum(fg_made),
    max_fg_made = max(fg_long),
    total_pat_made = sum(pat_made),
  ) %>% 
  arrange(desc(total_fg_made))

qb_players <- nfl_player_stats %>% filter(position %in% c("QB")) %>% select(player_lookup) %>% as.list()
rb_players <- nfl_player_stats %>% filter(position %in% c("RB")) %>% select(player_lookup) %>% as.list()
wr_players <- nfl_player_stats %>% filter(position %in% c("WR")) %>% select(player_lookup) %>% as.list()
te_players <- nfl_player_stats %>% filter(position %in% c("TE")) %>% select(player_lookup) %>% as.list()
flex_players <- c(rb_players, wr_players, te_players)
k_players <- nfl_player_stats %>% filter(position %in% c("K")) %>% select(player_lookup) %>% as.list()
def_teams <- nfl_teams %>% select(team_abbr) %>% as.list()

ui <- fluidPage(
  titlePanel("Playoff Fantasy Football"),
  tabsetPanel(
    tabPanel(
      "Statistics",
      sidebarLayout(
        sidebarPanel(
          # this is a single select way to provide positions for the DT table
          selectInput(
            inputId = "selected_position",
            label = "Choose Position:",
            choices = list("QB", "RB", "WR", "TE", "K"),
            selected = "QB"
          ),
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
            p(),
            tabPanel("2023 Season Totals", DTOutput("statistics_season")),
            tabPanel("By Week", DTOutput("statistics_weekly"))
          )
        )
      )
    ),
    tabPanel(
      "Select Roster",
      selectizeInput(
        inputId = "selected_qb1",
        label = "Quarterback (1 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_qb2",
        label = "Quarterback (2 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_qb3",
        label = "Quarterback (3 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_rb1",
        label = "Running Back (1 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_rb2",
        label = "Running Back (2 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_rb3",
        label = "Running Back (3 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_wr1",
        label = "Wide Receiver (1 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_wr2",
        label = "Wide Receiver (2 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_wr3",
        label = "Wide Receiver (3 of 3):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_te1",
        label = "Tight End (1 of 2):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_te2",
        label = "Tight End (2 of 2):",
        choices = NULL
      ),
      selectizeInput(
        inputId = "selected_flex",
        label = "Flex Position (RB, WR or TE):",
        choices = flex_players,
        multiple = FALSE
      ),
      selectizeInput(
        inputId = "selected_k",
        label = "Kicker:",
        choices = k_players,
        multiple = FALSE
      ),
      selectInput(
        inputId = "selected_defense",
        label = "Choose Defensive Team:",
        choices = def_teams,
        multiple = FALSE
      ),
      width = 5  
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
      "selected_qb1",
      choices = qb_players[!(qb_players %in% list(input$selected_qb1,input$selected_qb2,input$selected_qb3))],
      server = TRUE,
      selected = input$selected_qb1,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_qb2",
      choices = qb_players[!(qb_players %in% list(input$selected_qb1,input$selected_qb2,input$selected_qb3))],
      server = TRUE,
      selected = input$selected_qb2,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_qb3",
      choices = qb_players[!(qb_players %in% list(input$selected_qb1,input$selected_qb2,input$selected_qb3))],
      server = TRUE,
      selected = input$selected_qb3,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_rb1",
      choices = rb_players[!(rb_players %in% list(input$selected_rb1,input$selected_rb2,input$selected_rb3))],
      server = TRUE,
      selected = input$selected_rb1,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_rb2",
      choices = rb_players[!(rb_players %in% list(input$selected_rb1,input$selected_rb2,input$selected_rb3))],
      server = TRUE,
      selected = input$selected_rb2,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_rb3",
      choices = rb_players[!(rb_players %in% list(input$selected_rb1,input$selected_rb2,input$selected_rb3))],
      server = TRUE,
      selected = input$selected_rb3,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_wr1",
      choices = wr_players[!(wr_players %in% list(input$selected_wr1,input$selected_wr2,input$selected_wr3))],
      server = TRUE,
      selected = input$selected_wr1,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_wr2",
      choices = wr_players[!(wr_players %in% list(input$selected_wr1,input$selected_wr2,input$selected_wr3))],
      server = TRUE,
      selected = input$selected_wr2,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_wr3",
      choices = wr_players[!(wr_players %in% list(input$selected_wr1,input$selected_wr2,input$selected_wr3))],
      server = TRUE,
      selected = input$selected_wr3,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_te1",
      choices = te_players[!(te_players %in% list(input$selected_te1,input$selected_te2))],
      server = TRUE,
      selected = input$selected_te1,
      options = list(maxItems = 1)
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "selected_te2",
      choices = te_players[!(te_players %in% list(input$selected_te1,input$selected_te2))],
      server = TRUE,
      selected = input$selected_te2,
      options = list(maxItems = 1)
    )
  })
  
}


shinyApp(ui, server)


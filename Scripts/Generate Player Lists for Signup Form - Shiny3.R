# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(shiny)
library(data.table)
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

team_player_position <- nfl_player_stats %>% distinct(team_abbr, player_lookup, position) %>% as.data.table()
player_choices <- team_player_position %>% distinct(player_lookup) %>% as.list()
def_teams_choices <- nfl_teams %>% distinct(team_abbr) %>% as.list()

ui <- fluidPage(
  titlePanel("Playoff Fantasy Football"),
  tabsetPanel(
    tabPanel(
      "Regular Season 2023 Stats",
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
            tabPanel("Season Totals", DTOutput("statistics_season")),
            tabPanel("Weekly Totals", DTOutput("statistics_weekly"))
          )
        )
      )
    ),
    tabPanel(
      "Select Roster",
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            inputId = "selected_players",
            label = "Select players:",
            choices = c("",player_choices),
            options = list(maxItems = 13)
          ),
          selectizeInput(
            inputId = "selected_defense",
            label = "Select a Defensive Team:",
            choices = c("",def_teams_choices),
            selected = NULL,
            options = list(maxItems = 1)
          ),
          width = 3 
        ),
        mainPanel(
          textOutput(outputId = "player_slots_remaining_text"),
          textOutput(outputId = "teams_selected_text"),
          textOutput(outputId = "teams_unselected_text"),
          textOutput(outputId = "players_remaining_text"),
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
  
  
  # count the number of roster spots available and display text
  player_slots_remaining <- reactive({
    13-length(input$selected_players)
  }) 
  output$player_slots_remaining_text <- renderText({
      paste0("Open player slot(s) remaining: ", player_slots_remaining())
  })
  
  # keep track of teams selected on the roster
  teams_selected <- reactive({
    if(input$selected_defense == ""){
      c(
        team_player_position[player_lookup %in% input$selected_players, team_abbr] %>% 
          unique()
      ) %>% 
        sort()
    } else {
      c(
        team_player_position[player_lookup %in% input$selected_players, team_abbr] %>% 
          unique(),
        input$selected_defense
      ) %>% 
        sort()
    }
  })
  output$teams_selected_text <- renderText({
    paste0("Teams on roster: ", paste0(teams_selected(), collapse = ",  "))
  })
  
  
  
  teams_unselected <- reactive({
    team_player_position[!(team_abbr %in% teams_selected()), team_abbr] %>% unique() %>% sort()
  }) 
  output$teams_unselected_text <- renderText({
    paste0("Teams available: ", paste0(teams_unselected() %>% unlist(), collapse = ",  "))
  })
  
  # teams_remaining <- reactive({
  #   def_teams_choices[[1]][!(def_teams_choices[[1]] %in% input$selected_defense)]
  # }) 
  # 
  # output$teams_unselected_text <- reactive({
  #   def_teams_choices[[1]][!(def_teams_choices[[1]] %in% c(input$selected_defense,player_teams_selected()))] %>% sort()
  # }) 
  
  output$players_remaining_text <- renderText({
    team_player_position %>% 
      filter(!(team_abbr %in% input$selected_defense)) %>% 
      distinct(player_lookup) %>% 
      unlist() %>% 
      paste(collapse = "\t\n")
  })
  
  
  # observe({
  #   updateSelectizeInput(
  #     session,
  #     label = paste0("Select ",selections_remaining()," players:")
  #   )
  # })
  
}


shinyApp(ui, server)


# clean up environment and run the garbage collector
remove(list = ls())
gc()

## clear cache as needed
# nflreadr::clear_cache()

library(tidyverse)
library(shiny)

nfl_teams <- nflreadr::load_teams(current = TRUE) %>% 
  as_tibble() %>% 
  select(team_abbr, team_name, team_conf, team_division, team_logo_espn) %>% 
  mutate(team_name_w_abbr = paste0(team_name, " (", team_abbr, ")"))

nfl_players <- nflreadr::load_player_stats(seasons = 2023L) %>% 
  as_tibble() %>% 
  filter(position %in% c("QB", "RB", "FB", "WR", "TE", "K")) %>% 
  mutate(position = if_else(position == "FB", "RB", position)) %>% 
  select(
    week,
    player_id,
    player = player_name,
    team = recent_team,
    position,
    passing_yards,
    passing_tds,
    interceptions,
    sacks,
    sack_fumbles,
    sack_fumbles_lost,
    passing_2pt_conversions,
    rushing_yards,
    rushing_tds,
    rushing_fumbles,
    rushing_fumbles_lost,
    rushing_2pt_conversions,
    receiving_yards,
    receiving_tds,
    receiving_fumbles,
    receiving_fumbles_lost,
    receiving_2pt_conversions,
  ) %>%
  mutate(week = as.integer(week))

ui <- fluidPage(
  titlePanel("Playoff Fantasy Football"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_positions", "Choose Positions:",
        choiceNames = list("Quarterback (QB)", "Running Back (RB)", "Wide Receiver (WR)", "Tightend (TE)", "Kicker (K)"),
        choiceValues = list("QB", "RB", "WR", "TE", "K"),
        selected = list("QB", "RB", "WR", "TE", "K")
      ), 
      checkboxGroupInput("selected_teams", "Choose Teams:",
                         choiceNames = as.list(nfl_teams$team_name_w_abbr),
                         choiceValues = as.list(nfl_teams$team_abbr),
                         selected = list("ARI", "DEN")
      ), 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Statistics",
          value = tableOutput("player_stats")
        ),
        tabPanel("Create Fantasy Roster"),
      )
    )
  )
)

server <- function(input, output){
  
  # list the names of teams in the playoffs in quotes
  # all teams are listed below as a default
  output$player_stats <- renderTable(
    nflreadr::load_player_stats(season == 2023L) %>% 
      filter(team_abbr %in% input$selected_teams)
  )

}

shinyApp(ui, server)

# sidebarLayout(
#   sidebarPanel(
#     selectInput(
#       inputId = "playoff_teams",
#       label = "Select NFL Playoff Teams:",
#       choices = nfl_teams,
#       multiple = TRUE
#     )
#   ),
#   mainPanel()
# )
# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(tidyverse)
library(shiny)
library(data.table)
library(DT)
library(shinyjs)

source(paste0(getwd(),'/Scripts/Shiny Roster Dashboard/Fantasy Football Helper Functions.R'))

season_year <- 2023L
season_type <- c("REG")

# create data.table for NFL teams
dt_nfl_teams <- get_team_names()

# TODO currently I do not have functionality set up for team points calculated on pbp data
# create data.table for play-by-play data for scoring defensive points for each team
# dt_nfl_team_stats <- data.table::as.data.table(nflfastR::load_pbp(seasons = season_year))
# dt_nfl_team_stats[season_type %in% season_type]

# create data.table for players, which is a combination of the offensive scorers plus kickers
dt_nfl_player_stats <- get_player_stats()

# remove zero value statistics
dt_nfl_player_stats <- dt_nfl_player_stats[abs(fantasy_points) >= 1e-7 | abs(football_value) >= 1e-7]

# get a list of unique players for the lookup
dt_lookup <- unique(get_player_stats()[,.(position, lookup_string, team_abbr)], by=c('lookup_string'))
dt_lookup <- setorder(dt_lookup, cols = position, team_abbr, lookup_string)


player_stats <- get_position_stats(
  dt_nfl_player_stats, 
  "K", 
  summarized_boolean = FALSE, 
  long_format_boolean = TRUE
) %>% order_cols()


team_lookupstring_position <- bind_rows(
  dt_nfl_player_stats %>% 
    distinct(team_abbr, lookup_string, position) %>% 
    arrange(lookup_string),
  dt_nfl_teams %>% 
    mutate(position = "Defense",
           lookup_string = paste0(position,", ",team_abbr," (",team_division,")")) %>% 
    select(team_abbr, lookup_string, position)
) %>% as.data.table()
roster_choices <- team_lookupstring_position %>% distinct(lookup_string) %>% as.list()
def_teams_choices <- dt_nfl_teams %>% distinct(team_abbr) %>% as.list()

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

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Playoff Fantasy Football"),
  tabsetPanel(
    tabPanel(
      "Select Roster",
      actionButton(
        inputId = "toggleRosterSelector", 
        label = "Toggle Roster Selector",
        icon = icon("bars"),
        style = "margin-top:3px; margin-bottom:3px"
      ),
      sidebarLayout(
        div(id = "rosterSelector",
          sidebarPanel(
            selectizeInput(
              inputId = "roster_selections_made",
              label = "Select Player or Defensive Team",
              choices = roster_choices,
              options = list(maxItems = 1)
            ),
            actionButton(
              inputId = "add_player",
              label = "Add to Roster",
              icon = icon("add"),
              style="color: white; background-color: #0086b3; border-color: #2e6da4"
            ),
            p("", style="margin-top:10px"),
            textOutput(outputId = "roster_slots_remaining_text"),
            p("", style="margin-top:10px"),
            textOutput(outputId = "positions_available_text"),
            p("", style="margin-top:10px"),
            textOutput(outputId = "teams_available_text"),
            h1("", style = 'margin:100px'),
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
            p("", style="margin-top:10px"),
            textOutput(outputId = "positions_on_roster_text"),
            p("", style="margin-top:10px"),
            textOutput(outputId = "teams_on_roster_text"),
            p("", style='margin-bottom:25px'),
            fluidPage(
              h4("Participant Information", style='font-weight:bold'),
              textInput("fantasy_owner_name", label = "Name", placeholder = "John Doe"),
              textInput("fantasy_owner_email", label = "Email", placeholder = "abcd@gmail.com"),
              textInput("fantasy_team_name", label = "Fantasy Team Name", placeholder = "My Clever Team Name"),
              p("Note: Fantasy Team Name will be displayed in rankings"),
              checkboxInput("paid_checkbox", label = "I have paid the Commish because I am not a delinquent"),
              style = 'background-color:#ffffc2; border-style:solid; border-color:black;'
            ),
            p("", style='margin-bottom:20px'),
            downloadButton(
              outputId = "download_roster", 
              label = "Download Roster",
              # icon = icon("download-alt", lib = "glyphicon"),
              style = "color: white; background-color: #F62817;"
            ),
            p("Don't forget to email your roster to the Commish!"),
            width = 4
          )
        ),
        mainPanel(
          fluidRow(
            h3("Current Roster"),
            DTOutput(outputId = "players_on_roster_DT"),
            style="margin-left:2px"
          ),
          fluidRow(
            h3("Valid Player Selections Remaining", style="margin-top:100px"),
            DTOutput(outputId = "players_remaining_DT"),
            style="margin-left:2px"
          )
        )
      )
    ),
    tabPanel(
      "Explore 2023 Stats",
      actionButton(
        inputId = "toggleFilterOptions", 
        label = "Toggle Filter Options",
        icon = icon("bars"),
        style = "margin-top:3px; margin-bottom:3px"
      ),
      sidebarLayout(
        div(id = "filterOptions",
          sidebarPanel(
            # this is a single select way to provide positions for the DT table
            selectInput(
              inputId = "selected_position",
              label = "Inspect a Position:",
              choices = list("QB", "RB", "WR", "TE", "K"),
              selected = "QB"
            ),
            p("Inspect Team(s)", style = "font-weight:bold; margin-top:40px"),
            actionButton("select_all_teams", label="All", inline=TRUE),
            actionButton("deselect_all_teams", label="None", inline=TRUE),
            checkboxGroupInput(
              "selected_teams",
              label = "",
              choiceNames = as.list(dt_nfl_teams$team_name_w_abbr),
              choiceValues = as.list(dt_nfl_teams$team_abbr),
              selected = as.list(dt_nfl_teams$team_abbr)
            ),
            width = 2
          )
        ),
        mainPanel(
          tabsetPanel(
            p("", style="margin-top:10px"),
            tabPanel("Regular Season Totals", DTOutput("statistics_season")),
            tabPanel("Weekly Totals", DTOutput("statistics_weekly"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  ## this section is for stats exploration
  observeEvent(input$toggleFilterOptions, {
    shinyjs::toggle(id = "filterOptions")
  })
  
  
  stats_dropdown <- reactive({
    input$selected_position
  })

  output$statistics_weekly <- renderDT({
    player_stats <- get_position_stats(
      dt_nfl_player_stats, 
      stats_dropdown(), 
      summarized_boolean = FALSE, 
      long_format_boolean = FALSE
    ) %>% order_cols()
  })
  
  output$statistics_season <- renderDT({
    player_stats <- get_position_stats(
      dt_nfl_player_stats, 
      stats_dropdown(), 
      summarized_boolean = TRUE, 
      long_format_boolean = FALSE
    ) %>% order_cols()
  })
  
  observeEvent(
    input$select_all_teams, {
    updateCheckboxGroupInput(
      session,
      "selected_teams",
      label = "",
      choiceNames = as.list(dt_nfl_teams$team_name_w_abbr),
      choiceValues = as.list(dt_nfl_teams$team_abbr),
      selected = as.list(dt_nfl_teams$team_abbr)
    )
  })
  
  observeEvent(
    input$deselect_all_teams, {
    updateCheckboxGroupInput(
      session,
      "selected_teams",
      label = "",
      choiceNames = as.list(dt_nfl_teams$team_name_w_abbr),
      choiceValues = as.list(dt_nfl_teams$team_abbr),
      selected = NULL
    )
  })
  
  
  ## this section is for Roster Selection
  
  observeEvent(input$toggleRosterSelector, {
    shinyjs::toggle(id = "rosterSelector")
  })
  
  roster <- reactiveValues(players = c(NULL))
  
  observeEvent(input$add_player,{
    if(input$roster_selections_made == ""){
      
    } else{
      roster$players <- c(roster$players, input$roster_selections_made) %>% sort()
    }
  })
  
  observeEvent(input$remove_player,{
    roster$players <- roster$players[!(roster$players %in% input$roster_selections_removed)]
  })
  
  roster_slots_remaining <- reactive({
    14-length(roster$players)
  }) 
  output$roster_slots_remaining_text <- renderText({
      paste0("Roster slot(s) remaining: ", roster_slots_remaining(), " of 14")
  })

  
  # keep track of teams selected on the roster
  teams_on_roster <- reactive({
    team_lookupstring_position[lookup_string %in% roster$players, team_abbr] %>% 
      unique() %>% 
      sort()
  })
  output$teams_on_roster_text <- renderText({
    if(is_empty(teams_on_roster())){
      "Teams on roster: None"
    } else {
      paste0("Teams on roster: ", paste0(teams_on_roster(), collapse = ",  "))
    }
  })
  
  # keep track of unselected teams
  teams_available <- reactive({
    team_lookupstring_position[!(team_abbr %in% teams_on_roster()), team_abbr] %>% unique() %>% sort()
  }) 
  output$teams_available_text <- renderText({
    paste0("Teams remaining: ", paste0(teams_available() %>% unlist(), collapse = ",  "))
  })
  
  # keep track of positions on the roster
  positions_selected <- reactive({
    team_lookupstring_position[lookup_string %in% roster$players, position]
  })
  output$positions_on_roster_text <- renderText({
    if(is_empty(positions_selected())){
      "Positions Filled: None"
    } else {
      paste0("Positions Filled: ", paste0(count_positions(positions_selected()) %>% unlist(), collapse = ",  "))
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
      paste0("Positions Remaining: ", paste0(remaining_positions %>% unlist(), collapse = ",  "))
    }
  })
  
  players_remaining <- reactive({

    players_remaining <- team_lookupstring_position %>%
      filter(!(team_abbr %in% teams_on_roster()))
    
    if(length(positions_selected()[positions_selected() == "Defense"])>=1L){
      players_remaining <- players_remaining %>%
        filter(position != "Defense")
    }   
    if(length(positions_selected()[positions_selected() == "K"])>=1L){
      players_remaining <- players_remaining %>%
        filter(position != "K")
    }
    if(length(positions_selected()[positions_selected() == "QB"])>=3L){
      players_remaining <- players_remaining %>%
        filter(position != "QB")
    }
    # for RB, TE and WR, need to consider the flex position when filtering
    if((length(positions_selected()[positions_selected() == "RB"])==3L & 
       (length(positions_selected()[positions_selected() == "TE"])==3L |
        length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "RB"])>=4L)){
      players_remaining <- players_remaining %>%
        filter(position != "RB")
    }
    if((length(positions_selected()[positions_selected() == "TE"])==2L & 
        (length(positions_selected()[positions_selected() == "RB"])==4L |
         length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "TE"])>=3L)){
      players_remaining <- players_remaining %>%
        filter(position != "TE")
    }
    if((length(positions_selected()[positions_selected() == "WR"])==3L & 
        (length(positions_selected()[positions_selected() == "TE"])==3L |
         length(positions_selected()[positions_selected() == "RB"])==4L) )|
       (length(positions_selected()[positions_selected() == "WR"])>=4L)){
      players_remaining <- players_remaining %>%
        filter(position != "WR")
    }
    
    players_remaining <- players_remaining %>% select(position, team_abbr, lookup_string)
    
  })
  
  output$players_on_roster_DT <- renderDT({
    if(is_empty(roster$players)){
      DT::datatable(data.table(lookup_string = "Roster is empty"), options = list(pageLength = 25))
    } else {
      DT::datatable(
        team_lookupstring_position %>%
          filter(lookup_string %in% roster$players) %>%
          select(position, team_abbr, lookup_string),
        options = list(pageLength = 25)
      )
    }
  })
  
  output$players_remaining_DT <- renderDT({players_remaining()})
  
  observeEvent(
    input$add_player,{
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
  
  


  # reactive boolean for activating download button
  participant_info <- reactive({
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
      participant_info()$fantasy_owner_name!="",
      str_detect(participant_info()$fantasy_owner_email,"[:graph:]{3,}@[:alnum:]{1,}\\.[:alnum:]{2,}"),
      participant_info()$fantasy_team_name!="",
      participant_info()$paid,
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
  
  roster_data <- reactive({
    team_lookupstring_position %>%
      filter(lookup_string %in% roster$players) %>%
      select(position, team_abbr, lookup_string) %>% 
      mutate(
        `Fantasy Owner` = rep(participant_info()$fantasy_owner_name,14),
        `Fantasy Owner Email` = rep(participant_info()$fantasy_owner_email,14),
        `Fantasy Team Name` = rep(participant_info()$fantasy_team_name,14),
        `Roster` = 1:14,
        `Position Type` = if_else(position == "Defense", "Defense / Special teams", "Player"),
        `Automation Mapping` = if_else(
          position == "Defense", 
          team_abbr, 
          str_remove(lookup_string, "^.*, ID: ")
        ),
        `Check 1 - Selection is Unique` = TRUE,
        `Check 2 - Team is Unique` = TRUE
      ) %>% 
      group_by(
        position
      ) %>% 
      mutate(
        `Position Code` = if_else(position %in% c("QB","WR","TE","RB"), paste0(position,1:n()), position)
      ) %>% 
      ungroup() %>% 
      rename(
        `Position Group` = position,
        `Team Abbr.` = team_abbr,
        `Selection` = lookup_string
      ) %>%
      mutate(
        `Position Group` = if_else(`Position Code` == "K", "SPEC", 
                           if_else(`Position Code` %in% c("RB4","WR4","TE3"), "FLEX", `Position Group`, 
                           if_else(`Position Code` == "Defense", "D", `Position Group`)))
      ) %>% 
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
  
}


shinyApp(ui, server)


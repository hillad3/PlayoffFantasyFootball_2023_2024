# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(tidyverse)
library(shiny)
library(data.table)
library(DT)
library(shinyjs)

# when pulling the getwd() in the console, it does not include the folders Scripts and ShinyApp
# However, when shiny::runApp() is called, the getwd() gets updated to include these folders. 
# therefore this structure will not work unless using runApp()
tryCatch(
  expr = source(paste0(getwd(),'/Fantasy Football Helper Functions.R')),
  warning = function(cond){
    print("Warning: File not found in the directory path, or directory path does not exist. Trying alternate path")
    source(paste0(getwd(),'/Scripts/ShinyApp/Fantasy Football Helper Functions.R'))
  }
)

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

team_lookupstring_position <- bind_rows(
  dt_lookup,
  dt_nfl_teams %>% 
    mutate(position = "Defense",
           lookup_string = paste0(position,", ",team_abbr," (",team_division,")")) %>% 
    select(team_abbr, lookup_string, position)
) %>% as.data.table()
roster_choices <- team_lookupstring_position %>% distinct(lookup_string) %>% as.list()
def_teams_choices <- dt_nfl_teams %>% distinct(team_abbr) %>% as.list()


ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Playoff Fantasy Football"),
  tabsetPanel(
    tabPanel(
      "How to Play",
      fluidPage(
        h2("Game Overview"),
        p("Playoff Fantasy Football is an elimination based version of Fantasy Football:"),
        tags$ul(
          tags$li("Each contestant will create a diversified roster prior to the start of playoffs."),
          tags$li("Your roster must include one player from each of the 14 teams in the playoffs."),
          tags$li("Your roster must include 1 Kicker (K), 3 Quaterbacks (QB), 3 Running Backs (RB), 3 Wide Receivers (WR), 2 Tight Ends (TE), 1 Flex Position (either RB, WR or TE) and 1 Defense / Special Teams."),
          tags$li("The roster will be locked from changes after submission to the Commissioner. Each week, as teams are eliminated from the playoffs, so does the pool of potential players on your roster who can score points."),
          tags$li("Therefore, your overall roster success is as dependent on each player's longevity in the playoffs as it is on the player itself."),
          tags$li("Fantasy scoring is calculated based on each player's performance during a game."),
          tags$li("The types of statistics converted into Fantasy scores is consistent with typical scoring rules (see details below)"),
          tags$li("All playoff games, including wildcards and the Super Bowl, will be considered in the scoring."),
          tags$li("Rosters must be submitted, valid, and paid for by the start of the first wildcard game."),
          tags$li("Multiple rosters are allowed per Owner, as long as each are paid for in full."),
          tags$li("Prizes will be awarded to the top 5 scoring entries."),
          tags$li("Prize purse will be announced after wildcard playoff weekend, since prize purse is dependent on the number of entries."),
          tags$li("If you think you're going to win, spread the word: The more participants, the larger the prizes."),
          tags$li("If you think you're going to lose, spread the word: Imagine the commaraderie of shared experience!"),
          tags$li("The Commissioner will (probably) provide weekly updates on Fantasy Team standings throughout the contest. Final summary of scoring and standings will be provided.")
        ),
        h2("How To Use this Dashboard"),
        p("You can use this dashboard to explore player statistics and create your roster:"),
        tags$ul(
          tags$li("Regular season statistics are available on the 'Explore 2023 Stats' tab, which may help provide insights on each player you should prioritize. Statistics are available in 'football values' and in 'fantasy points'. Defense / Special Team points are not available here -- but you know, like, google it."),
          tags$li("Use the 'Select Roster' tab on this dashboard to start creating your roster."),
          tags$li("Add players to your roster based on the combination you think will score the most points by the end of the Superbowl."),
          tags$li("When a player is added to your roster, the team associated with that player (and any of its remaining players) will be removed from your next possible selections. For example: if you pick Jalen Hurts as one of your quarterbacks, you no longer be able to select an Eagles player for another other player on your roster."),
          tags$li("When you've satisified the maximum number of positions on your roster, any player associated with that poisiton will be removed from your next possible selection. For example: if you pick Jalen Hurts as your third quarterback, you no longer be able to select a quarterback from any other team."),
          tags$li("As needed, you can remove players from your team, which will release that Team and/or Position for your next possible selection."),
          tags$li("You must include your Name, Email and Fantasy Team Name in the Participant Information Box. Don't forget to confirm that you've paid the Commish."),
          tags$li("The roster can only be downloaded after all parameters have been satisfied (i.e. a completed roster and all participant information)."),
          tags$li("You must still email the commissioner your roster. This dashboard does not save your roster.", style="color:red; font-weight:bold;"),
        ),
        h2("Alternate Roster in Excel"),
        p("The email sent to you by the Commissioner should contain an Excel file that is equivalent to this dashboard. If you prefer, you can complete that roster template and email the Excel file back to the Commissioner."),
        h2("Scoring"),
        h4("Passing"),
        tags$ul(
          tags$li("TD Pass = 6 points"),
          tags$li("Every 50 passing yards = 1 point"),
          tags$li("400+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Passing TD = 2 points"),
          tags$li("2pt Passing Conversion = 2 points"),
          tags$li("Interception Thrown = -2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        h4("Rushing"),
        tags$ul(
          tags$li("TD Rush = 6 points"),
          tags$li("Every 10 rushing yards = 1 point"),
          tags$li("200+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Passing TD = 2 points"),
          tags$li("2pt rushing Conversion = 2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        h4("Receiving"),
        tags$ul(
          tags$li("TD Receiving = 6 points"),
          tags$li("Every 10 receiving yards = 1 point"),
          tags$li("200+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Receiving TD = 2 points"),
          tags$li("2pt rushing Conversion = 2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        h4("Kicking"),
        tags$ul(
          tags$li("PAT Made = 1 point"),
          tags$li("PAT Missed = -1 point"),
          tags$li("FG Made = 3 points"),
          tags$li("FG Made (40-49 yards) Bonus = 1 point"),
          tags$li("FG Made (50+ yards) Bonus = 2 points"),
          tags$li("FG Missed = -1 point"),
        ),
        h4("Defense / Special Teams"),
        tags$ul(
          tags$li("Each Sack = 1 point"),
          tags$li("Each Interception = 2 points"),
          tags$li("Each Safety = 2 points"),
          tags$li("Each Fumble Recovery = 2 points"),
          tags$li("Each Blocked Punt, PAT or FG = 2 points"),
          tags$li("Interception Return TD = 6 points"),
          tags$li("Fumble Return TD = 6 points"),
          tags$li("Kickoff Return TD = 6 points"),
          tags$li("Punt Return TD = 6 points"),
          tags$li("Blocked Punt or FG Return TD = 6 points"),
          tags$li("0 Points Allowed = 10 points"),
          tags$li("1-6 Points Allowed = 7 points"),
          tags$li("7-13 Points Allowed = 4 points"),
          tags$li("14-21 Points Allowed = 1 points"),
          tags$li("22-27 Points Allowed = -1 points"),
          tags$li("28-34 Points Allowed = -4 points"),
          tags$li("35-45 Points Allowed = -7 points"),
          tags$li("46+ Points Allowed = -10 points"),
        )
      )
    ),
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
              h4("Participant Information", style='font-weight:bold; margin-bottom:0px'),
              p("* required", style = "color:red; margin-top:3px"),
              textInput("fantasy_owner_name", label = "Name *", placeholder = "Dick Butkus"),
              textInput("fantasy_owner_email", label = "Email * ", placeholder = "myemail@gmail.com"),
              textInput("fantasy_team_name", label = "Fantasy Team Name * ", placeholder = "Unique Team Name (especially if submitting multiple rosters)"),
              checkboxInput("paid_checkbox", label = "I have paid the Commish because I am not a delinquent *"),
              p("Note: Fantasy Team Name will be displayed in rankings", style='margin-top:20px'),
              style = 'background-color:#ffffc2; border-style:solid; border-color:black;'
            ),
            p("", style='margin-bottom:20px'),
            downloadButton(
              outputId = "download_roster", 
              label = "Download Roster",
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
            selectInput(
              inputId = "stat_type",
              label = "Statistic Type:",
              choices = list("Football Values", "Fantasy Points", "Both"),
              selected = "Football Value"
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
    ) 
    
    player_stats <- order_cols(player_stats[team_abbr %in% input$selected_teams])
    if(input$stat_type == "Football Values"){
      cols <- c('position','lookup_string', 'week', 'player_id', 'player_name', 
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^football_value")])
      return(player_stats[, .SD, .SDcols = cols])
    } else if (input$stat_type == "Fantasy Points"){
      cols <- c('position','lookup_string', 'week', 'player_id', 'player_name', 
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^fantasy_points")])
      return(player_stats[, .SD, .SDcols = cols])
    } else {
      return(player_stats)
    }
  })
  
  output$statistics_season <- renderDT({
    player_stats <- get_position_stats(
      dt_nfl_player_stats, 
      stats_dropdown(), 
      summarized_boolean = TRUE, 
      long_format_boolean = FALSE
    ) %>% order_cols()
    
    player_stats <- order_cols(player_stats[team_abbr %in% input$selected_teams])
    if(input$stat_type == "Football Values"){
      cols <- c('position','lookup_string', 'player_id', 'player_name', 
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^football_value")])
      return(player_stats[, .SD, .SDcols = cols])
    } else if (input$stat_type == "Fantasy Points"){
      cols <- c('position','lookup_string', 'player_id', 'player_name', 
                'team_abbr', 'team_conf', 'team_division',
                names(player_stats)[str_detect(names(player_stats),"^fantasy_points")])
      return(player_stats[, .SD, .SDcols = cols])
    } else {
      return(player_stats)
    }
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
      roster$players <- c(roster$players, input$roster_selections_made) %>% sort()
  })
  
  observeEvent(input$remove_player,{
    roster$players <- roster$players[!(roster$players %in% input$roster_selections_removed)]
  })
  
  roster_slots_remaining <- reactive({
    14-length(roster$players)
  }) 
  
  roster_full <- reactive({
    if(length(roster$players) == 14L){
      TRUE
    } else {
      FALSE
    }
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
  
  observeEvent(
    roster_full(),
    {
      if(roster_full()) {
        shinyjs::disable("add_player")
        
      } else {
        shinyjs::enable("add_player")
      }
    }
  )
  


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


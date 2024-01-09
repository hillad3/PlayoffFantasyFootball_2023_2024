# clean up environment and run the garbage collector
remove(list = ls())
gc()

library(tidyverse)
library(shiny)
library(data.table)
library(DT)
library(shinyjs)
library(shinythemes)

playoff_year <- 2023L
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


get_last_csv <- function(key){
  f <- list.files(path = "data")
  f <- f[str_detect(f, key)]
  f <- str_remove(f, paste0(key,"_"))
  f <- str_remove(f, ".csv")
  max <- max(as.integer(f))
  return(paste0("data/",key,"_",max,".csv"))
}

dt_nfl_teams <- fread(get_last_csv("nfl_teams"))

dt_roster <- fread(get_last_csv("nfl_rosters"))

dt_nfl_player_stats <- fread(get_last_csv("player_stats"))

team_lookupstring_position <- fread(get_last_csv("lookups"))

source("helper_funcs.R")

# how to play module. there is only a UI side to this module
howToPlayUI <- function(id){
  tagList()
}


ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.freelancer.css"),
    tags$title("Playoff Fantasy Football League")
  ),
  tags$h1("Playoff Fantasy Football League", style = "text-align:center"),
  tabsetPanel(
    tabPanel(
      "How to Play",
      fluidPage(
        tags$h2("Game Overview"),
        tags$p("Playoff Fantasy Football is an elimination based version of Fantasy Football:"),
        tags$ul(
          tags$li("Each contestant will create a diversified roster prior to the start of playoffs."),
          tags$ul(
            tags$li("Your roster must include one player from each of the 14 teams in the playoffs."),
            tags$li("Your roster must include:"),
              tags$ul(
                tags$li("1 Kicker (K)"),
                tags$li("3 Quaterbacks (QB)"),
                tags$li("3 Running Backs (RB)"),
                tags$li("3 Wide Receivers (WR)"),
                tags$li("2 Tight Ends (TE)"),
                tags$li("1 Flex Position (either RB, WR or TE)"),
                tags$li("1 Defense / Special Teams.")
              ),
          ),
          tags$li("The roster will be locked from changes after submission to the Commissioner."),
          tags$ul(
            tags$li("Rosters must be submitted, valid, and paid for by kickoff of the first wildcard game (1pm Saturday, January 13th, 2023)."),
            tags$li("Late rosters will not be accepted."),
            tags$li("Multiple rosters are allowed per Owner, as long as each are paid for.")
          ),
          tags$li("Each week, as teams are eliminated from the playoffs, so does the pool of potential players on your roster who can score points."),
          tags$ul(
            tags$li("Therefore, your overall roster success is as dependent on each player's longevity in the playoffs as much as it is on the player's performance.")
          ),
          tags$li("Fantasy scoring is calculated based on each player's performance during a game."),
          tags$li("The types of statistics converted into Fantasy points is consistent with typical scoring rules (see details below)"),
          tags$li("Points are cumulative throughout the playoffs (including wildcard games and Super Bowl)."),
          tags$li("The person with the most points at the end of the playoffs wins the grand prize."),
          tags$li("Prizes will be awarded to the top 5 scoring entries."),
          tags$li("Prize purse will be announced after wildcard playoff weekend, since prize purse is dependent on the number of entries."),
          tags$li("If you think you're going to win, spread the word: The more participants, the larger the prizes."),
          tags$li("If you think you're going to lose, spread the word: Imagine the commaraderie of shared experience!"),
          tags$li("The Commissioner will (probably) provide weekly updates on Fantasy Team standings throughout the contest. Final summary of scoring and standings will be provided.")
        ),
        tags$h2("How To Use this Dashboard"),
        tags$p("You can use this dashboard to explore player statistics and create your roster:"),
        tags$ul(
          tags$li("Regular season statistics are available on the 'Explore Stats' tab, which may help provide insights on each player you should prioritize. Statistics are available in 'football values' and in 'fantasy points'."),
          tags$li("Use the 'Build Roster' tab on this dashboard to start creating your roster."),
          tags$li("Add players to your roster based on the combination you think will score the most points by the end of the Superbowl."),
          tags$li("When a player is added to your roster, the team associated with that player (and any of its remaining players) will be removed from your next possible selections. For example: if you pick Jalen Hurts as one of your quarterbacks, you no longer be able to select an Eagles player on your roster."),
          tags$li("When you've satisified the maximum number of positions on your roster, any player associated with that position will be removed from your next possible selection. For example: if you pick Jalen Hurts as your third (and last) quarterback, you no longer be able to select a quarterback."),
          tags$li("As needed, you can remove players from your team, which will release that Team and/or Position as a next possible selection."),
          tags$li("You must include your Name, Email and Fantasy Team Name in the Participant Information Box. Don't forget to confirm that you've paid the Commish."),
          tags$li("The roster can only be downloaded after all parameters have been satisfied (that is, a completed roster of 14 players and the Participant Information box is filled in with valid information)."),
          tags$li("You must still email the commissioner your roster downloaded from this website. This website does not save your roster.", style="color:red; font-weight:bold;"),
        ),
        # tags$h2("Alternate Roster in Excel"),
        # tags$p("The email sent to you by the Commissioner should contain an Excel file that is equivalent to this dashboard. If you prefer, you can complete that roster template and email the Excel file back to the Commissioner. I don't know why you would do this, but technically it is possible."),
        tags$h2("Scoring"),
        tags$h4("Passing"),
        tags$ul(
          tags$li("TD Pass = 6 points"),
          tags$li("Every 50 passing yards = 1 point"),
          tags$li("400+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Passing TD Bonus = 2 points"),
          tags$li("2pt Passing Conversion = 2 points"),
          tags$li("Interception Thrown = -2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        tags$h4("Rushing"),
        tags$ul(
          tags$li("TD Rush = 6 points"),
          tags$li("Every 10 rushing yards = 1 point"),
          tags$li("200+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Passing TD Bonus = 2 points"),
          tags$li("2pt rushing Conversion = 2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        tags$h4("Receiving"),
        tags$ul(
          tags$li("TD Receiving = 6 points"),
          tags$li("Every 10 receiving yards = 1 point"),
          tags$li("200+ Yards in a single game = 2 points"),
          tags$li("40+ Yard Receiving TD Bonus = 2 points"),
          tags$li("2pt rushing Conversion = 2 points"),
          tags$li("Fumble Lost = -2 points"),
        ),
        tags$h4("Kicking"),
        tags$ul(
          tags$li("PAT Made = 1 point"),
          tags$li("PAT Missed = -1 point"),
          tags$li("FG Made = 3 points"),
          tags$li("FG Made (40-49 yards) Bonus = 1 point"),
          tags$li("FG Made (50+ yards) Bonus = 2 points"),
          tags$li("FG Missed = -1 point"),
        ),
        tags$h4("Defense / Special Teams"),
        tags$ul(
          tags$li("Each Sack = 1 point"),
          tags$li("Each Interception = 2 points"),
          tags$li("Each Safety = 2 points"),
          tags$li("Each Fumble Recovery = 2 points"),
          tags$li("Each Blocked Punt/PAT/FG = 2 points"),
          tags$li("Interception Return TD = 6 points"),
          tags$li("Fumble Return TD = 6 points"),
          tags$li("Kickoff Return TD = 6 points"),
          tags$li("Punt Return TD = 6 points"),
          tags$li("Blocked Punt or FG Return TD = 6 points"),
          tags$li("0 Points Allowed = 10 points"),
          tags$li("1-6 Points Allowed = 7 points"),
          tags$li("7-13 Points Allowed = 4 points"),
          tags$li("14-17 Points Allowed = 1 points"),
          tags$li("18-21 Points Allowed = 0 points"),
          tags$li("22-27 Points Allowed = -1 points"),
          tags$li("28-34 Points Allowed = -4 points"),
          tags$li("35-45 Points Allowed = -7 points"),
          tags$li("46+ Points Allowed = -10 points"),
        )
      )
    ),
    tabPanel(
      "Build Roster",
      br(),
      actionButton(
        inputId = "toggleRosterSelector", 
        label = "Roster Selector Menu",
        icon = icon("bars"),
        style = "margin-bottom:10px"
      ),
      sidebarLayout(
        div(id = "rosterSelector",
          sidebarPanel(
            selectizeInput(
              inputId = "roster_selections_made",
              label = "Select Player or Defensive Team",
              choices = c("",as.list(unique(team_lookupstring_position[,lookup_string]))),
              selected = "",
              options = list(maxItems = 1)
            ),
            actionButton(
              inputId = "add_player",
              label = "Add to Roster",
              icon = icon("add"),
              style="color: white; background-color: #0086b3; border-color: #2e6da4"
            ),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "roster_slots_remaining_text"),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "positions_available_text"),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "teams_available_text"),
            tags$h1("", style = 'margin:100px'),
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
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "positions_on_roster_text"),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = "teams_on_roster_text"),
            tags$p("", style='margin-bottom:25px'),
            fluidPage(
              tags$p("", style="margin:8px"),
              tags$span("Participant Information", style='font-weight:bold; font-size:16px; margin-right: 3px'),
              tags$span("* required", style = "color:red;"),
              tags$p("", style="margin:8px"),
              textInput("fantasy_owner_name", label = "Name *", placeholder = "Dick Butkus"),
              textInput("fantasy_owner_email", label = "Email *", placeholder = "myemail@gmail.com"),
              textInput("fantasy_team_name", label = "Fantasy Team Name *", placeholder = "Unique Team Name"),
              checkboxInput("paid_checkbox", label = "I have paid the Commish because I am not a delinquent *"),
              tags$p("Note: Fantasy Team Name will be displayed in rankings", style='margin-top:20px'),
              style = 'background-color:#ffffc2; border-style:solid; border-color:black;'
            ),
            tags$p("", style='margin-bottom:20px'),
            downloadButton(
              outputId = "download_roster", 
              label = "Download Roster",
              style = "color: white; background-color: #F62817;"
            ),
            tags$p("Don't forget to email your roster to the Commish!"),
            width = 3
          )
        ),
        mainPanel(
          fluidRow(
            tags$h3("Current Roster"),
            DTOutput(outputId = "players_on_roster_DT"),
            style="margin-left:2px"
          ),
          fluidRow(
            tags$h3("Valid Player Selections Remaining", style="margin-top:100px"),
            DTOutput(outputId = "players_remaining_DT"),
            style="margin-left:2px"
          )
        )
      )
    ),
    tabPanel(
      "Explore Stats",
      br(),
      actionButton(
        inputId = "toggleFilterMenu", 
        label = "Filter Menu",
        icon = icon("bars"),
        style = "margin-right:25px;",
        inline = TRUE
      ),
      shinyWidgets::materialSwitch(
        inputId = "pivot_data",
        label = "Pivot Data",
        inline = TRUE
      ),
      sidebarLayout(
        div(id = "filterMenu",
          sidebarPanel(
            width = 2,
            selectInput(
              inputId = "selected_position",
              label = "Position:",
              choices = list("QB", "RB", "WR", "TE", "K", "Defense"),
              selected = "QB"
            ),
            selectInput(
              inputId = "reg_or_post",
              label = "Regular or Post Season:",
              choices = list("Regular","Post"),
              selected = "Regular"
            ),
            selectInput(
              inputId = "stat_type",
              label = "Statistic Type:",
              choices = list("Fantasy Points", "Football Values", "Both"),
              selected = "Fantasy Points"
            ),
            tags$p("Inspect Team(s)", style = "font-weight:bold; margin-top:40px"),
            actionButton("select_all_teams", label="All", inline=TRUE),
            actionButton("deselect_all_teams", label="None", inline=TRUE),
            checkboxGroupInput(
              "selected_teams",
              label = "",
              choiceNames = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_name_w_abbr]),
              choiceValues = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_abbr]),
              selected = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_abbr])
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(paste0(playoff_year," Season Totals"), br(), DTOutput("statistics_season")),
            tabPanel(paste0(playoff_year," by Week"), br(), DTOutput("statistics_weekly"))
          )
        )
      )
    )#,tabPanel(
    #   "Fantasy Results",
    #   id = "fantasyResultsPage",
    #   sidebarLayout(
    #     sidebarPanel(
    #       selectInput(
    #         inputId = "fantasy_team",
    #         label = "Fantasy Team:",
    #         choices = list("TBD"),
    #         selected = "TBD"
    #       ),
    #     ),
    #     mainPanel(
    #       h1("Coming soon... maybe", style="font-family:Arial")
    #     )
    #   )
    # )
  )
)

server <- function(input, output, session) {
  
  ## this section is for Roster Selection
  observeEvent(input$toggleRosterSelector, {
    shinyjs::toggle(id = "rosterSelector")
  })
  
  roster <- reactiveValues(players = c(NULL))
  
  observeEvent(input$add_player,{
    if(str_length(input$roster_selections_made)==0L){
      roster$players
    } else {
      roster$players <- c(roster$players, input$roster_selections_made) |> sort()
    }
  })
  
  observeEvent(input$remove_player,{
    roster$players <- roster$players[!(roster$players %in% input$roster_selections_removed)]
  })
  
  roster_reactive <- reactive({
    remaining <- 14-length(roster$players)
    roster_full <- ifelse(length(roster$players) == 14L,TRUE,FALSE)
    data.table(
      'slots_remaining' = remaining,
      'roster_full' = roster_full
    )
  }) 
  
  output$roster_slots_remaining_text <- renderText({
      paste0("Roster slot(s) remaining: ", roster_reactive()$slots_remaining, " of 14")
  })

  
  # keep track of teams selected on the roster
  teams_on_roster <- reactive({
    unique(team_lookupstring_position[lookup_string %in% roster$players, team_abbr])
  })
  
  output$teams_on_roster_text <- renderText({
    if(is_empty(teams_on_roster())){
      "Teams on roster: None"
    } else {
      paste0("Teams on roster: ", paste0(teams_on_roster() |> unlist(), collapse = ",  "))
    }
  })
  
  # keep track of unselected teams
  teams_available <- reactive({
    team_lookupstring_position[!(team_abbr %in% teams_on_roster()), team_abbr] |> unique() |> sort()
  }) 
  output$teams_available_text <- renderText({
    paste0("Teams remaining: ", paste0(teams_available() |> unlist(), collapse = ",  "))
  })
  
  # keep track of positions on the roster
  positions_selected <- reactive({
    team_lookupstring_position[lookup_string %in% roster$players, position]
  })
  output$positions_on_roster_text <- renderText({
    if(is_empty(positions_selected())){
      "Positions Filled: None"
    } else {
      paste0("Positions Filled: ", paste0(count_positions(positions_selected()) |> unlist(), collapse = ",  "))
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
      paste0("Positions Remaining: ", paste0(remaining_positions |> unlist(), collapse = ",  "))
    }
  })
  
  players_remaining <- reactive({

    players_remaining <- team_lookupstring_position[!(team_abbr %in% teams_on_roster())]
    
    if("Defense" %in% positions_selected()){
      players_remaining <- players_remaining[position != "Defense"]
    }   
    if("K" %in% positions_selected()){
      players_remaining <- players_remaining[position != "K"]
    }
    if(length(positions_selected()[positions_selected() == "QB"])>=3L){
      players_remaining <- players_remaining[position != "QB"]
    }
    # for RB, TE and WR, need to consider the flex position when filtering
    if((length(positions_selected()[positions_selected() == "RB"])==3L & 
       (length(positions_selected()[positions_selected() == "TE"])==3L |
        length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "RB"])>=4L)){
      players_remaining <- players_remaining[position != "RB"]
    }
    if((length(positions_selected()[positions_selected() == "TE"])==2L & 
        (length(positions_selected()[positions_selected() == "RB"])==4L |
         length(positions_selected()[positions_selected() == "WR"])==4L) )|
       (length(positions_selected()[positions_selected() == "TE"])>=3L)){
      players_remaining <- players_remaining[position != "TE"]
    }
    if((length(positions_selected()[positions_selected() == "WR"])==3L & 
        (length(positions_selected()[positions_selected() == "TE"])==3L |
         length(positions_selected()[positions_selected() == "RB"])==4L) )|
       (length(positions_selected()[positions_selected() == "WR"])>=4L)){
      players_remaining <- players_remaining[position != "WR"]
    }
    
    players_remaining
    
  })
  
  output$players_remaining_DT <- renderDT({players_remaining()})
  
  output$players_on_roster_DT <- renderDT({
    if(is_empty(roster$players)){
      DT::datatable(
        data.table(lookup_string = "Roster is empty"), 
        options = list(pageLength = 25)
      )
    } else {
      DT::datatable(
        team_lookupstring_position[lookup_string %in% roster$players],
        options = list(pageLength = 25)
      )
    }
  })
  
  
  observeEvent(
    input$add_player,{
    updateSelectizeInput(
      session,
      inputId = "roster_selections_made",
      choices = c("",as.list(players_remaining()[,lookup_string])),
      selected = ""
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
        choices = c("",as.list(players_remaining()[,lookup_string])),
        selected = ""
      )
      
      updateSelectizeInput(
        session,
        inputId = "roster_selections_removed",
        choices = roster$players
      )
    })
  
  observeEvent(
    roster_reactive()$roster_full,
    {
      if(roster_reactive()$roster_full) {
        shinyjs::disable("add_player")
        
      } else {
        shinyjs::enable("add_player")
      }
    }
  )
  


  # reactive boolean for activating download button
  participant_reactive <- reactive({
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
      participant_reactive()$fantasy_owner_name!="",
      str_detect(participant_reactive()$fantasy_owner_email,"[:graph:]{3,}@[:alnum:]{1,}\\.[:alnum:]{2,}"),
      participant_reactive()$fantasy_team_name!="",
      participant_reactive()$paid,
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
  
  # create final roster for downloadHandler
  roster_data <- reactive({
    team_lookupstring_position |>
      filter(lookup_string %in% roster$players) |>
      select(position, team_abbr, lookup_string) |> 
      mutate(
        `Fantasy Owner` = rep(participant_reactive()$fantasy_owner_name,14),
        `Fantasy Owner Email` = rep(participant_reactive()$fantasy_owner_email,14),
        `Fantasy Team Name` = rep(participant_reactive()$fantasy_team_name,14),
        `Roster` = 1:14,
        `Position Type` = if_else(position == "Defense", "Defense / Special teams", "Player"),
        `Automation Mapping` = if_else(
          position == "Defense", 
          team_abbr, 
          str_remove(str_remove(lookup_string, "^.*, ID: "),"\\)")
        ),
        `Check 1 - Selection is Unique` = TRUE,
        `Check 2 - Team is Unique` = TRUE
      ) |> 
      group_by(
        position
      ) |> 
      mutate(
        `Position Code` = if_else(position %in% c("QB","WR","TE","RB"), paste0(position,1:n()), 
                          if_else(position == "Defense", "D", position))
      ) |> 
      ungroup() |> 
      rename(
        `Position Group` = position,
        `Team Abbr.` = team_abbr,
        `Selection` = lookup_string
      ) |>
      mutate(
        `Position Group` = case_when(
          `Position Code` == "K" ~ "SPEC", 
          `Position Code` %in% c("RB4","WR4","TE3") ~ "FLEX", 
          `Position Code` == "D" ~ "D", 
          .default = `Position Group`)
      ) |> 
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
  
  ## this section is for stats exploration
  observeEvent(input$toggleFilterMenu, {
    shinyjs::toggle(id = "filterMenu")
  })
  
  stat_teams <- reactive({input$selected_teams})
  
  stat_params <- reactive({
    data.table(
      'pos' = input$selected_position,
      'season_type' = input$reg_or_post,
      'stat_type' = input$stat_type,
      'pivot_data' = input$pivot_data
    )
    
  })
  
  output$statistics_weekly <- renderDT({
    
    if(is_empty(dt)){
      DT::datatable(
        data.table(lookup_string = "No data available")
      )
    } else {
      update_app_stats(
        dt_nfl_player_stats, 
        stat_params()$pos, 
        stat_params()$season_type,
        stat_params()$stat_type,
        stat_teams(),
        is_summed_stat = FALSE, 
        is_wide_table = stat_params()$pivot_data
      )
    }
  })
  
  output$statistics_season <- renderDT({
    
    if(is_empty(dt)){
      DT::datatable(
        data.table(lookup_string = "No data available")
      )
    } else {
      update_app_stats(
        dt_nfl_player_stats, 
        stat_params()$pos, 
        stat_params()$season_type,
        stat_params()$stat_type,
        stat_teams(),
        is_summed_stat = TRUE, 
        is_wide_table = stat_params()$pivot_data
      )
    }
  })
  
  observeEvent(
    input$select_all_teams, {
      updateCheckboxGroupInput(
        session,
        "selected_teams",
        label = "",
        choiceNames = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_name_w_abbr]),
        choiceValues = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_abbr]),
        selected = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_abbr])
      )
    })
  
  observeEvent(
    input$deselect_all_teams, {
      updateCheckboxGroupInput(
        session,
        "selected_teams",
        label = "",
        choiceNames = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_name_w_abbr]),
        choiceValues = as.list(dt_nfl_teams[team_abbr %in% playoff_teams, team_abbr]),
        selected = NULL
      )
    })
  
  # this section is for the fantasy results section
  
}


shinyApp(ui, server)




buildRosterUI <- function(id,team_lookupstring_position_){
  tagList(
    br(),
    actionButton(
      inputId = NS(id,"toggleRosterSelector"),
      label = "Show / Hide Menu",
      icon = icon("bars"),
      style = "margin-bottom:10px"
    ),
    sidebarLayout(
      div(id = NS(id,"rosterSelector"),
          sidebarPanel(
            selectizeInput(
              inputId = NS(id,"roster_selections_made"),
              label = "Select Player or Defensive Team",
              choices = c("",as.list(unique(team_lookupstring_position_[,lookup_string]))),
              selected = "",
              options = list(maxItems = 1)
            ),
            actionButton(
              inputId = NS(id,"add_player"),
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
              inputId = NS(id,"roster_selections_removed"),
              label = "Remove Player or Defensive Team",
              choices = NULL,
              options = list(maxItems = 1),
            ),
            actionButton(
              inputId = NS(id,"remove_player"),
              label = "Remove",
              icon = icon("trash", lib = "glyphicon"),
              style="color: white; background-color: gray; border-color: black"
            ),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = NS(id,"positions_on_roster_text")),
            tags$p("", style="margin-top:10px"),
            textOutput(outputId = NS(id,"teams_on_roster_text")),
            tags$p("", style='margin-bottom:25px'),
            fluidPage(
              tags$p("", style="margin:8px"),
              tags$span("Participant Information", style='font-weight:bold; font-size:16px; margin-right: 3px'),
              tags$span("* required", style = "color:red;"),
              tags$p("", style="margin:8px"),
              textInput(NS(id,"fantasy_owner_name"), label = "Name *", placeholder = "Dick Butkus"),
              textInput(NS(id,"fantasy_owner_email"), label = "Email *", placeholder = "myemail@gmail.com"),
              textInput(NS(id,"fantasy_team_name"), label = "Fantasy Team Name *", placeholder = "Unique Team Name"),
              checkboxInput(NS(id,"paid_checkbox"), label = "I have paid the Commish because I am not a delinquent *"),
              tags$p("Note: Fantasy Team Name will be displayed in rankings", style='margin-top:20px'),
              style = 'background-color:#ffffc2; border-style:solid; border-color:black;'
            ),
            tags$p("", style='margin-bottom:20px'),
            downloadButton(
              outputId = NS(id,"download_roster"),
              label = "Download Roster",
              style = "color: white; background-color: #F62817;"
            ),
            tags$p(""),
            tags$span("The Download button will activate once you have 14 players on your roster and the participant information is complete."),
            tags$span("Don't forget to email your roster to the Commish!", style="color:red"),
            width = 3
          )
      ),
      mainPanel(
        fluidRow(
          tags$h3("Current Roster"),
          DTOutput(outputId = NS(id,"players_on_roster_DT")),
          style="margin-left:2px"
        ),
        fluidRow(
          tags$h3("Valid Player Selections Remaining", style="margin-top:100px"),
          DTOutput(outputId = NS(id,"players_remaining_DT")),
          style="margin-left:2px"
        )
      ) # close MainPanel
    ) # close SidebarLayout
  ) # close tagList
}

buildRosterServer <- function(id, team_lookupstring_position_){
  moduleServer(
    id,
    function(input,output,session){

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
        unique(team_lookupstring_position_[lookup_string %in% roster$players, team_abbr])
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
        team_lookupstring_position_[!(team_abbr %in% teams_on_roster()), team_abbr] |> unique() |> sort()
      })
      output$teams_available_text <- renderText({
        paste0("Teams remaining: ", paste0(teams_available() |> unlist(), collapse = ",  "))
      })

      # keep track of positions on the roster
      positions_selected <- reactive({
        team_lookupstring_position_[lookup_string %in% roster$players, position]
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

        players_remaining <- team_lookupstring_position_[!(team_abbr %in% teams_on_roster())]

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
            data.table(" " = "Roster is empty"),
            options = list(pageLength = 25)
          )
        } else {
          DT::datatable(
            team_lookupstring_position_[lookup_string %in% roster$players],
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
        team_lookupstring_position_ |>
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
    }
  )
}
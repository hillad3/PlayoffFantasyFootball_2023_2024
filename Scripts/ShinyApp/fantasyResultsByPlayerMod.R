fantasyResultsbyPlayerUI <- function(id, dt_team_info_, playoff_teams_, playoff_year_){
  tagList(
    br(),
    actionButton(
      inputId = NS(id,"toggleFilterMenu"),
      label = "Show / Hide Menu",
      icon = icon("bars"),
      style = "margin-right:25px;",
      inline = TRUE
    ),
    sidebarLayout(
      div(id = NS(id,"filterMenu"),
          sidebarPanel(
            width = 2,
            selectInput(
              inputId = NS(id,"selected_position"),
              label = "Position:",
              choices = list("QB", "RB", "WR", "TE", "K", "Defense","All"),
              selected = "QB"
            ),
            tags$p("Inspect Team(s)", style = "font-weight:bold; margin-top:40px"),
            actionButton(NS(id,"select_all_teams"), label="All", inline=TRUE),
            actionButton(NS(id,"deselect_all_teams"), label="None", inline=TRUE),
            checkboxGroupInput(
              NS(id,"selected_teams"),
              label = "",
              choiceNames = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_name_w_abbr]),
              choiceValues = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_abbr]),
              selected = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_abbr])
            )
          )
      ),
      mainPanel(
        tabPanel("Player Totals", br(), DTOutput(NS(id,"stats")))
      ) # close mainPanel
    ) # close sidebarLayout
  ) # close tagList
}

fantasyResultsbyPlayerServer <- function(id, dt_stats_, dt_team_info_, playoff_teams_){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(input$toggleFilterMenu, {
        shinyjs::toggle(id = "filterMenu")
      })
      
      stat_teams <- reactive({input$selected_teams})

      output$stats <- renderDT({
        
        if(is_empty(input$selected_teams)){
          DT::datatable(
            data.table(" " = "No data available"),
            options = list(pageLength = 25)
          )
        } else {
          DT::datatable(
            update_app_totals(
              dt_stats_,
              input$selected_position,
              input$selected_teams
            ),
            options = list(pageLength = 25)
          )
        }
      })
      
      observeEvent(
        input$select_all_teams, {
          updateCheckboxGroupInput(
            session,
            "selected_teams",
            label = "",
            choiceNames = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_name_w_abbr]),
            choiceValues = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_abbr]),
            selected = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_abbr])
          )
        })
      
      observeEvent(
        input$deselect_all_teams, {
          updateCheckboxGroupInput(
            session,
            "selected_teams",
            label = "",
            choiceNames = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_name_w_abbr]),
            choiceValues = as.list(dt_team_info_[team_abbr %in% playoff_teams_, team_abbr]),
            selected = NULL
          )
        })
    }
  ) # close moduleServer
}
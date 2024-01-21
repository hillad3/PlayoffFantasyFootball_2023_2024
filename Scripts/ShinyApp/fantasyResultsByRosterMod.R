fantasyResultsbyRosterUI <- function(id, summary_by_team_){
  tagList(
    br(),
    tags$p("Please reach out if you see any discrepancies in your roster."),
    actionButton(
      inputId = NS(id,"toggleLeagueResultsMenu"),
      label = "Show / Hide Menu",
      icon = icon("bars"),
      style = "margin-right:25px; margin-bottom:10px",
      inline = TRUE
    ),
    br(),
    sidebarLayout(
      div(id = NS(id,"leagueResultsMenu"),
          sidebarPanel(
            tags$p("Top Teams", style = "font-weight:bold; margin-top:3px"),
            actionButton(NS(id,"select_all_rosters"), label="All", inline=TRUE),
            actionButton(NS(id,"select_top5_rosters"), label="5", inline=TRUE),
            actionButton(NS(id,"select_top10_rosters"), label="10", inline=TRUE),
            actionButton(NS(id,"select_top25_rosters"), label="25", inline=TRUE),
            actionButton(NS(id,"select_none_rosters"), label="None", inline=TRUE),
            selectInput(
              NS(id,"selected_rosters"),
              label = "",
              choices = c("",summary_by_team_$fantasy_team_and_initials |> sort()),
              multiple = TRUE,
              selected = summary_by_team_$fantasy_team_and_initials[1:5]
            ),
            width=3
          )
      ),
      mainPanel(
        tags$p(paste0("Fantasy Teams Competing: ", length(summary_by_team_$fantasy_team_and_initials))),
        tags$h2("Team Summary"),
        DTOutput(NS(id,"roster_summary_table")),
        br(),
        tags$h2(NS(id,"Roster and Player Breakdown")),
        br(),
        DTOutput(NS(id,"roster_breakout_table")),
        br()
      ) # close mainpanel
    ) # close sidebarLayout
  ) # close tagList
}

fantasyResultsbyRosterServer <- function(id, summary_by_team_, summary_by_team_and_player_){
  moduleServer(
    id,
    function(input,output,session){
      observeEvent(input$toggleLeagueResultsMenu, {
        shinyjs::toggle(id = "leagueResultsMenu")
      })

      # chunk to filter roster ranking list based on 5 different buttons
      observeEvent(input$select_all_rosters, {
        updateSelectizeInput(
          session,
          "selected_rosters",
          choices = c("", summary_by_team_$fantasy_team_and_initials |> sort()),
          selected = summary_by_team_$fantasy_team_and_initials |> sort()
        )
      })
      observeEvent(input$select_top5_rosters, {
        updateSelectizeInput(
          session,
          "selected_rosters",
          choices = c("", summary_by_team_$fantasy_team_and_initials |> sort()),
          selected = summary_by_team_$fantasy_team_and_initials[1:5]
        )
      })
      observeEvent(input$select_top10_rosters, {
        updateSelectizeInput(
          session,
          "selected_rosters",
          choices = c("", summary_by_team_$fantasy_team_and_initials |> sort()),
          selected = summary_by_team_$fantasy_team_and_initials[1:10]
        )
      })
      observeEvent(input$select_top25_rosters, {
        updateSelectizeInput(
          session,
          "selected_rosters",
          choices = c("", summary_by_team_$fantasy_team_and_initials |> sort()),
          selected = summary_by_team_$fantasy_team_and_initials[1:25]
        )
      })
      observeEvent(input$select_none_rosters, {
        updateSelectizeInput(
          session,
          "selected_rosters",
          choices = c("", summary_by_team_$fantasy_team_and_initials |> sort()),
          selected = "",
          options = list(placeholder = "Type to search by name")
        )
      })

      output$roster_summary_table <- renderDT({
        if(is_empty(input$selected_rosters)){
          DT::datatable(
            data.table(" " = "No data available")
          )
        } else {
          dt <- summary_by_team_[fantasy_team_and_initials %in% input$selected_rosters]
          DT::datatable(dt)
        }
      })

      output$roster_breakout_table <- renderDT({
        if(is_empty(input$selected_rosters)){
          DT::datatable(
            data.table(" " = "No data available"),
            options = list(pageLength = 14)
          )
        } else {
          dt <- summary_by_team_and_player_[fantasy_team_and_initials %in% input$selected_rosters]
          DT::datatable(
            dt,
            options = list(pageLength = 14)
          )
        }
      })
    }
  )
}
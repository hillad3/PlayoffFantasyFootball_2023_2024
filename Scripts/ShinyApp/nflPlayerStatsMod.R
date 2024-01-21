
nflPlayerStatsUI <- function(id, dt_team_info_, playoff_teams_, playoff_year_){
  tagList(
    br(),
    actionButton(
      inputId = NS(id,"toggleFilterMenu"),
      label = "Show / Hide Menu",
      icon = icon("bars"),
      style = "margin-right:25px;",
      inline = TRUE
    ),
    shinyWidgets::materialSwitch(
      inputId = NS(id,"pivot_data"),
      label = "Pivot Data",
      inline = TRUE
    ),
    sidebarLayout(
      div(id = NS(id,"filterMenu"),
          sidebarPanel(
            width = 2,
            selectInput(
              inputId = NS(id,"selected_position"),
              label = "Position:",
              choices = list("QB", "RB", "WR", "TE", "K", "Defense"),
              selected = "QB"
            ),
            selectInput(
              inputId = NS(id,"reg_or_post"),
              label = "Regular or Post Season:",
              choices = list("Regular","Post"),
              selected = "Post"
            ),
            selectInput(
              inputId = NS(id,"stat_type"),
              label = "Statistic Type:",
              choices = list("Fantasy Points", "Football Values", "Both"),
              selected = "Fantasy Points"
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
        tabsetPanel(
          type = "pills",
          tabPanel(paste0(playoff_year_," Season Totals"), br(), DTOutput(NS(id,"statistics_season"))),
          tabPanel(paste0(playoff_year_," by Week"), br(), DTOutput(NS(id,"statistics_weekly")))
        )
      ) # close mainPanel
    ) # close sidebarLayout
  ) # close tagList
}

nflPlayerStatsServer <- function(id, dt_stats_, dt_team_info_, playoff_teams_){
  moduleServer(
    id,
    function(input, output, session){
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

        if(is_empty(input$selected_teams)){
          DT::datatable(
            data.table(" " = "No data available"),
            options = list(pageLength = 14)
          )
        } else {
          update_app_stats(
            dt_stats_,
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

        if(is_empty(input$selected_teams)){
          DT::datatable(
            data.table(" " = "No data available"),
            options = list(pageLength = 14)
          )
        } else {
          update_app_stats(
            dt_stats_,
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
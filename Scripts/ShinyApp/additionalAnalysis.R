

additionalAnalysisUI <- function(id){
  tagList(
    br(),
    h2("Position / NFL Team / Player Name / Fantasy Team Hierarchy"),
    tagList(div(
      div(
        p("Top Teams: "),
        style = "font-size:110%; display:inline-block; "
      ),
      div(
        selectizeInput(
          inputId=NS(id,"select_overall_distro"),
          label="",
          choices = c("All","1","3","5","10","25","50","100"),
          selected = "All"
        ),
        style = "display: inline-block; "
      )
    )),
    plotlyOutput(NS(id,"overall_treemap")),
    br(),
    h2("Overall Player Distribution"),
    tagList(div(
      div(
        p("Top Teams: "),
        style = "font-size:110%; display:inline-block; "
      ),
      div(
        selectizeInput(
          inputId=NS(id,"select_player_distro"),
          label="",
          choices = c("All","1","3","5","10","25","50","100"),
          selected = "All"
        ),
        style = "display: inline-block; "
      )
    )),
    plotlyOutput(NS(id,"player_treemap")),
    br(),
    tags$h2("Overall Defense Distribution"),
    tagList(div(
      div(
        p("Top Teams: "),
        style = "font-size:110%; display:inline-block; "
      ),
      div(
        selectizeInput(
          inputId=NS(id,"select_defense_distro"),
          label="",
          choices = c("All","1","3","5","10","25","50","100"),
          selected = "All"
        ),
        style = "display: inline-block; "
      )
    )),
    plotlyOutput(NS(id,"defense_treemap"))
  ) # close tagList
}

additionalAnalysisServer <- function(id, dt_fantasy_rosters_){
  moduleServer(
    id,
    function(input,output,session){
      output$overall_treemap <- renderPlotly({

        if(input$select_overall_distro == "All"){

          dt_treemap <- bind_rows(
            dt_fantasy_rosters_ |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              distinct(position) |>
              mutate(ids = position,
                     parents = "") |>
              rename(labels = position) |>
              mutate(values = 1),
            dt_fantasy_rosters_ |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              distinct(position, team_abbr) |>
              unite("ids", c(position, team_abbr), remove = FALSE, sep = "-") |>
              mutate(parents = position) |>
              rename(labels = team_abbr) |>
              mutate(values = 1),
            dt_fantasy_rosters_ |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              distinct(position, team_abbr, player_name) |>
              unite("ids", c(team_abbr, player_name), remove = FALSE, sep = "-") |>
              unite("parents", c(position, team_abbr), remove = FALSE, sep = "-") |>
              rename(labels = player_name) |>
              mutate(values = 1),
            dt_fantasy_rosters_ |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              group_by(team_abbr, player_name, fantasy_team_and_initials) |>
              reframe(values = n()) |>
              unite("ids", c(player_name, fantasy_team_and_initials), remove = FALSE, sep = "-") |>
              unite("parents", c(team_abbr, player_name), remove = FALSE, sep = "-") |>
              rename(labels = fantasy_team_and_initials)
          )

        } else {

          # this adds in a filter before creating the parents, labels and ids
          dt_treemap <- bind_rows(
            dt_fantasy_rosters_ |>
              filter(rank <= as.integer(input$select_overall_distro)) |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              distinct(position) |>
              mutate(ids = position,
                     parents = "") |>
              rename(labels = position) |>
              mutate(values = 1),
            dt_fantasy_rosters_ |>
              filter(rank <= as.integer(input$select_overall_distro)) |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              distinct(position, team_abbr) |>
              unite("ids", c(position, team_abbr), remove = FALSE, sep = "-") |>
              mutate(parents = position) |>
              rename(labels = team_abbr) |>
              mutate(values = 1),
            dt_fantasy_rosters_ |>
              filter(rank <= as.integer(input$select_overall_distro)) |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              distinct(position, team_abbr, player_name) |>
              unite("ids", c(team_abbr, player_name), remove = FALSE, sep = "-") |>
              unite("parents", c(position, team_abbr), remove = FALSE, sep = "-") |>
              rename(labels = player_name) |>
              mutate(values = 1),
            dt_fantasy_rosters_ |>
              filter(rank <= as.integer(input$select_overall_distro)) |>
              mutate(
                position = case_when(
                  str_detect(position_code,"QB") ~ "QB",
                  str_detect(position_code,"RB") ~ "RB",
                  str_detect(position_code,"WR") ~ "WR",
                  str_detect(position_code,"TE") ~ "TE",
                  str_detect(position_code,"K") ~ "K",
                  str_detect(position_code,"Defense") ~ "D"
                )
              ) |>
              group_by(team_abbr, player_name, fantasy_team_and_initials) |>
              reframe(values = n()) |>
              unite("ids", c(player_name, fantasy_team_and_initials), remove = FALSE, sep = "-") |>
              unite("parents", c(team_abbr, player_name), remove = FALSE, sep = "-") |>
              rename(labels = fantasy_team_and_initials)
          )
        }

        tm_overall <- plot_ly(
          dt_treemap,
          type="treemap",
          labels = ~labels,
          parents = ~parents,
          ids = ~ids,
          values = ~values
        )
      })

      output$player_treemap <- renderPlotly({

        if(input$select_player_distro == "All"){

          player_frequency <- dt_fantasy_rosters_ |>
            filter(position_type == "Player") |>
            group_by(position_type, player_name) |>
            reframe(counts = n()) |>
            arrange(-counts) |>
            mutate(player_name = fct_inorder(player_name)) |>
            as.data.table()

        } else {

          player_frequency <- dt_fantasy_rosters_ |>
            filter(
              position_type == "Player" &
                fantasy_team_and_initials %in%
                summary_by_team$fantasy_team_and_initials[1:as.numeric(input$select_player_distro)]
            ) |>
            group_by(position_type, player_name) |>
            reframe(counts = n()) |>
            arrange(-counts) |>
            mutate(player_name = fct_inorder(player_name)) |>
            as.data.table()

        }

        plot_ly(
          type="treemap",
          labels = player_frequency$player_name |> unlist(),
          parents = player_frequency$position_type |> unlist(),
          values = player_frequency$counts |> unlist()
        )

      })

      output$defense_treemap <- renderPlotly({
        if(input$select_defense_distro == "All"){

          player_frequency <- dt_fantasy_rosters_ |>
            filter(position_type != "Player") |>
            group_by(position_type, player_name) |>
            reframe(counts = n()) |>
            arrange(-counts) |>
            mutate(player_name = fct_inorder(player_name)) |>
            as.data.table()

        } else {

          player_frequency <- dt_fantasy_rosters_ |>
            filter(
              position_type != "Player" &
                fantasy_team_and_initials %in%
                summary_by_team$fantasy_team_and_initials[1:as.numeric(input$select_defense_distro)]
            ) |>
            group_by(position_type, player_name) |>
            reframe(counts = n()) |>
            arrange(-counts) |>
            mutate(player_name = fct_inorder(player_name)) |>
            as.data.table()

        }

        plot_ly(
          type="treemap",
          labels = player_frequency$player_name |> unlist(),
          parents = player_frequency$position_type |> unlist(),
          values = player_frequency$counts |> unlist()
        )
      })
    }
  )
}
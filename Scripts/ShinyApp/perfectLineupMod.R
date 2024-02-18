require(data.table)
require(lpSolve)
require(tidyr)

get_perfect_lineup <- function(dt, teams, weeks=NULL){
  
  if(!is.null(weeks)){
    dt <- dt[week %in% weeks]
  }

  # double up positions so that FLEX is independent from RB, WR and TE positions
  dt1 <- dt[season_type=="Post" & stat_type == "fantasy_points"]
  dt1[,position1:=position]
  dt1 <- dt1[,.(fantasy_points= sum(stat_values)),.(position, position1, team_abbr, player_name, player_id)]

  dt2 <- dt[season_type=="Post" & stat_type == "fantasy_points" & position %in% c("RB","WR","TE")]
  dt2[,position1:="FLEX"]
  dt2 <- dt2[,.(fantasy_points= sum(stat_values)),.(position, position1, team_abbr, player_name, player_id)]

  dt3 <- rbindlist(list(dt1,dt2))


  # this provides options to solve if the team had a bye and hasn't played yet
  # or if all of their players were duds so picking a player with no points optimizes
  for(x in seq_along(teams)){

    team <- teams[x]

    # add defense only if they have had a bye or zero points
    if(!(team %in% dt3$team_abbr)){
      dt3 <- rbindlist(list(
        dt3,
        data.table(
          "position" = "Defense",
          "position1" = "Defense",
          "team_abbr" = team,
          "player_name" = "dummy",
          "player_id" = team,
          "fantasy_points" = 0
        )
      ))
    }

    dt3 <- rbindlist(list(
      dt3,
      data.table(
        "position" = c("QB","WR","RB","TE","K","FLEX"),
        "position1" = c("QB","WR","RB","TE","K","FLEX"),
        "team_abbr" = c(rep(team, 6)),
        "player_name" = paste0("dummy_",team),
        "player_id" = paste0(team,"-",c(1:6),"-dummy","-",c("QB","WR","RB","TE","K","FLEX")),
        "fantasy_points" = 0
      )
    ))

  }

  # create binaries for positions (including defense)
  positions <- c("QB"=3,"WR"=3,"RB"=3,"TE"=2,"FLEX"=1,"K"=1,"Defense"=1)
  for(x in seq_along(positions)){
    tmp1 <- names(positions)[x]
    tmp2 <- dt3$position1 == tmp1
    dt3[,paste0("is_",tmp1)] <- tmp2*1
  }

  # create binaries for teams
  for(x in seq_along(teams)){
    tmp1 <- teams[x]
    tmp2 <- dt3$team_abbr == tmp1
    dt3[,paste0("is_",tmp1)] <- tmp2*1
  }

  player_names <- dt3 |>
    filter(position1 != "Defense") |>
    distinct(player_id) |>
    as_vector() |>
    unname()

  if(any(duplicated(player_names))){
    stop("There are duplicated player names which need to be unique for lpsolver logic to work.")
  }

  # create binaries for players (excluding defense)
  for(x in seq_along(player_names)){
    tmp1 <- player_names[x]
    tmp2 <- dt3$player_name == tmp1
    dt3[,paste0("is_",tmp1)] <- tmp2*1
  }

  obj_in <- dt3$fantasy_points |> as.numeric()
  dt4 <- copy(dt3)

  constr_mat <- dt4[,c("position","position1","team_abbr","player_name","player_id","fantasy_points"):=NULL] |> t()

  constr_rhs <- c(positions, # these are the positions, QB,WR,RB,TE,K,Defense
                  rep(1, length(teams)), # these are the playoff teams
                  rep(1, length(player_names)))

  constr_dir <- c(
    rep("=", length(positions)),
    rep("<=", length(teams)),
    rep("<=", length(player_names))
  )

  lp_sol <- lp(
    direction = "max",
    objective.in = obj_in,
    const.mat = constr_mat,
    const.dir = constr_dir,
    const.rhs = constr_rhs,
    binary.vec = c(1:length(constr_rhs))
  )

  dt3[,is_perfect_lineup:=ceiling(lp_sol$solution)]
  dt5 <- dt3[,.(position, position1, team_abbr, player_name, player_id, fantasy_points, is_perfect_lineup)]
  dt5 <- dt5[is_perfect_lineup != 0L]
  dt5[,position:=ifelse(position1=="FLEX",paste0(position," (FLEX)"),position)]
  dt5[,is_perfect_lineup:=NULL]
  dt5[,position1:=NULL]
  dt5 <- merge(
    dt5,
    {
      dt6 <- dt[season_type=="Post" & stat_type == "fantasy_points"]
      dt6 <- dt6[,.(week_points=sum(stat_values)), by = .(position,player_id,week)]
    },
    by = c("player_id"),
    all.x = TRUE,
    allow.cartesian = TRUE
  )[,position.y:=NULL]
  
  dt5 <- dt5 |> 
    pivot_wider(
      id_cols = c(position.x, team_abbr, player_name, fantasy_points), 
      names_from = week,
      values_from = week_points, 
      values_fill = 0
    ) |> 
    select(-starts_with("NA")) |> 
    as.data.table()
  
  setorder(dt5, -fantasy_points)
  
  possible_cols <-
    c(
      "position.x",
      "team_abbr",
      "player_name",
      "19",
      "20",
      "21",
      "22",
      "fantasy_points"
    )
  new_names <-
    c(
      "Position",
      "Team Abbr.",
      "Player Name",
      "Wild Card (Week 1)",
      "Divisional (Week 2)",
      "Conference (Week 3)",
      "Superbowl (Week 4)",
      "Total Points"
    )
  
  new_order <- possible_cols[possible_cols %in% names(dt5)]
  new_names <- new_names[possible_cols %in% names(dt5)]
  
  dt5 <- dt5[,.SD, .SDcols = new_order]
  setnames(dt5, new = new_names)

  result <- sum(dt5$`Total Points`)

  return(list(dt5, result))

}


perfectLineupUI <- function(id, dt_stats_){
  tagList(
    br(),
    checkboxGroupInput(
      inputId = NS(id,"perfline_weeks"),
      label = "Playoff Week(s)",
      choiceNames = paste0("Week ",1:length(unique(dt_stats_[season_type=="Post"]$week))),
      choiceValues = c(19:max(unique(dt_stats_[season_type=="Post"]$week))),
      selected = c(19:max(unique(dt_stats_[season_type=="Post"]$week))), 
      inline = TRUE
    ),
    tags$span("Theoretical max points with a perfect lineup: "),
    textOutput(NS(id,"perfect_lineup_points"), inline = TRUE),
    br(),
    br(),
    div(
      DTOutput(outputId = NS(id,"perfect_lineup")),
      style = "margin-left:20px; width:80%"
    ),
    br()
  ) # close taglist
}

perfectLineupServer <- function(id, dt_stats_, playoff_teams_){
  moduleServer(
    id, 
    function(input,output,session){
      # perfect lineup section
      
      output$perfect_lineup_points <- renderText({
        if(is_empty(input$perfline_weeks)){
          "N/A, no playoffs weeks selected"
        } else {
          get_perfect_lineup(dt_stats_, playoff_teams_, input$perfline_weeks)[[2]]
        }    
      })
      
      output$perfect_lineup <- renderDT({
        if(is_empty(input$perfline_weeks)){
          DT::datatable(
            data.table(" " = "No playoff weeks selected"),
            options = list(pageLength = 14)
          )
        } else {
          DT::datatable(
            get_perfect_lineup(
              dt_stats_, 
              playoff_teams_, 
              input$perfline_weeks)[[1]],
            options = list(pageLength = 14)
          )
        }
      })
    }
  ) #close moduleServer
}

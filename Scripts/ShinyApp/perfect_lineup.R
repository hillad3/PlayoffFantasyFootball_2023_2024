require(data.table)
require(lpSolve)

get_perfect_lineup <- function(dt, teams, weeks=NULL){
  
  if(!is.null(weeks)){
    dt <- dt[week %in% weeks]
  }

  # double up positions so that FLEX is independent from RB, WR and TE positions
  dt1 <- dt[season_type=="Post" & stat_type == "fantasy_points"]
  dt1[,position1:=position]
  dt1 <- dt1[,.(points= sum(stat_values)),.(position, position1, team_abbr, player_name, player_id)]

  dt2 <- dt[season_type=="Post" & stat_type == "fantasy_points" & position %in% c("RB","WR","TE")]
  dt2[,position1:="FLEX"]
  dt2 <- dt2[,.(points= sum(stat_values)),.(position, position1, team_abbr, player_name, player_id)]

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
          "points" = 0
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
        "points" = 0
      )
    ))

  }

  positions <- c("QB"=3,"WR"=3,"RB"=3,"TE"=2,"FLEX"=1,"K"=1,"Defense"=1)
  for(x in seq_along(positions)){
    tmp1 <- names(positions)[x]
    tmp2 <- dt3$position1 == tmp1
    dt3[,paste0("is_",tmp1)] <- tmp2*1
  }

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

  for(x in seq_along(player_names)){
    tmp1 <- player_names[x]
    tmp2 <- dt3$player_name == tmp1
    dt3[,paste0("is_",tmp1)] <- tmp2*1
  }

  obj_in <- dt3$points |> as.numeric()
  dt4 <- copy(dt3)

  constr_mat <- dt4[,c("position","position1","team_abbr","player_name","player_id","points"):=NULL] |> t()

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

  dt3[,perfect_lineup:=ceiling(lp_sol$solution)]
  dt5 <- dt3[,.(position, position1, team_abbr, player_name, points, perfect_lineup)]
  dt5 <- dt5[perfect_lineup != 0L]
  dt5[,position:=ifelse(position1=="FLEX",paste0(position," (FLEX)"),position)]
  dt5[,perfect_lineup:=NULL]
  dt5[,position1:=NULL]
  setorder(dt5, -points)

  result <- sum(dt5$points)

  return(list(dt5, result))

}

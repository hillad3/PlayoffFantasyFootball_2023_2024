library(ggplot2)
library(png)

playoff_teams <- c("BAL","BUF","CLE","DAL","DET","GB","HOU","KC","LA","MIA","PHI","PIT","SF","TB")

i <- data.table('file_name' = list.files(path = "./data/", pattern = ".png"))
i[,team:=str_remove(file_name,".png")]
i[,team:=ifelse(team=="lac","la",team)]
i[,team:=str_to_upper(team)]
i <- i[team %in% playoff_teams]

imgs <- readPNG(source = paste0("./data/",i$file_name))

ggdraw() +
  draw_image

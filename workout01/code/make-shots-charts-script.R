#title: Warriors Shot Charts
#description: The script creates individual shot charts of the 5 Warriors and also a faceted shot chart of the 5 Warriors shooting, all of these are overlayed over an NBA court.
#input: 5 csvs for the players shooting, combined csv of the players shooting, image of NBA court for overlaying
#output: PDFs of the players shot chart and faceted shot chart, PNG of faceted shot chart

library(ggplot2)
library(jpeg)
library(grid)

#4.1: Klay Thompson Chart
thompson <-read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE) 
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"

thompson_scatterplot <- ggplot(data = thompson)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,"npc"),
  height = unit(1, "npc"))

thompson_shot_chart <- ggplot(data = thompson)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+
  ylim(-50,420)+ 
  ggtitle('Shot Chart: Klay Thompson (2016 Season)')+
  theme_minimal()

pdf(file = "../images/klay-thompson-shot-chart.pdf", width=6.5, height=5)
thompson_shot_chart
dev.off()


#4.1: Andre Iguodala Chart
iguodala <-read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE) 
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"

iguodala_scatterplot <- ggplot(data = iguodala)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,"npc"),
  height = unit(1, "npc"))

iguodala_shot_chart <- ggplot(data = iguodala)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+
  ylim(-50,420)+ 
  ggtitle('Shot Chart: Andre Iguodala (2016 Season)')+
  theme_minimal()

pdf(file = "../images/andre-iguodala-shot-chart.pdf", width=6.5, height=5)
iguodala_shot_chart
dev.off()



#4.1: Draymond Green chart
green <-read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE) 
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"

green_scatterplot <- ggplot(data = green)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,"npc"),
  height = unit(1, "npc"))

green_shot_chart <- ggplot(data = green)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+
  ylim(-50,420)+ 
  ggtitle('Shot Chart: Draymond Green (2016 Season)')+
  theme_minimal()

pdf(file = "../images/draymond-green-shot-chart.pdf", width=6.5, height=5)
green_shot_chart
dev.off()



#4.1: Kevin Durant chart
durant <-read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE) 
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"

durant_scatterplot <- ggplot(data = durant)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,"npc"),
  height = unit(1, "npc"))

durant_shot_chart <- ggplot(data = durant)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+
  ylim(-50,420)+ 
  ggtitle('Shot Chart: Kevin Durant (2016 Season)')+
  theme_minimal()

pdf(file = "../images/kevin-durant-shot-chart.pdf", width=6.5, height=5)
durant_shot_chart
dev.off()



#4.1: Stephen Curry chart
curry <-read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE) 
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"

curry_scatterplot <- ggplot(data = curry)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,"npc"),
  height = unit(1, "npc"))

curry_shot_chart <- ggplot(data = curry)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+
  ylim(-50,420)+ 
  ggtitle('Shot Chart: Stephen Curry (2016 Season)')+
  theme_minimal()

pdf(file = "../images/stephen-curry-shot-chart.pdf", width=6.5, height=5)
curry_shot_chart
dev.off()



#4.2: Faceted shot chart
shots_chart <-read.csv("../data/shots-data.csv", stringsAsFactors = FALSE) 

shots_chart_scatterplot <- ggplot(data = shots_chart)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,"npc"),
  height = unit(1, "npc"))

combined_shot_chart <- ggplot(data = shots_chart)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+
  facet_wrap( ~ name , ncol=3)+
  ylim(-50,420)+ 
  ggtitle('Shot Chart: GSW (2016 Season)')+
  theme_minimal()

pdf(file = "../images/gsw-shot-charts.pdf", width=8, height=7)
combined_shot_chart
dev.off()

png(file = "../images/gsw-shot-charts.png", width=8, height=7, units='in', res=360)
combined_shot_chart
dev.off()
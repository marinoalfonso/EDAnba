library(BasketballAnalyzeR)
library(ggplot2)
library(measurements)
library(tidyverse)
library(fmsb)
library(ggrepel)
library(stringr)
library(leaflet)


#import dataset
#https://www.kaggle.com/datasets/blitzapurv/nba-players-data-1950-to-2021
nba = read.csv("seasons_stats.csv")
player_stat = read.csv("player_data.csv")

#visualization structures
player_stat %>%
  glimpse()

nba %>%
  glimpse()

#removal NA  and eventually duplicates in player_stat
player_stat %>% 
  select(everything()) %>% 
  summarise_all(list(~sum(is.na(.))))

player_stat %>%
  filter(is.na(Wt) == T)

player_stat = player_stat %>%
  drop_na(Wt)

player_stat = player_stat %>% 
  distinct(Player, .keep_all = T)

#converion measurments in cm and kg
convertHt <- function(x) {
  x <- as.character(x)
  split <- strsplit(x, "-")
  feet <- as.numeric(split[[1]][1])
  inch <- as.numeric(split[[1]][2])
  round(conv_unit(feet, "ft", "cm") + conv_unit(inch, "inch", "cm"),0)
}

player_stat <- player_stat %>% 
  rowwise() %>% 
  mutate(Ht = convertHt(Ht), Wt = round(conv_unit(Wt, "lbs", "kg")))

player_stat %>%
  select(c(Wt, Ht)) %>%
  head(10)

#Removal of records for the year 2022 as the same as those for 2021
nba = nba %>% 
  filter(Year<2022) 

#removal of useless columns in order to analysis
nba = nba %>% 
  select(-c(X,GS,DWS, OWS,OBPM,DBPM,BPM, PER, X3PAr, FTr, VORP))

colnames(nba)

#addition column for hall of famer
#consequently removal of asterisk in the name of hall of famee player
nba %>%
  filter(str_detect(nba$Player, ".*\\*$")) %>%
  select(Player) 

nba = nba %>%
  mutate(HallOfFame = if_else(str_detect(Player, ".*\\*$"), "Yes", "No"))

nba$Player = gsub("\\*$", "", nba$Player)
player_stat$Player <- gsub("\\*$", "", player_stat$Player)

#addition statistics per-game and FTr (fourth Oliver's factor )
nba = nba %>% 
  mutate(MpG = round(MP/G,3), PpG = round(PTS/G,3), ApG = round(AST/G,3), 
         RpG = round(TRB/G,3), TOpG = round(TOV/G,3), BpG = round(BLK/G,3), 
         SpG = round(STL/G,3),FpG = round(PF/G, 3), .before = 9)

nba = nba %>% 
  mutate(FTr = round(FT/FGA,3), .before = 30)

#rename positions
#SF = Small Forward
#SG = Shooting Guard
#PF = Power Forward
#PG = Point Guard
#C = Centre
unique(nba$Pos)

nba <- nba %>%
  mutate(Pos = case_when(
    Pos == "PF-C" ~ "PF",
    Pos == "C-F" ~ "C",
    Pos == "SF-SG" ~ "SF",
    Pos == "C-PF" ~ "C",
    Pos == "SG-SF" ~ "SG",
    Pos == "PF-SF" ~ "PF",
    Pos == "SF-PF" ~ "SF",
    Pos == "SG-PG" ~ "SG",
    Pos == "SF-PG" ~ "SF",
    Pos == "C-SF" ~ "C",
    Pos == "PG-SG" ~ "PG",
    Pos == "PG-SF" ~ "PG",
    Pos == "SG-PF" ~ "SG",
    Pos == "SF-C" ~ "SF",
    Pos == "F-C" ~ "PF",
    Pos == "F-G" ~ "SF",
    Pos == "G-F" ~ "SF",
    Pos == "F" ~ "PF",
    Pos == "G" ~ "SG",
    TRUE ~ Pos
  ))

table(nba$Pos)

#merging of datasets

player_stat = player_stat %>% 
  select(c("Player","Ht", "Wt", "From", "To", "Career", "Colleges"))
player_stat = as.data.frame(player_stat)

nba = left_join(nba, player_stat, by = "Player", relationship = "many-to-many")

nba = nba %>% 
  select(Year:Tm, Ht:Colleges, HallOfFame, everything()) 

nba = nba %>% 
  distinct(Player,Year, Tm, Age, .keep_all = T)


#adjusted dataset with TOT team
nba_withTOT = nba %>%
  group_by(Year, Player) %>%
  mutate(count_tot = sum(Tm == "TOT")) %>%
  filter(count_tot == 0 | (count_tot > 0 & Tm == "TOT")) %>%
  select(-count_tot)

nba_withTOT = as.data.frame(nba_withTOT)

#adjusted dataset for performance analysis
nba_performance = nba %>% 
  distinct(Year, Player, .keep_all = T) %>% 
  filter(MpG > mean(MpG) | G > mean(G))

nba_performance = as.data.frame(nba_performance)

#analysis about NA values for nba_performance dataset
#datasets contain NA values or missing information (" ") for different reasons
#e.g. because some statistics have been registered starting from a certain year or
#simply because that information misses for that player
nba_performance %>% 
  summarise_all(list(~sum(is.na(.))))

#The NA values related to the Ht, Wt, From, To, Career, Colleges columns are due to the fact 
#that those players are not present in the player_stat dataset


#the technique foundamentals are developed in colleges
#there are 787 colleges in the dataset (excluding NA values and "")
#the lack of values for that column could be due to European players having a different system than the American 
#or simply due to lack of information
player_stat %>%
  filter(is.na(Colleges) != F | Colleges != "") %>%
  distinct(Colleges)%>%
  count()


#10 most occurances colleges
player_stat %>%
  filter(Colleges != "" & !is.na(Colleges)) %>%
  distinct(Colleges, Player) %>%
  count(Colleges, sort = T) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Colleges, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of players per college",
       x = "College",
       y = "Number of players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0,100,15))

#over the years are increased the number of teams in the league
#Starting from 17 in 1950, moving from 8 in 1960 to the current 30 teams
team <- nba %>%
  filter(!Tm == "TOT") %>% 
  group_by(Year) %>%
  summarise(nPlayers = n_distinct(Player), 
            nTeams = n_distinct(Tm), 
            Roster = round(nPlayers/nTeams, 2))

team %>%
  ggplot() +
  geom_line(aes(Year, nTeams, linetype = "Trend line")) +
  ggtitle("Evolution of the number of franchises") +
  geom_hline(aes(yintercept = mean(nTeams), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1), guide = guide_legend(reverse = TRUE)) +
  ylab("Number of franchises") +
  xlab("Year")+
  theme_minimal()+
  theme(legend.position="bottom")

#from the season 1949/1950 to the present, there are been 68 teams whiche have populated the league
#but each franchise have an own story, this implies that over the years, the same team was able to have
#different city, name or nickname
nba %>%
  filter(Tm != "TOT") %>%
  distinct(Tm) %>%
  count()

nba %>%
  filter(Year == 2021 & Tm != "TOT") %>%
  select(Tm) %>%
  unique()

#map of teams in 2021 
nba_teams <- data.frame(
  team = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls",
           "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors",
           "Houston Rockets", "Indiana Pacers", "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies",
           "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks",
           "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers",
           "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards"),
  lat = c(33.7572, 42.3662, 40.6826, 35.2251, 41.8806,
          41.4966, 32.7904, 39.7392, 42.3314, 37.8044,
          29.7604, 39.7684, 34.0522, 34.2, 35.1495,
          25.7617, 43.0389, 44.9778, 29.9511, 40.7128,
          35.4676, 28.5383, 39.9526, 33.4484, 45.5051,
          38.5816, 29.4241, 43.6532, 40.7608, 38.9072),
  lon = c(-84.3963, -71.0621, -73.9743, -80.8393, -87.6742,
          -81.6882, -96.7969, -104.9903, -83.0458, -122.2711,
          -95.3698, -86.1581, -118.2437, -118.5, -90.049,
          -80.1918, -87.9065, -93.2644, -90.0715, -74.006,
          -97.5164, -81.3792, -75.1652, -112.0738, -122.6501,
          -121.4944, -98.4936, -79.3832, -111.891, -77.0369)
)


leaflet(data = nba_teams) %>%
  addTiles() %>%  
  setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%  
  addMarkers(lng = ~lon, lat = ~lat, popup = ~team)  

#consequently are incresead also number of player
team %>%
  ggplot(aes(Year, nPlayers, fill=nPlayers)) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = mean(nPlayers), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_fill_gradient(name = "N. Players",low = "green", high = "red") +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = 2) +
  labs(x = "Year", y="Number of players", title="Evolution of the number of players")+
  theme(legend.position="bottom")+
  theme_minimal()


#The dataset reports the teams in which the players played. 
#Sometimes there may be a TOT value that reports the total statistics for a season 
#for that player who changed teams during the year
unique(nba$Tm)

#e.g. Rajon Rondo or James Harden
nba %>% 
  select(Year, Player, Tm, everything()) %>% 
  filter(Year==2021, Player== "Rajon Rondo" | Player == "James Harden")

#over the years the number of franchises, commercial revenues, 
#thus economic interests have increased we can identify as a direct consequence 
#of this the increase in transfers of players during a season
transfer <- nba %>% 
  select(Tm, Year) %>% 
  filter(Tm == "TOT") %>% 
  group_by(Year) %>% 
  count(sort=T)

chisq.test(transfer)
#p.value is minor of 0.05, this means that values aren't independents

mod <- lm(transfer$n~transfer$Year)
summary(mod)
#the dependence of the variables is confirmed by the significance of the two variables 
#the R-squared value tells us that the model fits the data quite well, 
#assuming a value just below 70% 
#A one-unit increase in the year (thus each successive year) 
#is associated with an estimated increase of 0.8458 in the number of transfers 

attributes(mod)
mod$coefficients[1] #intercept
mod$coefficients[2] #slope


#line chart con regressione
ggplot(transfer, aes(Year, n)) +
  geom_line( color="#69b3a2", linewidth=1, alpha=1, linetype=1) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  geom_smooth(method = lm, color="red", linetype = 5, linewidth = 0.7, se = T) +
  labs(title="Evolution of mid-season transfers", 
       y="Number of transfer", 
       x="Year")+
  theme_minimal()


#let notes as transfers are increased in confront of first years
#due to increased contractual strength, players want to play with team whoch offer them 
#the best salary


#physique evolution
avg <- nba_withTOT %>% 
  group_by(Year) %>% 
  summarise("Height"=mean(Ht, na.rm = T), "Weight"=mean(Wt, na.rm = T))

ggplot(avg, aes(x=Year, y=Height, linetype = "Trend line")) +
  geom_line()+
  labs(x="Year", y="Height (cm)", title = "Average height evolution")+
  geom_hline(aes(yintercept = mean(Height), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1))+
  theme_minimal()


ggplot(avg, aes(x=Year, y=Weight, linetype = "Trend line")) +
  geom_line()+
  labs(x="Year", y="Weight (Kg)", title = "Average weight evolution")+
  geom_hline(aes(yintercept = mean(Weight), linetype = "Avg line"), col = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = c(2, 1))+
  theme_minimal()


#set position color
PosColorCode <- c("C"="red", "PF"="orange", 
                  "SF"="yellow" ,"SG"="blue", "PG"="green")

#'50 years
#introduction 24 second rule
Team_Scores <- nba %>%
  filter(!Tm == "TOT") %>% 
  group_by(Year) %>%
  summarise(Tot_PTS = sum(PTS),
            nTeam = n_distinct(Tm),
            nGames = max(G),
            Tot_G = round((nGames*nTeam)/2, 0),
            avg_Tm_Scores = round((Tot_PTS/Tot_G)/2, 2)) 

Team_Scores %>%
  group_by(Year) %>%
  ggplot(aes(Year, avg_Tm_Scores, fill=avg_Tm_Scores)) + 
  geom_bar(stat="identity", alpha=0.8) +
  scale_fill_gradient(name = "N. Points",low = "#FFA500", high = "#800080") +
  geom_hline(aes(yintercept = mean(Team_Scores$avg_Tm_Scores),linetype = "Avg line"), col = "red", alpha = 0.5) +
  labs(title = "Average scores per team by year", x = "Year", y ="Points per game")+
  scale_x_continuous(breaks = seq(1950, 2021, 10)) +
  scale_linetype_manual(name = "", values = 2)+
  theme_minimal()

#'60 years
#best rebounders
tot_rbd <- nba_withTOT %>% 
  group_by(Player) %>% 
  summarise("SumRebounds"=sum(TRB)) %>% 
  arrange(desc(SumRebounds)) %>% 
  head(10) 

tot_rbd %>% 
  arrange(SumRebounds) %>% 
  mutate(Player=factor(Player,levels=Player)) %>% 
  ggplot( aes(x=Player, y=SumRebounds)) +
  geom_bar(stat="identity", fill="orange", alpha=.6, width=.4) +
  coord_flip() +
  xlab("")+
  ylab("Total rebounds")+
  geom_text(aes(label=SumRebounds), vjust = -0.5, size = 3)+
  ggtitle("Best rebounders")+
  theme_minimal()

#comparison of stats
RC <- nba_performance %>%
  filter(Player %in% c("Wilt Chamberlain", "Bill Russell", 
                       "Jerry West", "Oscar Robertson", "Elgin Baylor")) %>% 
  group_by(Player) %>% 
  summarise("PpG" = mean(PpG)/max(nba$PpG), "ApG"=mean(ApG)/max(nba$ApG),
            "RpG"=mean(RpG)/max(nba$RpG, na.rm = T),
            "FG."=mean(FG.),"WS48"=mean(WS.48), "FTr" = mean(FTr)) %>% 
  select(-Player)

RC <- rbind(rep(1,7), rep(0,7), RC)
colors_border=c("green", "yellow", "brown", "pink", "violet")
colors_in=c(adjustcolor("green", alpha.f = 0.2), adjustcolor("yellow", alpha.f = 0.2),
            adjustcolor("brown", alpha.f = 0.2), adjustcolor("pink", alpha.f = 0.2), 
            adjustcolor("violet", alpha = 0.2))

radarchart( RC, axistype=1, title = "Stats comparison",
            pcol=colors_border , pfcol=colors_in , plwd=3, plty=1 , 
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=1.1,
            vlcex=0.8)

legend(x=1, y=0.2, legend = c("Chamberlain", "Russell", "West", "Robertson", "Baylor"), bty = "n", pch=20 , 
       col=colors_border , text.col = "black", cex=0.9, pt.cex=1.6)


#anni 70
#barchart top 10 scorers
tot_pts <- nba_withTOT %>% 
  group_by(Player) %>% 
  summarise("SumPts"=sum(PTS)) %>% 
  arrange(desc(SumPts)) %>% 
  head(10) 

tot_pts %>% 
  arrange(SumPts) %>% 
  mutate(Player=factor(Player,levels=Player)) %>% 
  ggplot( aes(x=Player, y=SumPts)) +
  geom_bar(stat="identity", fill="orange", alpha=.6, width=.4) +
  coord_flip() +
  xlab("")+
  ylab("Total points")+
  geom_text(aes(label=SumPts), vjust = -0.5, size = 3)+
  ggtitle("Best Scorers")+
  theme_minimal()


#ANNI 80
#barchart best assist man
tot_ast <- nba_withTOT %>% 
  group_by(Player) %>% 
  summarise("SumAst"=sum(AST)) %>% 
  arrange(desc(SumAst)) %>% 
  head(10)

tot_ast %>%
  arrange(SumAst) %>% 
  mutate(Player=factor(Player, levels=Player)) %>% 
  ggplot(aes(x=Player, y=SumAst))+
  geom_bar(stat="identity", fill="orange", alpha=.6, width=.4)+
  coord_flip()+
  xlab("")+
  ylab("Total Assist")+
  geom_text(aes(label=SumAst), vjust = -0.5, size = 3)+
  ggtitle("Best assist-man")+
  theme_minimal()


#comparison % 3-point shoot and 2-point shoot
shoot <- nba_withTOT %>% 
  filter(Year > 1979) %>%
  group_by(Year) %>% 
  summarise("X2P"=(sum(X2PA)/(sum(X2PA)+sum(X3PA)))*100, 
            "X3P"=(sum(X3PA)/(sum(X2PA)+sum(X3PA)))*100)

ggplot(shoot, aes(x = Year)) +
  geom_line(aes(y = X2P, color = "2P")) +
  geom_line(aes(y = X3P, color = "3P")) +
  scale_color_manual(values = c("2P" = "blue", "3P" = "red"), name = "") +
  labs(title = "Proportion of frequency percentage of shots", x = "Year", y = "%")


#anni 90
nba %>% 
  filter(Tm == "CHI" & (Year > 1990 & Year < 1999) & 
           Player %in% c("Michael Jordan", "Scottie Pippen","Dennis Rodman","Horace Grant", "Steve Kerr")) %>% 
  group_by(Player) %>% 
  summarise(MPG = round(mean(MpG),1), PPG = round(mean(PpG),1), 
            APG = round(mean(ApG),1), RPG = round(mean(RpG),1), 
            FPG = round(mean(FpG),1), BPG = round(mean(BpG),1), 
            TOPG = round(mean(TOpG),1), SPG = round(mean(SpG),1))

#ANNI 2000
#comparison 4 factor between kobe and michael
BJ <- nba_performance %>%
  filter(Player == "Kobe Bryant" | Player == "Michael Jordan") %>% 
  group_by(Player) %>% 
  summarise("eFG%"=mean(eFG.)*100, "FTr%"=mean(FTr)*100,
            "TOV%"= mean(TOV.), 
            "ORB%"= mean(ORB.))

#grouped barplot
df1 <- data.frame(stats = rep(c("eFG%", "FTr%", "TOV%", "ORB%"), each = 2),
                  Player = rep(BJ$Player, times = 4),
                  value = c(BJ$`eFG%`,BJ$`FTr%`, BJ$`TOV%`, BJ$`ORB%`))

ggplot(df1, aes(fill = Player, y = stats, x = value))+
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("Four Factors: KB vs MJ")+
  coord_flip()+
  scale_fill_manual(values = c("blueviolet","brown1" ))+
  xlab("%")+
  ylab("")+
  theme(legend.position = "right")+
  labs(fill = "")+
  theme_minimal()

#ANNI 2010 
#version basketballanalyzer
three_point = nba_withTOT %>% 
  group_by(Player) %>% 
  reframe("X3P"=sum(X3P), "X3P."=mean(X3P.), "X3PA"= sum(X3PA), "POS"= Pos) %>% 
  distinct(Player, .keep_all = T) %>% 
  arrange(desc(X3P)) %>% 
  head(40)

three_point$POS = as.factor(three_point$POS)

bubbleplot(data = three_point,id = "Player", x = "X3P", y = "X3PA", 
           col = "POS", size = "X3P.", 
           labels = c("Shots realized", "Shots attempted", "Pos","WS"),
           title = "Best 3-points shooter")
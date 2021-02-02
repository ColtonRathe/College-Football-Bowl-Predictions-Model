############# Create predictions for College football bowl games ##########################
# For 2020, Train 2007-2019 deploy on 2020 
# Team 1 Represents Team with higher SRS 
# Metrics are differenced Team 1-Team 2
# Logistic regression, random forest model created
# Models judged on acccuracy

######### Load Libraries ######################
#install.packages("rvest")
library(rvest)
#install.packages("pylr")
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("conflicted")
library(conflicted)

######### Pull Data  ##########################
#Train 2007-2019, test 2020
years <- c("2007", "2008", "2009", "2010", "2011" , "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

master_offense = data.frame()
master_defense = data.frame()
master_special_teams = data.frame()
master_scores = data.frame()
master_conferences = data.frame()
master_ratings = data.frame()
master_standings = data.frame()

for (year in years) {
  
  # pull offense stats
  url <-paste("https://www.sports-reference.com/cfb/years/", year, "-team-offense.html", sep="")
  pg <- read_html(url)
  tb_offense <- html_table(pg, fill = TRUE)
  tb_offense <- tb_offense[[1]]
  names(tb_offense) <- tb_offense[1,]
  tb_offense <- subset(tb_offense, Rk != "Rk" & Rk != "")
  tb_offense$Year <- year
  master_offense <- rbind(master_offense,tb_offense)

  # pull defense stats 
  url <-paste("https://www.sports-reference.com/cfb/years/", year, "-team-defense.html", sep="")
  pg <- read_html(url)
  tb_defense <- html_table(pg, fill = TRUE)
  tb_defense <- tb_defense[[1]]
  names(tb_defense) <- tb_defense[1,]
  tb_defense <- subset(tb_defense, Rk != "Rk" & Rk != "")
  tb_defense$Year <- year
  master_defense <- rbind(master_defense,tb_defense)

  # pull special teams stats 
  url <-paste("https://www.sports-reference.com/cfb/years/", year, "-special-teams.html", sep="")
  pg <- read_html(url)
  tb_special_teams <- html_table(pg, fill = TRUE)
  tb_special_teams<- tb_special_teams[[1]]
  names(tb_special_teams) <- tb_special_teams[1,]
  tb_special_teams <- subset(tb_special_teams, Rk != "Rk" & Rk != "")
  tb_special_teams$Year <- year
  master_special_teams <- rbind(master_special_teams,tb_special_teams)
}

#added another section was getting a time out error
for (year in years) {
  
  # conferences
  url <-paste("https://www.sports-reference.com/cfb/years/", year, ".html", sep="")
  pg <- read_html(url)
  tb_conferences <- html_table(pg, fill = TRUE)
  tb_conferences <- tb_conferences[[1]]
  names(tb_conferences) <- tb_conferences[1,]
  tb_conferences <- subset(tb_conferences, Rk != "Rk" & Rk != "")
  tb_conferences$Year <- year
  master_conferences <- rbind(master_conferences,tb_conferences)

  # ratings
  url <-paste("https://www.sports-reference.com/cfb/years/", year, "-ratings.html", sep="")
  pg <- read_html(url)
  tb_ratings <- html_table(pg, fill = TRUE)
  tb_ratings <- tb_ratings[[1]]
  names(tb_ratings) <- tb_ratings[1,]
  tb_ratings <- subset(tb_ratings, Rk != "Rk" & Rk != "")
  tb_ratings$Year <- year
  master_ratings <- rbind(master_ratings, tb_ratings)
 
  # standings
  url <- paste("https://www.sports-reference.com/cfb/years/", year, "-standings.html", sep="")
  pg <- read_html(url)
  tb_standings <- html_table(pg, fill = TRUE)
  tb_standings <- tb_standings[[1]]
  names(tb_standings) <- tb_standings[1,]
  tb_standings <- subset(tb_standings, Rk != "Rk" & Rk != "")
  tb_standings$Rk <- NULL 
  tb_standings$Notes <- NULL
  tb_standings$Year <- year

  # Won't have final ranking for current unfinished year/ won't have current ranking for past years 
  tb_standings$'AP Rank' <- NULL 
  tb_standings$'AP Curr' <- NULL
  tb_standings$'AP Post' <- NULL
  master_standings <- rbind(master_standings, tb_standings)
 
  # pull scores
  url <-paste("https://www.sports-reference.com/cfb/years/", year, "-schedule.html", sep="")
  pg <- read_html(url)
  tb_scores <- html_table(pg, fill = TRUE)
  tb_scores <- tb_scores[[1]]
  tb_scores <- subset(tb_scores, Rk != "Rk" & Rk != "")
  tb_scores$Year <- year
  #Didn't start recording time in data until 2013
  tb_scores$Rk <- NULL 
  tb_scores$Time <- NULL
  tb_scores$Notes <- NULL 
  names(tb_scores)[6]<-"at"
  tb_scores$visiting_team_wins<- ifelse(tb_scores$at == "@", 1, 0)
  master_scores <- rbind(master_scores,tb_scores)
}

head(master_offense)
head(master_defense)
head(master_special_teams) 
head(master_scores)
head(master_conferences)
head(master_ratings)
head(master_standings)
tail(master_scores)
str(master_ratings)

######### Set up Team  1 to be Higher SRS ##########################
#Rename variables in Winner Offense variables 

# Split current ranking and team for both winning and losing team
master_scores$RankingA <-  gsub("[[:punct:]]", "", master_scores$Winner)
master_scores$RankingB <- gsub("[[:alpha:]]", "", master_scores$RankingA)
master_scores$Winning_Team_Current_Ranking <- as.numeric(gsub("[[:blank:]]", "", master_scores$RankingB))
master_scores$RankingA <- NULL
master_scores$RankingB <- NULL

master_scores$WinnerA  <- gsub("[[:digit:]]", "", master_scores$Winner) 
master_scores$WinnerB <- gsub("\\()", "", master_scores$WinnerA)
master_scores$Winning_Team <- as.character(gsub("(^\\s+)|(\\s+$)", "", master_scores$WinnerB))
master_scores$WinnerA <- NULL
master_scores$WinnerB <- NULL

master_scores$RankingA <-  gsub("[[:punct:]]", "", master_scores$Loser)
master_scores$RankingB <- gsub("[[:alpha:]]", "", master_scores$RankingA)
master_scores$Losing_Team_Current_Ranking <- as.numeric(gsub("[[:blank:]]", "", master_scores$RankingB))
master_scores$RankingA <- NULL
master_scores$RankingB <- NULL

master_scores$LosingA  <- gsub("[[:digit:]]", "", master_scores$Loser) 
master_scores$LosingB <- gsub("\\()", "", master_scores$LosingA)
master_scores$Losing_Team <- as.character(gsub("(^\\s+)|(\\s+$)", "", master_scores$LosingB))
master_scores$LosingA <- NULL
master_scores$LosingB <- NULL
master_scores$Winning_Team

#revalue teams with different names to make all school names the same across multiple data sets
master_scores$Winning_Team <- revalue(master_scores$Winning_Team, c("Alabama-Birmingham"   = "UAB",
                                                                    "Brigham Young"        = "BYU",
                                                                    "Central Florida"      = "UCF",
                                                                    "Louisiana State"      = "LSU",
                                                                    "Mississippi"          = "Ole Miss",
                                                                    "Nevada-Las Vegas"     = "Nevada",
                                                                    "Pittsburgh"           = "Pitt",
                                                                    "Southern California"  = "USC",
                                                                    "Southern Methodist"   = "SMU",
                                                                    "Texas-El Paso"        = "UTEP",
                                                                    "Texas-San Antonio"    = "UTSA"))
master_scores
master_scores$Losing_Team <- revalue(master_scores$Losing_Team, c("Alabama-Birmingham"     = "UAB",
                                                                  "Brigham Young"        = "BYU",
                                                                  "Central Florida"      = "UCF",
                                                                  "Louisiana State"      = "LSU",
                                                                  "Mississippi"          = "Ole Miss",
                                                                  "Nevada-Las Vegas"     = "Nevada",
                                                                  "Pittsburgh"           = "Pitt",
                                                                  "Southern California"  = "USC",
                                                                  "Southern Methodist"   = "SMU",
                                                                  "Texas-El Paso"        = "UTEP",
                                                                  "Texas-San Antonio"    = "UTSA"))

master_offense$School <- revalue(master_offense$School, c("Brigham Young"        = "BYU"))
master_defense$School <- revalue(master_defense$School, c("Brigham Young"        = "BYU"))
master_special_teams$School <- revalue(master_special_teams$School, c("Brigham Young"        = "BYU"))

master_scores_schools <- master_scores %>% select(Year, Wk, Date, Winning_Team, Winning_Team_Current_Ranking, Losing_Team, Losing_Team_Current_Ranking)
master_scores
master_ratings

master_ratings_select <- master_ratings %>%  select(Year, School, SRS)
master_ratings_select
master_ratings_select$SRS <- as.numeric(master_ratings_select$SRS)

str(master_ratings_select)
str(master_scores_schools)

#join master ratings for the winner to the scores 
master_scores_wr <- left_join(master_scores_schools, master_ratings_select, by = c("Year" = "Year", "Winning_Team" = "School"))
master_scores_wr

#join master rating for the loser to the scores 
master_scores_wr_lr <- left_join(master_scores_wr, master_ratings_select, by = c("Year" = "Year", "Losing_Team" = "School"))
master_scores_wr_lr 

#Setting up Team 1 as the higher SRS rather than the winner in Column 1 (not predictive) similar to seeding in NCAA March Madness
master_scores_wr_lr$Team1 <- ifelse(master_scores_wr_lr$SRS.x  >  master_scores_wr_lr$SRS.y , 
                                    master_scores_wr_lr$Winning_Team, master_scores_wr_lr$Losing_Team)
master_scores_wr_lr

master_scores_wr_lr$Team2 <- ifelse(master_scores_wr_lr$SRS.x > master_scores_wr_lr$SRS.y,
                                    master_scores_wr_lr$Losing_Team, master_scores_wr_lr$Winning_Team)

#Team 1 is the winner - this will be the binary outcome prediction variable 
master_scores_wr_lr$Team1
master_scores_wr_lr$Team1_Wins <- ifelse(master_scores_wr_lr$Team1 == master_scores_wr_lr$Winning_Team, 1, 0)

master_scores_wr_lr$Team1
str(master_scores_wr_lr)

#SRS for team 1
master_scores_wr_lr$Team1_SRS <- ifelse(master_scores_wr_lr$Team1 == master_scores_wr_lr$Winning_Team, 
                                            master_scores_wr_lr$SRS.x, master_scores_wr_lr$SRS.y)
#SRS for team 2 
master_scores_wr_lr$Team2_SRS <- ifelse(master_scores_wr_lr$Team2 == master_scores_wr_lr$Winning_Team, 
                                            master_scores_wr_lr$SRS.x, master_scores_wr_lr$SRS.y)
table(master_scores_wr_lr$Team1_Wins)

#select variables need for the master scores- everything will be joined to this 
master_scores_team1_team2 <- master_scores_wr_lr %>% select(Year,Wk, Date, Team1, Team2, Team1_Wins, Team1_SRS, Team2_SRS)
master_scores_team1_team2

master_scores_team1_team2
str(master_scores_team1_team2)

######### Rename variabes for team 1 and denote the source ##########################
#Example T10- Team 1 Offense Table
#In the final dataset, Team 1 metrics will be differenced by Team 2
#Team 1 - Team 2 

conflict_prefer("rename", "dplyr")

master_offense_T1 <- master_offense %>% 
  rename(T1O_Offense_Rk          = Rk,
         School                  = School,
         T1_Games                = G, 
         T1O_Pts                 = Pts,
         T1O_Passing_Cmp         = Cmp, 
         T1O_Passing_Att         = Att, 
         T1O_Passing_Pct         = Pct,
         T1O_Passing_Yds         = Yds,
         T1O_Passing_TD          = TD, 
         T1O_Rushing_Att         = Att.1,
         T1O_Rushing_Yds         = Yds.1,
         T1O_Rushing_Avg         = Avg,
         T1O_Rushing_TD          = TD.1,
         T1O_Total_Offense_Plays = Plays,
         T1O_Total_Offense_Yds   = Yds.2,
         T1O_Total_Offense_Avg   = Avg.1,
         T1O_First_Down_Pass     = Pass,
         T1O_First_Down_Rush     = Rush,
         T1O_First_Down_Pen      = Pen,
         T1O_First_Down_Tot      = Tot,
         T1O_Penalties_No        = No. ,
         T1O_Penalties_Yds       = Yds.3 ,
         T1O_Turnovers_Fum       = Fum,
         T1O_Turnovers_Int       = Int,
         T1O_Turnovers_Tot       = Tot.1,
         Year                   = Year)

master_defense_T1 <- master_defense %>% 
  rename(T1D_Defense_Rk          = Rk, 
         T1D_Defense_Pts         = Pts, 
         T1D_Passing_Cmp         = Cmp, 
         T1D_Passing_Att         = Att, 
         T1D_Passing_Pct         = Pct, 
         T1D_Passing_Yds         = Yds,
         T1D_Passing_TD          = TD,
         T1D_Rushing_Att         = Att.1,
         T1D_Rushing_Yds         = Yds.1,
         T1D_Rushing_Avg         = Avg,
         T1D_Rushing_TD          = TD.1,
         T1D_Total_Offense_Plays = Plays,
         T1D_Total_Offense_Yds   = Yds.2,
         T1D_Total_Offense_Avg   = Avg.1,
         T1D_First_Downs_Pass    = Pass,
         T1D_First_Downs_Rush    = Rush,
         T1D_First_Downs_Pen     = Pen,
         T1D_First_Downs_Tot     = Tot,
         T1D_Penalities_No.      = No.,
         T1D_Penalities_Yds      = Yds.3,
         T1D_Turnovers_Fum       = Fum,
         T1D_Turnovers_Int       = Int,
         T1D_Turnovers_Tot       = TO)


master_special_teams_T1<-master_special_teams  %>% 
  rename(T1SP_RK            = Rk, 
         T1SP_Kicking_XPM   = XPM, 
         T1SP_Kicking_XPA   = XPA,
         'T1SP_Kicking_XP%'   = 'XP%',
         T1SP_Kicking_FGM   = FGM,
         T1SP_Kicking_FGA   = FGA,
         'T1SP_Kicking_FG%'   = 'FG%',
         T1SP_Kicking_Pts   = Pts,
         T1SP_Punting_Punts = Punts,
         T1SP_Punting_Yds   = Yds, 
         T1SP_Punting_Avg   = Avg,
         T1SP_Kick_Ret_Ret  = Ret,
         T1SP_kick_Ret_Yds  = Yds.1,
         T1SP_Kick_Ret_Avg  = Avg.1,
         T1SP_Kick_Ret_TD   = TD,
         T1SP_Punt_Ret_Ret  = Ret.1,
         T1SP_Punt_Ret_Yds  = Yds.2,
         T1SP_Punt_Ret_Avg  = Avg.2,
         T1SP_Punt_Ret_TD   = TD.1 )

master_ratings 
master_ratings_T1 <- master_ratings %>% 
  rename(T1R_OSRS                          = OSRS,
         T1R_DSRS                          = DSRS,
         T1R_Off_Adj_Pts                   = Off,
         T1R_Def_Adj_Pts                   = Def,
         T1R_O_Adj_Passing_Yds_Per_Attempt = Off.1,
         T1R_D_Adj_Passing_Yds_Per_Attempt = Def.1,
         T1R_O_Adj_Rushing_Yds_Per_Attempt = Off.2,
         T1R_D_Adj_Rushing_Yds_Per_Attempt = Def.2,
         T1R_O_Adj_Total_Yds_Per_Attempt   = Off.3,
         T1R_D_Adj_Total_Yds_Per_Attempt   = Def.3)
         
master_ratings_T1 <- master_ratings_T1 %>% select(School, Year, T1R_OSRS, 
                                                  T1R_DSRS,T1R_Off_Adj_Pts,T1R_Def_Adj_Pts, T1R_O_Adj_Passing_Yds_Per_Attempt,
                                                  T1R_D_Adj_Passing_Yds_Per_Attempt,T1R_O_Adj_Rushing_Yds_Per_Attempt,
                                                  T1R_D_Adj_Rushing_Yds_Per_Attempt,T1R_O_Adj_Total_Yds_Per_Attempt,
                                                  T1R_D_Adj_Total_Yds_Per_Attempt)
master_standings_T1 <- master_standings %>% 
  rename(T1S_Overall_W           = W,
         T1S_Overall_L           = L,
         T1S_Overall_Pct         = Pct,
         T1S_Conference_W        = W.1,
         T1S_Conference_L        = L.1,
         T1S_Conference_Pct      = Pct.1,
         T1S_Points_Per_Game_Off = Off,
         T1S_Points_Per_Game_Def = Def,
         T1S_SRS_SRS             = SRS,
         T1S_SRS_SOS             = SOS,
         T1S_Polls_AP_Pre        = 'AP Pre',
         T1S_Polls_AP_High       = 'AP High')
master_scores
master_scores<- master_scores %>%
  rename(Winner_Pts = Pts,
         Loser_Pts  = Pts.1)

str(master_scores)

######### Rename variabes for team 2 and denote the source ##########################

master_offense_T2 <- master_offense %>% 
  rename(T2O_Offense_Rk          = Rk,
         T2_Games                = G, 
         T2O_Pts                 = Pts,
         T2O_Passing_Cmp         = Cmp, 
         T2O_Passing_Att         = Att, 
         T2O_Passing_Pct         = Pct,
         T2O_Passing_Yds         = Yds,
         T2O_Passing_TD          = TD, 
         T2O_Rushing_Att         = Att.1,
         T2O_Rushing_Yds         = Yds.1,
         T2O_Rushing_Avg         = Avg,
         T2O_Rushing_TD          = TD.1,
         T2O_Total_Offense_Plays = Plays,
         T2O_Total_Offense_Yds   = Yds.2,
         T2O_Total_Offense_Avg   = Avg.1,
         T2O_First_Down_Pass     = Pass,
         T2O_First_Down_Rush     = Rush,
         T2O_First_Down_Pen      = Pen,
         T2O_First_Down_Tot      = Tot,
         T2O_Penalties_No        = No. ,
         T2O_Penalties_Yds       = Yds.3 ,
         T2O_Turnovers_Fum       = Fum,
         T2O_Turnovers_Int       = Int,
         T2O_Turnovers_Tot       = Tot.1)

# Loser Defense Stats
master_defense_T2 <- master_defense %>% 
  rename(T2D_Defense_Rk          = Rk, 
         T2D_Defense_Pts         = Pts, 
         T2D_Passing_Cmp         = Cmp, 
         T2D_Passing_Att         = Att, 
         T2D_Passing_Pct         = Pct, 
         T2D_Passing_Yds         = Yds,
         T2D_Passing_TD          = TD,
         T2D_Rushing_Att         = Att.1,
         T2D_Rushing_Yds         = Yds.1,
         T2D_Rushing_Avg         = Avg,
         T2D_Rushing_TD          = TD.1,
         T2D_Total_Offense_Plays = Plays,
         T2D_Total_Offense_Yds   = Yds.2,
         T2D_Total_Offense_Avg   = Avg.1,
         T2D_First_Downs_Pass    = Pass,
         T2D_First_Downs_Rush    = Rush,
         T2D_First_Downs_Pen     = Pen,
         T2D_First_Downs_Tot     = Tot,
         T2D_Penalities_No.      = No.,
         T2D_Penalities_Yds      = Yds.3,
         T2D_Turnovers_Fum       = Fum,
         T2D_Turnovers_Int       = Int,
         T2D_Turnovers_Tot       = TO)

master_special_teams_T2<-master_special_teams  %>% 
  rename(T2SP_RK            = Rk, 
         T2SP_Kicking_XPM   = XPM, 
         T2SP_Kicking_XPA   = XPA,
         'T2SP_Kicking_XP%'   = 'XP%',
         T2SP_Kicking_FGM   = FGM,
         T2SP_Kicking_FGA   = FGA,
         'T2SP_Kicking_FG%'   = 'FG%',
         T2SP_Kicking_Pts   = Pts,
         T2SP_Punting_Punts = Punts,
         T2SP_Punting_Yds   = Yds, 
         T2SP_Punting_Avg   = Avg,
         T2SP_Kick_Ret_Ret  = Ret,
         T2SP_kick_Ret_Yds  = Yds.1,
         T2SP_Kick_Ret_Avg  = Avg.1,
         T2SP_Kick_Ret_TD   = TD,
         T2SP_Punt_Ret_Ret  = Ret.1,
         T2SP_Punt_Ret_Yds  = Yds.2,
         T2SP_Punt_Ret_Avg  = Avg.2,
         T2SP_Punt_Ret_TD   = TD.1 )

master_ratings_T2 <- master_ratings %>% 
  rename(T2R_OSRS                          = OSRS,
         T2R_DSRS                          = DSRS,
         T2R_Off_Adj_Pts                   = Off,
         T2R_Def_Adj_Pts                   = Def,
         T2R_O_Adj_Passing_Yds_Per_Attempt = Off.1,
         T2R_D_Adj_Passing_Yds_Per_Attempt = Def.1,
         T2R_O_Adj_Rushing_Yds_Per_Attempt = Off.2,
         T2R_D_Adj_Rushing_Yds_Per_Attempt = Def.2,
         T2R_O_Adj_Total_Yds_Per_Attempt   = Off.3,
         T2R_D_Adj_Total_Yds_Per_Attempt   = Def.3)

master_ratings_T2 <- master_ratings_T2 %>% select(School, Year, T2R_OSRS, 
                                                  T2R_DSRS,T2R_Off_Adj_Pts,T2R_Def_Adj_Pts, T2R_O_Adj_Passing_Yds_Per_Attempt,
                                                  T2R_D_Adj_Passing_Yds_Per_Attempt,T2R_O_Adj_Rushing_Yds_Per_Attempt,
                                                  T2R_D_Adj_Rushing_Yds_Per_Attempt,T2R_O_Adj_Total_Yds_Per_Attempt,
                                                  T2R_D_Adj_Total_Yds_Per_Attempt)
master_standings_T2 <- master_standings %>% 
  rename(T2S_Overall_W           = W,
         T2S_Overall_L           = L,
         T2S_Overall_Pct         = Pct,
         T2S_Conference_W        = W.1,
         T2S_Conference_L        = L.1,
         T2S_Conference_Pct      = Pct.1,
         T2S_Points_Per_Game_Off = Off,
         T2S_Points_Per_Game_Def = Def,
         T2S_SRS_SRS             = SRS,
         T2S_SRS_SOS             = SOS,
         T2S_Polls_AP_Pre        = 'AP Pre',
         T2S_Polls_AP_High       = 'AP High')

######### Join offense, defense, special teams, standings, ratings on the scores table for the winner  ##########################
master_scores_team1_team2
master_ratings_T1
master_scores_wo <- left_join(master_scores_team1_team2, master_offense_T1, by = c("Year" = "Year", "Team1" = "School")) 
master_scores_wo
master_scores_wod <- left_join(master_scores_wo, master_defense_T1, by = c("Year" = "Year", "Team1" = "School"))
master_scores_wodst <- left_join(master_scores_wod, master_special_teams_T1, by = c("Year" = "Year", "Team1" = "School"))
master_scores_wodsts <- left_join(master_scores_wodst, master_standings_T1, by = c("Year" = "Year", "Team1" = "School"))
master_scores_wodstsr <- left_join(master_scores_wodsts, master_ratings_T1, by = c("Year" = "Year", "Team1" = "School"))
master_ratings_T1
master_scores_wodstsr
str(master_scores_wodstsr)
master_scores_wodstsr$Off
str(master_scores_wodstsr, list.len=ncol(master_scores_wodstsr))

######### Join offense, defense, special teams, standings, ratings on the scores table for the loser  ##########################
master_scores_wodstsr_lo <- left_join(master_scores_wodstsr, master_offense_T2, by = c("Year" = "Year", "Team2" = "School"))
master_scores_wodstsr_lod<- left_join(master_scores_wodstsr_lo, master_defense_T2, by = c("Year" = "Year", "Team2" = "School"))
master_scores_wodstsr_lodst<- left_join(master_scores_wodstsr_lod, master_special_teams_T2, by = c("Year" = "Year", "Team2" = "School"))
master_scores_wodstsr_lodsts <- left_join(master_scores_wodstsr_lodst, master_standings_T2, by = c("Year" = "Year", "Team2" = "School"))
master_scores_wodstsr_lodstsr <- left_join(master_scores_wodstsr_lodsts, master_ratings_T2, by = c("Year" = "Year", "Team2" = "School"))
master_scores_wodstsr_lodstsr
tail(master_scores_wodstsr_lodstsr)

str(master_scores_wodstsr_lodstsr)
master_scores_wodstsr_lodstsr$Team1
master_scores_wodstsr_lodstsr$Team2
masterA <- master_scores_wodstsr_lodstsr[complete.cases(master_scores_wodstsr_lodstsr$T1S_SRS_SRS), ]
master <- masterA[complete.cases(masterA$T2S_SRS_SRS), ]
master$Team1

#check to make sure byu and use are properly named 
byu_test  <- master[ which(master$Team1=='BYU' 
                                     | master$Team2 == 'BYU'), ]
byu_test
usc_test  <- master[ which(master$Team1=='USC' 
                                    | master$Team2 == 'USC'), ]

usc_test 
str(master)
str(master, list.len=ncol(master))

tail(master)

master$Wk <- as.numeric(master$Wk)
master$Year <- as.numeric(master$Year)

########## Make variables numeric and difference ##########################
str(master)

#Make variables numeric 
master$T1O_Pts <- as.numeric(master$T1O_Pts)
master$T1D_Defense_Pts <- as.numeric(master$T1D_Defense_Pts)
master$T2O_Pts <- as.numeric(master$T2O_Pts)
master$T2D_Defense_Pts <- as.numeric(master$T2D_Defense_Pts)

#Average Points margin
master$T1_Average_Margin <- master$T1O_Pts - master$T1D_Defense_Pts 
master$T2_Average_Margin <- master$T2O_Pts - master$T2D_Defense_Pts
master$T1_T2_Average_Margin <- master$T1O_Pts - master$T2O_Pts

str(master, list.len=ncol(master))
#Make variables numeric 
master$T1R_Off_Adj_Pts <- as.numeric(master$T1R_Off_Adj_Pts)
master$T2R_Off_Adj_Pts <- as.numeric(master$T2R_Off_Adj_Pts)
master$T1R_Def_Adj_Pts <- as.numeric(master$T1R_Def_Adj_Pts)
master$T2R_Def_Adj_Pts <- as.numeric(master$T2R_Def_Adj_Pts)

#Average Adj Points margin 
#Adjusted variables only available in 2020
master$T1_Adj_Average_Margin <- master$T1R_Off_Adj_Pts - master$T1R_Def_Adj_Pts 
master$T2_Adj_Average_Margin <- master$T2R_Off_Adj_Pts - master$T2R_Def_Adj_Pts 
master$T1_T2_Adj_Average_Margin <- master$T1_Adj_Average_Margin - master$T2_Adj_Average_Margin

# Make variables numeric
master$T1R_OSRS <- as.numeric(master$T1R_OSRS)
master$T2R_OSRS <- as.numeric(master$T2R_OSRS)
master$T1R_DSRS <- as.numeric(master$T1R_DSRS)
master$T2R_DSRS <- as.numeric(master$T2R_DSRS)

# Team 1 Offense Strength of Record vs Team 2 Defense Strength of record
master$T1_T2_OSRS_DSRS <- master$T1R_OSRS - master$T2R_DSRS
master$T1_T2_DSRS_OSRS <- master$T1R_DSRS - master$T2R_OSRS

#Make variable numeric
master$T1R_O_Adj_Passing_Yds_Per_Attempt <- as.numeric(master$T1R_O_Adj_Passing_Yds_Per_Attempt)
master$T1R_D_Adj_Passing_Yds_Per_Attempt <- as.numeric(master$T1R_D_Adj_Passing_Yds_Per_Attempt)
master$T2R_O_Adj_Passing_Yds_Per_Attempt <- as.numeric(master$T2R_O_Adj_Passing_Yds_Per_Attempt)
master$T2R_D_Adj_Passing_Yds_Per_Attempt <- as.numeric(master$T2R_D_Adj_Passing_Yds_Per_Attempt)

#Passing Yards Per Attempt Team 1 Offense vs Team 2Defense, Team 1 Defense vs Team 2 Offense
master$T1_T2_O_D_Adj_Passing_Yds_Per_Attempt<- master$T1R_O_Adj_Passing_Yds_Per_Attempt - master$T2R_D_Adj_Passing_Yds_Per_Attempt
master$T1_T2_D_O_Adj_Passing_Yds_Per_Attempt<- master$T1R_D_Adj_Passing_Yds_Per_Attempt - master$T2R_O_Adj_Passing_Yds_Per_Attempt

#Make variable numeric
master$T1R_O_Adj_Rushing_Yds_Per_Attempt <- as.numeric(master$T1R_O_Adj_Rushing_Yds_Per_Attempt)
master$T1R_D_Adj_Rushing_Yds_Per_Attempt <- as.numeric(master$T1R_D_Adj_Rushing_Yds_Per_Attempt)
master$T2R_O_Adj_Rushing_Yds_Per_Attempt <- as.numeric(master$T2R_O_Adj_Rushing_Yds_Per_Attempt)
master$T2R_D_Adj_Rushing_Yds_Per_Attempt <- as.numeric(master$T2R_D_Adj_Rushing_Yds_Per_Attempt)

#Rushing Yards Per Attempt Team 1 Offense vs Team 2Defense, Team 1 Defense vs Team 2 Offense
master$T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt <- master$T1R_O_Adj_Rushing_Yds_Per_Attempt - master$T2R_D_Adj_Rushing_Yds_Per_Attempt
master$T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt <- master$T1R_D_Adj_Rushing_Yds_Per_Attempt - master$T2R_O_Adj_Rushing_Yds_Per_Attempt

#Make variable numeric
master$T1R_O_Adj_Total_Yds_Per_Attempt <- as.numeric(master$T1R_O_Adj_Total_Yds_Per_Attempt)
master$T1R_D_Adj_Total_Yds_Per_Attempt <- as.numeric(master$T1R_D_Adj_Total_Yds_Per_Attempt)
master$T2R_O_Adj_Total_Yds_Per_Attempt <- as.numeric(master$T2R_O_Adj_Total_Yds_Per_Attempt)
master$T2R_D_Adj_Total_Yds_Per_Attempt <- as.numeric(master$T2R_D_Adj_Total_Yds_Per_Attempt)

#Total Yards Per Attempt Team 1 Offense vs Team 2Defense, Team 1 Defense vs Team 2 Offense
master$T1_T2_O_D_Adj_Total_Yds_Per_Attempt<- master$T1R_O_Adj_Total_Yds_Per_Attempt - master$T2R_D_Adj_Total_Yds_Per_Attempt
master$T1_T2_D_O_Adj_Total_Yds_Per_Attempt<- master$T1R_D_Adj_Total_Yds_Per_Attempt - master$T2R_O_Adj_Total_Yds_Per_Attempt

#Make variables numeric
master$T1D_Turnovers_Tot <- as.numeric(master$T1D_Turnovers_Tot)
master$T1O_Turnovers_Tot <- as.numeric(master$T1O_Turnovers_Tot)
master$T2D_Turnovers_Tot <- as.numeric(master$T2D_Turnovers_Tot)
master$T2O_Turnovers_Tot <- as.numeric(master$T2O_Turnovers_Tot)

#Average turnover margin
master$T1_Turnover_Margin <- master$T1D_Turnovers_Tot - master$T1O_Turnovers_Tot
master$T2_Turnover_Margin <- master$T2D_Turnovers_Tot - master$T2O_Turnovers_Tot
master$T1_T2_Turnover_Margin <- master$T1_Turnover_Margin - master$T2_Turnover_Margin

master$T1S_SRS_SRS <- as.numeric(master$T1S_SRS_SRS)
master$T2S_SRS_SRS <- as.numeric(master$T2S_SRS_SRS)
master$T1S_SRS_SOS <- as.numeric(master$T1S_SRS_SOS)
master$T2S_SRS_SOS <- as.numeric(master$T2S_SRS_SOS)

#Difference SRS and SOS 
master$T1_T2_SRS_SRS <- master$T1S_SRS_SRS - master$T2S_SRS_SRS
master$T1_T2_SRS_SOS <- master$T1S_SRS_SOS - master$T2S_SRS_SOS

str(master)
master$T1O_Penalties_Yds <- as.numeric(master$T1O_Penalties_Yds)
master$T1D_Penalities_Yds <- as.numeric(master$T1D_Penalities_Yds)
master$T2O_Penalties_Yds <- as.numeric(master$T2O_Penalties_Yds)
master$T2D_Penalities_Yds <- as.numeric(master$T2D_Penalities_Yds)

# Net Penality yards if negative Team 2 has more 
master$T1_T2_Penalties_Yds <- (master$T1O_Penalties_Yds + master$T1D_Penalities_Yds) - (master$T2O_Penalties_Yds + master$T2D_Penalities_Yds)

master$T1O_Passing_Yds <- as.numeric(master$T1O_Passing_Yds)
master$T2D_Passing_Yds <- as.numeric(master$T2D_Passing_Yds)
master$T2O_Passing_Yds <- as.numeric(master$T2O_Passing_Yds)
master$T1D_Passing_Yds <- as.numeric(master$T1D_Passing_Yds)
master$T1O_Rushing_Yds <- as.numeric(master$T1O_Rushing_Yds)
master$T2D_Rushing_Yds <- as.numeric(master$T2D_Rushing_Yds)
master$T2O_Rushing_Yds <- as.numeric(master$T2O_Rushing_Yds)
master$T1D_Rushing_Yds <- as.numeric(master$T1D_Rushing_Yds)

# Difference Passing and Rushing 
master$T1_T2_Passing_Yds <- master$T1O_Passing_Yds -  master$T2D_Passing_Yds
master$T2_T1_Passing_Yds <- master$T2O_Passing_Yds -  master$T1D_Passing_Yds
master$T1_T2_Rushing_Yds <- master$T1O_Rushing_Yds - master$T2D_Rushing_Yds
master$T2_T1_Rushing_Yds <- master$T2O_Rushing_Yds - master$T1D_Rushing_Yds

master

master$G.x <- NULL 
master$G.x.x <- NULL 
master$G.y <- NULL 
master$G.y.y <- NULL 


master$T1SP_Kicking_FGA <- NULL 
master$T1SP_Kicking_FG. <- NULL 
master$T2SP_Kicking_FGA <- NULL 
master$T2SP_Kicking_FG. <- NULL 

master$T2S_Points_Per_Game_Off <- NULL
master$T1S_Points_Per_Game_Off <- NULL 
master

master$Team1_Current_Ranking <- as.numeric(master$Team1_Current_Ranking)
master$Team2_Current_Ranking <- as.numeric(master$Team2_Current_Ranking)
master$Team1_Current_Ranking <- ifelse(is.na(master$Team1_Current_Ranking), 26, master$Team1_Current_Ranking)
master$Team2_Current_Ranking <- ifelse(is.na(master$Team2_Current_Ranking), 26, master$Team2_Current_Ranking)

master$Team1_Team2_Current_Ranking <- master$Team1_Current_Ranking - master$Team2_Current_Ranking

master$T1S_Polls_AP_Pre <- as.numeric(master$T1S_Polls_AP_Pre)
master$T1S_Polls_AP_High <- as.numeric(master$T1S_Polls_AP_High)
master$T2S_Polls_AP_Pre <- as.numeric(master$T2S_Polls_AP_Pre)
master$T2S_Polls_AP_High <- as.numeric(master$T2S_Polls_AP_High)

master$T1S_Polls_AP_Pre <- ifelse(is.na(master$T1S_Polls_AP_Pre), 26, master$T1S_Polls_AP_Pre)
master$T1S_Polls_AP_High <- ifelse(is.na(master$T1S_Polls_AP_High), 26, master$T1S_Polls_AP_High)
master$T2S_Polls_AP_Pre <- ifelse(is.na(master$T2S_Polls_AP_Pre), 26, master$T2S_Polls_AP_Pre)
master$T2S_Polls_AP_High <- ifelse(is.na(master$T2S_Polls_AP_High), 26, master$T2S_Polls_AP_High)

master$T1_T2S_Polls_AP_Pre <- master$T1S_Polls_AP_Pre - master$T2S_Polls_AP_Pre 
master$T1_T2S_Polls_AP_High <- master$T1S_Polls_AP_High - master$T2S_Polls_AP_High

master$T1S_Conference_Pct <- as.numeric(master$T1S_Conference_Pct)
master$T1S_Conference_W<- as.numeric(master$T1S_Conference_W)
master$T1S_Conference_L <- as.numeric(master$T1S_Conference_L)

master$T1S_Conference_W <- ifelse(is.na(master$T1S_Conference_W),0, master$T1S_Conference_W)
master$T1S_Conference_L <- ifelse(is.na(master$T1S_Conference_L),0, master$T1S_Conference_L)
master$T1S_Conference_Pct <- ifelse(is.na(master$T1S_Conference_Pct), 0, master$T1S_Conference_Pct)

master$T2S_Conference_W <- as.numeric(master$T2S_Conference_W)
master$T2S_Conference_L <- as.numeric(master$T2S_Conference_L)
master$T2S_Conference_Pct <- as.numeric(master$T2S_Conference_Pct)

master$T2S_Conference_W <- ifelse(is.na(master$T2S_Conference_W),0, master$T2S_Conference_W)
master$T2S_Conference_L <- ifelse(is.na(master$T2S_Conference_L),0, master$T2S_Conference_L)
master$T2S_Conference_Pct <- ifelse(is.na(master$T2S_Conference_Pct), 0, master$T2S_Conference_Pct)

master$T1_T2S_Conference_W <- master$T1S_Conference_W - master$T2S_Conference_W
master$T1_T2S_Conference_L <- master$T1S_Conference_L - master$T2S_Conference_L
master$T1_T2S_Conference_Pct <- master$T1S_Conference_Pct - master$T2S_Conference_Pct 

#winning percentage
master$T1S_Overall_Pct <- as.numeric(master$T1S_Overall_Pct)
master$T2S_Overall_Pct <- as.numeric(master$T2S_Overall_Pct)
master$T1_T2S_Overall_Pct <- master$T1S_Overall_Pct - master$T2S_Overall_Pct
str(master, list.len=ncol(master))

str(master)

tail(master)

#T1S_Overall_L 
#master$T1_T2D_Defense_Rk <- master$T1D_Defense_Rk - master$T2D_Defense_Rk

########## select only variables needed for master  ##########################
master <- master %>% select(Year, Wk, Date, Team1, Team2, Team1_Wins, Conf.x, Conf.y,
                            T1_T2_SRS_SRS,
                            T1_T2_OSRS_DSRS,
                            T1_T2_DSRS_OSRS,
                            
                            T1_T2_SRS_SOS,
                            
                            T1_T2_Average_Margin,
                            T1_T2_Adj_Average_Margin,
                            
                            T1_T2_Turnover_Margin,
                            
                            T1_T2_Penalties_Yds,
                            T1_T2_Passing_Yds,
                            T2_T1_Passing_Yds, 
                            T1_T2_Rushing_Yds,
                            T2_T1_Rushing_Yds, 
                            
                            T1_T2S_Polls_AP_Pre,
                            T1_T2S_Polls_AP_High,
                            
                            T1_T2S_Conference_Pct,
                            T1_T2S_Overall_Pct,
                            
                            T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt,
                            T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt,
                            T1_T2_O_D_Adj_Total_Yds_Per_Attempt,
                            T1_T2_D_O_Adj_Total_Yds_Per_Attempt,
                            T1_T2_O_D_Adj_Passing_Yds_Per_Attempt,
                            T1_T2_D_O_Adj_Passing_Yds_Per_Attempt)
                            
       

########## subset to only bowl games, create training and test set  ##########################

training2007_2017 <- subset(master,  master$Wk >= 15 & master$Year == 2007|
                              master$Wk >= 16 & master$Year == 2008|
                              master$Wk >= 16 & master$Year == 2009|
                              master$Wk >= 16 & master$Year == 2010|
                              master$Wk >= 16 & master$Year == 2011|
                              master$Wk >= 16 & master$Year == 2012|
                              master$Wk >= 17 & master$Year == 2013|
                              master$Wk >= 17 & master$Year == 2014|
                              master$Wk >= 16 & master$Year == 2015|
                              master$Wk >= 17 & master$Year == 2016|
                              master$Wk >= 17 & master$Year == 2017)

training2007_2018 <- subset(master,  master$Wk >= 15 & master$Year == 2007|
                              master$Wk >= 16 & master$Year == 2008|
                              master$Wk >= 16 & master$Year == 2009|
                              master$Wk >= 16 & master$Year == 2010|
                              master$Wk >= 16 & master$Year == 2011|
                              master$Wk >= 16 & master$Year == 2012|
                              master$Wk >= 17 & master$Year == 2013|
                              master$Wk >= 17 & master$Year == 2014|
                              master$Wk >= 16 & master$Year == 2015|
                              master$Wk >= 17 & master$Year == 2016|
                              master$Wk >= 17 & master$Year == 2017|
                              master$Wk >= 17 & master$Year == 2018)

training2007_2019  <- subset(master,  master$Wk >= 15 & master$Year == 2007|
                              master$Wk >= 16 & master$Year == 2008|
                              master$Wk >= 16 & master$Year == 2009|
                              master$Wk >= 16 & master$Year == 2010|
                              master$Wk >= 16 & master$Year == 2011|
                              master$Wk >= 16 & master$Year == 2012|
                              master$Wk >= 17 & master$Year == 2013|
                              master$Wk >= 17 & master$Year == 2014|
                              master$Wk >= 16 & master$Year == 2015|
                              master$Wk >= 17 & master$Year == 2016|
                              master$Wk >= 17 & master$Year == 2017|
                              master$Wk >= 17 & master$Year == 2018|
                              master$Wk >= 18 & master$Year == 2019)
tail(training2007_2019)

test_2018 <- subset(master,   master$Wk >= 17 & master$Year == 2018)
test_2019 <- subset(master,   master$Wk >= 18 & master$Year == 2019)
test_2020 <- subset(master,   master$Wk == 16 & master$Year == 2020 & master$Date == 'Dec 21, 2020'|
                              master$Wk >= 17 & master$Year == 2020)

########## Data collection is over  ##########################

########## Exploratory phase  ################################
#Exploratory 
#install.packages("car")
library(car)
#install.packages("knitr")
library(knitr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("mice")
library(mice)
#install.packages("lattice")
library(lattice)
#install.packages("reshape2")
library(reshape2)
#install.packages("Hmisc")
library(Hmisc)

#Could use any training set to look at 
training2007_2018
scatterplot(Team1_Wins~ T1_T2_SRS_SRS, data = training2007_2018)
summary(training2007_2018)
#team 1 (team with higher SRS wins 74 %)
str(training2007_2017)

num_2017 <- training2007_2017 %>% select (Team1_Wins,
                            T1_T2_SRS_SRS,
                            T1_T2_OSRS_DSRS,
                            T1_T2_DSRS_OSRS,
                            
                            T1_T2_SRS_SOS,
                            
                            T1_T2_Average_Margin,
                            
                            #only available for 2020 so far
                            #T1_T2_Adj_Average_Margin,
                            
                            T1_T2_Turnover_Margin,
                            
                            T1_T2_Penalties_Yds,
                            T1_T2_Passing_Yds,
                            T2_T1_Passing_Yds, 
                            T1_T2_Rushing_Yds,
                            T2_T1_Rushing_Yds, 
                            
                            T1_T2S_Polls_AP_Pre,
                            T1_T2S_Polls_AP_High,
                            
                            T1_T2S_Conference_Pct,
                            T1_T2S_Overall_Pct)

# SRS .35 , overall win percent .42
round(cor(num_2017),digits = 2) # rounded to 2 decimals

num_2017$Team1_Wins
# Convert the variable dose from a numeric to a factor variable
num_2017$Team1_Wins <- as.factor(num_2017$Team1_Wins)

# Look at boxplots to explore how variables look based on win/loss for team1
boxplot_SRS <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_SRS_SRS)) + geom_boxplot()
boxplot_SRS
boxplot_SOS <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_SRS_SOS)) + geom_boxplot()
boxplot_SOS
boxplot_OSRS_DSRS <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_OSRS_DSRS)) + geom_boxplot()
boxplot_OSRS_DSRS
boxplot_DSRS_OSRS <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_DSRS_OSRS)) + geom_boxplot()
boxplot_DSRS_OSRS
boxplot_Average_Margin <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_Average_Margin)) + geom_boxplot()
boxplot_Average_Margin
boxplot_Turnover_Margin <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_Turnover_Margin)) + geom_boxplot()
boxplot_Turnover_Margin
boxplot_Penalties_Yds <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_Penalties_Yds)) + geom_boxplot()
boxplot_Penalties_Yds
boxplot_Conference_Pct <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2S_Conference_Pct)) + geom_boxplot()
boxplot_Conference_Pct
boxplot_Overall_Pct <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2S_Overall_Pct)) + geom_boxplot()
boxplot_Overall_Pct
boxplot_T1_T2_Passing_Yds <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_Passing_Yds)) + geom_boxplot()
boxplot_T1_T2_Passing_Yds
boxplot_T2_T1_Passing_Yds <- ggplot(num_2017, aes(x=Team1_Wins, y=T2_T1_Passing_Yds)) + geom_boxplot()
boxplot_T2_T1_Passing_Yds
boxplot_T1_T2_Rushing_Yds <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2_Rushing_Yds)) + geom_boxplot()
boxplot_T1_T2_Rushing_Yds
boxplot_T2_T1_Passing_Yds <- ggplot(num_2017, aes(x=Team1_Wins, y=T2_T1_Passing_Yds)) + geom_boxplot()
boxplot_T2_T1_Passing_Yds
boxplot_Polls_AP_Pre <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2S_Polls_AP_Pre)) + geom_boxplot()
boxplot_Polls_AP_Pre
boxplot_Poll_AP_High <- ggplot(num_2017, aes(x=Team1_Wins, y=T1_T2S_Polls_AP_High)) + geom_boxplot()
boxplot_Poll_AP_High

########## Model Building ###############################
#Used to predict bowl game outcomes
#Used manual backwards elimination based on p value of .05

########## train 2007-2017, test 2018 ###################
#Logistic regression 2007-2017 train, test 2018
training2007_2017
mylogit <- glm(Team1_Wins ~  T1_T2_SRS_SRS + 
               #T1_T2_OSRS_DSRS + 
               #T1_T2_DSRS_OSRS +
               T1_T2_SRS_SOS +
               #T1_T2_Average_Margin + 
               #T1_T2_Adj_Average_Margin + 
               #T1_T2_Turnover_Margin + 
               #T1_T2_Penalties_Yds + 
               #T1_T2_Passing_Yds + 
               #T2_T1_Passing_Yds +
               #T1_T2_Rushing_Yds +
               T2_T1_Rushing_Yds +
               T1_T2S_Polls_AP_Pre + 
               T1_T2S_Polls_AP_High + 
               T1_T2S_Conference_Pct + 
               T1_T2S_Overall_Pct 
               #Adj variables just added for 2020 so NA, will be used in future models
               #T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt +
               #T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt + 
               #T1_T2_O_D_Adj_Total_Yds_Per_Attempt + 
               #T1_T2_D_O_Adj_Total_Yds_Per_Attempt + 
               #T1_T2_O_D_Adj_Passing_Yds_Per_Attempt + 
               #T1_T2_D_O_Adj_Passing_Yds_Per_Attempt
              , data = training2007_2017, family = "binomial")
summary(mylogit)

test_2018$Team1_Wins_P<- predict(mylogit, newdata = test_2018, type = "response")
test_2018$Team1_Wins_Prediction <- ifelse(test_2018$Team1_Wins_P > .5, 1, 0)
test_2018$Prediction_Correct <- ifelse(test_2018$Team1_Wins_Prediction == test_2018$Team1_Wins, 1, 0)
test_2018$Prediction_Correct
accuracy_LR_2018 <- sum(test_2018$Prediction_Correct)/nrow(test_2018)
accuracy_LR_2018
#84%
write.csv(test_2018, "logistic_regression_2018.csv")


#Random Forest 2007-2017 train, test 2018
library(ranger) 
RFmodel<- ranger(Team1_Wins~ T1_T2_SRS_SRS + 
                   #T1_T2_OSRS_DSRS + 
                   #T1_T2_DSRS_OSRS +
                   T1_T2_SRS_SOS +
                   #T1_T2_Average_Margin + 
                   #T1_T2_Adj_Average_Margin + 
                   #T1_T2_Turnover_Margin + 
                   #T1_T2_Penalties_Yds + 
                   #T1_T2_Passing_Yds + 
                   #T2_T1_Passing_Yds +
                   #T1_T2_Rushing_Yds +
                   T2_T1_Rushing_Yds +
                   T1_T2S_Polls_AP_Pre + 
                   T1_T2S_Polls_AP_High + 
                   T1_T2S_Conference_Pct + 
                   T1_T2S_Overall_Pct 
                   #T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt +
                   #T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt + 
                   #T1_T2_O_D_Adj_Total_Yds_Per_Attempt + 
                   #T1_T2_D_O_Adj_Total_Yds_Per_Attempt + 
                   #T1_T2_O_D_Adj_Passing_Yds_Per_Attempt + 
                   #T1_T2_D_O_Adj_Passing_Yds_Per_Attempt
                 , data=training2007_2017, probability = TRUE,importance="impurity" ) 
pred_prob_rf<-predict(RFmodel, data= test_2018, type = "response" )
test_2018$Team1_Wins_P<-pred_prob_rf$predictions[,1]
test_2018$Team1_Wins_Prediction<-ifelse(test_2018$Team1_Wins_P > .5, 1, 0)
test_2018$Prediction_Correct <- ifelse(test_2018$Team1_Wins_Prediction == test_2018$Team1_Wins, 1, 0)
accuracy_RF_2018 <- sum(test_2018$Prediction_Correct)/nrow(test_2018)
accuracy_RF_2018
#89 %

write.csv(test_2018, "random_forest_2018.csv")
sort(importance(RFmodel),decreasing=TRUE)[1:10]

########## train 2007-2018, test 2019 ###################
#Logistic regression- train 2007-2018, test on 2019
mylogit <- glm(Team1_Wins~  T1_T2_SRS_SRS + 
                 #T1_T2_OSRS_DSRS + 
                 #T1_T2_DSRS_OSRS +
                 T1_T2_SRS_SOS +
                 #T1_T2_Average_Margin + 
                 #T1_T2_Adj_Average_Margin + 
                 #T1_T2_Turnover_Margin + 
                 #T1_T2_Penalties_Yds + 
                 #T1_T2_Passing_Yds + 
                 #T2_T1_Passing_Yds +
                 #T1_T2_Rushing_Yds +
                 T2_T1_Rushing_Yds +
                 T1_T2S_Polls_AP_Pre + 
                 T1_T2S_Polls_AP_High + 
                 T1_T2S_Conference_Pct + 
                 T1_T2S_Overall_Pct 
                 #T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt +
                 #T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt + 
                 #T1_T2_O_D_Adj_Total_Yds_Per_Attempt + 
                 #T1_T2_D_O_Adj_Total_Yds_Per_Attempt + 
                 #T1_T2_O_D_Adj_Passing_Yds_Per_Attempt + 
                 #T1_T2_D_O_Adj_Passing_Yds_Per_Attempt
                , data = training2007_2018, family = "binomial")
summary(mylogit)

test_2019$Team1_Wins_P<- predict(mylogit, newdata = test_2019, type = "response")
test_2019$Team1_Wins_Prediction <- ifelse(test_2019$Team1_Wins_P > .5, 1, 0)
test_2019$Prediction_Correct <- ifelse(test_2019$Team1_Wins_Prediction == test_2019$Team1_Wins, 1, 0)
test_2019$Prediction_Correct
accuracy_LR_2019 <- sum(test_2019$Prediction_Correct)/nrow(test_2019)
accuracy_LR_2019
#85 percent 

write.csv(test_2019, "logistic_regression_2019.csv")

# Do more exploration on T2_T1_Rushing_Yds

#Random Forest- train 2007-2018, test on 2019
RFmodel1<- ranger(Team1_Wins~ T1_T2_SRS_SRS + 
                    #T1_T2_OSRS_DSRS + 
                    #T1_T2_DSRS_OSRS +
                    T1_T2_SRS_SOS +
                    #T1_T2_Average_Margin + 
                    #T1_T2_Adj_Average_Margin + 
                    #T1_T2_Turnover_Margin + 
                    #T1_T2_Penalties_Yds + 
                    #T1_T2_Passing_Yds + 
                    #T2_T1_Passing_Yds +
                    #T1_T2_Rushing_Yds +
                    T2_T1_Rushing_Yds +
                    T1_T2S_Polls_AP_Pre + 
                    T1_T2S_Polls_AP_High + 
                    T1_T2S_Conference_Pct + 
                    T1_T2S_Overall_Pct  
                    #T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt +
                    #T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt + 
                    #T1_T2_O_D_Adj_Total_Yds_Per_Attempt + 
                    #T1_T2_D_O_Adj_Total_Yds_Per_Attempt + 
                    #T1_T2_O_D_Adj_Passing_Yds_Per_Attempt + 
                    #T1_T2_D_O_Adj_Passing_Yds_Per_Attempt
                  , data=training2007_2018, probability = TRUE,importance="impurity" ) 
pred_prob_rf<-predict(RFmodel1, data= test_2019, type = "response" )
test_2019$Team1_Wins_P<-pred_prob_rf$predictions[,1]
test_2019$Team1_Wins_Prediction<-ifelse(test_2019$Team1_Wins_P > .5, 1, 0)
test_2019$Prediction_Correct <- ifelse(test_2019$Team1_Wins_Prediction == test_2019$Team1_Wins, 1, 0)
accuracy_RF_2019 <- sum(test_2019$Prediction_Correct)/nrow(test_2019)
accuracy_RF_2019
#80%

write.csv(test_2019, "random_forest_2019.csv")
sort(importance(RFmodel1),decreasing=TRUE)[1:10]

########## train 2007-2019, test 2020 ###################
#Logistic Regression- train 2007-2019, test on 2020
mylogit <- glm(Team1_Wins~  T1_T2_SRS_SRS + 
                  #T1_T2_OSRS_DSRS + 
                  #T1_T2_DSRS_OSRS +
                  T1_T2_SRS_SOS +
                  #T1_T2_Average_Margin + 
                  #T1_T2_Adj_Average_Margin + 
                  #T1_T2_Turnover_Margin + 
                  #T1_T2_Penalties_Yds + 
                  #T1_T2_Passing_Yds + 
                  #T2_T1_Passing_Yds +
                  #T1_T2_Rushing_Yds +
                  T2_T1_Rushing_Yds +
                  T1_T2S_Polls_AP_Pre + 
                  T1_T2S_Polls_AP_High + 
                  T1_T2S_Conference_Pct + 
                  T1_T2S_Overall_Pct 
                  # + 
                  #T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt +
                  #T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt + 
                  #T1_T2_O_D_Adj_Total_Yds_Per_Attempt + 
                  #T1_T2_D_O_Adj_Total_Yds_Per_Attempt + 
                  #T1_T2_O_D_Adj_Passing_Yds_Per_Attempt + 
                  #T1_T2_D_O_Adj_Passing_Yds_Per_Attempt .08
                  , data = training2007_2019, family = "binomial")
summary(mylogit)

test_2020$Team1_Wins_P<- predict(mylogit, newdata = test_2020, type = "response")
test_2020$Team1_Wins_Prediction <- ifelse(test_2020$Team1_Wins_P > .5, 1, 0)
test_2020$Prediction_Correct <- ifelse(test_2020$Team1_Wins_Prediction == test_2020$Team1_Wins, 1, 0)
test_2020$Prediction_Correct
accuracy_LR_2020 <- sum(test_2020$Prediction_Correct)/nrow(test_2020)
accuracy_LR_2020
#80%

write.csv(test_2020, "logistic_regression_2020.csv")

RFmodel1<- ranger(Team1_Wins~ T1_T2_SRS_SRS + 
                    #T1_T2_OSRS_DSRS + 
                    #T1_T2_DSRS_OSRS +
                    T1_T2_SRS_SOS +
                    #T1_T2_Average_Margin + 
                    #T1_T2_Adj_Average_Margin + 
                    #T1_T2_Turnover_Margin + 
                    #T1_T2_Penalties_Yds + 
                    #T1_T2_Passing_Yds + 
                    #T2_T1_Passing_Yds +
                    #T1_T2_Rushing_Yds +
                    T2_T1_Rushing_Yds +
                    T1_T2S_Polls_AP_Pre + 
                    T1_T2S_Polls_AP_High + 
                    T1_T2S_Conference_Pct + 
                    T1_T2S_Overall_Pct  
                    #T1_T2_O_D_Adj_Rushing_Yds_Per_Attempt +
                    #T1_T2_D_O_Adj_Rushing_Yds_Per_Attempt + 
                    #T1_T2_O_D_Adj_Total_Yds_Per_Attempt + 
                    #T1_T2_D_O_Adj_Total_Yds_Per_Attempt + 
                    #T1_T2_O_D_Adj_Passing_Yds_Per_Attempt + 
                    #T1_T2_D_O_Adj_Passing_Yds_Per_Attempt
                  , data=training2007_2019, probability = TRUE,importance="impurity" ) 
pred_prob_rf<-predict(RFmodel1, data= test_2020, type = "response" )

test_2020$Team1_Wins_P<-pred_prob_rf$predictions[,1]
test_2020$Team1_Wins_Prediction<-ifelse(test_2020$Team1_Wins_P > .5, 1, 0)
test_2020$Prediction_Correct <- ifelse(test_2020$Team1_Wins_Prediction == test_2020$Team1_Wins, 1, 0)
accuracy_RF_2020 <- sum(test_2020$Prediction_Correct)/nrow(test_2020)
accuracy_RF_2020
# 84 %

write.csv(test_2020, "random_forest_2020.csv")
sort(importance(RFmodel1),decreasing=TRUE)[1:10]


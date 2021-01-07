```{r setup, include=FALSE}
library(tidyverse)
library(gganimate)
library(magick)
library(magrittr)
library(gt)
library(nflfastr)

#excelinfo = list.files(pattern="*.csv")
#gamesfile <- read_csv("games.csv")

df_tracking = read_csv("week1.csv")
df_plays = read_csv("plays.csv")
targetedrecievers_data = read_csv("targetedReceiver.csv")
df_players <- read_csv("players.csv")
df_games <- read.csv("games.csv")

#weeks of NFL season
weeks <- seq(1, 17)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("week",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}

#filter plays where y of ball is not on field in firt frame
averageyofball <- df_tracking %>% 
  filter(displayName == "Football") %>%
  group_by(gameId, playId) %>%
  summarise(firstframey = y[1]) %>%
  filter(firstframey < 0) %>%
  mutate("gameIdplayId" = paste(gameId,playId))

#filter out if ball started out of bounds
df_plays <- df_plays %>%
  mutate("gameIdplayId" = paste(gameId,playId)) %>%
  filter(!(gameIdplayId %in% averageyofball$gameIdplayId))

#merging dfs together
df_merged <- inner_join(df_games,
                        df_plays,
                        by = c("gameId" = "gameId")) %>%
  
  #merging targeted reciever and then player data from playerId
  inner_join(targetedrecievers_data,
             by = c("gameId" = "gameId",
                    "playId" = "playId")) %>%
  inner_join(df_players %>% dplyr::select(nflId, "targetedReceiveronplay" = displayName, "targetedReceiveronplayposition" = position),
             by = c("targetNflId" = "nflId")) %>%
  
  #merging games data to previously merged frame
  inner_join(df_tracking,
             by = c("gameId" = "gameId",
                    "playId" = "playId")) %>%
  mutate(Target = ifelse(targetNflId == nflId,1,0)) %>%
  mutate(Target = ifelse(displayName == "Football",0,Target))

passArivalEvents <- c('pass_outcome_caught',
                      'pass_arrived',
                      'pass_outcome_incomplete',
                      'pass_outcome_interception',
                      'pass_outcome_touchdown')

#Distance Code
df_passarrived_distanceToFootball <- df_merged %>% group_by(gameId,playId,frameId) %>% summarise(sum(Target))
df_merged <- left_join(df_merged, df_passarrived_distanceToFootball, by = c("gameId"="gameId","playId"="playId","frameId"="frameId"))
df_passarrived_distanceToFootball <- df_merged %>% filter(`sum(Target)` == 1) %>%
  
  #filter for pass_arrived frames
  filter(event == 'pass_arrived') %>%
  
  #filter out Dallas Cowboys play where Terrance Williams was targeted receiver with no tracking data
  #filter(!(gameId == 2018090910 & playId == 477)) %>%
  #filter(!(gameId == 2018090912 & playId == 2439)) %>%
  #filter(!(gameId == 2018091600 & playId == 3549)) %>%#Could change pass_arrive football frame_id
  filter(!(gameId == 2018100710 & playId == 4418)) %>%
  filter(!(gameId == 2018111900 & playId == 2988)) %>%
  filter(!(gameId == 2018111900 & playId == 5048)) %>%
  filter(!(gameId == 2018112510 & playId == 3572)) %>%
  filter(!(gameId == 2018120206 & playId == 3991)) %>%
  filter(!(gameId == 2018122400 & playId == 2493)) %>%
  filter(!(gameId == 2018123013 & playId == 2436)) %>%
  
  #determining side of ball
  mutate(sideOfBall = ifelse(#if tracked player is home and home has ball
    ((team == "home") &
       (possessionTeam == homeTeamAbbr)) |
      
      #if tracked player is away and away has ball
      ((team == "away") &
         (possessionTeam == visitorTeamAbbr)),
    
    
    #if either condition is true, offense
    "offense",
    
    #if neither condition is true, defense
    "defense"),
    
    #defining defensive team
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,
                           visitorTeamAbbr,
                           homeTeamAbbr)) %>%
  
  #grouping by game, play and frame
  group_by(gameId, playId, frameId) %>%
  
  #checking if football reading is in frame
  mutate(footballInPlay = sum(displayName == "Football") > 0) %>%
  
  #using only frames with football marked; some plays its missing
  filter(footballInPlay) %>%
  
  #adding x and y location of football as columns
  mutate(xFootballAtBallArrival = x[displayName == "Football"],
         yFootballAtBallArrival = y[displayName == "Football"],
         xTargetedReceiverAtBallArrival = x[Target == 1],
         yTargetedReceiverAtBallArrival = y[Target == 1]) %>%
  
  #ungrouping
  ungroup() %>%
  
  #grouping by game and play
  group_by(gameId, playId) %>%
  
  #selecting first frame with in case there are multiple
  filter(frameId == min(frameId)) %>%
  
  #calculating distance to football
  mutate(
    
    distToFootballAtBallArrival = sqrt((x - xFootballAtBallArrival) ^ 2 +
                                         (y - yFootballAtBallArrival) ^ 2),
    
    distToTargetedReceiveratBallArrival = sqrt((x - xTargetedReceiverAtBallArrival) ^ 2 +
                                                 (y - yTargetedReceiverAtBallArrival) ^ 2)
    
  )

df_targetedreceiveratballarrival <- df_passarrived_distanceToFootball %>%   
  
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId),
         
         #removing offensive players
         targetNflId == nflId)

df_closetdefenderatballarrival <- df_passarrived_distanceToFootball %>%   
  
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId),
         
         #removing offensive players
         sideOfBall == "defense") %>%
  
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  
  #filtering for closest defender to ball
  filter(distToFootballAtBallArrival == 
           min(distToFootballAtBallArrival))


closetdefenderatballarrival <- df_closetdefenderatballarrival %>%
  dplyr::select(gameId, 
                playId, 
                "closestdefenderatballarrivalnflId" = nflId, 
                "closestdefenderatballarrival" = displayName, 
                "closestdefenderatballarrivalposition" = position,
                "closestdefendertoballdistanceatballarrival" = distToFootballAtBallArrival,
                "closestdefendertotargetedreceiverdistanceatballarrival" = distToTargetedReceiveratBallArrival,
                "closestdefendertotargetedreceiverspeed" = s,
                "closestdefendertotargetedreceiverdirection" = dir,
                "closestdefendertotargetedreceiverorientation" = o)

df_totalatballarrival <- inner_join(df_targetedreceiveratballarrival,
                                    closetdefenderatballarrival,
                                    by = (c("gameId",
                                            "playId")))

# Ball Speed
ballspeed <- df_tracking %>% filter(event %in% passArivalEvents) %>%
  filter (displayName== 'Football') %>%
  dplyr::select(gameId,playId,s)
colnames(ballspeed)[3] <- "FBSpeed"

#Joining the FB Speed with the rest of the data

df_totalatballarrival <- inner_join(df_totalatballarrival, ballspeed, by = c("playId", 'gameId'))

# Time Between Create
timemarks <- c("ball_snap","pass_forward","pass_shovel")
time <- df_tracking %>% filter(event %in% timemarks) %>% group_by(gameId,playId) %>% filter(position=="QB") 
time <- time %>% group_by(gameId,playId) %>% mutate(snap_time = min(time),
                                                    pass_time = max(time),
                                                    time_to_throw = difftime(pass_time,snap_time))
time <- time %>% group_by(gameId,playId) %>% slice(1)

#Time Between Merge
df_totalatballarrival <- df_totalatballarrival %>% left_join(time %>% dplyr::select(gameId, playId, time_to_throw), by=c("gameId"="gameId","playId"="playId"))

# Separation At Pass Forward

# Pass Forward Events 
passForwardEvents <- c("pass_forward","pass_shovel")
# Closest Defender To Target At Throw
distanceToTargetedReceiver_throw <- df_merged %>% filter(`sum(Target)` == 1) %>%
  
  # removing play without receiver coordinates
  
  #determining side of ball
  mutate(sideOfBall = ifelse(#if tracked player is home and home has ball
    ((team == "home") &
       (possessionTeam == homeTeamAbbr)) |
      
      #if tracked player is away and away has ball
      ((team == "away") &
         (possessionTeam == visitorTeamAbbr)),
    
    
    #if either condition is true, offense
    "offense",
    
    #if neither condition is true, defense
    "defense"),
    
    #defining defensive team
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,
                           visitorTeamAbbr,
                           homeTeamAbbr)) %>%
  
  #grouping by game, play and frame
  group_by(gameId, playId, frameId) %>%
  
  #adding x and y location of football as columns
  mutate(xTarget = x[Target == 1],
         yTarget = y[Target == 1]) %>%
  
  #ungrouping
  ungroup() %>%
  
  #grouping by game and play
  group_by(gameId, playId) %>%
  
  #selecting frames that contain pass arrival events
  filter(event %in% passForwardEvents) %>%
  
  #selecting first frame with in case there are multiple
  filter(frameId == min(frameId)) %>%
  
  #calculating distance to target at pass forward
  mutate(
    
    distToTarget_throw = sqrt((x - xTarget) ^ 2 +
                                (y - yTarget) ^ 2)
    
  )

# Closest Defender Separation At Throw
separation_throw <- distanceToTargetedReceiver_throw %>%  filter(sideOfBall=="defense") %>%
  
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId),
         
         #removing defensive PI
         !isDefensivePI) %>%
  
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  
  #filtering for closest defender to target
  filter(distToTarget_throw == 
           min(distToTarget_throw))

# Merge Separation At Throw To Total DF
df_totalatballarrival <- df_totalatballarrival %>% left_join(separation_throw %>% dplyr::select("gameId","playId","distToTarget_throw","nflId"),by=c("gameId"="gameId","playId"="playId"), suffix =c("","_closest_def_throw"))

# Same Closest Defender

df_totalatballarrival <- df_totalatballarrival %>% mutate(
  same_defender = ifelse(closestdefenderatballarrivalnflId==nflId_closest_def_throw,1,0)
)
```
``` {r}
# Football X,Y At Throw
football_coord_throw <- df_tracking %>% filter(event %in% passForwardEvents) %>% filter(displayName=="Football") %>% dplyr::select(gameId,playId,x,y)
colnames(football_coord_throw)[3:4] <- c("x_throw","y_throw")
# Football X,Y At Arrival
football_coord_arrival <- df_tracking %>% filter(event %in% passArivalEvents) %>% filter(displayName=="Football") %>% dplyr::select(gameId,playId,x,y)
colnames(football_coord_arrival)[3:4] <- c("x_arrival","y_arrival")
# Merge
football_coords <- merge(football_coord_throw,football_coord_arrival,by=c("gameId"="gameId","playId"="playId"))
# Distance Between
football_coords <- football_coords %>%
  mutate(
    distThrown= sqrt((x_throw - x_arrival) ^ 2 +
                       (y_throw - y_arrival) ^ 2)
  )

# Merge To Total DF
df_totalatballarrival <- df_totalatballarrival %>% left_join(football_coords %>% dplyr::select(gameId,playId,distThrown), by = c("gameId"="gameId","playId"="playId"))

#PI
df_totalatballarrival$passResult[df_totalatballarrival$isDefensivePI == "TRUE"] <- "C"

tippedplays<-c(df_merged$playId[which(df_merged$event=="pass_tipped")])
df_totalatballarrival$tipped <- ifelse(df_totalatballarrival$playId %in% tippedplays, 1, 0)
```

``` {r}
# s a o dir are all of the targeted receiver
df_model <- df_totalatballarrival %>% dplyr::select(passResult,s,a,o,dir,distToFootballAtBallArrival,closestdefendertoballdistanceatballarrival,closestdefendertotargetedreceiverdistanceatballarrival,closestdefendertotargetedreceiverspeed,closestdefendertotargetedreceiverdirection,closestdefendertotargetedreceiverorientation,FBSpeed,time_to_throw,distToTarget_throw,same_defender,distThrown,tipped,xFootballAtBallArrival,yFootballAtBallArrival,x,y)
df_model <- df_model %>% group_by(gameId,playId) %>% slice(1)
df_model <- df_model %>% mutate(
  passResult = ifelse(passResult=="C",1,0)
)

## Split Train and Test
#Set seed for random samples and random forests
set.seed(1)
#Define the plays to be in the test data set

# Test on 3,000 plays -- 3,000 plays divided by 256 games is 11.7 (11) plays per game
df_model <- df_model %>% mutate(
  gameplay = paste(gameId,playId)
)

Test_Plays <- df_model %>%
  group_by(gameId) %>%
  sample_n(11, replace = FALSE)

#Define the test data set for the win probability model
plays_test <- df_model %>% 
  filter(gameplay %in% Test_Plays$gameplay)
##Training data set
plays_train <- df_model %>% 
  filter(!gameplay %in% Test_Plays$gameplay)
#Check the dimensions to make sure we didn't lose any observations
dim(df_model)
dim(plays_test)
dim(plays_train)
isTRUE(dim(df_model)[1] == dim(plays_test)[1] + dim(plays_train)[1])
# all good here

testmodel <- glm(passResult ~ s+a+distToFootballAtBallArrival+closestdefendertoballdistanceatballarrival*FBSpeed+closestdefendertotargetedreceiverdistanceatballarrival+closestdefendertotargetedreceiverspeed+distToFootballAtBallArrival*closestdefendertoballdistanceatballarrival+time_to_throw+same_defender+distThrown, data = plays_train,family="binomial")
summary(testmodel)

plays_test$completion_perc <- predict(testmodel,type='response',plays_test)
plays_test <- plays_test %>% mutate(
  completion_est = ifelse(completion_perc>=0.5,1,0)
)
plays_test <- plays_test %>% mutate(
  correct = ifelse(completion_est==passResult,1,0)
)
#2758 rows in plays_test
plays_test <- plays_test %>% filter(!is.na(correct))
sum(plays_test$correct)
# 2260 correct
2260/2759
#81.91% accurate
```
``` {r}
# no duplicates
df_totalatballarrival <- df_totalatballarrival %>% group_by(gameId,playId) %>% slice(1)

model <- glm(passResult ~ s+a+distToFootballAtBallArrival+closestdefendertoballdistanceatballarrival*FBSpeed+closestdefendertotargetedreceiverdistanceatballarrival+closestdefendertotargetedreceiverspeed+distToFootballAtBallArrival*closestdefendertoballdistanceatballarrival+time_to_throw+same_defender+distThrown, data = df_model,family="binomial")
summary(model)

# predicting on full data set
df_totalatballarrival$completion_probability <- predict(model,type='response',df_totalatballarrival)
# difference between probability and what actually happened
df_totalatballarrival <- df_totalatballarrival %>% mutate(
  passResult = ifelse(passResult=="C",1,0)
)
df_totalatballarrival$difference <- df_totalatballarrival$completion_probability - df_totalatballarrival$passResult

# Defenders
df_defender <- df_totalatballarrival %>% dplyr::select(gameId,playId,closestdefenderatballarrival,difference)
defender_targets <- table(df_defender$closestdefenderatballarrival)
defender_targets <- as.data.frame(defender_targets)
colnames(defender_targets)[1:2] <- c("Defender","Targets")

df_defender <- df_defender %>% filter(!is.na(difference))
defender_diff <- df_defender %>% group_by(closestdefenderatballarrival) %>% summarise(sum(difference))
# Score Is The Sum of Percentage Point Differences .... A Catch = 1 and an Interception or Incompletion = 0
# To obtain the score.. the final result(1 or 0) is subtracted from the completion probability ... you get points for breaking up a pass and lose points for allowing completions .. more points gained for high completion probabilities that are broken up and less points lost for high completion probabilities that are completed
colnames(defender_diff)[1:2] <- c("Defender","Score")
defender_scores <- merge(defender_targets,defender_diff,by="Defender")
defender_scores$Per_Target <- defender_scores$Score/defender_scores$Targets
plot(density(defender_scores$Per_Target), main="Density Plot: Score Per Target", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(defender_scores$Per_Target), 2)))  
targets_above_30 <- defender_scores %>% filter(Targets>30) %>% arrange(-Per_Target)
density(defender_scores$Targets)
top_twenty <- targets_above_30 %>% top_n(20,Per_Target)
bottom_twenty <- targets_above_30 %>% top_n(-20,Per_Target)

top_bottom_forty <- rbind(top_twenty,bottom_twenty)

top_bottom_forty %>% gt() %>% fmt_number(columns = c(3:4),decimals = 4) %>% opt_row_striping() %>% cols_label("Defender"=md("**Defender**"),"Targets"=md("**Targets**"),"Score"=md("**Cumulative<br>Score**"),"Per_Target"=md("**Score<br>Per Target**")) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray3"),
      cell_text(color="white",size="18")),
    locations = cells_column_labels(c(1:4))) %>% tab_header(md("**Top 20 & Bottom 20 Scores Per Target**"),subtitle = "2018 Regular Season") %>% tab_row_group(group="Bottom Twenty",rows = c(21:40)) %>% tab_row_group(group="Top Twenty",rows=c(1:20)) %>% tab_style(
      style = list(
        cell_fill(color="gray1"),
        cell_text(color="white",size="18",align="center")),
      locations = cells_row_groups(groups = TRUE)
    )


df_players <- df_players %>% left_join(df_tracking[!duplicated(df_tracking$displayName),] %>% dplyr::select(displayName,team),all.x=TRUE,all.y=FALSE)
df_games$homeTeamAbbr <- as.character(df_games$homeTeamAbbr)
df_games$visitorTeamAbbr <- as.character(df_games$visitorTeamAbbr)
df_tracking <- df_tracking %>% left_join(df_games %>% dplyr::select(gameId,homeTeamAbbr,visitorTeamAbbr))
df_tracking <- df_tracking %>% mutate(
  player_team = ifelse(team=="home",homeTeamAbbr,visitorTeamAbbr)
)
df_2 <- df_tracking[!duplicated(df_tracking$displayName),]
df_players <- df_players %>% left_join(df_2 %>% dplyr::select(displayName,player_team),by="displayName")
defender_scores <- defender_scores %>% left_join(df_players %>% dplyr::select(displayName,player_team),by=c("Defender"="displayName"))

# Cumulative Scores and Targets By Team
team_targets <- table(defender_scores$player_team,defender_scores$Targets)
team_targets <- as.data.frame.matrix(team_targets)
team_targets <- defender_scores %>% group_by(player_team) %>% summarise("Team Targets" = sum(Targets))
defender_scores$player_team[which(defender_scores$Defender=="Delano Hill")] <- "SEA"
defender_scores$player_team[which(defender_scores$Defender=="Kenny Moore")] <- "IND"
defender_scores$player_team[which(defender_scores$Defender=="Trey Walker")] <- "DET"
defender_scores$player_team[which(defender_scores$Defender=="Vernon Hargreaves")] <- "TB"
team_score <- defender_scores %>% group_by(player_team) %>% summarise("Team Cumulative Score" = sum(Score),"Team Targets"=sum(Targets))
team_score$Per_Target <- team_score$`Team Cumulative Score`/team_score$`Team Targets`
team_score <- team_score %>% arrange(-Per_Target)

team_score %>% gt() %>% fmt_number(columns = c(2,4),decimals = 6) %>% opt_row_striping() %>% cols_label("player_team"=md("**Team**"),"Team Cumulative Score"=md("**Team Cumulative<br>Score**"),"Team Targets"=md("**Team<br>Targets**"),"Per_Target"=md("**Score<br>Per Target**")) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray3"),
      cell_text(color="white",size="18",align="center")),
    locations = cells_column_labels(c(1:4))) %>% tab_header(md("**Team Scores**"),subtitle = "2018 Regular Season") 
```
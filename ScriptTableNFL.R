#Had computer issues reading in the code, so was done on another computer and then the CSV's were dropped to me


Team <- read.csv("TeamScore.csv")
Defender<- read.csv("DefenderScore.csv")
Positions <- read.csv ("C:/Users/hound/OneDrive/Desktop/R/BDB/players.csv")
TeamPlay<- read.csv ("Defender.csv")



Arman <- read.csv("Armandata.csv")

Arman <- Arman %>% mutate(
  passResult = ifelse(passResult=="C",1,0)
)

Arman2<- aggregate(Arman$passResult, by = list(Arman$displayName), FUN= sum)



library(dplyr)

POS <- Positions %>%
  select (displayName, position)


Total <- merge (POS, Defender, by.x='displayName', by.y='Defender')


Total$displayName <- unique(Total$displayName)

Total <- Total %>%
  distinct (displayName, .keep_all = TRUE)

player$displayName[duplicated(player$displayName)]



TeamPlay <- TeamPlay %>%
  select (Defender, player_team)


Total <- merge (Total, TeamPlay, by.x='displayName', by.y='Defender')


Total <- Total %>%
  left_join(teams_colors_logos, by = c('player_team' = 'team_abbr'))
Total<- merge(Total, Arman2, by.x='displayName', by.y='Group.1')
Total <- Total %>%
  mutate(Expected = Score+x)

Total$Expected <- round(Total$Expected, digits=3)
Total$Score <- round(Total$Score, digits=3)
Total$Per_Target<- round(Total$Per_Target, digits=3)



CB <- Total %>%
  filter(position== 'DB'| position== 'CB')
quantile(CB$Targets) #Top 50% 27%

CB <- CB %>%
  filter (Targets>26.99)

CB1<- head(arrange(CB, desc(Per_Target)),n=10)


URL1<- CB1$team_wordmark

#Table Work!
  
Table<-CB1 %>%
  select(displayName, team_wordmark, Targets, Score, Per_Target, x, Expected)%>%
  gt() %>%
  opt_all_caps()  %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  tab_header (
    title =md ("**CB's Expected Completion Rate**"), 
    subtitle = md ("*Top 10 with a minimum of 27 targets*")
  ) %>%
  cols_move_to_start(
    columns= vars(displayName, team_wordmark, Targets, Expected, x, Score, Per_Target)
  )%>%
  #Adds images into table
  text_transform(
    locations=cells_body(vars(team_wordmark)),
    fn=function(x) {
      web_image(
        url=URL1,
        height=px(28)
      )
    } 
  ) %>%
  cols_label(
    displayName= 'Name',
    team_wordmark= "Team",
    x= "Completions Allowed",
    Expected= 'xCompletions Allowed',
    Score= 'Total Score',
    Per_Target = 'Score Per Target'
  )  %>%
  tab_source_note(source_note = md ("**Big Data Bowl 2021**"))%>%
  
  cols_align(align="center")

Table %>%
  gtsave("CB2.png")


S <- Total %>%
  filter(position== 'SS'| position== 'FS'|position=='S')
quantile(S$Targets) #Top 50% 27%

S <- S %>%
  filter (Targets>17.99)

S1<- head(arrange(S, desc(Per_Target)),n=10)

URL1<- S1$team_wordmark


Table<-S1 %>%
  select(displayName, team_wordmark, Targets, Score, Per_Target, x, Expected)%>%
  gt() %>%
  opt_all_caps()  %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  tab_header (
    title =md ("**Safety Expected Completion Rate**"), 
    subtitle = md ("*Top 10 with a minimum of 18 targets*")
  ) %>%
  cols_move_to_start(
    columns= vars(displayName, team_wordmark, Targets, Expected, x, Score, Per_Target)
  )%>%
  #Adds images into table
  text_transform(
    locations=cells_body(vars(team_wordmark)),
    fn=function(x) {
      web_image(
        url=URL1,
        height=px(28)
      )
    } 
  ) %>%
  cols_label(
    displayName= 'Name',
    team_wordmark= "Team",
    x= "Completions Allowed",
    Expected= 'xCompletions Allowed',
    Score= 'Total Score',
    Per_Target = 'Score Per Target'
  )  %>%
  tab_source_note(source_note = md ("**Big Data Bowl 2021**"))%>%
  
  cols_align(align="center")

Table %>%
  gtsave("S2.png")

LB <- Total %>%
  filter(position== 'OLB'| position== 'MLB'|position=='LB')
quantile(LB$Targets) #Top 50% 16

LB <- LB %>%
  filter (Targets>15.99)

LB<- head(arrange(LB, desc(Per_Target)),n=10)

URL1<- LB$team_wordmark


Table<-LB %>%
  select(displayName, team_wordmark, Targets, Score, Per_Target, x, Expected)%>%
  gt() %>%
  opt_all_caps()  %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  tab_header (
    title =md ("**Linebacker Expected Completion Rate**"), 
    subtitle = md ("*Top 10 with a minimum of 16 targets*")
  ) %>%
  cols_move_to_start(
    columns= vars(displayName, team_wordmark, Targets, Expected, x, Score, Per_Target)
  )%>%
  #Adds images into table
  text_transform(
    locations=cells_body(vars(team_wordmark)),
    fn=function(x) {
      web_image(
        url=URL1,
        height=px(28)
      )
    } 
  ) %>%
  cols_label(
    displayName= 'Name',
    team_wordmark= "Team",
    x= "Completions Allowed",
    Expected= 'xCompletions Allowed',
    Score= 'Total Score',
    Per_Target = 'Score Per Target'
  )  %>%
  tab_source_note(source_note = md ("**Big Data Bowl 2021**"))%>%
  
  cols_align(align="center")

Table %>%
  gtsave("LB2.png")

quantile(Total$Targets)


Complete<- head(arrange(Total, desc(Score)),n=20)

URL1<- Complete$team_wordmark


Table<-Complete %>%
  select(displayName, team_wordmark, Targets, Score, Per_Target, x, Expected)%>%
  gt() %>%
  opt_all_caps()  %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  tab_header (
    title =md ("**Total Defense Expected Completion Rate**"), 
    subtitle = md ("*Top 20 Scores*")
  ) %>%
  cols_move_to_start(
    columns= vars(displayName, team_wordmark, Targets, Expected, x, Score, Per_Target)
  )%>%
  #Adds images into table
  text_transform(
    locations=cells_body(vars(team_wordmark)),
    fn=function(x) {
      web_image(
        url=URL1,
        height=px(28)
      )
    } 
  ) %>%
  cols_label(
    displayName= 'Name',
    team_wordmark= "Team",
    x= "CA",
    Expected= 'xCA',
    Score= 'xCA-CA',
    Per_Target = 'xCA-CA Per Target'
  )  %>%
  tab_source_note(source_note = md ("**Big Data Bowl 2021**"))%>%
  
  cols_align(align="center")

Table %>%
  gtsave("Total2.png")


Bottom<- head(arrange(Total, Per_Target),n=10)

URL1<- Bottom$team_wordmark


Table<-Bottom %>%
  select(displayName, team_wordmark, Targets, Score, Per_Target, x, Expected)%>%
  gt() %>%
  opt_all_caps()  %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  tab_header (
    title =md ("**Total Defense Expected Completion Rate**"), 
    subtitle = md ("*Bottom 10 Overall Scores*")
  ) %>%
  cols_move_to_start(
    columns= vars(displayName, team_wordmark, Targets, Expected, x, Score, Per_Target)
  )%>%
  #Adds images into table
  text_transform(
    locations=cells_body(vars(team_wordmark)),
    fn=function(x) {
      web_image(
        url=URL1,
        height=px(28)
      )
    } 
  ) %>%
  cols_label(
    displayName= 'Name',
    team_wordmark= "Team",
    x= "Completions Allowed",
    Expected= 'xCompletions Allowed',
    Score= 'Total Score',
    Per_Target = 'Score Per Target'
  )  %>%
  tab_source_note(source_note = md ("**Big Data Bowl 2021**"))%>%
  
  cols_align(align="center")

Table %>%
  gtsave("Bottom1.png")



Defense2018 <- read.csv("Defense2018.csv")
Defense2018 <- data_edit(Defense2018)

Defense2018 <- Defense2018 %>%
  rename(Team= 'ï..Teams' )
TeamScore <- read.csv("TeamScore.csv")

Defense2018$Team==TeamScore$player_team

TotalTeam <- merge(TeamScore, Defense2018, by.x='player_team', by.y='Team')

TotalTeam <- TotalTeam %>%
  select(player_team, Team.Targets, Team.Cumulative.Score, Per_Target, PYds.G, PYdsRank) %>%
  arrange(desc(Per_Target))

TotalTeam$Team.Cumulative.Score<- round(TotalTeam$Team.Cumulative.Score, digits=3)
TotalTeam$Per_Target<- round(TotalTeam$Per_Target, digits=3)

TotalTeam <- TotalTeam %>%
  left_join(teams_colors_logos, by = c('player_team' = 'team_abbr'))

TotalTeam2<- head(arrange(TotalTeam, desc(Team.Cumulative.Score)),n=10)

URL1<- TotalTeam2$team_wordmark



Table<-TotalTeam2 %>%
  select(team_wordmark, Team.Targets, Team.Cumulative.Score, Per_Target, PYds.G, PYdsRank)%>%
  gt() %>%
  opt_all_caps()  %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  tab_header (
    title =md ("**Team Defense Expected Completion Score Compared to Pass Yardage Ranks**"), 
    subtitle = md ("*Top 10 Scores*")
  ) %>%
  cols_move_to_start(
    columns= vars(team_wordmark, Team.Targets, Team.Cumulative.Score, Per_Target, PYds.G, PYdsRank)
  )%>%
  #Adds images into table
  text_transform(
    locations=cells_body(vars(team_wordmark)),
    fn=function(x) {
      web_image(
        url=URL1,
        height=px(28)
      )
    } 
  ) %>%
  cols_label(
    team_wordmark= "Team",
    Team.Targets= "Targets",
    Team.Cumulative.Score= 'Total Score',
    Per_Target = 'Score Per Target',
    PYds.G= "PYds/G",
    PYdsRank= "PYds/G 2018 Rank"
  )  %>%
  tab_source_note(source_note = md ("**Big Data Bowl 2021**"))%>%
  
  cols_align(align="center")
Table %>%
  gtsave('Team.png')

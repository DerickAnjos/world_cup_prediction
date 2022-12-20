# Installing and loading Packages ----------------------------------------------
packages <- c("tidyverse", "tibble", 'xlsx')


if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  instalador <- packages[!packages %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}


# Loading datasets -------------------------------------------------------------

# Datasets: All World Cup's matches / All World Cup's winners
# Source: "https://www.kaggle.com/datasets/evangower/fifa-world-cup?select=
# wcmatches.csv"
wc_matches_all <- read.csv(file = 'wc_data/wcmatches.csv', header = TRUE)
wc_winners <- read.csv(file = 'wc_data/WorldCups.csv', header = TRUE)

# Datasets: World cup - teams
# Source: FIFA
wc22_teams <- read.csv(file = 'wc_data/squads/wc_squads_2022.csv',
                       header = TRUE)
wc18_teams <- read.csv(file = 'wc_data/squads/wc_squads_2018.csv',
                       header = TRUE)
wc14_teams <- read.csv(file = 'wc_data/squads/wc_squads_2014.csv',
                       header = TRUE)
wc10_teams <- read.csv(file = 'wc_data/squads/wc_squads_2010.csv',
                       header = TRUE)
wc06_teams <- read.csv(file = 'wc_data/squads/wc_squads_2006.csv',
                       header = TRUE)

wc_teams <- list('wc_06' = wc06_teams, 'wc_10' = wc10_teams, 
                 'wc_14' = wc14_teams, 'wc_18' = wc18_teams,
                 'wc_22' = wc22_teams)

rm(wc22_teams, wc18_teams, wc14_teams, wc10_teams, wc06_teams)

# Datasets: World cup 2022 - matches
# Source: FIFA
wc22_matches <- read.csv2(file = 'wc_22/match_schedule.csv', 
                      header = TRUE)
wc_22_groups <- read.csv2(file = 'wc_22/wc22_team_group.csv', 
                          header = TRUE)

# Datasets: Fifa (game) players stats
# Source: https://sofifa.com
# Obs.: Webscraping realised in Python (Beautiful Soup)
player_stats_fifa_23 <- 
  read.csv(file = 'fifa_game_stats/web_scraping/fifa23.csv',
           header = TRUE)

player_stats_fifa_19 <- 
  read.csv(file = 'fifa_game_stats/web_scraping/fifa19.csv',
           header = TRUE)
player_stats_fifa_15 <- 
  read.csv(file = 'fifa_game_stats/web_scraping/fifa15.csv',
           header = TRUE)
player_stats_fifa_11 <- 
  read.csv(file = 'fifa_game_stats/web_scraping/fifa11.csv',
           header = TRUE)
player_stats_fifa_07 <- 
  read.csv(file = 'fifa_game_stats/web_scraping/fifa07.csv',
           header = TRUE)

player_stats <- list(player_stats_fifa_07, player_stats_fifa_11, 
                     player_stats_fifa_15, player_stats_fifa_19, 
                     player_stats_fifa_23)

rm(player_stats_fifa_07, player_stats_fifa_11, player_stats_fifa_15, 
   player_stats_fifa_19, player_stats_fifa_23)

# Datasets: Ballon D'or
# Source: FIFA
ballon_dor_all <- read.csv2(file = 'ballondor/ballondor_year_wc.csv', 
                            header = TRUE)

# Datasets: Fifa ranking
#Source: FIFA
fifa_ranking <- read.csv(file = 'fifa/fifa_ranking.csv', header = TRUE)

# Datasets: All match results - history
match_results <- read.csv(file = 'international_results/results.csv',
                          header = TRUE)


# Squad names -----------------------------------------------------------------

# Establishing a standard for the country's name 

wc_matches_all$home_team[wc_matches_all$home_team == "United States"] <- "USA"
wc_matches_all$away_team[wc_matches_all$away_team == "United States"] <- "USA"
wc_matches_all$home_team[wc_matches_all$home_team == "Iran"] <- "IR Iran"
wc_matches_all$away_team[wc_matches_all$away_team == "Iran"] <- "IR Iran"

# Datasets: World cup 2022 - teams
colnames(wc22_matches)[3] <- 'home_team'
colnames(wc22_matches)[4] <- 'away_team'

wc22_matches$home_team[wc22_matches$home_team == "Iran"] <- "IR Iran"
wc22_matches$away_team[wc22_matches$away_team == "Iran"] <- "IR Iran"

for (i in 1:length(player_stats)){
  
  colnames(wc_teams[[i]])[1] <- 'Nationality'
  wc_teams[[i]]$Nationality[wc_teams[[i]]$Nationality == "Iran"] <- "IR Iran"
  wc_teams[[i]]$Nationality[wc_teams[[i]]$Nationality == "United States"] <- 
    "USA"
  wc_teams[[i]]$Nationality[wc_teams[[i]]$Nationality == "Korea Republic"] <- 
    "South Korea"
  wc_teams[[i]]$Nationality[wc_teams[[i]]$Nationality == 
                              "Serbia and Montenegro"] <- "Serbia"

  
  player_stats[[i]]$Nationality[player_stats[[i]]$Nationality == 
                                  "United States"] <- "USA"
  player_stats[[i]]$Nationality[player_stats[[i]]$Nationality == 
                                  "Iran"] <- "IR Iran"
  player_stats[[i]]$Nationality[player_stats[[i]]$Nationality == 
                                  "Korea Republic"] <- "South Korea"
  player_stats[[i]]$Nationality[player_stats[[i]]$Nationality == 
                                  "Côte d'Ivoire"] <- "Ivory Coast"
  player_stats[[i]]$Nationality[player_stats[[i]]$Nationality == 
                                  "Korea DPR"] <- "North Korea"
}

fifa_ranking$country_full[fifa_ranking$country_full == "Korea Republic"] <- 
  "South Korea"

match_results$home_team[match_results$home_team == "United States"] <- "USA"
match_results$away_team[match_results$away_team == "United States"] <- "USA"
match_results$home_team[match_results$home_team == "Iran"] <- "IR Iran"
match_results$away_team[match_results$away_team == "Iran"] <- "IR Iran"


# Data cleaning ----------------------------------------------------------------

# Datasets: All World Cup's matches / All World Cup's winners
wc_matches_all <- subset(wc_matches_all, 
                     select = -c(country, city, outcome, win_conditions, date, 
                                 month, dayofweek, winning_team, losing_team))

wc_winners <- subset(wc_winners, 
                     select = -c(host, attendance))

# Datasets: Fifa (game) players stats
for (i in 1:length(player_stats)){
  player_stats[[i]] <- subset(player_stats[[i]], select = -c(ID, Value, Wage))
  player_stats[[i]] <- player_stats[[i]][!duplicated(player_stats[[i]]),]
}

# Datasets: Fifa ranking
# Using always the last FIFA ranking before World Cup
fifa_ranking <- 
  fifa_ranking[fifa_ranking$rank_date %in% 
                 c('2006-05-17', '2010-05-26', '2014-06-05', '2018-06-07', 
               '2022-10-06'),]

# Datasets: All match results - history
match_results <- subset(match_results, 
                        select = -c(city))


# Data transformation ----------------------------------------------------------

# Defining a vector with the World Cup's years of analysis  
wc_analysis <- as.numeric(c('2006', '2010', '2014', '2018', '2022'))
wc_test_group <- as.numeric(c('2006', '2010', '2014', '2018'))

wc22_group_phase <- wc22_matches[wc22_matches$phase == 'group matches',]
wc22_round_16 <- wc22_matches[wc22_matches$phase == 'round 16',]
wc22_quarter <- wc22_matches[wc22_matches$phase == 'quarter-finals',]
wc22_semi <- wc22_matches[wc22_matches$phase == 'semi-finals',]
wc22_third <- wc22_matches[wc22_matches$phase == 'third place',]
wc22_final <- wc22_matches[wc22_matches$phase == 'final',]

write.table(wc22_round_16, file='wc_22/wc22_round_16.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(wc22_quarter, file='wc_22/wc22_quarter.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(wc22_semi, file='wc_22/wc22_semi.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(wc22_third, file='wc_22/wc22_third.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(wc22_final, file='wc_22/wc22_final.csv', sep=';', dec=',', 
            row.names=FALSE)

wc22_group_phase <- left_join(wc22_group_phase, wc_22_groups, 
                              c('home_team'= 'Team'))

# Defining variable 'winner'
wc_matches_all$winner <-
  ifelse(wc_matches_all$home_score>wc_matches_all$away_score, 
         wc_matches_all$home_team, 
         ifelse(wc_matches_all$home_score<wc_matches_all$away_score,
                wc_matches_all$away_team, 'draw'))

# Defining the object 'wc_matches' and 'wc_matches_hist' (type list).
# 'wc_matches' has the World Cup matches in analysis in each element
# 'wc_matches_hist' has the same idea, but with previous matches as well
wc_matches <- list()
wc_matches_hist <- list()
for (i in 1:length(wc_analysis)){
  wc_matches[[i]] <- 
    subset(wc_matches_all[wc_matches_all$year == wc_analysis[i],])
  
  wc_matches_hist[[i]] <- 
    subset(wc_matches_all[wc_matches_all$year < wc_analysis[i],])
  
  if (i == 5){
    wc_matches[[i]] <- wc22_group_phase
  }
  
}

# Datasets: Fifa (game) stats
# Adjusting encoding and letter case
for (i in 1:length(wc_analysis)){
  
  colnames(player_stats[[i]])[2] <- 'Player'
  player_stats[[i]]$Player <- tolower(iconv(player_stats[[i]]$Player,
                                            to = "ASCII//TRANSLIT"))
  wc_teams[[i]]$Player <- tolower(iconv(wc_teams[[i]]$Player,
                                        to = "ASCII//TRANSLIT"))
  wc_teams[[i]]$Club <- tolower(iconv(wc_teams[[i]]$Club,
                                      to = "ASCII//TRANSLIT"))
}

# Datasets: Ballon D'or
# Adjusting encoding and letter case
ballon_dor_all$Player <- tolower(iconv(ballon_dor_all$Player,
                                       to = "ASCII//TRANSLIT"))

# Leaving only the top 10 players of Ballon D'or in each year of analysis
ballon_dor <- list()
for (i in 1:length(player_stats)){
  ballon_dor[[6-i]] <- ballon_dor_all[((i-1)*10 +1):(i*10),]
}
rm(ballon_dor_all)

# Fifa ranking
# Changing variable name
fifa_ranking$Nationality <- fifa_ranking$country_full
fifa_ranking$country_full <- NULL

# Squad power with players stats - preparing variables for the match
players_name_sep <- list() 
players_name <- list()

for (i in 1:length(wc_analysis)){
  
  players_name_sep[[i]] <- str_split(player_stats[[i]]$Player, pattern = ' ',
                                simplify = T)
  players_name[[i]] <- c(1:nrow(players_name_sep[[i]]))
  
  # Transforming variable 'Age' in numeric
  player_stats[[i]]$Age <- 
    as.numeric(gsub(pattern = "[][]", replacement = "", 
                    x = gsub("'", "", player_stats[[i]]$Age), perl = T))
  
  for(j in 1:nrow(players_name_sep[[i]])){
    
    for(k in 1:ncol(players_name_sep[[i]])){
      
      if (players_name_sep[[i]][j,ncol(players_name_sep[[i]]) + 1 - k] != "" && 
          players_name[[i]][j] == j){
        players_name[[i]][j] <- 
          players_name_sep[[i]][j,ncol(players_name_sep[[i]]) + 1 - k]
        
      }
      
    }
    
  }
  
}

# Datasets: All match results - history
# Transforming a complete date into just the Year
match_results$year <- as.numeric(substr(match_results$date, 1, 4))

# Defining variable 'winner'
match_results$winner <-
  ifelse(match_results$home_score>match_results$away_score, 
         match_results$home_team, 
         ifelse(match_results$home_score<match_results$away_score,
                match_results$away_team, 'draw'))

# Defining the historical analysis period (8 years - 2 cycles of World Cup)
match_history <- list()
for (i in 1:length(player_stats)){
  match_history[[i]] <- 
    subset(match_results[match_results$year %in% 
                           c((wc_analysis[i]-16):wc_analysis[i]),])
}

# Saving match history from World Cup 22 for using in the knockout stages
write.table(match_history[[5]], file='wc_22/match_history_wc_22.csv', sep=';', 
            dec=',', row.names=FALSE)

# Treating NAs values on dataset -----------------------------------------------

# Isolating Players not found for later manual search
overall_na <- list()
for (i in 1:length(wc_analysis)){
  
  wc_teams[[i]]$Overall <- NA
  
  for (j in 1:length(players_name[[i]])){
    
    index <- grep(players_name[[i]][j], wc_teams[[i]]$Player, 
                  ignore.case = TRUE)
    
    if(is.numeric(index) & !identical(index, integer(0))){
      
      for(k in 1:length(index)){
        
        if (player_stats[[i]]$Nationality[j] == 
            wc_teams[[i]]$Nationality[index[k]] & player_stats[[i]]$Age[j] %in% 
            c(wc_teams[[i]]$Age[index[k]] -1,wc_teams[[i]]$Age[index[k]],
              wc_teams[[i]]$Age[index[k]] + 1)){
          
          wc_teams[[i]]$Overall[index[k]] <- player_stats[[i]]$Overall[j]
          
        }
        
      }
      
    } 
    
  }
  
  overall_na[[i]] <- wc_teams[[i]][is.na(wc_teams[[i]]$Overall),]
  
}

# Saving the csv files with Players not found
write.table(overall_na[[1]], file='wc_team/overall_na_06.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(overall_na[[2]], file='wc_team/overall_na_10.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(overall_na[[3]], file='wc_team/overall_na_14.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(overall_na[[4]], file='wc_team/overall_na_18.csv', sep=';', dec=',', 
            row.names=FALSE)
write.table(overall_na[[5]], file='wc_team/overall_na_22.csv', sep=';', dec=',', 
            row.names=FALSE)

# Loading the Player's dataset after manual search
overall_na[[1]] <- 
  read.csv2(file = 'wc_team/manual_adjustment/overall_na_06.csv', header = TRUE)
overall_na[[2]] <- 
  read.csv2(file = 'wc_team/manual_adjustment/overall_na_10.csv', header = TRUE)
overall_na[[3]] <- 
  read.csv2(file = 'wc_team/manual_adjustment/overall_na_14.csv', header = TRUE)
overall_na[[4]] <- 
  read.csv2(file = 'wc_team/manual_adjustment/overall_na_18.csv', header = TRUE)
overall_na[[5]] <- 
  read.csv2(file = 'wc_team/manual_adjustment/overall_na_22.csv', header = TRUE)

# Treating NAs: mean of squad overall
for (i in 1:length(wc_analysis)){
  
  wc_teams[[i]][is.na(wc_teams[[i]]$Overall), "Overall"] <- 
    overall_na[[i]]$Overall
  
  for(j in 1:nrow(wc_teams[[i]])){
    
    if (is.na(wc_teams[[i]]$Overall[[j]])){
      
      wc_teams[[i]]$Overall[j] <- 
        mean(wc_teams[[i]]$Overall[wc_teams[[i]]$Nationality == 
                                  wc_teams[[i]]$Nationality[j]], na.rm = TRUE)
      
    }
    
  }
  
}

# Variables of interest - Fifa player stats, Ballon d'or, fifa ranking,
# performance in past matches and World Cups -----------------------------------

# Fifa player stats and a weight with Ballon d'or points
wc_teams_over_rank <- list()
for (i in 1:length(wc_analysis)){
  
  for (j in wc_teams[[i]]$Player[wc_teams[[i]]$Player %in% 
                                 ballon_dor[[i]]$Player]){
    
   wc_teams[[i]]$Overall[wc_teams[[i]]$Player == j] <- 
     wc_teams[[i]]$Overall[wc_teams[[i]]$Player == j] *
     (1 + ballon_dor[[i]]$Weight[ballon_dor[[i]]$Player == j])
   
  }
  
  wc_teams[[i]] %>% 
    group_by(Nationality) %>% 
    summarise('points' = mean(Overall)) -> wc_teams_over_rank[[i]]
  
}

# Fifa ranking
for (i in 1:length(unique(fifa_ranking$rank_date))){
  
  wc_teams_over_rank[[i]] <- left_join(wc_teams_over_rank[[i]], 
              fifa_ranking[fifa_ranking$rank_date == 
                             unique(fifa_ranking$rank_date)[i], 
                           c('Nationality', 'total_points')], 
              by = 'Nationality')
  colnames(wc_teams_over_rank[[i]]) <- c('Nationality', 'score', 
                                         'fifa_ranking')
  
}

# Joining the variables to the final dataset
for (i in 1:length(wc_analysis)){
  
  wc_matches[[i]] <- left_join(wc_matches[[i]], 
                               wc_teams_over_rank[[i]], 
                               by = c('home_team' ='Nationality'))
  
  names(wc_matches[[i]])[(ncol(wc_matches[[i]])-1):ncol(wc_matches[[i]])] <- 
    c('home_team_stats_score', 'home_team_ranking')
  
  wc_matches[[i]] <- left_join(wc_matches[[i]], 
                               wc_teams_over_rank[[i]], 
                               by = c('away_team' ='Nationality'))
  
  names(wc_matches[[i]])[(ncol(wc_matches[[i]])-1):ncol(wc_matches[[i]])] <- 
    c('away_team_stats_score', 'away_team_ranking')
  
}

# Performance in past matches and in World Cups
for (i in 1:length(wc_analysis)){
  
  for (j in 1:nrow(wc_matches[[i]])){
    
    # Performance in past matches
    home_team_victories <-
      wc_matches[[i]]$home_team[j] == (match_history[[i]]$winner[
        (wc_matches[[i]]$home_team[j] == match_history[[i]]$home_team | 
           wc_matches[[i]]$home_team[j] == match_history[[i]]$away_team) & 
          (wc_matches[[i]]$away_team[j] == match_history[[i]]$home_team | 
             wc_matches[[i]]$away_team[j] == match_history[[i]]$away_team)])
  
    wc_matches[[i]]$home_team_perform_hist[j] <-
      ifelse(length(home_team_victories) == 0, 0,
      sum(home_team_victories, na.rm = TRUE)/length(home_team_victories))
    
    away_team_victories <-
      wc_matches[[i]]$away_team[j] == (match_history[[i]]$winner[
        (wc_matches[[i]]$home_team[j] == match_history[[i]]$home_team | 
           wc_matches[[i]]$home_team[j] == match_history[[i]]$away_team) & 
          (wc_matches[[i]]$away_team[j] == match_history[[i]]$home_team | 
             wc_matches[[i]]$away_team[j] == match_history[[i]]$away_team)])
    
    wc_matches[[i]]$away_team_perform_hist[j] <-
      ifelse(length(away_team_victories) == 0, 0,
      sum(away_team_victories, na.rm = TRUE)/length(away_team_victories))
    
    # Performance in World Cups
    home_team_wc_perform <-
      wc_matches[[i]]$home_team[j] == (wc_matches_hist[[i]]$winner[
        (wc_matches[[i]]$home_team[j] == wc_matches_hist[[i]]$home_team | 
           wc_matches[[i]]$home_team[j] == wc_matches_hist[[i]]$away_team)])
    
    wc_matches[[i]]$home_team_wc_perform[j] <- 
      ifelse(length(home_team_wc_perform) == 0, 0, 
             sum(home_team_wc_perform, na.rm = TRUE)/
               length(home_team_wc_perform))
    
    away_team_wc_perform <-
      wc_matches[[i]]$away_team[j] == (wc_matches_hist[[i]]$winner[
        (wc_matches[[i]]$away_team[j] == wc_matches_hist[[i]]$home_team | 
           wc_matches[[i]]$away_team[j] == wc_matches_hist[[i]]$away_team)])
    
    wc_matches[[i]]$away_team_wc_perform[j] <- 
      ifelse(length(away_team_wc_perform) == 0, 0, 
             sum(away_team_wc_perform, na.rm = TRUE)/
               length(away_team_wc_perform))

  }
  
}

# Final dataset 
world_cup <- rbind(wc_matches[[1]], wc_matches[[2]], wc_matches[[3]], 
                   wc_matches[[4]])

world_cup_22 <- wc_matches[[5]]

for (i in 1:nrow(world_cup)){
  
  if (world_cup$winner[i] == world_cup$home_team[i]){
    
    world_cup$winner[i] <- 'home_team'
    
  } else if (world_cup$winner[i] == world_cup$away_team[i]) { 
    
    world_cup$winner[i] <- 'away_team'
    
  }
  
}

world_cup$winner <- as.factor(world_cup$winner)

write.table(world_cup, file='world_cup_data.csv', sep=';', dec=',', 
            row.names=FALSE)

write.table(world_cup_22, file='world_cup_22_data.csv', sep=';', dec=',', 
            row.names=FALSE)
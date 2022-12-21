# Installing and loading Packages ----------------------------------------------
packages <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl",
              "car", "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet",
              "magick", "cowplot")

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  installer <- packages[!packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}

# Reading the world cup dataset ------------------------------------------------
world_cup <- read.csv2(file = 'world_cup_data.csv', header = TRUE)
world_cup_22 <- read.csv2(file = 'world_cup_22_data.csv', header = TRUE)
match_history <- read.csv2(file = 'wc_22/match_history_wc_22.csv', 
                           header = TRUE)

world_cup$winner <- as.factor(world_cup$winner)

world_cup <- na.omit(world_cup)

# world_cup$stats_score <- scale(world_cup$home_team_stats_score/
#   world_cup$away_team_stats_score)
# world_cup$ranking_fifa <- scale(world_cup$home_team_ranking/
#   world_cup$away_team_ranking)
# world_cup$perform_hist <- scale(exp(world_cup$home_team_perform_hist)/
#   exp(world_cup$away_team_perform_hist))
# world_cup$wc_perform <- scale(exp(world_cup$home_team_wc_perform)/
#   exp(world_cup$away_team_wc_perform))

# Multinomial Logistic regression ----------------------------------------------

# Reference category
world_cup$winner <- relevel(world_cup$winner, ref = "draw")

# Train-test data
set.seed(123)
bool_train <- stats::runif(dim(world_cup)[1])>.20

wc_train <- world_cup[bool_train,]
wc_test  <- world_cup[!bool_train,]

# Model estimation - function 'multinom' from package 'nnet'
wc_model <- multinom(formula = winner ~
                       home_team_stats_score + away_team_stats_score +
                       home_team_ranking + away_team_ranking +
                       home_team_perform_hist + away_team_perform_hist +
                       home_team_wc_perform + away_team_wc_perform,
                     data = wc_train)

# wc_model <- multinom(formula = winner ~ 
#                        stats_score + ranking_fifa + perform_hist + 
#                        wc_perform, data = wc_train)

# Model parameters
summary(wc_model)

# LogLik
logLik(wc_model)

# Defining a Qui-square function
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

# Qui-square
Qui2(wc_model)

# Z-wald statistic  
zWald_wc_model <- (summary(wc_model)$coefficients / 
                     summary(wc_model)$standard.errors)

zWald_wc_model

# Betas p-values
round((pnorm(abs(zWald_wc_model), lower.tail = F) * 2), 4)

# Some variables are not statistically significant. Below, the stepwise method
# will be performed to remove these variables from the model

wc_model_step <- step(object = wc_model, 
                k = qchisq(p = .05, df = 1, lower.tail = F))


# Model effectiveness - Train Data ---------------------------------------------

# Adding results from the model to the train dataset 
wc_train$prediction <- predict(wc_model_step, newdata = wc_train, 
                                        type = "class")

wc_train %>%
  select(home_team, away_team, winner, prediction) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

attach(wc_train)
# Overall efficiency of 'wc_model'
oe_wc_model_train <- as.data.frame.matrix(table(prediction, winner))

oe_wc_model_train %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Confusion matrix (real values in column and predict values in row)
accuracy_train <- (round((sum(diag(table(winner, prediction))) / 
                      sum(table(winner, prediction))), 2))

accuracy_train


# Model effectiveness - Test Data ----------------------------------------------

# Adding results from the model to the train dataset 
wc_test$prediction <- predict(wc_model_step, newdata = wc_test, 
                               type = "class")

wc_test %>%
  select(home_team, away_team, winner, prediction) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

detach(wc_train)
attach(wc_test)

# Overall efficiency of 'wc_model'
oe_wc_model_test <- as.data.frame.matrix(table(prediction, winner))

oe_wc_model_test %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Confusion matrix (real values in column and predict values in row)
accuracy <- (round((sum(diag(table(winner, prediction))) / 
                            sum(table(winner, prediction))), 2))

accuracy


# World Cup 2022 Prediction ----------------------------------------------------

# Adding results from the model to the dataset 
world_cup_22$prediction <- predict(wc_model_step, newdata = world_cup_22, 
                              type = "class")

result_prob <- predict(wc_model_step, newdata = world_cup_22, 
                             type = "prob")

colnames(result_prob) <- c('draw_prob', 'away_prob', 'home_prob')

world_cup_22 <- cbind(world_cup_22, result_prob)

world_cup_22 %>%
  select(home_team, away_team, prediction) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

group <- list()

for(i in 1:length(unique(world_cup_22$Group))){

  group[[i]] <- data.frame(unique(subset(world_cup_22,
                              Group == unique(sort(world_cup_22$Group))[i] ,
                              select = home_team)))

  for (j in (group[[i]][,1])){
    
    group[[i]]$points[group[[i]][,1] == j] <- (sum(
      world_cup_22$prediction[world_cup_22$home_team == j] == 'home_team')*3 +
      sum(
        world_cup_22$prediction[world_cup_22$home_team == j] == 'draw'))
    
    group[[i]]$prob_criteria[group[[i]][,1] == j] <- (sum(
      world_cup_22$home_prob[world_cup_22$home_team == j & 
                               world_cup_22$prediction == 'home_team' ]) -
        sum(world_cup_22$away_prob[world_cup_22$home_team == j & 
                                   world_cup_22$prediction == 'away_team' ]) +
        sum(world_cup_22$away_prob[world_cup_22$away_team == j & 
                                   world_cup_22$prediction == 'away_team' ]) -
        sum(world_cup_22$home_prob[world_cup_22$away_team == j & 
                                   world_cup_22$prediction == 'home_team' ]))
    
    group[[i]] <- group[[i]] %>% 
      arrange(desc(points), desc(prob_criteria))
    
    group[[i]]$points[group[[i]][,1] == j] <- 
      group[[i]]$points[group[[i]][,1] == j] + 
      (sum(
      world_cup_22$prediction[world_cup_22$away_team == j] == 'away_team')*3 +
        sum(
          world_cup_22$prediction[world_cup_22$away_team == j] == 'draw'))
    
  }
  
  colnames(group[[i]]) <- c('team', 'points', 'prob_criteria')
  
}

# Classified - groups
groups_number <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')

classified <- data.frame()
aux <- data.frame('country' = NA, 'classification' = NA)

for(i in 1:length(unique(world_cup_22$Group))){
  
  aux$country <- group[[i]]$team[1]
  aux$classification <- paste0('1', as.character(groups_number[i]))
  
  classified <- rbind(classified, aux)
  
  aux$country <- group[[i]]$team[2]
  aux$classification <- paste0('2', as.character(groups_number[i]))
  
  classified <- rbind(classified, aux)
}

# Reading dataset phase
wc22_round_16 <- read.csv2(file = 'wc_22/wc22_round_16.csv', header = TRUE)
wc22_quarter <- read.csv2(file = 'wc_22/wc22_quarter.csv', header = TRUE)
wc22_semi <- read.csv2(file = 'wc_22/wc22_semi.csv', header = TRUE)
wc22_third <- read.csv2(file = 'wc_22/wc22_third.csv', header = TRUE)
wc22_final <- read.csv2(file = 'wc_22/wc22_final.csv', header = TRUE)


# Model efficiency on group phase
wc22_results <- read.csv2(file = 'wc_22/wc22_results.csv', header = TRUE)
wc22_results$home_score <- substring(wc22_results$Result, first = 1, 
                                          last = 1)
wc22_results$away_score <- substring(wc22_results$Result, first = 3, 
                                          last = 3)
wc22_results$Result <- NULL
wc22_results$winner <- 
  ifelse(wc22_results$home_score > wc22_results$away_score, 
         'home_team', 
         ifelse(wc22_results$home_score < wc22_results$away_score,
                'away_team', 'draw'))

# for (i in 1:nrow(world_cup_22)){
#   
#   
#   
# }

world_cup_22 <- 
  left_join(world_cup_22, wc22_results, by = c('home_team', 'away_team'))

detach(wc_test)
attach(world_cup_22)

# Overall efficiency of 'wc_model'
oe_wc_model <- as.data.frame.matrix(table(prediction, winner))

oe_wc_model %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Confusion matrix (real values in column and predict values in row)
accuracy <- (round((sum(diag(table(winner, prediction))) / 
                      sum(table(winner, prediction))), 2))

accuracy

detach(world_cup_22)
# Round 16 ---------------------------------------------------------------------
wc22_round_16$match <- as.character(wc22_round_16$match)

wc22_round_16 <- 
  left_join(wc22_round_16, classified, c("home_team" = "classification"))

wc22_round_16 <- 
  left_join(wc22_round_16, classified, c("away_team" = "classification"))

colnames(wc22_round_16) <- c('match', 'date', 'class1', 'class2', 'phase', 
                             'home_team', 'away_team')

wc22_round_16 <- 
  distinct(left_join(wc22_round_16, world_cup_22, 
                     c('home_team' = 'home_team'), suffix = c('', '.x')) %>% 
  select('match', 'home_team', 'away_team', 'home_team_stats_score',
         'home_team_ranking', 'home_team_wc_perform'))

wc22_round_16 <- 
  distinct(left_join(wc22_round_16, world_cup_22, 
                     c('away_team' = 'away_team'), suffix = c('', '.x')) %>% 
  select('match', 'home_team', 'away_team', 'home_team_stats_score',
         'home_team_ranking', 'home_team_wc_perform', 'away_team_stats_score',
         'away_team_ranking', 'away_team_wc_perform'))


# Performance in past matches and in World Cups
for (j in 1:nrow(wc22_round_16)){
    
    # Performance in past matches
    home_team_victories <-
      wc22_round_16$home_team[j] == (match_history$winner[
        (wc22_round_16$home_team[j] == match_history$home_team | 
           wc22_round_16$home_team[j] == match_history$away_team) & 
          (wc22_round_16$away_team[j] == match_history$home_team | 
             wc22_round_16$away_team[j] == match_history$away_team)])
    
    wc22_round_16$home_team_perform_hist[j] <-
      ifelse(length(home_team_victories) == 0, 0,
             sum(home_team_victories, na.rm = TRUE)/length(home_team_victories))
    
    away_team_victories <-
      wc22_round_16$away_team[j] == (match_history$winner[
        (wc22_round_16$home_team[j] == match_history$home_team | 
           wc22_round_16$home_team[j] == match_history$away_team) & 
          (wc22_round_16$away_team[j] == match_history$home_team | 
             wc22_round_16$away_team[j] == match_history$away_team)])
    
    wc22_round_16$away_team_perform_hist[j] <-
      ifelse(length(away_team_victories) == 0, 0,
             sum(away_team_victories, na.rm = TRUE)/length(away_team_victories))
    
}
  
# Adding results from the model to the dataset 
wc22_round_16$prediction <- predict(wc_model_step, newdata = wc22_round_16, 
                                   type = "class")

result_prob <- predict(wc_model_step, newdata = wc22_round_16, 
                       type = "prob")

colnames(result_prob) <- c('draw_prob', 'away_prob', 'home_prob')

wc22_round_16 <- cbind(wc22_round_16, result_prob)

wc22_round_16 %>%
  select(home_team, away_team, prediction) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Classified - round 16
classified_round_16 <- data.frame()
aux <- data.frame('country' = NA, 'match' = NA)

for(i in 1:nrow(wc22_round_16)){
  
  if (wc22_round_16$prediction[i] == 'draw'){
    
    wc22_round_16$prediction[i] <- 
      ifelse(wc22_round_16$home_prob[i] > wc22_round_16$away_prob[i], 
             'home_team', 'away_team')
    
  }
  
  aux$country <- wc22_round_16[i,as.character(wc22_round_16$prediction[i])]
  aux$match <- wc22_round_16$match[i]
  
  classified_round_16 <- rbind(classified_round_16, aux)
  
}


# Quarter ----------------------------------------------------------------------

wc22_quarter$home_team <- as.character(sub(pattern = "W", replacement = "", 
                              wc22_quarter$home_team))
wc22_quarter$away_team <- as.character(sub(pattern = "W", replacement = "", 
                              wc22_quarter$away_team))

wc22_quarter <- left_join(wc22_quarter, classified_round_16, 
                          c("home_team" = "match"))

wc22_quarter <- left_join(wc22_quarter, classified_round_16, 
                          c("away_team" = "match"))

wc22_quarter <- wc22_quarter %>% 
  select('match', 'date', 'country.x', 'country.y')

colnames(wc22_quarter) <- c('match', 'date', 'home_team', 'away_team')

aux_home <- data.frame()
aux_away <- data.frame()
aux <- data.frame()
aux2 <- data.frame()
aux3 <- data.frame()
aux4 <- data.frame()

for(i in 1:nrow(wc22_quarter)){
  
  # Home team
  if(wc22_quarter$home_team[i] %in% wc22_round_16$home_team){
    
    aux <- rbind(aux, (left_join(wc22_quarter, wc22_round_16, 
                              c("home_team" = 'home_team'), 
                              suffix = c('', '.x')) %>% 
      select('match', 'home_team', 'away_team', 'home_team_stats_score',
             'home_team_ranking', 'home_team_wc_perform'))[i,])
  } else {
    aux2 <- (left_join(wc22_quarter, wc22_round_16, 
                                 c("home_team" =  'away_team'), 
                                 suffix = c('', '.x')) %>%
      select('match', 'home_team', 'away_team', 'away_team_stats_score',
             'away_team_ranking', 'away_team_wc_perform'))[i,]
    
    colnames(aux2) <- 
      c('match', 'home_team', 'away_team', 'home_team_stats_score',
        'home_team_ranking', 'home_team_wc_perform')
  
  }
  aux_home <- distinct(rbind(aux_home, aux, aux2))
  
  # Away team
  if(wc22_quarter$away_team[i] %in% wc22_round_16$home_team){

    aux3 <- (left_join(wc22_quarter, wc22_round_16,
                                 c("away_team" =  'home_team'),
                                 suffix = c('', '.x')) %>%
      select('match', 'home_team', 'away_team', 'home_team_stats_score',
             'home_team_ranking', 'home_team_wc_perform'))[i,]

    colnames(aux3) <-
      c('match', 'home_team', 'away_team', 'away_team_stats_score',
        'away_team_ranking', 'away_team_wc_perform')

  } else {
    aux4 <- (left_join(wc22_quarter, wc22_round_16,
                       c("away_team" =  'away_team'),
                       suffix = c('', '.x')) %>%
      select('match', 'home_team', 'away_team', 'away_team_stats_score',
             'away_team_ranking', 'away_team_wc_perform'))[i,]

  }
  aux_away <- distinct(rbind(aux_away, aux3, aux4))
  
}

wc22_quarter <- cbind(aux_home, aux_away) %>% 
  select('match', 'home_team', 'away_team', 'home_team_stats_score',
         'home_team_ranking', 'home_team_wc_perform', 'away_team_stats_score',
         'away_team_ranking', 'away_team_wc_perform')

# Performance in past matches and in World Cups
for (j in 1:nrow(wc22_quarter)){
  
  # Performance in past matches
  home_team_victories <-
    wc22_quarter$home_team[j] == (match_history$winner[
      (wc22_quarter$home_team[j] == match_history$home_team | 
         wc22_quarter$home_team[j] == match_history$away_team) & 
        (wc22_quarter$away_team[j] == match_history$home_team | 
           wc22_quarter$away_team[j] == match_history$away_team)])
  
  wc22_quarter$home_team_perform_hist[j] <-
    ifelse(length(home_team_victories) == 0, 0,
           sum(home_team_victories, na.rm = TRUE)/length(home_team_victories))
  
  away_team_victories <-
    wc22_quarter$away_team[j] == (match_history$winner[
      (wc22_quarter$home_team[j] == match_history$home_team | 
         wc22_quarter$home_team[j] == match_history$away_team) & 
        (wc22_quarter$away_team[j] == match_history$home_team | 
           wc22_quarter$away_team[j] == match_history$away_team)])
  
  wc22_quarter$away_team_perform_hist[j] <-
    ifelse(length(away_team_victories) == 0, 0,
           sum(away_team_victories, na.rm = TRUE)/length(away_team_victories))
  
}

# Adding results from the model to the dataset 
wc22_quarter$prediction <- predict(wc_model_step, newdata = wc22_quarter, 
                                    type = "class")

result_prob <- predict(wc_model_step, newdata = wc22_quarter, 
                       type = "prob")

colnames(result_prob) <- c('draw_prob', 'away_prob', 'home_prob')

wc22_quarter <- cbind(wc22_quarter, result_prob)

wc22_quarter %>%
  select(home_team, away_team, prediction) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Classified - quarter
classified_quarter <- data.frame()
aux <- data.frame('country' = NA, 'match' = NA)

for(i in 1:nrow(wc22_quarter)){
  
  if (wc22_quarter$prediction[i] == 'draw'){
    
    wc22_quarter$prediction[i] <- 
      ifelse(wc22_quarter$home_prob[i] > wc22_quarter$away_prob[i], 
             'home_team', 'away_team')
    
  }
  
  aux$country <- wc22_quarter[i,as.character(wc22_quarter$prediction[i])]
  aux$match <- wc22_quarter$match[i]
  
  classified_quarter <- rbind(classified_quarter, aux)
  
}

classified_quarter$match <- as.character(classified_quarter$match)


# Semi -------------------------------------------------------------------------

wc22_semi$home_team <- as.character(sub(pattern = "W", replacement = "", 
                                        wc22_semi$home_team))
wc22_semi$away_team <- as.character(sub(pattern = "W", replacement = "", 
                                        wc22_semi$away_team))

wc22_semi <- left_join(wc22_semi, classified_quarter, 
                          c("home_team" = "match"))

wc22_semi <- left_join(wc22_semi, classified_quarter, 
                          c("away_team" = "match"))

wc22_semi <- wc22_semi %>% 
  select('match', 'date', 'country.x', 'country.y')

colnames(wc22_semi) <- c('match', 'date', 'home_team', 'away_team')

aux_home <- data.frame()
aux_away <- data.frame()
aux <- data.frame()
aux2 <- data.frame()
aux3 <- data.frame()
aux4 <- data.frame()

for(i in 1:nrow(wc22_semi)){
  
  # Home team
  if(wc22_semi$home_team[i] %in% wc22_quarter$home_team){
    
    aux <- rbind(aux, (left_join(wc22_semi, wc22_quarter, 
                                 c("home_team" = 'home_team'), 
                                 suffix = c('', '.x')) %>% 
      select('match', 'home_team', 'away_team', 'home_team_stats_score',
             'home_team_ranking', 'home_team_wc_perform'))[i,])
  } else {
    aux2 <- (left_join(wc22_semi, wc22_quarter, 
                       c("home_team" =  'away_team'), 
                       suffix = c('', '.x')) %>%
      select('match', 'home_team', 'away_team', 'away_team_stats_score',
                      'away_team_ranking', 'away_team_wc_perform'))[i,]
    
    colnames(aux2) <- 
      c('match', 'home_team', 'away_team', 'home_team_stats_score',
        'home_team_ranking', 'home_team_wc_perform')
    
  }
  aux_home <- distinct(rbind(aux_home, aux, aux2))
  
  # Away team
  if(wc22_semi$away_team[i] %in% wc22_quarter$home_team){
    
    aux3 <- (left_join(wc22_semi, wc22_quarter,
                       c("away_team" =  'home_team'),
                       suffix = c('', '.x')) %>%
        select('match', 'home_team', 'away_team', 'home_team_stats_score',
                      'home_team_ranking', 'home_team_wc_perform'))[i,]
    
    colnames(aux3) <-
      c('match', 'home_team', 'away_team', 'away_team_stats_score',
        'away_team_ranking', 'away_team_wc_perform')
    
  } else {
    aux4 <- (left_join(wc22_semi, wc22_quarter,
                       c("away_team" =  'away_team'),
                       suffix = c('', '.x')) %>%
        select('match', 'home_team', 'away_team', 'away_team_stats_score',
                      'away_team_ranking', 'away_team_wc_perform'))[i,]
    
  }
  
  aux_away <- distinct(rbind(aux_away, aux3, aux4))
  
}

wc22_semi <- cbind(aux_home, aux_away) %>% 
  select('match', 'home_team', 'away_team', 'home_team_stats_score',
         'home_team_ranking', 'home_team_wc_perform', 'away_team_stats_score',
         'away_team_ranking', 'away_team_wc_perform')

# Performance in past matches and in World Cups
for (j in 1:nrow(wc22_semi)){
  
  # Performance in past matches
  home_team_victories <-
    wc22_semi$home_team[j] == (match_history$winner[
      (wc22_semi$home_team[j] == match_history$home_team | 
         wc22_semi$home_team[j] == match_history$away_team) & 
        (wc22_semi$away_team[j] == match_history$home_team | 
           wc22_semi$away_team[j] == match_history$away_team)])
  
  wc22_semi$home_team_perform_hist[j] <-
    ifelse(length(home_team_victories) == 0, 0,
           sum(home_team_victories, na.rm = TRUE)/length(home_team_victories))
  
  away_team_victories <-
    wc22_semi$away_team[j] == (match_history$winner[
      (wc22_semi$home_team[j] == match_history$home_team | 
         wc22_semi$home_team[j] == match_history$away_team) & 
        (wc22_semi$away_team[j] == match_history$home_team | 
           wc22_semi$away_team[j] == match_history$away_team)])
  
  wc22_semi$away_team_perform_hist[j] <-
    ifelse(length(away_team_victories) == 0, 0,
           sum(away_team_victories, na.rm = TRUE)/length(away_team_victories))
  
}

# Adding results from the model to the dataset 
wc22_semi$prediction <- predict(wc_model_step, newdata = wc22_semi, 
                                   type = "class")

result_prob <- predict(wc_model_step, newdata = wc22_semi, 
                       type = "prob")

colnames(result_prob) <- c('draw_prob', 'away_prob', 'home_prob')

wc22_semi <- cbind(wc22_semi, result_prob)

wc22_semi %>%
  select(home_team, away_team, prediction) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Classified - quarter
classified_semi <- data.frame()
aux <- data.frame('country' = NA, 'match' = NA)

for(i in 1:nrow(wc22_semi)){
  
  if (wc22_semi$prediction[i] == 'draw'){
    
    wc22_semi$prediction[i] <- 
      ifelse(wc22_semi$home_prob[i] > wc22_semi$away_prob[i], 
             'home_team', 'away_team')
    
  }
  
  aux$country <- wc22_semi[i,as.character(wc22_semi$prediction[i])]
  aux$match <- wc22_semi$match[i]
  
  classified_semi <- rbind(classified_semi, aux)
  
}

classified_semi$match <- as.character(classified_semi$match)


# Final ------------------------------------------------------------------------

wc22_final$home_team <- as.character(sub(pattern = "W", replacement = "", 
                                         wc22_final$home_team))
wc22_final$away_team <- as.character(sub(pattern = "W", replacement = "", 
                                         wc22_final$away_team))

wc22_final <- left_join(wc22_final, classified_semi, 
                       c("home_team" = "match"))

wc22_final <- left_join(wc22_final, classified_semi, 
                       c("away_team" = "match"))

wc22_final <- wc22_final %>% 
  select('match', 'date', 'country.x', 'country.y')

colnames(wc22_final) <- c('match', 'date', 'home_team', 'away_team')

aux_home <- data.frame()
aux_away <- data.frame()
aux <- data.frame()
aux2 <- data.frame()
aux3 <- data.frame()
aux4 <- data.frame()

for(i in 1:nrow(wc22_final)){
  
  # Home team
  if(wc22_final$home_team[i] %in% wc22_semi$home_team){
    
    aux <- rbind(aux, (left_join(wc22_final, wc22_semi, 
                                 c("home_team" = 'home_team'), 
                                 suffix = c('', '.x')) %>% 
        select('match', 'home_team', 'away_team', 'home_team_stats_score',
               'home_team_ranking', 'home_team_wc_perform'))[i,])
  } else {
    aux2 <- (left_join(wc22_final, wc22_semi, 
                       c("home_team" =  'away_team'), 
                       suffix = c('', '.x')) %>%
        select('match', 'home_team', 'away_team', 'away_team_stats_score',
                      'away_team_ranking', 'away_team_wc_perform'))[i,]
    
    colnames(aux2) <- 
      c('match', 'home_team', 'away_team', 'home_team_stats_score',
        'home_team_ranking', 'home_team_wc_perform')
    
  }
  aux_home <- distinct(rbind(aux_home, aux, aux2))
  
  # Away team
  if(wc22_final$away_team[i] %in% wc22_semi$home_team){
    
    aux3 <- (left_join(wc22_final, wc22_semi,
                       c("away_team" =  'home_team'),
                       suffix = c('', '.x')) %>%
        select('match', 'home_team', 'away_team', 'home_team_stats_score',
                      'home_team_ranking', 'home_team_wc_perform'))[i,]
    
    colnames(aux3) <-
      c('match', 'home_team', 'away_team', 'away_team_stats_score',
        'away_team_ranking', 'away_team_wc_perform')
    
  } else {
    aux4 <- (left_join(wc22_final, wc22_semi,
                       c("away_team" =  'away_team'),
                       suffix = c('', '.x')) %>%
        select('match', 'home_team', 'away_team', 'away_team_stats_score',
                      'away_team_ranking', 'away_team_wc_perform'))[i,]
    
  }
  
  aux_away <- distinct(rbind(aux_away, aux3, aux4))
  
}

wc22_final <- cbind(aux_home, aux_away) %>% 
  select('match', 'home_team', 'away_team', 'home_team_stats_score',
         'home_team_ranking', 'home_team_wc_perform', 'away_team_stats_score',
         'away_team_ranking', 'away_team_wc_perform')

# Performance in past matches and in World Cups
for (j in 1:nrow(wc22_final)){
  
  # Performance in past matches
  home_team_victories <-
    wc22_final$home_team[j] == (match_history$winner[
      (wc22_final$home_team[j] == match_history$home_team | 
         wc22_final$home_team[j] == match_history$away_team) & 
        (wc22_final$away_team[j] == match_history$home_team | 
           wc22_final$away_team[j] == match_history$away_team)])
  
  wc22_final$home_team_perform_hist[j] <-
    ifelse(length(home_team_victories) == 0, 0,
           sum(home_team_victories, na.rm = TRUE)/length(home_team_victories))
  
  away_team_victories <-
    wc22_final$away_team[j] == (match_history$winner[
      (wc22_final$home_team[j] == match_history$home_team | 
         wc22_final$home_team[j] == match_history$away_team) & 
        (wc22_final$away_team[j] == match_history$home_team | 
           wc22_final$away_team[j] == match_history$away_team)])
  
  wc22_final$away_team_perform_hist[j] <-
    ifelse(length(away_team_victories) == 0, 0,
           sum(away_team_victories, na.rm = TRUE)/length(away_team_victories))
  
}

# Adding results from the model to the dataset 
wc22_final$prediction <- predict(wc_model_step, newdata = wc22_final, 
                                type = "class")

result_prob <- predict(wc_model_step, newdata = wc22_final, 
                       type = "prob")

names(result_prob) <- c('draw_prob', 'away_prob', 'home_prob')

wc22_final <- cbind(wc22_final, result_prob)

wc22_final %>%
  select(home_team, away_team, prediction) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Classified - quarter
champions <- data.frame()
aux <- data.frame('country' = NA, 'match' = NA)

for(i in 1:nrow(wc22_final)){
  
  if (wc22_final$prediction[i] == 'draw'){
    
    wc22_final$prediction[i] <- 
      ifelse(wc22_final$home_prob[i] > wc22_final$away_prob[i], 
             'home_team', 'away_team')
    
  }
  
  aux$country <- wc22_final[i,as.character(wc22_final$prediction[i])]
  aux$match <- wc22_final$match[i]
  
  champions <- rbind(champions, aux)
  
}

champions$match <- as.character(champions$match)



detach(wc_test)
attach(world_cup_22)

# Overall efficiency of 'wc_model'
oe_wc_model_test <- as.data.frame.matrix(table(prediction, winner))

oe_wc_model_test %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Confusion matrix (real values in column and predict values in row)
accuracy <- (round((sum(diag(table(winner, prediction))) / 
                      sum(table(winner, prediction))), 2))

accuracy
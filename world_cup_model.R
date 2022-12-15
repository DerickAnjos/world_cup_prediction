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
world_cup$winner <- as.factor(world_cup$winner)

world_cup <- na.omit(world_cup)


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
# March Madness Odds breakdown
library(margins)
library(dplyr)

# PROJECT DATA DICTIONARY
# Proj_Win - Projected Winner, based on seeds of teams
# Decision - The winner of the game, as decided by the logit odds, measured in binary. Denoted as 1 if favored team wins
# Winner - Stores the name of the decided team
# Choice - Team chosen by choice algorithm, determined by upset threshold

# Set NaN to 0
Nan0 <- function(x){
  if(is.nan(x) == TRUE){
    x <- 0
  } else {
    x <- x
  }
}

# Import Bracket from 2023-2024
# Definitely add at least one more later
MM_df <- read.csv("C:\\Users\\duranjo\\Downloads\\MM_Data (1).csv")
MM_df <- MM_df[MM_df$Proj_Win != 'NA', ]
# MM_df <- read.csv("C:\\Users\\willd\\Downloads\\MM_Data.csv")
# MM_df <- MM_df[MM_df$Proj_Win != 'NA', ]

# MM_df <- MM_df[1:123,]

# Variables of interest are seed differential, winner, and interaction between round and seed differential
# Logit model predicting probability of higher-seed team winning
logit <- glm(Expected ~ Seed_Diff + Round + Seed_Diff*Round, family = binomial(link = 'logit'), data = MM_df)
summary(logit)
# Results
results <- margins(logit)
summary(results)

# Import table for bracket prediction
bracket <- read.csv("C:\\Users\\duranjo\\Downloads\\MM_Pred.csv")
# bracket <- read.csv("C:\\Users\\willd\\Downloads\\MM_Pred.csv")
bracket[is.na(bracket)] <- NA




# ------------------------------------------------------------ #
# ------------------------ SIMULATION ------------------------ #
# ------------------------------------------------------------ #

# -- Select a winner for each round 1 game based on the calculated odds -- #
# An example calculation for higher seed winning: Decision = B0 + Seed_Diff*B1 + Round*B2 + Seed_Diff*B3
# We assume that the higher seed is always picked, which will generally maximize odds of completing a perfect bracket
# This simulation is used to create random odds to determine a winner.

# Calculate probabilities of favored team winning
# probs <- predict(logit, newdata = bracket, type = "response")
# # Loop over each round 1 game, each row denoted as game
#   for(game in seq(1:nrow(bracket))){
#     # If round of next game isn't 1, stop
#     if(bracket$Round[game] > 1){
#       break
#     } else {
#       # Odds composed of calculated win chance for favored team
#       odds <- c(probs[game], 1-probs[game])
#       bracket$Decision[game] <- sample(c(1,0), size = 1, prob = odds)
#     }
#   }
# 
# print(paste0('32 total round 1 games were predicted. By using the axiom of selecting only the favored team, we missed on ',
#              nrow(bracket[bracket$Decision == 0,]), ' games.'))


### Create function to iteratively predict each game until the finals ###
# Calculate probabilities of favored team winning
probs <- predict(logit, newdata = bracket, type = "response")
probs <- na.omit(probs)
# Minor data cleaning
bracket$Seed_1 <- bracket$Seed_1 %>% as.numeric()
bracket$Seed_2 <- bracket$Seed_2 %>% as.numeric()

# Initializing additional columns
bracket$Choice <- NA
bracket$Odds <- NaN
bracket$Winner <- NA
bracket$Outcome <- NaN
# Initializing Upset specifically to reference if team ever upset another, useful for simulating 'real' outcome
bracket$Upset <- NA

# Composing a make-shift dictionary, very lazily
name <- c(bracket$Team_1[1:32], bracket$Team_2[1:32])
seed <- c(bracket$Seed_1[1:32], bracket$Seed_2[1:32])
seed_dict <- data.frame(name, seed)

# Loop over each game, each row denoted as i
for(i in seq(1:nrow(bracket))){
  # STEP 1: SIMULATE OUTCOME
    # Odds composed of calculated win chance for favored team
    # Provision for equal seeds
    if(bracket$Seed_1[i] == bracket$Seed_2[i]){
      odds <- c(0.5, 0.5)
      bracket$Decision[i] <- sample(c(1,0), size = 1, prob = odds) 
    } else {
      odds <- c(probs[i], 1-probs[i])
      bracket$Decision[i] <- sample(c(1,0), size = 1, prob = odds)      
    }
    # Select projected winner
    if(bracket$Seed_1[i] < bracket$Seed_2[i]){
      bracket$Proj_Win[i] <- bracket$Team_1[i]
    } else if(bracket$Seed_2[i] < bracket$Seed_1[i]) {
      bracket$Proj_Win[i] <- bracket$Team_2[i]
    } else {
      bracket$Proj_Win[i] <- 'Neither'
    }
  
    # Assigning odds column for each team
    bracket$Odds[i] <- as.numeric(probs[i])
    
    # Establish winning team, accounting for equal seeds
    if(bracket$Seed_1[i] == bracket$Seed_2[i]){
      if(bracket$Decision[i] == 1){
        bracket$Winner[i] <- bracket$Team_1[i]
      } else {
        bracket$Winner[i] <- bracket$Team_2[i]
      }
    } else {
      # Establish winning team based on sample results
      teams <- c(bracket$Team_1[i], bracket$Team_2[i])
      if(bracket$Decision[i] == 1){
        bracket$Winner[i] <- bracket$Proj_Win[i]
      } else {
        # Convoluted way of selecting the team that's NOT the projected winner
        teams <- teams[!(teams %in% bracket$Proj_Win[i])]
        bracket$Winner[i] <- teams[1]
      }}

    
    # Set expected to 1 if proj_win = winner
    if(bracket$Proj_Win[i] == bracket$Winner[i]){
      bracket$Expected[i] <- 1
    } else {
      bracket$Expected[i] <- 0
    }
  
  # STEP 2: DETERMINE 'REAL' OUTCOME OF GAME
    # Catalog if team has ever upset another team previously in the bracket
    if(bracket$Decision[i] == 0){
      bracket$Upset[i] <- bracket$Winner[i]
    } else {
      bracket$Upset[i] <- NaN
    }
    
    # Logic here is that if the game includes ANY TEAM who won a game that was not predicted to win, or if we made the wrong choice, outcome = 0
    if(bracket$Decision[i] == 0){
      bracket$Outcome[i] <- 0
    } else if(bracket$Team_1[i] %in% bracket$Upset| bracket$Team_2[i] %in% bracket$Upset) {
      bracket$Outcome[i] <- 0
    } else {
      bracket$Outcome[i] <- 1
    }

    
  # STEP 3: BREAK AFTER LAST OBS
    if(i == nrow(bracket)){
      break
    }
    
  # STEP 4: INITIALIZE NEXT ROUND
    #-- Following occurs only at the end of a round of games --#
    # Mechanism for structuring the next round of the bracket, after completing final game of current round
    # If there's a change in the next round
    if(bracket$Round[i] != bracket$Round[i+1]){
      # Initialize round info
      prev_round <- bracket$Round[i] - 1
      curr_round <- bracket$Round[i]
      next_round <- bracket$Round[i + 1]
      # Set n equal to number of games in next round
      n <- nrow(bracket[bracket$Round == next_round,])
      # Initialize c for bracket winner selection calculator, c2 for comparing against n
      c <- 1
      c2 <- 0
        for(j in 0:(n-1)){
          # Break for loop once new round is full
          if(c2 == n){ break }
          # Establishing bracket reorganization dictionary
          iter_vec <- c(32, 16, 8, 4, 2)
          # Assigning teams to the next round of the bracket
          bracket$Team_1[i+c] <- bracket$Winner[max(0, Nan0(prev_round/prev_round))*sum(iter_vec[1:prev_round]) + 1 + 2*j]
          bracket$Team_2[i+c] <- bracket$Winner[max(0, Nan0(prev_round/prev_round))*sum(iter_vec[1:prev_round]) + 2 + 2*j]
          c = c + 1
          c2 = c2 + 1 
        } 
      # Mechanism to grab seeds for each team, grab from dictionary df. For loop, because n is small
      bracket[bracket == ""] <- NA
      for(num in 1:nrow(bracket[is.na(bracket$Team_1) == FALSE , ])){
        bracket$Seed_1[num] <- seed_dict[seed_dict$name == bracket$Team_1[num], 'seed']
        }
      for(num in 1:nrow(bracket[is.na(bracket$Team_2) == FALSE , ])){
        bracket$Seed_2[num] <- seed_dict[seed_dict$name == bracket$Team_2[num], 'seed']
        }
      bracket$Seed_Diff <- abs(bracket$Seed_1 - bracket$Seed_2)
      
      # Simulate new probs for next round
      temp_df <- bracket[bracket$Round == next_round,]
      temp_probs <- predict(logit, newdata = temp_df, type = "response")
      probs <- c(probs, temp_probs)
      } # New Round finished initializing  
}

print(paste0('Across the entire bracket, there were ', nrow(bracket[bracket$Decision == 0, ]), ' upsets when selecting the most favored team each game.'))

# Calculate exact odds of previous March Madness winners of winning that exact game
# Do this by multiplying our expected odds for each favored winner across the entire bracket
print(paste0('The odds of getting a perfect march madness bracket, according to this estimation, is ', prod(bracket$Odds)*100, 
             '%, or about 1 in ', 1/prod(bracket$Odds))) 

# How many brackets need to be completed so that we'd expect someone produces a perfect bracket
inv_odds <- 1 - bracket$Odds
print(paste0("We expect ", log(0.01)/log(1-prod(bracket$Odds)) , " brackets to be completed for there to be a 99% chance of at least one person getting",' ',
                                    'a perfect bracket.'))

# Define a function that iteratively runs the loop, retrieving the summary statistics and repeating them t times
# STEPS
  # 1) Create accuracy metric of the bracket. Essentially, how many games were correctly predicted by the decision axiom?
      # - Need to adjust how the bracket is defining a correct 'decision'. In addition to the normal decision prediction system we have now,
      # there needs to be a 'real outcome' variable. Basically, If we predict the incorrect winner in round 1 game 1, round 2 game 1 should be counted
      # as a fail, NO MATTER WHAT
  # 2) Run loop iteratively for specified n, aggregate the accuracy and create a confidence interval.
  # 3) Run this process for varying thresholds of allowed upsets




# -------------------------------------------------------------- #
# ------------------------ MONTE CARLO ------------------------- #
# -------------------------------------------------------------- #

# Define upset thresholds
# Thresholds define how many upsets we're allowed to predict. Currently, decision is made on highest seed.
# Upset threshold will allow bracket to predict upsets more often
upset_vector <- c(1:20)
u_vec <- c()
mean_vec <- c()
upperCI <- c()
lowerCI <- c()
output_df <- data.frame(u_vec, mean_vec, lowerCI, upperCI)

# Monte Carlo simulation to create accuracy/performance interval
MCB <- function(repetitions = 5){
    # Initialize accuracy estimates vector
    accs <- c()
    
    # Repeat bracket simulation for number of reps specified
    for(rep in 1:repetitions){
      # Initialize upset counter
      u_count <- 0
      # Loop over each game, each row denoted as i
      for(i in seq(1:nrow(bracket))){
        # STEP 1: SIMULATE OUTCOME
        # Projected Winner - Favored team, by seed
        if(bracket$Seed_1[i] < bracket$Seed_2[i]){
          bracket$Proj_Win[i] <- bracket$Team_1[i]
        } else if(bracket$Seed_2[i] < bracket$Seed_1[i]) {
          bracket$Proj_Win[i] <- bracket$Team_2[i]
        } else {
          bracket$Proj_Win[i] <- 'Neither'
        }
        
        # Odds composed of calculated win chance for favored team
        # Decision - The actual simulated outcome of the game
        if(bracket$Seed_1[i] == bracket$Seed_2[i]){
          odds <- c(0.5, 0.5)
          bracket$Decision[i] <- sample(c(1,0), size = 1, prob = odds) 
        } else {
          odds <- c(probs[i], 1-probs[i])
          bracket$Decision[i] <- sample(c(1,0), size = 1, prob = odds)      
        }

        # Assigning odds column for each team
        bracket$Odds[i] <- as.numeric(probs[i])
        
        
        # Establish winning team, accounting for equal seeds
          if(bracket$Seed_1[i] == bracket$Seed_2[i]){
            if(bracket$Decision[i] == 1){
              bracket$Winner[i] <- bracket$Team_1[i]
            } else {
              bracket$Winner[i] <- bracket$Team_2[i]
            }
          } else {
            # Establish winning team based on sample results
            teams <- c(bracket$Team_1[i], bracket$Team_2[i])
            if(bracket$Decision[i] == 1){
              bracket$Winner[i] <- bracket$Proj_Win[i]
            } else {
              # Convoluted way of selecting the team that's NOT the projected winner
              teams <- teams[!(teams %in% bracket$Proj_Win[i])]
              bracket$Winner[i] <- teams[1]
            }}
        
        # Set expected to 1 if proj_win = winner
        if(bracket$Proj_Win[i] == bracket$Winner[i]){
          bracket$Expected[i] <- 1
        } else {
          bracket$Expected[i] <- 0
        }
        
        # STEP 2: DETERMINE 'REAL' OUTCOME OF GAME
        # Catalog if team has ever upset another team previously in the bracket
        if(bracket$Decision[i] == 0){
          bracket$Upset[i] <- bracket$Winner[i]
        } else {
          bracket$Upset[i] <- NaN
        }
        
        # Logic here is that if the game includes ANY TEAM who won a game that was not predicted to win, or if we made the wrong choice, outcome = 0
        if(bracket$Decision[i] == 0){
          bracket$Outcome[i] <- 0
        } else if(bracket$Team_1[i] %in% bracket$Upset| bracket$Team_2[i] %in% bracket$Upset) {
          bracket$Outcome[i] <- 0
        } else {
          bracket$Outcome[i] <- 1
        }
        
        # STEP 3: BREAK AFTER LAST OBS
        if(i == nrow(bracket)){
          break
        }
        
        # STEP 4: INITIALIZE NEXT ROUND
        #-- Following occurs only at the end of a round of games --#
        # Mechanism for structuring the next round of the bracket, after completing final game of current round
        # If there's a change in the next round
        if(bracket$Round[i] != bracket$Round[i+1]){
          # Initialize round info
          prev_round <- bracket$Round[i] - 1
          curr_round <- bracket$Round[i]
          next_round <- bracket$Round[i + 1]
          # Set n equal to number of games in next round
          n <- nrow(bracket[bracket$Round == next_round,])
          # Initialize c for bracket winner selection calculator, c2 for comparing against n
          c <- 1
          c2 <- 0
          for(j in 0:(n-1)){
            # Break for loop once new round is full
            if(c2 == n){ break }
            # Establishing bracket reorganization dictionary
            iter_vec <- c(32, 16, 8, 4, 2)
            # Assigning teams to the next round of the bracket
            bracket$Team_1[i+c] <- bracket$Winner[max(0, Nan0(prev_round/prev_round))*sum(iter_vec[1:prev_round]) + 1 + 2*j]
            bracket$Team_2[i+c] <- bracket$Winner[max(0, Nan0(prev_round/prev_round))*sum(iter_vec[1:prev_round]) + 2 + 2*j]
            c = c + 1
            c2 = c2 + 1 
          } 
          # Mechanism to grab seeds for each team, grab from dictionary df. For loop, because n is small
          bracket[bracket == ""] <- NA
          for(num in 1:nrow(bracket[is.na(bracket$Team_1) == FALSE , ])){
            bracket$Seed_1[num] <- seed_dict[seed_dict$name == bracket$Team_1[num], 'seed']
          }
          for(num in 1:nrow(bracket[is.na(bracket$Team_2) == FALSE , ])){
            bracket$Seed_2[num] <- seed_dict[seed_dict$name == bracket$Team_2[num], 'seed']
          }
          bracket$Seed_Diff <- abs(bracket$Seed_1 - bracket$Seed_2)
          
          # Simulate new probs for next round
          temp_df <- bracket[bracket$Round == next_round,]
          temp_probs <- predict(logit, newdata = temp_df, type = "response")
          probs <- c(probs, temp_probs)
        } # New Round finished initializing  
      } # Single bracket simulation complete
    # Calculate total accuracy / performance of the bracket, store inside accs vector
    acc <- nrow(bracket[bracket$Outcome == 1,])/nrow(bracket)
    accs <- c(accs, acc)
  } # Brackets simulated rep times
    return(t.test(accs))
}

# Store mean values and CI's of Monte Carlo simulations to construct probabilities graph, using varying levels of upsets predictions

# Testing loop functionality
testvec <- c()
for(i in 1:10){
  for(j in 1:5){
   x <- sample(c(j,0), size = 1, prob = c(0.5,0.5))
   testvec <- c(testvec, x)
  }
}
testvec

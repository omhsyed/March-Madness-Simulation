library(tidyverse)
library(rvest)

print("Retrieving data...")

# Live NCAA 2026 bracket page

bracket_url = "https://www.ncaa.com/march-madness-live/bracket"

sesh = read_html_live(bracket_url)

seed_tags = html_elements(sesh, ".round-1 .overline.color_lvl_-5")
team_tags = html_elements(sesh, ".round-1 .body.body_2.color_lvl_-5")

seeds = html_text(seed_tags)
teams = html_text(team_tags)

seeds <- as.numeric(seeds[seeds != ""])
teams <- teams[is.na(as.numeric(teams))] # removing faulty entries of team score in first round match (problem occurs after rounds begin)
teams <- teams[teams != ""] # skipping empty entries
teams <- teams[is.na(suppressWarnings(as.numeric(teams)))] # skipping faulty entries created from the score of a match (first four)


# fixing name mismatches between NCAA page and Bart Torvik
teams[teams == "UConn"] <- "Connecticut"
teams[teams == "Long Island"] <- "LIU"
teams[teams == "Queens (N.C.)"] <- "Queens"
teams[teams == "Miami (FL)"] <- "Miami FL"
teams[teams == "McNeese"] <- "McNeese St."
teams[teams == "Miami (Ohio)"] <- "Miami OH"


scaled_seeds <- as.numeric(scale(seeds))

teams_df_unscaled <- tibble(Name = teams, Seed = seeds)

teams_df_unscaled <- teams_df_unscaled[1:64,]

# Reordering blocks; swapping 2 and 3 to match correct bracket order
block2 <- teams_df_unscaled[17:32,]
teams_df_unscaled[17:32,] <- teams_df_unscaled[33:48,]
teams_df_unscaled[33:48,] <- block2

teams_df <- tibble(Name = teams_df_unscaled$Name, Seed = as.numeric(scale(teams_df_unscaled$Seed))) # scale everything at the end



# kenpom alternative (barttorvik) stats page

url = "https://barttorvik.com/?year=2026&sort=&hteam=&t2value=&conlimit=NCAA&state=All&begin=20251101&end=20260501&top=0&revquad=0&quad=5&venue=All&type=All&mingames=0#"

s = read_html_live(url)

stats_df = html_table(s)[[1]]

colnames(stats_df) <- stats_df[1,]

stats_df <- stats_df[stats_df$Team != "Team", ] # removing all column headers that appear as rows 

stats_df[, 6:24] <- lapply(stats_df[, 6:24], as.numeric) # making all the stats float/double values

stats_df$Team <- as.character(stats_df$Team)
stats_df$Team <- gsub("\u00A0", " ", stats_df$Team) # replacing weird space
stats_df$Team <- sub("   .*", "", stats_df$Team) # getting rid of the three spaces and the seed label that follows it

stats_df[, 6:24] <- scale(stats_df[, 6:24]) # standardizing (with z-score) all stats so that they are on the same scale




k = 1
w = c(-0.25, 0.3, -0.2, 0.15, 0.1)

Px <- function(ox, dx, oy, dy) 
{
  i = 1:5
  return((1 + exp(-k*(sum(w[i]*((ox[i] - dx[i]) - (oy[i] - dy[i]))))))^(-1))
}



simulate_game <- function(teamX, teamY)
{
  if (!teamX %in% stats_df$Team) stop(paste("Not found in stats_df:", teamX))
  if (!teamY %in% stats_df$Team) stop(paste("Not found in stats_df:", teamY))

  x_seed = teams_df[teams_df$Name == teamX, 2][[1]]
  x_off_stats = c(x_seed)
  x_def_stats = c(0)
  
  y_seed = teams_df[teams_df$Name == teamY, 2][[1]]
  y_off_stats = c(y_seed)
  y_def_stats = c(0)
    
  for (s in seq(9, 15, 2))
  {
    x_off_stats = c(x_off_stats, stats_df[stats_df$Team == teamX, s][[1]])
    x_def_stats = c(x_def_stats, stats_df[stats_df$Team == teamX, s + 1][[1]])
    
    y_off_stats = c(y_off_stats, stats_df[stats_df$Team == teamY, s][[1]])
    y_def_stats = c(y_def_stats, stats_df[stats_df$Team == teamY, s + 1][[1]])
  }
  
  prob = Px(x_off_stats, x_def_stats, y_off_stats, y_def_stats)
  #print(prob)
  rand = runif(n = 1, 0, 1)
  
  if (rand <= prob) 
  {
    return(teamX)
  }
  else
  {
    return(teamY)
  }
  
}



simulate_round <- function(v) 
{

  round_vect <- c()
  
  for (i in seq(1, length(v), 2))
  {
    game_winner <- simulate_game(v[i], v[i+1])
    round_vect <- c(round_vect, game_winner)
  }
  
  return(round_vect)
  
}



simulate_tournament <- function()
{
  
  round_results <- pull(teams_df, Name)
  full_bracket <- tibble(round_results)
  
  while (length(round_results) > 1)
  {
    round_results <- simulate_round(round_results)
    r_padded <- round_results
    length(r_padded) <- 64
    full_bracket <- cbind(full_bracket, r_padded)
  }
  
  colnames(full_bracket) <- c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5", "Round 6", "Round 7")
  return(full_bracket)

}

print("Simulating brackets...")

sims <- 100

all_results <- list()

round_df = tibble(Team = teams_df$Name, Seed = teams_df_unscaled$Seed, Top32 = 0, Sweet16 = 0, Elite8 = 0, Final4 = 0, Finals = 0, Champion = 0)

for (s in 1:sims) 
{
  
  all_results[[s]] <- simulate_tournament()
  
  for (r in 1:32)
  {
    for (c in 2:7)
    {
      if (!is.na(all_results[[s]][r,c]))
      {
        round_df[round_df$Team == all_results[[s]][r,c], c + 1] <- round_df[round_df$Team == all_results[[s]][r,c], c + 1] + 1
      }
    }
  }
  
  
}



round_df[,3:8] <- round_df[,3:8]/sims

#print(round_df, n = 100)
# print(arrange(round_df[,c(1,2,3)], desc(Top32)), n = 100)
# print(arrange(round_df[,c(1,2,4)], desc(Sweet16)), n = 100)
# print(arrange(round_df[,c(1,2,5)], desc(Elite8)), n = 100)
# print(arrange(round_df[,c(1,2,6)], desc(Final4)), n = 100)
# print(arrange(round_df[,c(1,2,7)], desc(Finals)), n = 100)
# print(arrange(round_df[,c(1,2,8)], desc(Champion)), n = 100)



# Scoring every simulation bracket against the rest, and returning the one with the highest average score

round_weights <- c(1, 2, 4, 8, 16, 32)



print("Finding best bracket...")

calculate_score <- function(bracket_df, prob_df) {
  
  bracket_score = 0
  
  for (c in 1:6) 
  {
    
    for (r in 1:(length(na.omit(bracket_df[[c]])) / 2))
    {
      if (r != 1) {r = r*2 - 1}
      seed1 = teams_df[teams_df$Name == bracket_df[[c]][r], 2]
      seed2 = teams_df[teams_df$Name == bracket_df[[c]][r + 1], 2] 
      if (r != 1) {r = (r+1)/2}
    
      bracket_score = bracket_score + round_weights[c] * prob_df[prob_df$Team == bracket_df[[c + 1]][r], c + 2]
      
      better_seed = min(seed1, seed2)
      
      current_seed = teams_df[teams_df$Name == bracket_df[[c + 1]][r], 2]
      
      if (current_seed > better_seed)
      {
        bracket_score = bracket_score + (current_seed - better_seed) * prob_df[prob_df$Team == bracket_df[[c + 1]][r], c + 2]
      }
      

    }
  
  }
  
  return(bracket_score)
  
}

scores <- sapply(all_results, calculate_score, prob_df = round_df)

best_index <- which.max(scores)

best_bracket <- all_results[[best_index]]

print("Complete!")

print(best_bracket)


# 
# scores = c()
# 
# for (i in 1:length(all_results))
# {
#   bracket_score = 0
#   
#   for (j in 1:length(all_results))
#   {
#     for (c in 1:6)
#     {
#       for (r in 1:(length(na.omit(all_results[[i]][[c]])) / 2))
#       {
#         if (r != 1) {r = r*2 - 1}
#         seed1 = teams_df[teams_df$Name == all_results[[i]][[c]][r], 2]
#         seed2 = teams_df[teams_df$Name == all_results[[i]][[c]][r + 1], 2] 
#         if (r != 1) {r = (r+1)/2}
#         
#         if (all_results[[i]][[c + 1]][r] == all_results[[j]][[c + 1]][r])
#         {
#           bracket_score = bracket_score + round_weights[c]
#           
#           current_seed = teams_df[teams_df$Name == all_results[[i]][[c + 1]][r], 2]
#           
#           if (current_seed >= seed1 && current_seed >= seed2)
#           {
#             better_seed = 0
#             if (current_seed == seed1) {better_seed = seed2} else {better_seed = seed1}
#             
#             bracket_score = bracket_score + (current_seed - better_seed)
#           }
#         }
#       
#         
#       }
#     }
#   }
#   
#   bracket_score = bracket_score/length(all_results)
#   scores = c(scores, bracket_score)
# }
# 
# best_index = which.max(scores)
# best_bracket = all_results[[best_index]]
# 
# print("Complete!")
# 
# print(best_bracket)

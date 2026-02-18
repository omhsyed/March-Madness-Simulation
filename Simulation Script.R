
Px <- function(x, y, r) 
{
  return((1 + exp(0.1*(r+1)*(x-y)))^(-1))
}



seed <- c(
  # Region 1 (East)
  "UConn" = 1, "Stetson" = 16, "FAU" = 8, "Northwestern" = 9,
  "San Diego St" = 5, "UAB" = 12, "Auburn" = 4, "Yale" = 13,
  "BYU" = 6, "Duquesne" = 11, "Illinois" = 3, "Morehead St" = 14,
  "Washington St" = 7, "Drake" = 10, "Iowa St" = 2, "South Dakota St" = 15,
  
  # Region 2 (West)
  "North Carolina" = 1, "Wagner" = 16, "Mississippi St" = 8, "Michigan St" = 9,
  "Saint Mary's" = 5, "Grand Canyon" = 12, "Alabama" = 4, "Charleston" = 13,
  "Clemson" = 6, "New Mexico" = 11, "Baylor" = 3, "Colgate" = 14,
  "Dayton" = 7, "Nevada" = 10, "Arizona" = 2, "Long Beach St" = 15,
  
  # Region 3 (South)
  "Houston" = 1, "Longwood" = 16, "Nebraska" = 8, "Texas A&M" = 9,
  "Wisconsin" = 5, "James Madison" = 12, "Duke" = 4, "Vermont" = 13,
  "Texas Tech" = 6, "NC State" = 11, "Kentucky" = 3, "Oakland" = 14,
  "Florida" = 7, "Colorado" = 10, "Marquette" = 2, "Western Kentucky" = 15,
  
  # Region 4 (Midwest)
  "Purdue" = 1, "Grambling" = 16, "Utah St" = 8, "TCU" = 9,
  "Gonzaga" = 5, "McNeese" = 12, "Kansas" = 4, "Samford" = 13,
  "South Carolina" = 6, "Oregon" = 11, "Creighton" = 3, "Akron" = 14,
  "Texas" = 7, "Colorado St" = 10, "Tennessee" = 2, "Saint Peter's" = 15
)




group <- c(
  # Region 1 (East)
  "UConn" = 1, "Stetson" = 1, "FAU" = 1, "Northwestern" = 1,
  "San Diego St" = 1, "UAB" = 1, "Auburn" = 1, "Yale" = 1,
  "BYU" = 1, "Duquesne" = 1, "Illinois" = 1, "Morehead St" = 1,
  "Washington St" = 1, "Drake" = 1, "Iowa St" = 1, "South Dakota St" = 1,
  
  # Region 2 (West)
  "North Carolina" = 2, "Wagner" = 2, "Mississippi St" = 2, "Michigan St" = 2,
  "Saint Mary's" = 2, "Grand Canyon" = 2, "Alabama" = 2, "Charleston" = 2,
  "Clemson" = 2, "New Mexico" = 2, "Baylor" = 2, "Colgate" = 2,
  "Dayton" = 2, "Nevada" = 2, "Arizona" = 2, "Long Beach St" = 2,
  
  # Region 3 (South)
  "Houston" = 3, "Longwood" = 3, "Nebraska" = 3, "Texas A&M" = 3,
  "Wisconsin" = 3, "James Madison" = 3, "Duke" = 3, "Vermont" = 3,
  "Texas Tech" = 3, "NC State" = 3, "Kentucky" = 3, "Oakland" = 3,
  "Florida" = 3, "Colorado" = 3, "Marquette" = 3, "Western Kentucky" = 3,
  
  # Region 4 (Midwest)
  "Purdue" = 4, "Grambling" = 4, "Utah St" = 4, "TCU" = 4,
  "Gonzaga" = 4, "McNeese" = 4, "Kansas" = 4, "Samford" = 4,
  "South Carolina" = 4, "Oregon" = 4, "Creighton" = 4, "Akron" = 4,
  "Texas" = 4, "Colorado St" = 4, "Tennessee" = 4, "Saint Peter's" = 4
)




team_df <- data.frame(
  team = names(seed),
  seed = as.numeric(seed),
  group = as.numeric(group)
)

num_teams = nrow(team_df)


bracket <- data.frame(Round1 = names(seed), Round2 = NA, Round3 = NA, Round4 = NA, Round5 = NA, Round6 = NA, CHAMPION = NA)







simulate_game <- function(teamA, teamB, round)
{
  
  prob = Px(teamA[1,2], teamB[1,2], round)
  
  rand = runif(n = 1, 0, 1)
  
  if (rand <= prob) 
  {
    return(teamA)
  }
  else
  {
    return(teamB)
  }
  
}




simulate_round <- function(df, round_number) 
{
  round_df = data.frame(team = character(0), seed = numeric(0), group = numeric(0))
  
  
  if (nrow(df) > 4)
  {
    
    for (g in seq(1, 4))
    {
      
      group = df[df$group == g, ]
      
      for (i in seq(1, nrow(group), 2))
      {
        game_winner <- simulate_game(group[i, ], group[i+1, ], round_number)
        round_df[nrow(round_df) + 1, ] = game_winner
      }
      
    }
    
  }
  else
  {
    for (i in seq(1, nrow(df), 2))
    {
      game_winner <- simulate_game(df[i, ], df[i+1, ], round_number)
      round_df[nrow(round_df) + 1, ] = game_winner
    }
  }
  
  num_empty = num_teams - nrow(round_df)
  
  bracket[, round_number + 1] <<- c(round_df[, 1], rep(NA, num_empty))
  
  
  return(round_df)
  
}



simulate_tournament <- function()
{
  
  round_results <- simulate_round(team_df, 1)
  round_results <- simulate_round(round_results, 2)
  round_results <- simulate_round(round_results, 3)
  round_results <- simulate_round(round_results, 4)
  round_results <- simulate_round(round_results, 5)
  round_results <- simulate_round(round_results, 6)
  
  
  
  game_wins <- c()
  
  for (c in seq(2, 7))
  {
    for (r in seq(1, 64))
    {
      if (!is.na(bracket[r, c]))
      {
        game_wins <- append(game_wins, bracket[r, c])
      }
    }
  }
  
  return(game_wins)
  
  
  
}






sims <- 10000



all_results <- t(replicate(sims, simulate_tournament()))

games_counts <- apply(all_results, 2, table)



game_winners = c()

for (i in 1:63)
{
  game_winners <- append(game_winners, names(games_counts[[i]])[which.max(games_counts[[i]])])
}
print(game_winners)




scores <- apply(all_results, 1, function(row) sum(row == game_winners))

best_sim_index <- which.max(scores)
max_matches <- max(scores)

print(best_sim_index)
print(all_results[best_sim_index, ])








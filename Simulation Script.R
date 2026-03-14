library(tidyverse)


# WEB SCRAPING


# previous bracket pages on NCAA

#library(rvest)
#bracket_url = "https://www.ncaa.com/brackets/basketball-men/d1/2024"
#sesh = read_html_live(bracket_url)
#team_tags = html_elements(sesh, "span.name")
#team_names = html_text(team_tags)



# Live NCAA 2026 bracket page (currently empty)

bracket_url = "https://www.ncaa.com/march-madness-live/bracket"

sesh = read_html_live(bracket_url)

seed_tags = html_elements(sesh, "span.overline color_lvl_-5")
team_tags = html_elements(sesh, "body body_2 color_lvl_-5")

seeds = html_text(seed_tags)
teams = html_text(team_tags)



# kenpom alternative (barttorvik) stats page

library(rvest)

url = "https://barttorvik.com/#"

s = read_html_live(url)

df = html_table(s)[[1]]

colnames(df) <- df[1,]

df <- df[-1,]

df[, 6:24] <- lapply(df[, 6:24], as.numeric)

df$Team <- as.character(df$Team)
df$Team <- gsub("\u00A0", " ", df$Team) # replacing weird space
df$Team <- trimws(sub("\\s*vs\\..*", "", df$Team)) # getting rid of "vs..."
df$Team <- trimws(sub("\\s*\\(.*", "", df$Team)) # getting rid of (... 







# test values for seed and teams until bracket is finalized

rand64 <- sample(1:64, 64)

seeds <- numeric(0)
teams <- character(0)

for (i in 1:length(rand64)) 
{
  seeds <- c(seeds, as.numeric(df[df$Rk == rand64[i], 1][[1]]))
}

for (i in 1:length(rand64)) 
{
  teams <- c(teams, df[df$Rk == rand64[i], 2][[1]])
}

team_info = tibble(Name = teams, Seed = seeds)
#print(team_info)
team_vect <- team_info[,1][[1]]
#print(team_vect)









k = 0.1
w = c(0.5, 0.2, 0.15, 0.1, 0.05)

Px <- function(ox, dx, oy, dy) 
{
  i = 1:5
  return((1 + exp(-k * (sum(w[i]*((ox[i] - dy[i]) - (oy[i] - dx[i]))))))^(-1))
}


Px(c(-5, 55, 13, 39, 32), c(0, 47, 11, 26, 19), c(-7, 57, 13, 35, 28), c(0, 52, 15, 26, 27))





simulate_game <- function(teamX, teamY)
{

  x_seed = team_info[team_info$Name == teamX, 2][[1]]
  x_off_stats = c(-1*x_seed)
  x_def_stats = c(0)
  
  y_seed = team_info[team_info$Name == teamY, 2][[1]]
  y_off_stats = c(-1*y_seed)
  y_def_stats = c(0)
    
  for (s in seq(9, 15, 2))
  {
    x_off_stats = c(x_off_stats, df[df$Team == teamX, s][[1]])
    x_def_stats = c(x_def_stats, df[df$Team == teamX, s + 1][[1]])
    
    y_off_stats = c(y_off_stats, df[df$Team == teamY, s][[1]])
    y_def_stats = c(y_def_stats, df[df$Team == teamY, s + 1][[1]])
  }
  
  prob = Px(x_off_stats, x_def_stats, y_off_stats, y_def_stats)
  
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
  
  round_results <- team_vect
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


simulate_tournament()






sims <- 100

all_results <- list()

#champ_count = tibble(Team = 1:64, Count = 0)

win_count = tibble(Team = team_info$Name, Count = 0)

for (s in 1:sims) 
{
  all_results[[s]] <- simulate_tournament()
  
  #winner = as.numeric(all_results[[s]][1,7])
  #print(winner)
  #champ_count[champ_count$Team == winner, 2] = champ_count[champ_count$Team == winner, 2] + 1
  
  
  for (r in 1:32)
  {
    for (c in 2:7) 
    {
      if (!is.na(all_results[[s]][r,c]))
      {
        win_count[win_count$Team == all_results[[s]][r,c], 2] = win_count[win_count$Team == all_results[[s]][r,c], 2] + 1
      }
    }
  }
  
  
}

#champ_count[,2] = champ_count[,2]/sims

#win_count[,2] = win_count[,2]/sims

win_count <- arrange(win_count, Count)

print(win_count, n = 100)

barplot(height = win_count$Count)




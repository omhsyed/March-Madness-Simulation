library(tidyverse)


# WEB SCRAPING


# previous bracket pages on NCAA

#bracket_url = "https://www.ncaa.com/brackets/basketball-men/d1/2024"
#sesh = read_html_live(bracket_url)
#team_tags = html_elements(sesh, "span.name")
#team_names = html_text(team_tags)

#print(team_names)


# Live NCAA 2026 bracket page (currently empty)

bracket_url = "https://www.ncaa.com/march-madness-live/bracket"

sesh = read_html_live(bracket_url)

seed_tags = html_elements(sesh, "span.overline color_lvl_-5")
team_tags = html_elements(sesh, "body body_2 color_lvl_-5")

seeds = html_text(seed_tags)
teams = html_text(team_tags)



# kenpom alternative (barttorvik) stats page

url = "https://barttorvik.com/#"

s = read_html_live(url)

stats_df = html_table(s)[[1]]

colnames(stats_df) <- stats_df[1,]

stats_df <- stats_df[stats_df$Team != "Team", ] # removing all column headers that appear as rows 

stats_df[, 6:24] <- lapply(stats_df[, 6:24], as.numeric) # making all the stats float/double values

stats_df$Team <- as.character(stats_df$Team)
stats_df$Team <- gsub("\u00A0", " ", stats_df$Team) # replacing weird space
stats_df$Team <- trimws(sub("\\s*vs\\..*", "", stats_df$Team)) # getting rid of "vs..."
stats_df$Team <- trimws(sub("\\s*\\(.*", "", stats_df$Team)) # getting rid of (... 

#print(stats_df)

stats_df[, 6:24] <- scale(stats_df[, 6:24]) # standardizing (with z-score) all stats so that they are on the same scale





# test values for seed and teams until bracket is finalized

seeds <- numeric(0)
teams <- character(0)

seeds <- c(seeds, as.numeric(stats_df[1:64, 1][[1]]))
teams <- c(teams, stats_df[1:64, 2][[1]])

seeds <- ceiling(seeds/4)

teams_df = tibble(Name = teams, Seed = seeds)

# Gemini code to establish correct order (1v16, 2v15, etc.)

# Define your desired seed matchup order
seed_matchups <- c(1, 16, 2, 15, 3, 14, 4, 13, 5, 12, 6, 11, 7, 10, 8, 9)

teams_df <- teams_df %>%
  group_by(Seed) %>%
  mutate(region = row_number()) %>% # Assigns 1-4 to each repeating seed
  ungroup() %>%
  arrange(region, factor(Seed, levels = seed_matchups)) %>%
  select(-region) # Optional: drop the region column if you no longer need it


teams_df$Seed <- scale(teams_df$Seed) # scaling seeds using z-score



k = c(1, 0.75, 0.5, 0.25, 0.15, 0.1, 0.05)
w = c(-0.7, 0.15, -0.1, 0.075, 0.05)

Px <- function(ox, dx, oy, dy, r) 
{
  i = 1:5
  return((1 + exp(-1*k[r] * (sum(w[i]*((ox[i] - dy[i]) - (oy[i] - dx[i]))))))^(-1))
}



simulate_game <- function(teamX, teamY, r)
{

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
  
  prob = Px(x_off_stats, x_def_stats, y_off_stats, y_def_stats, r)
  
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




simulate_round <- function(v, r) 
{

  round_vect <- c()
  
  for (i in seq(1, length(v), 2))
  {
    game_winner <- simulate_game(v[i], v[i+1], r)
    round_vect <- c(round_vect, game_winner)
  }
  
  return(round_vect)
  
}



simulate_tournament <- function()
{
  
  round_results <- pull(teams_df, Name)
  full_bracket <- tibble(round_results)
  
  current_round <- 1
  
  while (length(round_results) > 1)
  {
    round_results <- simulate_round(round_results, current_round)
    r_padded <- round_results
    length(r_padded) <- 64
    full_bracket <- cbind(full_bracket, r_padded)
    current_round <- current_round + 1
  }
  
  colnames(full_bracket) <- c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5", "Round 6", "Round 7")
  return(full_bracket)

}


simulate_tournament()






sims <- 1000

all_results <- list()

#champ_count = tibble(Team = teams_df$Name, Count = 0)

#win_count = tibble(Team = teams_df$Name, Count = 0)

round_df = tibble(Team = teams_df$Name, Top32 = 0, Sweet16 = 0, Elite8 = 0, Final4 = 0, Finals = 0, Champion = 0)

for (s in 1:sims) 
{
  
  all_results[[s]] <- simulate_tournament()
  
  for (r in 1:32)
  {
    for (c in 2:7)
    {
      if (!is.na(all_results[[s]][r,c]))
      {
        round_df[round_df$Team == all_results[[s]][r,c], c] <- round_df[round_df$Team == all_results[[s]][r,c], c] + 1
      }
    }
  }
  
  
}


round_df[,2:7] <- round_df[,2:7]/sims

ordered_results = arrange(round_df, desc(Champion))

print(ordered_results, n = 64)

#barplot(height = champ_count$Count)



library(tidyverse)
library(rvest)



# Live NCAA 2026 bracket page

bracket_url = "https://www.ncaa.com/march-madness-live/bracket"

sesh = read_html_live(bracket_url)

seed_tags = html_elements(sesh, "span.overline.color_lvl_-5")
team_tags = html_elements(sesh, "p.body.body_2.color_lvl_-5")

seeds = html_text(seed_tags)
teams = html_text(team_tags)

seeds <- as.numeric(seeds[seeds != ""])
seeds <- seeds[1:64]
teams <- teams[teams != ""] # skipping empty entries
teams <- teams[1:64]

#teams <- teams[seq(-2, -268, -2)] # skipping empty even entries
#teams <- teams[c(-17:-30, -47:-66, -83:-96, -113:-126)] #skipping unfilled bracket entries; 2nd vector entry skips a bit more than 13 since it picks up the final 4 blank area

scaled_seeds <- as.numeric(scale(seeds))

teams_df_unscaled <- tibble(Name = teams, Seed = seeds)

teams_df <- tibble(Name = teams, Seed = scaled_seeds)



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



# test values for seed and teams until bracket is finalized

seeds <- numeric(0)
teams <- character(0)

seeds <- 1:64
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



k = 1
w = c(-0.25, 0.3, -0.2, 0.15, 0.1)

Px <- function(ox, dx, oy, dy) 
{
  i = 1:5
  return((1 + exp(-k*(sum(w[i]*((ox[i] - dx[i]) - (oy[i] - dy[i]))))))^(-1))
}



simulate_game <- function(teamX, teamY)
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



sims <- 5000

all_results <- list()

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

#ordered_results = arrange(round_df, desc(Champion))

#print(ordered_results, n = 64)

print(arrange(round_df[,1:2], desc(Top32)))
print(arrange(round_df[,c(1,3)], desc(Sweet16)))
print(arrange(round_df[,c(1,4)], desc(Elite8)))
print(arrange(round_df[,c(1,5)], desc(Final4)))
print(arrange(round_df[,c(1,6)], desc(Finals)))
print(arrange(round_df[,c(1,7)], desc(Champion)))

#barplot(height = champ_count$Count)



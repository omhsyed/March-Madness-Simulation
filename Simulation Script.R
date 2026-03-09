library(rvest)
library(dplyr)


# WEB SCRAPING


# previous bracket pages on NCAA

library(rvest)

bracket_url = "https://www.ncaa.com/brackets/basketball-men/d1/2024"

sesh = read_html_live(bracket_url)

team_tags = html_elements(sesh, "span.name")

team_names = html_text(team_tags)



# Live NCAA 2026 bracket page (currently empty)

library(rvest)

bracket_url = "https://www.ncaa.com/march-madness-live/bracket"

sesh = read_html_live(bracket_url)

seed_tags = html_elements(sesh, "span.overline color_lvl_-5")
team_tags = html_elements(sesh, "body body_2 color_lvl_-5")

seed_nums = html_text(seed_tags)
team_names = html_text(team_tags)


# kenpom alternative (barttorvik) stats page

library(rvest)

url = "https://barttorvik.com/#"

s = read_html_live(url)

df = html_table(s)[[1]]

colnames(df) <- df[1,]

df <- df[-1,]

#print(df)







Px <- function(x, y, r) 
{
  return((1 + exp(0.1*(r+1)*(x-y)))^(-1))
}


k = 0.25
w = c(0.4, 0.25, 0.2, 0.15)

Px_new <- function(ox, dx, oy, dy) 
{
  i = 1:4
  return((1 + exp(-k * (sum(w[i]*((ox[i] - dy[i]) - (oy[i] - dx[i]))))))^(-1))
}


#Px_new(c(55, 13, 39, 32), c(47, 11, 26, 19), c(57, 13, 35, 28), c(52, 15, 26, 27))



team_vect <- as.character(1:64)


simulate_game <- function(teamX, teamY)
{
  x_off_stats = as.numeric(unlist(df[df$Rk == teamX, seq(9, 15, 2)], use.names = FALSE))
  x_def_stats = as.numeric(unlist(df[df$Rk == teamX, seq(10, 16, 2)], use.names = FALSE))
  
  y_off_stats = as.numeric(unlist(df[df$Rk == teamY, seq(9, 15, 2)], use.names = FALSE))
  y_def_stats = as.numeric(unlist(df[df$Rk == teamY, seq(10, 16, 2)], use.names = FALSE))
  
  prob = Px_new(x_off_stats, x_def_stats, y_off_stats, y_def_stats)
  
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
  full_bracket <- round_results

  
  while (length(round_results) > 1)
  {
    round_results <- simulate_round(round_results)
    full_bracket <- rbind(full_bracket, round_results)
  }
  
  rownames(full_bracket) <- c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5", "Round 6", "Round 7")
  return(full_bracket)

}





sims <- 100

all_results <- list()

for (s in 1:sims) 
{
  all_results[[s]] <- simulate_tournament()
}

print(all_results)



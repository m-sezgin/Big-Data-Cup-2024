

# Load Packages -----------------------------------------------------------

library(dplyr)
library(readr)



# Load and process data ---------------------------------------------------

data <- file.path(Sys.getenv("BDC24"), "data", "BDC_2024_Womens_Data.csv") |>
                    read_csv() |>
  # rename(shot_type = `Detail 1`,
  #        shot_destination = `Detail 2`,
  #        traffic = `Detail 3`,
  #        one_timer = `Detail 4`) |>
  janitor::clean_names()

roster_us <- file.path(Sys.getenv("BDC24"), 
                       "data", 
                       "rivalry_series_nov_23_us.csv") |>
  read_csv() |>
  select(-player_cap,
         -player_number,
         player_pos = position) |>
  mutate(player = case_when(player == "Becca Gilmore" ~ "Rebecca Gilmore",
                            player == "Ally Simpson" ~ "Allyson Simpson",
                            player == "Casey Obrien" ~ "Casey O'Brien",
                            T ~ player))
  
  
roster_ca <- file.path(Sys.getenv("BDC24"), 
                       "data", 
                       "rivalry_series_nov_23_canada.csv") |>
  read_csv() |>
  select(-player_cap,
         -player_number,
         player_pos = position
         ) |>
  mutate(
    player = case_when(
      player == "Ann-Renée Desbiens" ~ "Ann-Renee Desbiens",
      player == "Jaime Bourbonnais" ~ "Jamie Bourbonnais",
      player == "Kristin O’Neill" ~ "Kristin O'Neill",
      player == "Jessica Digirolamo" ~ "Jessica DiGirolamo",
      T ~ player
    )
  )

super_roster <- roster_us |>
  bind_rows(roster_ca)

#setdiff(union(roster_us$player, roster_ca$player), data$player |> unique())

data_pos <- data |> 
  left_join(super_roster, by = "player") |>
  left_join(super_roster |>
              rename(player_2_pos = player_pos), by = c("player_2" = "player")) 

# zone_exits <- data_pos |>
#   mutate((event == "Play" | event == "Incomplete Play" | event == "Dump In/Out"),
#          x_coordinate <= 75) |>
  
# TODO: I want a column that records time in zone for each event
test <- data_pos |>
  mutate(zone = case_when(x_coordinate >= 125 ~ "offensive",
                          x_coordinate <= 75 ~ "defensive",
                          T ~ "neutral"),
         zone_usa = case_when(team == "Women - Canada" & zone == "offensive " ~ "defensive",
                              team == "Women - Canada" & zone == "defensive" ~ "offensive",
                              T ~ zone),
         zone_canada = case_when(zone_usa == "defensive" ~ "offensive",
                                 zone_usa == "offensive" ~ "defensive",
                                 T ~ zone),
         power_play = case_when(home_team_skaters < away_team_skaters ~ T,
                                away_team_skaters < home_team_skaters ~ T,
                                T ~ F),
         pp_extra_players = if_else(power_play == T, abs(home_team_skaters - away_team_skaters), NA)
         ) |>
  group_by(date, period) |>
  arrange(date, period, desc(clock)) |>
  mutate(time_z_ee = case_when(
    as.character(clock) == "20:00:00" ~ clock,
    dplyr::lag(zone_usa) != zone_usa ~ clock,
    T ~ NA)) |>
  tidyr::fill(time_z_ee, .direction = "downup") |>
  ungroup() |>
  group_by(date, period, time_z_ee) |>
  #arrange(date, period, time_z_ee, desc(clock)) |>
  mutate(time_in_zone = (time_z_ee - clock)/60) |>
  ungroup() |>
  group_by(gr = data.table::rleid(event)) |>
  mutate(consec = 1:n()
         ) |>
  ungroup() |>
  group_by(date, period) |>
  arrange(date, period, desc(clock)) |>
  mutate(
    consec_passes_before_attempt = 
      case_when((event == "Shot" | event == "Goal") &
               lag(event) == "Play" ~ lag(consec),
               (event == "Shot" | event == "Goal") ~ 0,
              T ~ NA),
    
    ) |>
  ungroup() #|>
  #select(date, period, clock, team, player, event, gr, consec, time_z_ee, consec_passes_before_attempt)

# possible question... when controlling for other relevant factors does
# consecutive passes before event predict shot efficacy on the PP?
test |>
  filter(power_play == T, (event == "Shot" | event == "Goal")) |>
  mutate(success = if_else(event == "Shot", 0, 1)) |>
  group_by(consec_passes_before_attempt) |>
  summarize(sum(success)/n(), count = n())

test |>
  select(event, time_in_zone, count) |> View() 

# are shots super risky in general? maybe not... 
# unconditional MLE estimator of 37.5% turnover risk within sample
test |>
  mutate(shot_turnover = if_else(event == "Shot" &
                                  lead(event) == "Puck Recovery" &
                                  team != lead(team), 1, 0)) |>
  filter(event == "Shot") |>
  summarize(turnovers = sum(shot_turnover), total = n(), prop = turnovers/total)

test |>
  filter(event == "Goal") |> 
  arrange(time_in_zone) |> View()
  ggplot(aes(x = time_in_zone)) +
  geom_histogram()

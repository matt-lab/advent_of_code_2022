# Source: https://adventofcode.com/2022/day/2
library(tidyverse)

data <- read_delim(
    file = "input.txt",
    delim = " ",
    col_names = c("opponent_raw", "you_raw")
    )

#### Part one ####

# Make readable
codes_opponent <- tibble(
    opponent_raw = c("A", "B", "C"),
    opponent_shape = c("rock", "paper", "scissors")
    )
codes_you <- tibble(
    you_raw = c("X", "Y", "Z"),
    you_shape = c("rock", "paper", "scissors")
    )
data <- data |>
    left_join(codes_opponent, by = "opponent_raw") |>
    left_join(codes_you, by = "you_raw")

# Calculate shape points
data <- data |>
    mutate(points_shape =
        ifelse(
            you_shape == "rock",
            1,
            ifelse(
                you_shape == "paper",
                2,
                3
            )
        ))

# Calculate outcome points
data <- data |>
    mutate(is_win =
            (opponent_shape == "rock" & you_shape == "paper") |
            (opponent_shape == "paper" & you_shape == "scissors") |
            (opponent_shape == "scissors" & you_shape == "rock")
    ) |>
    mutate(is_draw = opponent_shape == you_shape) |>
    mutate(points_outcome = ifelse(is_win, 6, ifelse(is_draw, 3, 0)))

# Calculate total points
data <- data |>
    mutate(points_total = points_shape + points_outcome)

data |>
    summarise(points_total = sum(points_total)) |>
    pull(points_total)


#### Part two ####
data <- data |>
    select(ends_with("raw"))

codes_win <- tibble(
    you_raw = c("X", "Y", "Z"),
    points_outcome = c(0, 3, 6)
    )
trial_outcomes <- tibble(
    opponent_shape = rep(c("rock", "paper", "scissors"), each = 3),
    you_shape = rep(c("rock", "paper", "scissors"), 3)
    ) |>
    mutate(is_win =
            (opponent_shape == "rock" & you_shape == "paper") |
            (opponent_shape == "paper" & you_shape == "scissors") |
            (opponent_shape == "scissors" & you_shape == "rock")
    ) |>
    mutate(is_draw = opponent_shape == you_shape) |>
    mutate(points_outcome = ifelse(is_win, 6, ifelse(is_draw, 3, 0)))


data <- data |>
    left_join(codes_opponent, by = "opponent_raw") |>
    left_join(codes_win, by = "you_raw") |>
    left_join(trial_outcomes, by = c("opponent_shape", "points_outcome"))

# Count points
data <- data |>
    mutate(points_shape =
        ifelse(
            you_shape == "rock",
            1,
            ifelse(
                you_shape == "paper",
                2,
                3
            )
        )) |>
    mutate(points_total = points_shape + points_outcome)

data |>
    summarise(points_total = sum(points_total)) |>
    pull(points_total)

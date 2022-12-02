# Link: https://adventofcode.com/2022/day/1
library(tidyverse)

# Part 1

data <- tibble(input = read_lines("input.txt"))

data <- data |>
    mutate(id = cumsum(input == "")) |>
    mutate(calories = ifelse(is.na(input), 0, as.integer(input)))

data |>
    filter(!is.na(calories)) |>
    group_by(id) |>
    summarise(calories_total = sum(calories)) |>
    slice_max(calories_total) |>
    pull(calories_total)

# Part 2

data |>
    filter(!is.na(calories)) |>
    group_by(id) |>
    summarise(calories_total = sum(calories)) |>
    slice_max(calories_total, n = 3) |>
    summarise(calories_total = sum(calories_total)) |>
    pull(calories_total)
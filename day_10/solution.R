library(tidyverse)

data <- tibble(input = read_lines("input.txt"))

data <- data |>
    mutate(cycles_increment = ifelse(str_detect(input, "noop"), 1, 2)) |>
    mutate(x_command =
        ifelse(
            str_detect(input, "^addx"),
            as.integer(str_remove(input, "^[a-z]+ ")),
            as.integer(0))) |>
    rowwise() |>
    mutate(x_increment = list(c(rep(0, cycles_increment - 1), x_command))) |>
    unnest(x_increment) |>
    mutate(x = cumsum(lag(x_increment, default = 1))) |>
    mutate(cycles = row_number()) |>
    mutate(is_important_cycle = cycles %in% c(20, 60, 100, 140, 180, 220)) |>
    mutate(signal_strength = ifelse(is_important_cycle, x * cycles, 0))

#### Part one ####
solution_1 <- data |>
    summarise(total = sum(signal_strength)) |>
    pull(total)

#### Part two ###
data <- data |>
    mutate(screen_y = (cycles - 1) %/% 40) |>
    mutate(screen_x = (cycles - 1) %% 40) |>
    mutate(output = ifelse(abs(x - screen_x) <= 1, "#", "."))
solution_2 <- data |>
    group_by(screen_y) |>
    summarise(output = str_c(output, collapse = ""))
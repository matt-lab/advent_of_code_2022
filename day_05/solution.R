library(tidyverse)

input <- read_lines("input.txt")
# Data
data <- tibble(raw = input[str_detect(input, "^\\[")])
data <- data |>
    mutate(height = row_number()) |>
    mutate(crate = str_extract_all(raw, "[:alpha:]")) |>
    mutate(position = str_locate_all(raw, "[:alpha:]")) |>
    mutate(position = map(position, ~ (.x[ , 1] + 2) / 4)) |>
    unnest(c(position, crate)) |>
    mutate(across(where(is.double), as.integer)) |>
    group_by(position) |>
    mutate(from_top = height - min(height) + 1) |>
    select(-raw, -height) |>
    ungroup()
instructions <- input[str_detect(input, "^move")]
#### Part one ####
data_1 <- data
lapply(instructions, function(instruction) {
    instruction <- as.integer(str_extract_all(instruction, "[0-9]+")[[1]])
    data_1 <<- data_1 |>
        mutate(to_move =
            position == instruction[2]
            & from_top <= instruction[1]) |>
        mutate(from_top = ifelse(to_move, -from_top, from_top)) |>
        mutate(position = ifelse(to_move, instruction[3], position)) |>
        group_by(position) |>
        mutate(from_top = rank(from_top)) |>
        ungroup(position) |>
        select(-to_move)
    print(sprintf("Moved %d crates", instruction[1]))
})
data_1 |>
    filter(from_top == 1) |>
    arrange(position) |>
    summarise(solution = paste0(crate, collapse = "")) |>
    pull(solution)
#### Part two ####
data_2 <- data
lapply(instructions, function(instruction) {
    instruction <- as.integer(str_extract_all(instruction, "[0-9]+")[[1]])
    data_2 <<- data_2 |>
        mutate(to_move =
            position == instruction[2]
            & from_top <= instruction[1]) |>
        mutate(from_top = ifelse(
            to_move,
            from_top - instruction[1],
            from_top)) |>
        mutate(position = ifelse(to_move, instruction[3], position)) |>
        group_by(position) |>
        mutate(from_top = rank(from_top)) |>
        ungroup(position) |>
        select(-to_move)
    print(sprintf("Moved %d crates", instruction[1]))
})
data_2 |>
    filter(from_top == 1) |>
    arrange(position) |>
    summarise(solution = paste0(crate, collapse = "")) |>
    pull(solution)
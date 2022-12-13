library(tidyverse)
library(tidygraph)
library(ggraph)

data <- tibble(input = read_lines("input.txt")) |>
    mutate(row = row_number()) |>
    mutate(value = str_split(input, "")) |>
    unnest(value) |>
    group_by(row) |>
    mutate(column = row_number()) |>
    ungroup() |>
    select(-input)

graph <- {
    create_lattice(
        c(max(data$column), max(data$row)),
        directed = TRUE,
        mutual = TRUE) |>
    mutate(data)
}

nodes <- as_tibble(graph) |>
    mutate(elevation =
        case_when(
            value == "S" ~ 1L,
            value == "E" ~ 26L,
            TRUE ~ match(value, letters)))

paths <- graph |>
    activate("edges") |>
    filter(nodes$elevation[to] - nodes$elevation[from] <= 1) |>
    activate("nodes") |>
    mutate(distance = node_distance_to(which(nodes$value == "E"), mode = "out"))

#### Part one ####
solution_1 <- paths |>
    filter(value == "S") |>
    as_tibble() |>
    slice_min(distance, n = 1) |>
    pull(distance)

#### Part two ####
solution_2 <- paths |>
    filter(value == "a") |>
    as_tibble() |>
    slice_min(distance, n = 1) |>
    pull(distance)
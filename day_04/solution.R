library(tidyverse)

# Interpret input
data <- tibble(input = read_lines("input.txt"))
data <- data |>
    separate(
        input,
        into = c("area_1", "area_2"),
        sep = ","
    ) |>
    mutate(pair = 1:n()) |>
    pivot_longer(
        starts_with("area"),
        names_prefix = "area_",
        names_transform = as.integer,
        names_to = "elf",
        values_to = "areas"
    ) |>
    separate(
        areas,
        into = c("area_min", "area_max"),
        sep = "-"
    ) |>
    mutate(across(starts_with("area_"), as.integer))
glimpse(data)

# Identify subsets of areas among pairs
data |>
    pivot_wider(
        names_from = "elf",
        values_from = starts_with("area_")
    ) |>
    mutate(subset_1_of_2 = area_min_1 >= area_min_2 & area_max_1 <= area_max_2) |>
    mutate(subset_2_of_1 = area_min_2 >= area_min_1 & area_max_2 <= area_max_1) |>
    filter(subset_1_of_2 | subset_2_of_1) |>
    nrow()

#### Part Two ####
data |>
    pivot_wider(
        names_from = "elf",
        values_from = starts_with("area_")
    ) |>
    mutate(no_overlap = (area_max_1 < area_min_2) | (area_max_2 < area_min_1)) |>
    filter(!no_overlap) |>
    nrow()
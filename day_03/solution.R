library(tidyverse)

input <- read_lines("input.txt")
data <- tibble(
    content = input,
    content_id = 1:length(input)
    )

# Identify compartments
# Couldn't get tidyr::separate to work :(
data <- data |>
    mutate(compartment_size = as.integer(nchar(content) / 2)) |>
    mutate(compartment_1 = substr(content, 1, compartment_size)) |>
    mutate(compartment_2 = substr(content, compartment_size + 1, nchar(content)))
data <- data |>
    pivot_longer(
        compartment_1:compartment_2,
        names_to = "compartment",
        names_prefix = "compartment_",
        names_transform = as.integer,
        values_to = "compartment_contents")

data <- data |>
    mutate(items = str_split(compartment_contents, "")) |>
    unnest(items)

# Identify priority
data_priority <- tibble(
    items = c(letters, LETTERS),
    priority = 1:length(items)
)
data <- data |>
    left_join(data_priority, by = "items")

# Identify mistakes
data |>
    distinct() |>
    group_by(content_id) |>
    mutate(is_error = duplicated(items)) |>
    ungroup() |>
    filter(is_error) |>
    summarise(priority_total = sum(priority))

#### Part Two ####
data_groups <- tibble(
    content_id = 1:length(input),
    group = rep(seq(1, length(input) / 3), each = 3)
    )
data <- data |>
    left_join(data_groups, by = "content_id")

data |>
    select(content_id, items, priority, group) |>
    distinct() |>
    group_by(group) |>
    filter(duplicated(items)) |>
    filter(duplicated(items)) |>
    ungroup() |>
    summarise(priority_total = sum(priority))

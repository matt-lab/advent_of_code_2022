library(tidyverse)
signal <- read_lines("input.txt")
# Solution function
find_marker <- function(signal, window_size) {
    data <- tibble(
        window_end = seq(window_size, nchar(signal), by = 1),
        window_start = window_end - window_size + 1)
    solution <- data |>
        rowwise() |>
        mutate(marker = substr(signal, window_start, window_end)) |>
        mutate(marker_unique_n = 
            length(unique(unlist(str_split(marker, ""))))) |>
        filter(marker_unique_n == window_size) |>
        ungroup() |>
        slice_min(window_end) |>
        pull(window_end)
    return(solution)
}
#### Part one ####
solution_1 <- find_marker(signal, 4)
#### Part two ####
solution_2 <- find_marker(signal, 14)
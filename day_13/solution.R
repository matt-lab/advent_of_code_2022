library(tidyverse)

parse_as_list <- function(text) {
    new_text <- text |>
        str_replace_all("\\]", ")") |>
        str_replace_all("\\[", "list(")
    return(list(eval(parse(text = new_text))))
}

is_ordered <- function(left, right) {
    # Using the clever recursive solution of Antoine Fabri
    for (index in seq_len(max(length(left), length(right)))) {
        if (index > length(left)) {
            return(TRUE)
        }
        if (index > length(right)) {
            return(FALSE)
        }
        if (is.list(left[[index]]) || is.list(right[[index]])) {
            ordered <- is_ordered(
                as.list(left[[index]]),
                as.list(right[[index]]))
            if (length(ordered) != 0) {
                if (!is.na(ordered)) {
                    return(ordered)
                }
            }
            next
        }
        if (left[[index]] < right[[index]]) {
            return(TRUE)
        }
        if (left[[index]] > right[[index]]) {
            return(FALSE)
        }
    }
}

data <- {
    read_file("input.txt") |>
    enframe(name = NULL) |>
    separate_rows(value, sep = "\r\n\r\n") |>
    separate(value, into = c("left", "right"), sep = "\r\n") |>
    mutate(index = row_number()) |>
    rowwise() |>
    mutate(across(c(left, right), parse_as_list)) |>
    mutate(is_ordered = is_ordered(left, right)) |>
    ungroup()
}
data

#### Part one ###
solution_1 <- data |>
    filter(is_ordered) |>
    summarise(sum = sum(index)) |>
    pull(sum)

#### Part two ####
solution_2 <- data |>
    pivot_longer(
        cols = c(left, right),
        values_to = "packet",
        names_to = "location"
    ) |>
    rowwise() |>
    mutate(before_divider_1 = is_ordered(packet, list(list(2)))) |>
    mutate(before_divider_2 = is_ordered(packet, list(list(6)))) |>
    ungroup() |>
    summarise(
        divider_1_index = length(which(before_divider_1)) + 1,
        divider_2_index = length(which(before_divider_2)) + 2
    ) |>
    mutate(decoder_key = divider_1_index * divider_2_index) |>
    pull(decoder_key)
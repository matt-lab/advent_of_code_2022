library(tidyverse)
terminal <- read_lines("input.txt")
memory <- tibble(
    path = as.list(as.list("character")),
    file = as.character(),
    size = as.integer()
)
# I'm never touching nested lists again
for (line in terminal) {
    if (str_detect(line, "^\\$ cd")) {
        if (str_detect(line, "\\$ cd \\/")) {
            current_directory <- list("/")
        }
        if (str_detect(line, "^\\$ cd ..$")) {
            current_directory[[length(current_directory)]] <- NULL
        }
        if (str_detect(line, "^\\$ cd [:alnum:]+$")) {
            current_directory <- current_directory |>
                c(list(c(
                    unlist(tail(current_directory, 1)),
                    str_remove(line, "\\$ cd "))))
        }
    }
    if (str_detect(line, "^[0-9]+ ")) {
        # Output from ls for file
        memory <- memory |>
            add_case(
                path = current_directory,
                file = str_remove(line, "^[0-9]+ "),
                size = as.integer(str_extract(line, "^[0-9]+")))
    }
}
memory <- memory |>
    distinct() |>
    group_by(path) |>
    summarise(size = sum(size))
#### Part one ####
memory |>
    filter(size <= 100000) |>
    summarise(size = sum(size)) |>
    pull(size)
#### Part two ####
space_required <- 30000000 - (70000000 - max(memory$size))
memory |>
    filter(size >= space_required) |>
    slice_min(size, n = 1) |>
    pull(size)

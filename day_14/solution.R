library(stringr)

rock_sequence <- {
    readLines("input.txt") |>
    str_split(" -> ") |>
    lapply(str_split, ",", simplify = TRUE)
}

rock_max <- do.call('rbind', rock_sequence) |>
    apply(2, as.integer) |>
    apply(2, max)
locations <- matrix(0, rock_max[2] + 3, rock_max[1] + 500)

rock_sequence <- rock_sequence |>
    append(list(matrix(c(1, rock_max[1] + 500, rep(rock_max[2] + 2, 2)), 2, 2)))

for (rock_position in seq_along(rock_sequence)) {
    rock_location <- rock_sequence[[rock_position]]
    for (rock in 2:nrow(rock_location)) {
        x_locations <- seq(rock_location[rock - 1, 1], rock_location[rock, 1])
        y_locations <- seq(rock_location[rock - 1, 2], rock_location[rock, 2])
        locations[y_locations + 1, x_locations] <- 1
    }
}

count_sand <- function(cave) {
    locations <- cave
    voided_sand <- FALSE
    while (!voided_sand) {
        sand_row <- 1
        sand_column <- 500
        sand_falls <- locations[sand_row, sand_column] == 0
        if (!sand_falls) {voided_sand <- TRUE}
        locations[sand_row, sand_column] <- -1
        while (sand_falls) {
            locations[sand_row, sand_column] <- 0
            if (nrow(locations) == sand_row) {
                sand_falls <- FALSE
                voided_sand <- TRUE
                break
            }

            sand_row <- sand_row + 1
            column_adjust <- c(0, -1, 1)
            is_air <- locations[sand_row, sand_column + column_adjust] == 0
            sand_falls <- !all(!is_air)

            if (sand_falls) {
                sand_column <- sand_column + column_adjust[which.max(is_air)]
                locations[sand_row, sand_column] <- -1
            } else {
                locations[sand_row - 1, sand_column] <- -1
            }
        }
    }
    return(length(unlist(which(locations == -1))))
}

#### Part one ####

solution_1 <- count_sand(locations[seq(1, nrow(locations) - 2), ])

#### Part two ####

solution_2 <- count_sand(locations)
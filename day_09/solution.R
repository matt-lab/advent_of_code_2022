input <- readLines("input.txt")

position_x_head <- lapply(input, function(move) {
    times <- as.integer(gsub("^[A-Z]+ ", "", move))
    direction <- ifelse(grepl("^U|^D", move), 0, ifelse(grepl("^R", move), 1, -1))
    rep(direction, times)
})
position_y_head <- lapply(input, function(move) {
    times <- as.integer(gsub("^[A-Z]+ ", "", move))
    direction <- ifelse(grepl("^L|^R", move), 0, ifelse(grepl("^U", move), 1, -1))
    rep(direction, times)
})

position_x <- matrix(0, nrow = length(unlist(position_x_head)), ncol = 10)
position_y <- matrix(0, nrow = length(unlist(position_y_head)), ncol = 10)
position_x[, 1] <- cumsum(unlist(position_x_head))
position_y[, 1] <- cumsum(unlist(position_y_head))

for (knot in seq(from = 2, to = ncol(position_x))) {
    for (time in seq(from = 2, to = nrow(position_x))) {
        tension_x <- position_x[time, knot - 1] - position_x[time - 1, knot]
        tension_y <- position_y[time, knot - 1] - position_y[time - 1, knot]
        if (max(abs(tension_x), abs(tension_y)) == 2) {
            position_x[time, knot] <-
                sign(tension_x) + position_x[time - 1, knot]
            position_y[time, knot] <-
                sign(tension_y) + position_y[time - 1, knot]
        } else {
            position_x[time, knot] <- position_x[time - 1, knot]
            position_y[time, knot] <- position_y[time - 1, knot]
        }
    }
}

#### Part one ###
solution_1 <- cbind(position_x[, 2], position_y[, 2]) |>
    unique() |>
    nrow()
solution_1

#### Part two ###
solution_2 <- cbind(
        position_x[, ncol(position_x)],
        position_y[, ncol(position_y)]) |>
    unique() |>
    nrow()
solution_2
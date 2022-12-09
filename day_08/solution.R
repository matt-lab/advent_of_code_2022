# Let's try base today
input <- do.call('rbind', lapply(strsplit(readLines("input.txt"), NULL), as.integer)) # nolint
inputs <- list(input, t(input))
#### Part one ####
seen_trees <- lapply(inputs, function(trees) {
    apply(trees, 1, function(tree_row) {
       c(1, diff(cummax(tree_row))) > 0 | rev(c(1, diff(cummax(rev(tree_row)))))
    })
})
seen_total <- length(which(seen_trees[[1]] | t(seen_trees[[2]])))
#### Part two ####
# Couldn't quite get this working, this will do for now
scenic <- lapply(inputs, function(trees) {
    apply(trees, 1, function(tree_row) {
        scenic_scores <- integer(length(tree_row))
        for (tree in 2:(length(tree_row) - 1)) {
            see_trees_1 <- cumsum(rev(diff(tree_row[1:tree]))) > 0
            see_trees_2 <- cumsum(rev(diff(rev(tree_row[tree:length(tree_row)])))) > 0
            score_1 <- ifelse(all(see_trees_1), length(see_trees_1), which.min(see_trees_1))
            score_2 <- ifelse(all(see_trees_2), length(see_trees_2), which.min(see_trees_2))
            scenic_scores[tree] <- score_1 * score_2
        }
        return(scenic_scores)
    })
})
scenic_scores <- scenic[[1]] * t(scenic[[2]])
scenic_scores_max <- max(scenic_scores)
library(tidyverse)

monkey <- {
  read_file("input.txt") |>
  enframe(name = NULL) |>
  mutate(value = str_to_lower(value)) |>
  separate_rows(value, sep = "\r\n\r\n") |>
  separate("value",
    into = c(
        "monkey", "items_start", "operation",
        "test", "outcome_true", "outcome_false"),
    sep = "\r\n") |>
    mutate(across(everything(), str_remove, "^[:space:]+|[:space:]$"))
}

monkey <- monkey |>
    mutate(monkey = as.integer(str_extract(monkey, "[0-9]"))) |>
    mutate(items_start = str_remove(items_start, "starting items: ")) |>
    mutate(items_start = str_split(items_start, ", ")) |>
    mutate(operation_multiply =
        as.integer(
            str_extract(
                str_extract(operation, "\\* [0-9]+"),
                "[0-9]+"))) |>
    mutate(operation_multiply = replace_na(operation_multiply, 1)) |>
    mutate(operation_addition =
        as.integer(
            str_extract(
                str_extract(operation, "\\+ [0-9]+"),
                "[0-9]+"))) |>
    mutate(operation_addition = replace_na(operation_addition, 0)) |>
    mutate(operation_power = str_count(operation, "\\* old") + 1) |>
    mutate(
        across(
            c(test, outcome_true, outcome_false),
            ~ as.integer(str_extract(.x, "[0-9]+$"))))

items_start <- monkey |>
    select(monkey, items_start) |>
    unnest(items_start) |>
    rename(worry = items_start) |>
    mutate(worry = as.integer(worry))
monkey <- monkey |>
    select(-items_start, -operation)

#### Part one ####
# Let the monkey business begin
items <- items_start
rounds <- 20
monkey_business <- integer(nrow(monkey)) # won't line up with monkey data

for (round in seq(rounds)) {
    for (monkey_num in (seq_along(monkey_business) - 1)) {
        inspected_items <- items |>
            filter(monkey == monkey_num) |>
            left_join(monkey, by = "monkey") |>
            mutate(worry = worry
                ^ operation_power
                * operation_multiply
                + operation_addition) |>
            mutate(worry = floor(worry / 3)) |>
            mutate(outcome = worry %% test == 0) |>
            mutate(new_monkey = ifelse(outcome, outcome_true, outcome_false))
        thrown_items <- inspected_items |>
            select(worry, new_monkey) |>
            rename(monkey = new_monkey)
        monkey_business[monkey_num + 1] <- {
            monkey_business[monkey_num + 1] +
            nrow(inspected_items)
        }
        items <- items |>
            filter(monkey != monkey_num) |>
            add_case(thrown_items)
    }
}

solution_1 <- cumprod(rev(sort(monkey_business)))[2]

#### Part two ####
items <- items_start
rounds <- 10000
monkey_business <- integer(nrow(monkey)) # won't line up with monkey data
div <- prod(monkey$test)

for (round in seq(rounds)) {
    for (monkey_num in (seq_along(monkey_business) - 1)) {
        inspected_items <- items |>
            filter(monkey == monkey_num)
        if (nrow(inspected_items) > 0) {
        inspected_items <- inspected_items |>
            left_join(monkey, by = "monkey") |>
            mutate(worry = worry
                ^ operation_power
                * operation_multiply
                + operation_addition) |>
            mutate(worry = worry %% div) |>
            mutate(outcome = worry %% test == 0) |>
            mutate(new_monkey = ifelse(outcome, outcome_true, outcome_false))
        thrown_items <- inspected_items |>
            select(worry, new_monkey) |>
            rename(monkey = new_monkey)
        monkey_business[monkey_num + 1] <- {
            monkey_business[monkey_num + 1] +
            nrow(inspected_items)
        }
        items <- items |>
            filter(monkey != monkey_num) |>
            add_case(thrown_items)
        }
    }
}

solution_2 <- prod(rev(sort(monkey_business))[1:2])
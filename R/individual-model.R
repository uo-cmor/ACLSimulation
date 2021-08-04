update_state <- function(initial_state, transitions) {
  n <- nrow(transitions)
  sample.int(n, 1, prob = transitions[initial_state, ])
}

simulate_individual <- function(initial_state, transitions, t) {
  states <- vector("integer", length = t + 1)
  states[1] <- initial_state

  for (x in seq_len(t)) {
    states[x + 1] <- update_state(states[x], transitions)
  }

  states
}

run_individual_model <- function(initial_states, transitions, t) {
  n <- length(initial_states)

  states <- matrix(nrow = n, ncol = t + 1)
  for (x in seq_len(n)) {
    states[x, ] <- simulate_individual(initial_states[x], transitions, t)
  }

  states
}

calculate_individual_outcome_value <- function(states, values) {
  matrix(values[states], nrow = nrow(states))
}

calculate_average_outcomes <- function(states, values, discount_rate = 0) {
  outcomes <- calculate_individual_outcome_value(states, values)
  n <- nrow(outcomes)
  t <- ncol(outcomes)
  for (x in seq_len(n)) {
    outcomes[x, ] <- outcomes[x, ] * (1 - discount_rate) ^ ((1:t) - 1)
  }
  mean(rowSums(outcomes))
}

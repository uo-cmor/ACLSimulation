update_probabilities <- function(probs, transitions) {
  as.vector(probs %*% transitions)
}

run_cohort_model <- function(initial_probs, transitions, t) {
  probabilities <- matrix(nrow = t + 1, ncol = length(initial_probs))
  probabilities[1, ] <- initial_probs

  for (x in seq_len(t)) {
    probabilities[x + 1, ] <- update_probabilities(probabilities[x, ],
                                                   transitions)
  }

  probabilities
}

calculate_outcome_value <- function(probs, values) {
  sum(probs * values)
}

calculate_outcomes <- function(probabilities, values, discount_rate = 0) {
  t <- nrow(probabilities)
  sum((probabilities %*% values) * (1 - discount_rate) ^ ((1:t) - 1))
}

update_probabilities <- function(probs, transitions) {
  new_probs <- numeric(length(probs))
  for (y in seq_along(new_probs)) {
    for (x in seq_along(probs)) {
      new_probs[y] <- new_probs[y] + probs[x] * transitions[x, y]
    }
  }

  new_probs
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

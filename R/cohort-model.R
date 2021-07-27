update_probabilities <- function(probs, transitions) {
  new_probs <- numeric(length(probs))
  for (y in seq_along(new_probs)) {
    for (x in seq_along(probs)) {
      new_probs[y] <- new_probs[y] + probs[x] * transitions[x, y]
    }
  }

  new_probs
}

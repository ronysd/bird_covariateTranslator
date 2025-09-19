#  cv for height Function
cv <- function(x) {
  y <- na.omit(sample(x, size = min(10, length(x)), replace = FALSE))
  if (length(y) < 2) return(NA)
  return(sd(y) / mean(y))
}
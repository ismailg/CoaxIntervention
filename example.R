# Create a synthetic dataset
set.seed(42)
n <- 300
states <- c(rep(1, n/3), rep(2, n/3), rep(3, n/3))
y <- numeric(n)
x <- numeric(n)

y[states == 1] <- rnorm(sum(states == 1), mean = 0, sd = 1)
y[states == 2] <- rnorm(sum(states == 2), mean = 5, sd = 1)
y[states == 3] <- rnorm(sum(states == 3), mean = 10, sd = 1)

x[states == 1] <- sample(0:1, size = sum(states == 1), prob = c(0.7, 0.3), replace = TRUE)
x[states == 2] <- sample(0:1, size = sum(states == 2), prob = c(0.3, 0.7), replace = TRUE)
x[states == 3] <- sample(0:1, size = sum(states == 3), prob = c(0.5, 0.5), replace = TRUE)

data <- data.frame(y = y, x = x)

# Fit an HMM model with a binary covariate in the transition function
hmm <- depmix(y ~ 1, data = data, nstates = 3, family = gaussian(),
              transition = ~ x)
fitted_model <- fit(hmm)

# Get the estimates of the transition function
transition_pars <- getpars(fitted_model, which = "transition")

# Get the covariance matrix of the transition function parameters
cov_matrix <- vcov(fitted_model, which = "transition")


# Find the indices of the binary covariate coefficients in the transition function
binary_covariate_indices <- c(4, 8)

# Wald tests
for (i in binary_covariate_indices) {
  estimate <- transition_pars[i]
  standard_error <- sqrt(cov_matrix$vcov[i, i])
  wald_statistic <- (estimate / standard_error)^2
  p_value <- pchisq(wald_statistic, df = 1, lower.tail = FALSE)
  
  cat("Wald statistic (", i, "):", wald_statistic, "\n")
  cat("p-value (", i, "):", p_value, "\n")
  
  # Interpret the result
  alpha <- 0.05
  if (p_value < alpha) {
    cat("The binary covariate coefficient (", i, ") is significantly different from 0.\n")
  } else {
    cat("The binary covariate coefficient (", i, ") is not significantly different from 0.\n")
  }
}

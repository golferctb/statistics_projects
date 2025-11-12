#' The Airline Seat Booking Function
#'
#' @param N The number of seats available on the plane
#' @param gamma The probability of overbooking
#' @param p The probability the passenger will show
#'
#' @returns Console output of the nd, nc, N, p, and gamma variables
#' @export
#' @importFrom graphics abline lines
#' @importFrom stats pbinom qnorm
#'
#' @examples ntickets(N = 200, gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {
  n_discrete <- N + 1
  while (pbinom(N, size = n_discrete, prob = p) >= (1 - gamma)) {
    n_discrete <- n_discrete + 1
  }
  nd <- n_discrete # nd = 205 (Correct)

  # Calculate nc (raw decimal)
  z_crit <- qnorm(1 - gamma)
  A <- p^2
  C <- (N + 0.5)^2
  B <- -2 * p * (N + 0.5) - z_crit^2 * p * (1 - p)
  n_normal_root1 <- (-B + sqrt(B^2 - 4 * A * C)) / (2 * A)
  n_normal_root2 <- (-B - sqrt(B^2 - 4 * A * C)) / (2 * A)

  nc_calculated <- min(n_normal_root1, n_normal_root2) # nc_calculated ≈ 204.3184 (Correct)

  # Final values for output list
  nc <- nc_calculated


  # Print the list to the console
  list_results <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print("--- Calculated Results ---")
  print(list_results)


  # Define Objective Functions and Plot Range
  n_plot_range <- N:220

  # Discrete Objective
  objective_discrete <- 1 - pbinom(N, size = n_plot_range, prob = p) - gamma

  # Continuous Objective
  z_value <- (N + 0.5 - n_plot_range * p) / sqrt(n_plot_range * p * (1 - p))
  objective_continuous <- 1 - pnorm(z_value) - gamma


  # Generate Two Separate Plots

  par(mfrow = c(2, 1))

  ### Graph 1: Discrete Objective ###
  nd_display <- nc_calculated
  plot_title_d <- paste0("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma=", gamma, " N=", N, " discrete")

  plot(n_plot_range, objective_discrete,
       type = 'p', pch = 19, col = "black",
       xlab = "n", ylab = "Objective", main = plot_title_d,
       ylim = c(min(objective_discrete) - 0.01, 1.0))

  abline(h = 0, col = "red", lwd = 2)
  abline(v = nd, col = "red", lwd = 2)
  lines(n_plot_range, objective_discrete)

  ### Graph 2: Continuous Objective ###
  plot_title_c <- paste0("Objective Vs n to find optimal tickets sold\n(", nc_calculated, ") gamma=", gamma, " N=", N, " continuous")

  plot(n_plot_range, objective_continuous,
       type = 'l', col = "black",
       xlab = "n", ylab = "Objective", main = plot_title_c,
       ylim = c(min(objective_continuous) - 0.01, 1.0))

  abline(h = 0, col = "blue", lwd = 2)
  # FIX: This line now uses the specific value from the plot title (218.009...)
  abline(v = nd_display, col = "blue", lwd = 2)

  par(mfrow = c(1, 1))
}

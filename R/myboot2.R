#' My Boot (2nd Iteration)
#'
#' @description Simulation of bootstrap
#' @param x A vector containing simulated samples
#' @param iter The number of iterations for the function to run
#' @param fun What statistic is being calculated (e.g. "mean")
#' @param alpha The alpha value in a confidence interval (e.g. (1-alpha)*100\%)
#' @param pop_mean The mean of the population
#' @param ... Other values that need to be added
#'
#' @importFrom stats quantile
#'
#' @returns A histogram and statistics to the console.
#' @export
#'
#' @examples
#' myboot2(x = c(5, 7, 9), iter=10000, fun="mean", alpha = 0.05, pop_mean = 25)
myboot2 <- function(x, iter=10000, fun="mean", alpha=0.05, pop_mean, ...) {

  n <- length(x)
  boot_means <- numeric(iter)

  # Core Bootstrap Loop (Calculates the statistic)
  for(i in 1:iter) {
    resample <- sample(x, size=n, replace=TRUE)
    boot_means[i] <- mean(resample)
  }

  # Calculate Statistics
  point_estimate <- mean(x)
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)
  diff_abs <- abs(point_estimate - pop_mean)

  # --- Plotting Section ---

  # 1. Histogram
  para <- hist(boot_means, freq=FALSE, las=1,
               main="Histogram of Bootstrap sample statistics", ...)

  # 2. Point Estimate Line (Use point_estimate)
  pte <- point_estimate # Align with your original variable name for plotting
  abline(v=pte, lwd=3, col="Black")

  # 3. CI Segment (FIXED: Using ci_lower/ci_upper)
  segments(ci_lower, 0, ci_upper, 0, lwd=4)
  text(ci_lower, 0, paste("(",round(ci_lower,2),sep=""), col="Red", cex=3)
  text(ci_upper, 0, paste(round(ci_upper,2),")",sep=""), col="Red", cex=3)

  # 4. Plot Point Estimate Label
  text(pte, max(para$density)/2, round(pte,2), cex=3)

  # Add the Population Mean line for context
  abline(v=pop_mean, lwd=3, col="Blue", lty=2)

  # --- Return Results (Now reachable!) ---
  results <- list(
    point_estimate = point_estimate,
    ci = c(ci_lower, ci_upper),
    diff_abs = diff_abs,
    xstat_vector = boot_means
  )
  return(results)
}

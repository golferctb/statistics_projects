# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stats)

# --- Define Log-Likelihood Functions (Negative for optimization) ---

# 1. Normal Distribution
nll_normal <- function(params, data) {
  mu <- params[1]
  sigma <- params[2]
  if (sigma <= 0) return(1e20) # Constraint check
  # Negative Log-Likelihood
  -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# 2. Exponential Distribution
nll_exponential <- function(params, data) {
  lambda <- params[1]
  if (lambda <= 0) return(1e20) # Constraint check
  # Negative Log-Likelihood
  -sum(dexp(data, rate = lambda, log = TRUE))
}

# 3. Poisson Distribution
nll_poisson <- function(params, data) {
  lambda <- params[1]
  if (lambda <= 0) return(1e20)
  # Negative Log-Likelihood
  -sum(dpois(data, lambda = lambda, log = TRUE))
}

# 4. Binomial Distribution (requires N (size) to be passed, here N_fixed)
nll_binomial <- function(params, data, N_fixed) {
  prob <- params[1]
  if (prob < 0 || prob > 1) return(1e20)
  # Negative Log-Likelihood
  -sum(dbinom(data, size = N_fixed, prob = prob, log = TRUE))
}

# 5. Uniform Distribution
# Note: MLE for Uniform (min/max) is non-calculus based and computationally trivial.
# We will use the sample min/max directly for the estimate and skip 'optim' for this.

# --- SHINY UI ---
ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation (MLE) Demonstrator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("1. Select Distribution & Sample"),
      selectInput("dist_choice", "Distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Binomial", "Uniform")),
      
      sliderInput("n_samples", "Sample Size (n):", min = 20, max = 500, value = 100, step = 10),
      
      h3("2. True Parameters (to generate data)"),
      # Dynamic UI for parameter inputs
      uiOutput("param_inputs"),
      
      actionButton("generate_data", "Generate Data & Estimate MLE", 
                   class = "btn-primary", width = '100%'),
      hr(),
      p("Click 'Generate Data' to draw a new random sample and calculate the MLE.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Visualization & Density", plotOutput("density_plot")),
        tabPanel("Log-Likelihood Surface", plotOutput("likelihood_plot")),
        tabPanel("MLE Results", verbatimTextOutput("mle_results"))
      )
    )
  )
)

# --- SHINY SERVER ---
server <- function(input, output, session) {
  
  # Reactive value to store data and MLE results
  r_values <- reactiveValues(data = NULL, mle = NULL, true_params = NULL)
  
  # 1. Dynamic UI for True Parameters
  output$param_inputs <- renderUI({
    switch(input$dist_choice,
           "Normal" = tagList(
             sliderInput("true_mu", "True Mean (\u03BC):", min = -5, max = 5, value = 0, step = 0.5),
             sliderInput("true_sigma", "True Std Dev (\u03C3):", min = 0.1, max = 3, value = 1, step = 0.1)
           ),
           "Exponential" = sliderInput("true_rate", "True Rate (\u03BB):", min = 0.1, max = 2, value = 0.5, step = 0.1),
           "Poisson" = sliderInput("true_lambda", "True Lambda (\u03BB):", min = 0.5, max = 10, value = 4, step = 0.5),
           "Binomial" = tagList(
             sliderInput("true_n", "Binomial Size (N):", min = 5, max = 30, value = 15, step = 1),
             sliderInput("true_p", "True Probability (p):", min = 0.1, max = 0.9, value = 0.5, step = 0.05)
           ),
           "Uniform" = tagList(
             sliderInput("true_min", "True Min (a):", min = -10, max = 0, value = -5, step = 1),
             sliderInput("true_max", "True Max (b):", min = 0, max = 10, value = 5, step = 1)
           )
    )
  })
  
  # 2. Data Generation and MLE Calculation
  observeEvent(input$generate_data, {
    req(input$dist_choice, input$n_samples)
    
    dist <- input$dist_choice
    n <- input$n_samples
    
    current_data <- NULL
    mle_result <- NULL
    
    switch(dist,
           "Normal" = {
             req(input$true_mu, input$true_sigma)
             current_data <- rnorm(n, mean = input$true_mu, sd = input$true_sigma)
             r_values$true_params <- c(mu = input$true_mu, sigma = input$true_sigma)
             
             # Numerical MLE using optim
             fit <- optim(par = c(mean(current_data), sd(current_data)),
                          fn = nll_normal,
                          data = current_data,
                          hessian = TRUE)
             
             mle_result <- c(mu_hat = fit$par[1], sigma_hat = fit$par[2])
           },
           
           "Exponential" = {
             req(input$true_rate)
             current_data <- rexp(n, rate = input$true_rate)
             r_values$true_params <- c(rate = input$true_rate)
             
             # Numerical MLE using optim
             fit <- optim(par = 1/mean(current_data),
                          fn = nll_exponential,
                          data = current_data,
                          method = "L-BFGS-B", lower = 1e-6,
                          hessian = TRUE)
             
             mle_result <- c(rate_hat = fit$par[1])
           },
           
           "Poisson" = {
             req(input$true_lambda)
             current_data <- rpois(n, lambda = input$true_lambda)
             r_values$true_params <- c(lambda = input$true_lambda)
             
             # Numerical MLE using optim
             # We use a single-parameter optimization method for simplicity
             fit <- optimize(f = function(lambda) nll_poisson(lambda, current_data), 
                             interval = c(0.01, max(current_data) * 2))
             
             mle_result <- c(lambda_hat = fit$minimum)
           },
           
           "Binomial" = {
             req(input$true_n, input$true_p)
             N_fixed <- input$true_n
             current_data <- rbinom(n, size = N_fixed, prob = input$true_p)
             r_values$true_params <- c(N = N_fixed, p = input$true_p)
             
             # Numerical MLE using optim
             fit <- optimize(f = function(p) nll_binomial(p, current_data, N_fixed), 
                             interval = c(0.01, 0.99))
             
             mle_result <- c(p_hat = fit$minimum, N_fixed = N_fixed)
           },
           
           "Uniform" = {
             req(input$true_min, input$true_max)
             current_data <- runif(n, min = input$true_min, max = input$true_max)
             r_values$true_params <- c(min = input$true_min, max = input$true_max)
             
             # Direct MLE for Uniform: min(X) and max(X)
             mle_result <- c(min_hat = min(current_data), max_hat = max(current_data))
           }
    )
    
    r_values$data <- current_data
    r_values$mle <- mle_result
  })
  
  # 3. Data Visualization and Estimated Density Plot
  output$density_plot <- renderPlot({
    req(r_values$data, r_values$mle)
    
    data <- r_values$data
    mle <- r_values$mle
    dist <- input$dist_choice
    
    p <- ggplot(data.frame(x = data), aes(x = x)) +
      labs(title = paste("Data Histogram and Estimated Density (", dist, ")"),
           x = "Observed Value", y = "Density") +
      theme_minimal(base_size = 14)
    
    if (dist %in% c("Normal", "Exponential", "Uniform")) {
      p <- p + 
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#1f78b4", color = "white", alpha = 0.7)
      
      # Add estimated density line
      x_range <- seq(min(data), max(data), length.out = 500)
      
      if (dist == "Normal") {
        y_density <- dnorm(x_range, mean = mle["mu_hat"], sd = mle["sigma_hat"])
      } else if (dist == "Exponential") {
        y_density <- dexp(x_range, rate = mle["rate_hat"])
      } else if (dist == "Uniform") {
        y_density <- dunif(x_range, min = mle["min_hat"], max = mle["max_hat"])
      }
      
      p <- p + geom_line(data = data.frame(x = x_range, y = y_density), 
                         aes(x = x, y = y), color = "#e31a1c", linewidth = 1.5)
      
    } else if (dist %in% c("Poisson", "Binomial")) {
      # Discrete distributions need a different visualization (bar plot)
      p <- ggplot(data.frame(x = data), aes(x = x)) +
        geom_bar(aes(y = after_stat(prop)), fill = "#1f78b4", color = "white", alpha = 0.7) +
        labs(title = paste("Data Distribution and Estimated Probabilities (", dist, ")"),
             x = "Observed Count", y = "Proportion / Probability") +
        theme_minimal(base_size = 14)
      
      x_range_int <- sort(unique(data))
      if (dist == "Poisson") {
        x_range_int <- min(data):max(data)
        y_prob <- dpois(x_range_int, lambda = mle["lambda_hat"])
      } else if (dist == "Binomial") {
        x_range_int <- 0:mle["N_fixed"]
        y_prob <- dbinom(x_range_int, size = mle["N_fixed"], prob = mle["p_hat"])
      }
      
      p <- p + geom_point(data = data.frame(x = x_range_int, y = y_prob),
                          aes(x = x, y = y), color = "#e31a1c", size = 4) +
        geom_segment(data = data.frame(x = x_range_int, y = y_prob),
                     aes(x = x, y = 0, xend = x, yend = y),
                     color = "#e31a1c", linewidth = 1, linetype = "dashed")
    }
    
    print(p)
  })
  
  # 4. Log-Likelihood Surface Plot
  output$likelihood_plot <- renderPlot({
    req(r_values$data, r_values$mle)
    
    data <- r_values$data
    mle <- r_values$mle
    dist <- input$dist_choice
    
    p <- NULL
    
    if (dist == "Normal") {
      # Bivariate plot (mu and sigma) - requires contour/surface
      mu_hat <- mle["mu_hat"]
      sigma_hat <- mle["sigma_hat"]
      
      mu_grid <- seq(mu_hat - 3*sd(data), mu_hat + 3*sd(data), length.out = 50)
      sigma_grid <- seq(max(0.1, sigma_hat - 0.5), sigma_hat + 0.5, length.out = 50)
      
      grid <- expand.grid(mu = mu_grid, sigma = sigma_grid)
      
      grid$LogLik <- apply(grid, 1, function(p) -nll_normal(p, data))
      
      p <- ggplot(grid, aes(x = mu, y = sigma, z = LogLik)) +
        geom_contour_filled(bins = 15, alpha = 0.8) +
        geom_point(aes(x = mu_hat, y = sigma_hat), color = "black", size = 5, shape = 4, stroke = 2) +
        labs(title = "Log-Likelihood Surface for Normal Distribution",
             subtitle = "Black 'x' marks the MLE",
             x = "Mean (\u03BC)", y = "Standard Deviation (\u03C3)") +
        scale_fill_viridis_d(name = "Log-Likelihood") +
        theme_minimal(base_size = 14)
      
    } else if (dist %in% c("Exponential", "Poisson", "Binomial")) {
      # Univariate plot
      if (dist == "Exponential") {
        param_name <- "Rate (\u03BB)"
        param_hat <- mle["rate_hat"]
        param_grid <- seq(max(0.01, param_hat * 0.5), param_hat * 2, length.out = 100)
        LogLik <- sapply(param_grid, function(p) -nll_exponential(p, data))
      } else if (dist == "Poisson") {
        param_name <- "Lambda (\u03BB)"
        param_hat <- mle["lambda_hat"]
        param_grid <- seq(max(0.01, param_hat * 0.5), param_hat * 2, length.out = 100)
        LogLik <- sapply(param_grid, function(p) -nll_poisson(p, data))
      } else if (dist == "Binomial") {
        param_name <- "Probability (p)"
        param_hat <- mle["p_hat"]
        N_fixed <- mle["N_fixed"]
        param_grid <- seq(max(0.01, param_hat * 0.5), min(0.99, param_hat * 1.5), length.out = 100)
        LogLik <- sapply(param_grid, function(p) -nll_binomial(p, data, N_fixed))
      }
      
      df_ll <- data.frame(param = param_grid, LogLik = LogLik)
      
      p <- ggplot(df_ll, aes(x = param, y = LogLik)) +
        geom_line(color = "#1f78b4", linewidth = 1.2) +
        geom_point(aes(x = param_hat, y = max(LogLik)), color = "#e31a1c", size = 5) +
        geom_vline(xintercept = param_hat, linetype = "dashed", color = "#e31a1c") +
        labs(title = paste("Log-Likelihood Curve for", dist, "Distribution"),
             subtitle = "Red point marks the Maximum Likelihood Estimate (MLE)",
             x = param_name, y = "Log-Likelihood") +
        theme_minimal(base_size = 14)
      
    } else if (dist == "Uniform") {
      # Uniform MLE is a boundary estimate. Likelihood is 1/(b-a)^n for a<=x_i<=b, 0 otherwise.
      # Plotting the surface is less instructive here, so we show an informative message.
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, size = 6, 
                 label = paste("The Uniform Likelihood is a step function (1/(b-a)^n) that is maximized",
                               "where 'a' is just below the minimum observation and 'b' is just above the maximum observation.",
                               "\n\n\u2192 The MLE is directly calculated as \u00E2 = min(X) and \u00EA = max(X) \u2190"),
                 color = "#1f78b4") +
        theme_void()
    }
    
    print(p)
  })
  
  # 5. Display Results
  output$mle_results <- renderPrint({
    req(r_values$data, r_values$mle)
    
    cat("--- Maximum Likelihood Estimation Results ---\n")
    cat("Distribution:", input$dist_choice, "\n")
    cat("Sample Size (n):", input$n_samples, "\n\n")
    
    cat("--- True Parameters (used for data generation) ---\n")
    print(r_values$true_params)
    cat("\n")
    
    cat("--- Maximum Likelihood Estimates (\u00CA) ---\n")
    print(r_values$mle)
    cat("\n")
    
    # Simple summary statistics of the data for comparison
    cat("--- Sample Summary ---\n")
    print(summary(r_values$data))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
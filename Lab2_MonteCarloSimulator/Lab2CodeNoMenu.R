# Kevin Deshayes DVGHI 23HT
# kede23@student.bth.se

library(moments)
library(ggplot2)

Log_Rayleigh_likelihood= function(sd, mu) {
    n = length(sd) #nrOf observations
    result = n*log(pi) + sum(log(sd)) - n*log(2) - 2*n*log(mu) - (pi* sum(sd^2))/(4*mu^2) 
    return(result)
}

numericalDerivative = function(f,data){
     # Ensure data is numeric
    if (!is.numeric(data)) {
        stop("Error: Data is not numeric.")
    }


    result = optim(
    par=5,
    fn = function(mu) f(data, mu),  # Optimize over mu, passing data via closure 
    method = "L-BFGS-B", #BFGS with restrictions
    lower = 0.001,  # Set a lower bound to avoid non-positive mu values
    control = list(fnscale= -1))
    
    return(result)
}

analyticalDerivative = function(mu,y){
    n = length(y)
    (-2*n)/mu + (pi * sum(y^2))/(2*mu^3)
}

analyticalMaximizing = function(y) {
    result = optim(par = 5, 
                   fn = function(mu) Log_Rayleigh_likelihood(y, mu), # Objective function
                   gr = function(mu) analyticalDerivative(mu, y),    # Gradient function
                   method = "L-BFGS-B", #BFGS with restrictions
                   lower = 0.001,  # Set a lower bound to avoid non-positive mu values
                   control = list(fnscale = -1))
    return(result)
}

rr = function(mu, n) {
    u = runif(n)
    r_samples = 2 * mu * sqrt(-log(1 - u) / pi)
    return(r_samples)
}

calc_bias = function(estimates, true_value){
    return(mean(estimates) - true_value)
}


calc_mse = function(estimates, true_value){
    return(mean((estimates) - true_value)^2)
}


calculate_metrics = function(estimates, true_value){
    mean_estimate = mean(estimates)
    bias = calc_bias(estimates,true_value)
    mse = calc_mse(estimates,true_value)
    skewness_val = skewness(estimates)
    kurtosis_val = kurtosis(estimates) 

    return(list(mean = mean_estimate, bias = bias, mse=mse, skewness = skewness_val,kurtosis=kurtosis_val))
}

computation =function(n)
{
    r_values = rr(mu = 5, n) #Generate random sample with n in size from n_values
   

    log_likelihood = Log_Rayleigh_likelihood(r_values, mu = 5) # mu is an initial guess

    numerical_result = numericalDerivative(Log_Rayleigh_likelihood, r_values)

    analytical_maximization = analyticalMaximizing(r_values)

    return(list(
        numerical_result = numerical_result, 
        analytical_maximization = analytical_maximization
    ))
}

clean_results = function(result, true_mu) {
    converged_estimates = list()

    # Filter out only converged results
    for (i in seq_along(result)) {
        res = result[[i]]
        
        # Check if 'res' is a list and contains 'convergence'
        if (is.list(res) && !is.null(res[["convergence"]]) && res[["convergence"]] == 0) {
            converged_estimates[[i]] = res$par  # Store the estimated parameter
        }
    }
    
    # Convert the list of converged estimates to a numeric vector
    converged_estimates = unlist(converged_estimates)

    if (length(converged_estimates) > 0) {
        # Calculate performance metrics
        metrics = calculate_metrics(converged_estimates, true_mu)

        return(list(
            converged_estimates = converged_estimates,
            metrics = metrics
        ))
    } else {
       return ("No converged models to analyze")
    }
}

# --- plots --- # 

plot_boxplot <- function(estimate_df) {
  # Plot boxplots of converged estimates for each sample size
  ggplot(estimate_df, aes(x = as.factor(Sample_Size), y = Estimate)) +
    geom_boxplot() +
    labs(title = "Boxplot of Converged Estimates for Different Sample Sizes", x = "Sample Size", y = "Estimate") +
    theme_minimal()
}


plot_histogram <- function(converged_estimates, n) {
  # Plot the histogram of converged estimates
  ggplot(data.frame(Estimate = converged_estimates), aes(x = Estimate)) +
    geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
    labs(title = paste("Histogram of Converged Estimates for n =", n), x = "Estimate", y = "Frequency") +
    theme_minimal()
}

plot_bias_mse <- function(bias_mse_df) {
  # Plot Bias and MSE for each sample size
  ggplot(bias_mse_df, aes(x = Sample_Size)) +
    geom_line(aes(y = Bias, color = "Bias")) +
    geom_line(aes(y = MSE, color = "MSE")) +
    labs(title = "Bias and MSE for Different Sample Sizes", y = "Value", x = "Sample Size") +
    scale_color_manual(values = c("Bias" = "blue", "MSE" = "red")) +
    theme_minimal()
}

plot_bias_vs_mse <- function(bias_mse_df) {
  # Plot Bias vs MSE for each sample size
  ggplot(bias_mse_df, aes(x = Bias, y = MSE)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Bias vs MSE", x = "Bias", y = "MSE") +
    theme_minimal()
}



#The main stuff happening #

n_value = c(10, 20, 50, 90, 140)
true_mu = 5;
all_results = list()


 # Run the Monte Carlo simulation and store results
    bias_mse_df = data.frame(Sample_Size = integer(), Bias = double(), MSE = double())
    estimate_df = data.frame(Sample_Size = integer(), Estimate = double())



    # --- Numbers loop --- #

    for (n in n_value) {
      cat("\n--- Processing n =", n, "---\n")
      result <- computation(n)
      
      # Clean output
      cleaned_result <- clean_results(list(result$numerical_result, result$analytical_maximization), true_mu)

      # Store the results for each sample size
      all_results[[as.character(n)]] <- cleaned_result

      print(cleaned_result)
      

    }

   for (n in n_value) {
        result = computation(n)
        cleaned_result = clean_results(list(result$numerical_result, result$analytical_maximization), true_mu)
        
        if (is.list(cleaned_result)) {
            # Store bias and MSE
            bias_mse_df = rbind(bias_mse_df, data.frame(
                Sample_Size = n,
                Bias = cleaned_result$metrics$bias,
                MSE = cleaned_result$metrics$mse
            ))
            
            # Store estimates for each sample size
            estimate_df = rbind(estimate_df, data.frame(
                Sample_Size = rep(n, length(cleaned_result$converged_estimates)),
                Estimate = cleaned_result$converged_estimates
            ))
            
            # Plot the histogram for this sample size
            print(plot_histogram(cleaned_result$converged_estimates, n))
        }
    }


    #After for loop
     # Plot Bias and MSE across sample sizes
    print(plot_bias_mse(bias_mse_df))

    # Plot boxplot of converged estimates
    print(plot_boxplot(estimate_df))

    # Plot Bias vs MSE scatter plot
    print(plot_bias_vs_mse(bias_mse_df))

 
    









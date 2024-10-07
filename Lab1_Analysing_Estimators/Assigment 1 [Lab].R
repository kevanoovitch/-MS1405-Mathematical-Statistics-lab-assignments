#Assignment 1 MS1403
#Kevin Deshayes 
#kede23@student.bth.se


# Task 1: Implement three estimators and run experiments for different scenarios

n_values = c(10,20,50,90,140) #Global vector

# --- Define Estimators ---

# Estimator N1: Returns the maximum element in the sample
firstEstimator = function(n_values){
  #Will return the biggest element of the sample values
  return(max(n_values)); 
}

# Estimator N2: (n + 1) / n * Max(Sample)
secondEstimator = function(n_values){
  #do the (n+1/n)*firstEstimator(n_values) 
  
    n=length(n_values)
    x_n = max(n_values)
  
    N_2 = ( (n+1) / n) * x_n
  
    return (N_2)
}


#For estimator N3
thirdEstimator = function(n_values){
    
    X_mean = mean(n_values)
    
    N_3 = 2*X_mean-1
    return(N_3)
    
  }
  
# --- Main Experiment Function --- 
  
Experiment = function(N, n_values, repetitions = 5){
    
  
  
  ResultMean = data.frame(
    sample_size = n_values,
    Mean_Estimation1 = numeric(length(n_values)),  # Use lists to store multiple repetitions
    Mean_Estimation2 = numeric(length(n_values)),
    Mean_Estimation3 = numeric(length(n_values))
  )
  
  ResultVariance = data.frame(
    sample_size = n_values,
    Variance_Estimation1 = numeric(length(n_values)),
    Variance_Estimation2 = numeric(length(n_values)),
    Variance_Estimation3 = numeric(length(n_values))
  )
  
  #Iterate for 5 times
  for (n in n_values){
    
    #Store the estimations
    N1_reps = numeric(repetitions)
    N2_reps = numeric(repetitions)
    N3_reps = numeric(repetitions)
    
    for (i in 1:repetitions){
      #generate the result of the draws, size nrOF draws, results range from 1 to N
      #And stores them in the vector drawResult
      drawResult = sample(1:N, size = n, replace = TRUE, prob = NULL)
      
      #Runs estimations using the drawResult and stores them in respective N_x container
      N1_reps[i] = firstEstimator(drawResult)
      N2_reps[i] = secondEstimator(drawResult)
      N3_reps[i] = thirdEstimator(drawResult)
    }
    
    # Find the row index that matches the current sample size
    row_index = which(ResultMean$sample_size == n)
    
   
    
    # Adds the mean estimation and variance for this n to results and rounds them to 5 digits, places it in the column for the current n
    ResultMean$Mean_Estimation1[row_index] = round(mean(N1_reps), digits = 5)
    ResultVariance$Variance_Estimation1[row_index] = round(var(N1_reps), digits = 5)
    
    ResultMean$Mean_Estimation2[row_index] = round(mean(N2_reps), digits = 5)
    ResultVariance$Variance_Estimation2[row_index] = round(var(N2_reps), digits = 5)
    
    ResultMean$Mean_Estimation3[row_index] = round(mean(N3_reps), digits = 5)
    ResultVariance$Variance_Estimation3[row_index] = round(var(N3_reps), digits = 5)
    
  }
  
  #Combines the two data frames into a list of two data frames and returns that. 
    return(list(Means = ResultMean, Variances = ResultVariance))
  
}


# call the experiment function twice once for each scenario (Ns)
smallScenario = Experiment(30,n_values)


largeScenario = Experiment(150,n_values)

cat("For small scenario N=30", "\n")
cat("-------------------------------", "\n")
print(smallScenario$Means)
print(smallScenario$Variances)
cat("\n\n")

cat("For large scenario N=150", "\n")
cat("-------------------------------", "\n")
print(largeScenario$Means)
print(largeScenario$Variances)

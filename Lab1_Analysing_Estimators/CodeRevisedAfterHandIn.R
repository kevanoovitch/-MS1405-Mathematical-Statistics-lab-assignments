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
    
  
  
   # Create an empty data frame to store all results for each sample size and estimator
    Result = data.frame(
    sample_size = integer(),  # Sample sizes
    repetition = integer(),   # Repetition number (1 to repetitions)
    Estimation1 = numeric(),  # First estimator results
    Estimation2 = numeric(),  # Second estimator results
    Estimation3 = numeric()   # Third estimator results
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

       # Add a new row to the data frame for each repetition
      Result = rbind(Result, data.frame(
        sample_size = n,
        repetition = i,
        Estimation1 = N1_reps[i],
        Estimation2 = N2_reps[i],
        Estimation3 = N3_reps[i]
      ))

    }
  }
  
  #Combines the two data frames into a list of two data frames and returns that. 
  return(Result)
  
}


# call the experiment function twice once for each scenario (Ns)
smallScenario = Experiment(30,n_values)


largeScenario = Experiment(150,n_values)

cat("For small scenario N=30", "\n")
cat("-------------------------------", "\n")
print(smallScenario)

cat("\n\n")

cat("For large scenario N=150", "\n")
cat("-------------------------------", "\n")
print(largeScenario)


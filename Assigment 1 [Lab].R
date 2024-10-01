#Assignment 1 MS1403
#Kevin Deshayes 
#kede23@student.bth.se


#task 1

#For estimator N1

firstEstimator = function(n_values){
  #Will return the biggest element of the sample values
  return(max(n_values)); 
}

n_values = c(10,20,50,90,140) 

N_1 = firstEstimator(n_values)

cat("This is N1: ", N_1, "\n")

#For estimator N2

secondEstimator = function(n_values){
  #do the (n+1/n)*firstEstimator(n_values) 
  
    n=length(n_values)
    x_n = max(n_values)
  
    N_2 = ( (n+1) / n) * x_n
  
    return (N_2)
}

  N_2 = secondEstimator(n_values)
  cat("This is N2: ", N_2, "\n")

#For estimator N3

  
thirdEstimator = function(n_values){
    
    X_mean = mean(n_values)
    
    N_3 = 2*X_mean-1
    return(N_3)
    
  }
  
  N_3 = thirdEstimator(n_values)
  cat("This is N3: ", N_3, "\n")

#Main function 
  

Experiment = function(N,n_values,){
  repetitions = 5;  
  
  ExperimentResult = list() #this is not used?
  
  #Itterate for 5 times
  for (n in n_values){
    
    #Store the estimations
    N1_reps = numeric(repetitions)
    N2_reps = numeric(repetitions)
    N3_reps = numeric(repetitions)
    
    for (i in repetitions){
      #generate the result of the draws, size nrOF draws, results range from 1 to N
      #And stores them in the vector drawResult
      drawResult = sample(1:N, size = n, replace = FALSE, prob = NULL)
      
      #Runs estimations using the drawResult
      N1_reps[i] = firstEstimator(drawResult)
      N2_reps[i] = secondEstimator(drawResult)
      N3_reps[i] = thirdEstimator(drawResult)
    }
    
    
  }
  
 
  
  
    #Save the result of the estimator in a list(?)
}


# call the experimetn function twice once for each scenarion (Ns)

smallScenario = Experiment(30,n_values)
largeScenaario = Experiment(150,n_values)
#Use the return values to return a finalized result
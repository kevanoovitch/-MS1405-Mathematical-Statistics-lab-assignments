# Kevin Deshayes DVGHI 23ht
# kede23@student.bth.se

Log_Rayleigh_likelihood= function(y, mu) {
    
    n = length(y) #nrOf observations
    n*log(pi) + sum(log(y)) - n*log(2) - 2*n*log(mu) - (pi* sum(y^2))/(4*mu^2) 
}

numericalDerivative = function(f,data){
    result = optim(par=1,fn=f, y=data, method ="BFGS", control = list(fnscale= -1))
    return(result)
}

analyticalDerivative = function(mu,y){
    n = length(y)
    (-2*n)/mu + (pi * sum(y^2))/(2*mu^3)
}

analyticalMaximizing = function(d) {
    result = optim(par=1, fn=d, gr=analyticalDerivative, y=data, method="BFGS", control = list(fnscale = -1))
    return(result)
} 

#The main stuff happening

n_value = c(10, 20, 50, 90, 140)

ray = Log_Rayleigh_likelihood()

AnalyticalDerivate = analyticalDerivative()
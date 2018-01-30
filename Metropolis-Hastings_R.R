# Metropolis-Hastings algorithm #
#################################

# This function implements the Metropolis-Hastings
# algorithm in R with a Gaussian proposal
# distribution.

Metropolis_Hastings <- function(f, samples, burn_in, plot_type){
  
  # The four parameters for the function are: a function f, which 
  # is the probability distribution we want to sample from, the
  # number of samples required, the number of burn in iterations
  # we give to allow the chain to converge to its equilibrium 
  # distribution and the plot type - this will allow the user to 
  # choose between a density plot of the sample and a trace plot 
  # of the sample (including burn in iterations)
    
  # sample vector which will contain our sample 
  
  Sample <- c()
  
  # Set the iteration, t, and initial state for the Markov chain
  
  t <- 0
  Sample[[1]] <- rnorm(1, 0, 5)
  
  if (mode(f) != "function"){  
    
    stop("Invalid function f: f is not a function")
    
  }else{
    
    while (t < samples){
      
      # Draw a value from the proposal distribution
      
      proposal <- rnorm(1, 0, 5)
      
      # Calculate the acceptance probability
      
      alpha <- min(1, (dnorm(Sample[t], proposal, 5) * f(proposal)) / (dnorm(proposal, Sample[t], 5) * f(Sample[t])))
      
      # Monte Carlo part of the algorithm. We draw from a uniform distribution on [0, 1]
      
      u <- runif(1, 0, 1)
      
      if (u <= alpha){
        
        # Accept the proposal value
        
        Sample[[t + 1]] <- proposal
        
      }else if (u > alpha){
        
        # Reject the proposal value
        
        Sample[[t + 1]] <- Sample[t]
      }
      t <- t + 1
    }
    
    if (plot_type == "trace"){
      plot(Sample, type = "l", col = "blue3", xlab = "Iteration t", ylab = "X", main = "Trace Plot of X")
      abline(v = burn_in, col = "red", lty = "dashed")
    
    }else if (plot_type == "density"){
      plot(density(Sample[burn_in:samples]), col = "blue3", xlab = "X", ylab = "Density Plot of X", 
           main = "Density Plot of X")
    
    }else if (plot_type != "trace" & plot_type != "density"){
      
      stop("Plot type is invalid")
    }
  }
}

# Gaussian distribution with mean 4 and unit variance as the function we
# want to sample from (also the equilibrium distribution) 

Gaussian <- function(x){
            exp(- (x - 4)^2 /2)
}
        
Metropolis_Hastings(Gaussian, 70000, 0, "density")  
points(density(rnorm(100000, 4, 1)), col = "red", type = "l")      
legend(5.8, 0.4, legend = c("Approx.", "Actual"), col = c("blue3", "red"), lty = 1,
       cex = 0.75) 
  
  
  
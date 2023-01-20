create_test_data <- function(nrow, ncol, lambda_pois= 1.5){
  
  
  data <- rpois(n = nrow * ncol, lambda = lambda_pois)
  
  df <- matrix(data = data, nrow = nrow, ncol = ncol, byrow = TRUE)
  
  treatment <- rbinom(n = nrow, size = 1, prob = 0.5)
  
  y <- rbinom(n = nrow, size = 1, prob = 0.5)
  
  return(list(df = df,
              treatment = treatment,
              y = y))
}


gen_random_words <- function(sigma = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u" ,"v", "w", "x","y","z"),
                            n = 100,
                            min_size = 2,
                            max_size = 10){
  

  word_vec <- vector()
  i <- 1
  while(length(word_vec) < n){
    len <- round(runif(1, min_size, max_size))
    temp <-  paste(sample(sigma, size = len, replace = TRUE), collapse="")
    if(!(temp %in% word_vec)){
     word_vec[i] <- temp
     i <- i + 1
    } else {
      
    }
  }
    
  return(word_vec)
  
}


gen_stm_data <- function(k, nrow){
  
  k <- 15
  nrow <- 100
  matched_x <- matrix(ncol = k, nrow = nrow, NA)
  
  for(row in 1:nrow(matched_x)){
    leftover <- 101
    for(col in 1:ncol(matched_x)){
      
      if (col == 1){
        matched_x[row, col] <- floor(runif(1,0,leftover))
      } else {
      
        matched_x[row, col] <- floor(runif(1,0, leftover))
      
      }
      leftover <- leftover - matched_x[row,col]
    }
    
    # random order
    matched_x[row, ] <- sample(x = matched_x[row, ], size = k)
    
#     cat(matched_x[row, ], "   ", sum(matched_x[row, ]),  "\n")
  }
  
  return(matched_x)
}


logit_fun <- function(y, X, theta){
  
  ## This is a logit function to be optimised with optim-function.
  ### Input: dependent variable vector y, independent variable matrix X. theta will be optimised and does not require
  ### an input.
  ### Output: summed maximum logged likelihood of function with given parameter values as a scalar.
  
  # if there are >= 2 independent variables:
  if(!is.null(ncol(X))){
    
    beta <- theta[1:ncol(X)]
    
    mu <- X %*% beta
    
    
    # if there is just one:
  } else {
    
    beta <- theta[1]
    
    mu <- X * beta
    
    
  }
  
  # caluclating probabilities
  p <- 1 / ( 1 + exp(-mu) )
  
  # summing logged likelihood
  logll <-  sum( y * log(p) + (1 - y) * log ( 1- p) )
  
  # return sum loglik value
  return(logll)
  
}
  

est_prop_score <- function(X, t, nsim = 1000){
  
  require(optimx)
  require(MASS)
  
  startvals <- rep(0, ncol(X))
  
  res <- optim(
    par = startvals, # start values
    fn = logit_fun, # function to optimise: the function argument not stated below (theta) will be optimised.
    y = t, # y is our dv
    X = X, # X are our ivs
    control = list(fnscale = -1), # a negative value in fnscale will lead to a maximisation of the function, rather than a minimmisation.
    hessian = TRUE, # do not include hessian matrix of 2nd derivatives. What be necessary if we calculated se by hand.
    method = "BFGS" # which hill-climbing algorithm to use.
  )
  
  # at which values for theta did the optim() function reach its maximum? Those are the coefficients.
  coef <- res$par
  vcov <- solve(-res$hessian)
  se <- sqrt(diag(vcov))
  nsim <- nsim
  S <- mvrnorm(nsim, coef, vcov)
  
  mu <- S %*% t(X)
  
  prob <- 1 / ( 1 + exp(-mu) )
  prop_score <- apply(prob, 2, mean)
  
  
  return(prop_score) 
  
}

create_test_data <- function(nrow, ncol, lambda_pois= 1.5){
  
  
  data <- rpois(n = nrow * ncol, lambda = lambda_pois)
  
  df <- matrix(data = data, nrow = nrow, ncol = ncol, byrow = TRUE)
  
  treatment <- rbinom(n = nrow, size = 1, prob = 0.5)
  
  y <- rbinom(n = nrow, size = 1, prob = 0.5)
  
  return(list(df = df,
              treatment = treatment,
              y = y))
}


estimate_stm <- function(){
  
  stm_out <- stm(documents = dfm,
                 vocab = vocab,
                 k = k)
  
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

i<- c(1,5,2,4,2,2,8)
j <- c(2,5,3,2,4,2,4)
x <- rpois(7,2)
M1 <- sparseMatrix(i,j,x=x)
M1


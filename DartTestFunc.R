AncientDarts <- function(n.tries, method = "greek", rad = 0.5, ...) {
  # simulates throwing a dart at a dartboard using a special time-lost method
  # ARGS: n.tries: number of tries, as an integer
  # method: which ancient method is used, as a string (greek is the default method)
  # rad: the target radius on a dartboard, as a float (0.5 is the default, which is hitting the board)
  # ...: additional arguments to be fed into a random value function
  # returns a dataframe with every simulated dart's coordinates, whether its on the board, its method, and the target radius
  
  if(method == "egyptian") {
    x.vect <- rnorm(n.tries, ...) # generates coordinates under a normal distribution with specified args
    
    y.vect <- rnorm(n.tries, ...)
    
    return.df <- data.frame(method = rep("Egypt", times = n.tries), x = x.vect, y = y.vect, rad = rep(rad, times = n.tries)) # takes all randomly generated values and puts them in a data frame.  The first column is completely filled with "Egypt" using rep, and the last is completely filled with the target radius.
    
  } else if(method == "greek") {
    x.vect <- rtri(n.tries, ...) # generates random values under a triangular distribution with specified args
    
    y.vect <- rtri(n.tries, ...)
    
    return.df <- data.frame(method = rep("Greek", times = n.tries), x = x.vect, y = y.vect, rad = rep(rad, times = n.tries)) # the remainder of this is the same as the Egypt throws section
    
  } else if(method == "celtic") {
    
    x.vect <- rbeta(n.tries, ...) # generates random values under a beta distribution with specified args
    
    y.vect <- rbeta(n.tries, ...)
    
    return.df <- data.frame(method = rep("Celtic", times = n.tries), x = x.vect, y = y.vect, rad = rep(rad, times = n.tries)) # the remainder of this is the same as the Egypt throws section
  }
  
  return.df$on_board <- F # used to check if a dart's coordinates are located within the target radius
  return.df$on_board[((return.df$x - 0.5)^2 + (return.df$y - 0.5)^2) < (rad ^ 2)] = T
  
  return(return.df)
}
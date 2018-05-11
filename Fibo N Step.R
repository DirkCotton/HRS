

fibn <- function(n, K, starting){
  sequence <- vector(mode='numeric', length=K)
  sequence[1:n] <- starting
  for(i in (n+1):K){
    sequence[i] <- sum(sequence[(i-n):(i-1)])
  }
  return(sequence)
}

n <- 4
K <- 100
starting <- c(0,1,1)

cat("\n Fib-N: ",fibn(n,K,starting))

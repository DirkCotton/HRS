k <- 4
n <- 150
series <- rep(0,n)

series[1] <- 1
series[2] <- 1
series[3] <- 2
series[4] <- 4
# series[5] <- 8
# series[6] <- 16
# series[7] <- 32
# series[8] <- 64
# series[9] <- 128
 # series[10] <- 256


# for (j in 3:n) {
#   series[j] <- series [j-1] + series[j-2] + series[j-3] + series[j-4] 
# }

fibo2 <- series  # save the normal Fibonacci series

# for (j in 5:k){
#   series[j] <- fibo2[j]
# }

for (m in (k+1):n){
  
    if (k == 2) series[m] <- series [m-1] + series[m-2] 
    if (k == 3) series[m] <- series [m-1] + series[m-2] + series[m-3]
    if (k == 4) series[m] <- series [m-1] + series[m-2] + series[m-3] + series[m-4]
    if (k == 9) series[m] <- series [m-1] + series[m-2] + series[m-3] + series[m-4] + series[m-5] + series[m-6] + series[m-7] + series[m-8] + series[m-9]
    if (k == 10) series[m] <- series [m-1] + series[m-2] + series[m-3] + series[m-4] + series[m-5] + series[m-6] + series[m-7] + series[m-8] + series[m-9] + series[m-10]
    
    }
 
cat("\nseries: ",series)
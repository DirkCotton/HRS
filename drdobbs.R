# X <- 10; sample(c("H","T"), replace=TRUE, size=X)
X <- rbinom(200, 1, 0.5)

rle(X == 1)


l <- rle(X == 1)$lengths
r <- rle(X == 1)$values


cat("\nLongest run of TRUE",max(l[r == T]))
length(X)
fib10 <- 55  #  fib of 2
tosses <- 200

kStreak <- 2
fibn <- c(1,1,2,3,5,8)
fibn <- 55

probKheads <- (fibn[kStreak] * (tosses + 1 - kStreak)) / (2 * fibn[kStreak] * (tosses + 1))  # probabilty of k heads in n tosses
cat ("\nProbability: ",probKheads)

oddsSucc <- (144*191) / (2*144 * 201)
oddFail <- 1 - (144*200)/(2^200)

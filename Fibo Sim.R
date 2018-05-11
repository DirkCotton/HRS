n <- 10000  # scenarios

set.seed <- 12345

# toss a coin 147 times and count longest strecth of 1's (heads)
longest <- rep(0,n)

for (scenario in 1:n) {
x <- rbinom(147, 1, .5)
# a <- data.frame(matrix(0,147,2))

a <- with(rle(x), {
  ok <- !is.na(values)
  ends <- cumsum(lengths)[ok]
  starts <- ends - lengths[ok] + 1
  # print(starts)
  # print(ends)
  # print(cbind(starts, ends))
  a <- data.frame("starts"=starts,"ends"=ends,"run"=ends - starts + 1,"heads"=x[starts])
})

b <- a[a$heads == 1,]
longestRunHeads <- b$run[which.max(b$run)]
longest[scenario] <- longestRunHeads
# cat("\nlongest run of heads :",longestRunHeads )
# print (a)

}

# Probability of tossing at least 8 heads in a row.
cat("\nProbability of tossing at least 8 heads in a row ",sum(longest >=8 )/n)
cat("\nProbability of tossing at least 9 heads in a row ",sum(longest >=9 )/n)

#### Do same for probability of market losses with p=.25

n <- 10000  # scenarios

set.seed <- 12345

# toss a coin 147 times and count longest strecth of 1's (heads)
longest <- rep(0,n)

for (scenario in 1:n) {
  x <- rbinom(147, 1, .25)
  
  a <- with(rle(x), {
    ok <- !is.na(values)
    ends <- cumsum(lengths)[ok]
    starts <- ends - lengths[ok] + 1
    # print(starts)
    # print(ends)
    # print(cbind(starts, ends))
    a <- data.frame("starts"=starts,"ends"=ends,"run"=ends - starts + 1,"heads"=x[starts])
  })
  
  b <- a[a$heads == 1,]
  longestRunHeads <- b$run[which.max(b$run)]
  longest[scenario] <- longestRunHeads
  # cat("\nlongest run of heads :",longestRunHeads )
  # print (a)
  
}

# Probability of tossing at least 8 heads in a row.
cat("\nProbability of at least 3 years of losses in a row ",sum(longest >=3 )/n)
cat("\nProbability of at least 4 years of losses in a row ",sum(longest >=4 )/n)
cat("\nProbability of at least 5 years of losses in a row ",sum(longest >=5 )/n)

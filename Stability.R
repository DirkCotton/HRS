malesPercent <- rep(0,12)
for (i in (5:12)){
  malesPercent[i] <- sum(cams$male[cams$earliestRetire == i]) / (sum(cams$male[cams$earliestRetire == i] == 0) + sum(cams$male[cams$earliestRetire == i]))
}
cat("\nPercent Males by Wave of Retirement in CAMS")
print(knitr::kable(malesPercent))

cat("\nMean for waves 5-12 ",mean(malesPercent[5:12]))
cat("\nSigma for waves 5-12 ",sd(malesPercent[5:12]))
cat("\n\n")

meanAge <- rep(0,12)
for (i in (5:12)){
  meanAge[i] <- mean(cams$ageAtRetirement1[cams$earliestRetire == i & cams$persons ==1])
}
cat("\nMean age by Wave of Retirement in CAMS for 1-person households")
print(knitr::kable(meanAge))

cat("\nMean for waves 5-12 ",mean(meanAge[5:12]))
cat("\nSigma for waves 5-12 ",sd(meanAge[5:12]))
cat("\n\n")

meanAge3 <- rep(0,12)
for (i in (5:12)){
  meanAge3[i] <- mean(cams$ageAtRetirement2[cams$earliestRetire == i & cams$persons == 2])
}
cat("\nMean age of spouse 2 by Wave of Retirement in CAMS for 2-person households")
print(knitr::kable(meanAge3))

cat("\nMean for waves 5-12 ",mean(meanAge3[5:12]))
cat("\nSigma for waves 5-12 ",sd(meanAge3[5:12]))
cat("\n\n")



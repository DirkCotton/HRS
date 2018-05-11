# compare model consumption predictions
plot.new
options(scipen=5)
hrsM1 <- hrs[!is.na(hrs$cndurImputedM1),]
hrsM2 <- hrs[!is.na(hrs$cndurImputedM2),]

# comparePredict1P <- cbind(dummy1P$hhid,dummy1P$cndurWOR,dummy1P$cndurExpected)
comparePredict1P <- cbind(hrsM1$hhid[hrsM1$persons == 1],hrsM1$cndurWOR[hrsM1$persons == 1],hrsM1$cndurImputedM1[hrsM1$persons == 1])
colnames(comparePredict1P) <- c("hhid","consumption","predicted consumption")
write.csv(comparePredict1P,"~/desktop/Predictions 1P Households.csv")

cat("\nCorrelation of One-Person Household Models",cor(hrsM1$cndurImputedM2[hrsM1$persons == 1],hrsM1$cndurImputedM1[hrsM1$persons ==1]))

plot(hrsM1$cndurImputedM2[hrsM1$persons ==1],hrsM1$cndurImputedM1[hrsM1$persons == 1],title(main=c(length(hrsM1$cndurImputedM1[hrsM1$persons ==1]),"One-Person Households")),xlab="Predicted Consumption Model 2",ylab="Predicted Consumption Model 1",cex=.4,col='blue',ylim=c(0,30000))
text(3000,20000,paste("correlation ",round(cor(hrsM1$cndurImputedM2[hrsM1$persons == 1],hrsM1$cndurImputedM1[hrsM1$persons ==1]),3)))
fitTrend <- lm(hrsM1$cndurImputedM2[hrsM1$persons ==1] ~ hrsM1$cndurImputedM1[hrsM1$persons == 1])
co <- coef(fitTrend)
abline(fitTrend, col="red", lwd=1) 

# comparePredict2P <- cbind(dummy1P$hhid,dummy1P$cndurWOR,dummy1P$cndurExpected)
comparePredict2P <- cbind(hrsM2$hhid[hrsM1$persons == 2],hrsM2$cndurWOR[hrsM1$persons == 2],hrsM2$cndurImputedM2[hrsM1$persons == 2])
colnames(comparePredict2P) <- c("hhid","consumption","predicted consumption")
write.csv(comparePredict2P,"~/desktop/Predictions 1P Households.csv")

cat("\nCorrelation of Two-Person Household Models",cor(hrsM2$cndurImputedM2[hrsM1$persons == 2],hrsM2$cndurImputedM1[hrsM1$persons == 2]))

plot(hrsM2$cndurImputedM2[hrsM2$persons == 2],hrsM2$cndurImputedM1[hrsM2$persons == 2],title(main=c(length(hrsM1$cndurImputedM1[hrsM1$persons == 2]),"Two-Person Households")),xlab="Predicted Consumption Model 2",ylab="Predicted Consumption Model 1",cex=.4,col='blue')
text(3000,3000000,paste("correlation ",round(cor(hrsM1$cndurImputedM2[hrsM1$persons == 2],hrsM1$cndurImputedM1[hrsM1$persons == 2]),3)))

fitTrend2 <- lm(hrsM2$cndurImputedM2[hrsM2$persons == 2] ~ hrsM2$cndurImputedM1[hrsM2$persons == 2])
co <- coef(fitTrend2)
abline(fitTrend2, col="red", lwd=1) 

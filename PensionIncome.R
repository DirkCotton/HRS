# Pension Income

cat("\nHRS Records for retired in first year of retirement: ",length(firstYrRet.df[,1]))
rwave2 <- which(colnames(firstYrRet.df) =="r2peninc")
swave2 <- which(colnames(firstYrRet.df) =="s2peninc")

# build a column of rows that contain at least one year of pension income
oneYearR <- rep(FALSE,length(firstYrRet.df[,1]))
oneYearR <- rowSums(firstYrRet.df[rwave2:(rwave2+10)],na.rm=TRUE) > 0

cat("\nRetired respondents with at least one year of pension income: ",sum(oneYearR))

oneYearS <- rep(FALSE,length(firstYrRet.df[,1]))
oneYearS <- rowSums(firstYrRet.df[swave2:(swave2+10)],na.rm=TRUE) > 0

cat("\nRetired spouses with at least one year of pension income: ",sum(oneYearS))

oneYearRindx <- which(rowSums(firstYrRet.df[rwave2:(rwave2+10)],na.rm=TRUE) > 0)
oneYearSindx <- which(rowSums(firstYrRet.df[swave2:(swave2+10)],na.rm=TRUE) > 0)
length(intersect(oneYearRindx,oneYearSindx))
cat("\nHouseholds with both respondent and spouse pension income in a least one year: ",length(intersect(oneYearRindx,oneYearSindx)))

## need to filter by DB pension


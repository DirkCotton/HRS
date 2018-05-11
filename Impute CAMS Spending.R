# The CAMS data set contains spending data for HRS respondents +  but only for about one-third of them.
# The purpose of this script is to impute CAMS spending data for HRS repondents not included in CAMS.
  
  # cams <- read.csv('~/Dropbox/Robo Research/Retirement-Selected CAMS data.csv')
  cams <- read.csv('~/Dropbox/CAMS data.csv')
    
  cat("\nCAMS 1-person Household observations= ",dim(cams1P)[1])
  cat("\nCAMS 1-person households not retired= ",sum(cams1P$selectedBy == "notRetired"))
  cat("\nConsumer Non-Dur spending for Wave of Retirement in 1-Person households is missing for ",sum(is.na(cams1P$cndurWOR))," CAMS observations -- rejected")
  cat("\nCAMS observations for retired 1-Person households=",sum(!(is.na(cams1P$cndurWOR)) & cams1P$selectedBy != "notRetired"))
  
  cat("\nNumber of 1-person households not retired= ",sum(cams$earliestRetire == 0 & cams$persons == 1))
  cat("\nNumber of 2-person households not retired= ", sum(cams$earliestRetire == 0 & cams$persons == 2))
  cat("\n1-person and 2-person households with HRS assets plus income <= 0 and rejected is ",sum(cams$assets <= 0))
  
  # define Gross Total Wealth as all income and assets available to pay expenses in a give year
  # cams2 contains both 1- and 2-person households
  cndurStart <- match("h5cndur",colnames(cams))  # first column of consumer non-durables for waves 5-12
  
  cams2 <- cams[cams$earliestRetire >= 5,]  # no CAMS data for retirement prior to wave 5
  for (j in 1:dim(cams2)[1]) {    # select consumer non-durable spending for wave of retirement
    cams2$cndurWOR[j] <- cams2[j,cams2$earliestRetire[j] + (cndurStart - 5)]
  }
  
  # reject all households not retired
  cams2 <- cams2[cams2$selectedBy != "notRetired",]
  
  # reject all households with zero  consumer non-durable spending
  cams2 <- cams2[cams2$cndurWOR > 0,]
  
  # reject all households with zero income and zero savings
  cams2$portfolioAssetsTotal[cams2$portfolioAssetsTotal < 0] <- 0 # if gtw is <= 0, reject from data set as implausible
  cams2$debt[cams2$portfolioAssetsTotal <= 0] <- 0 # if gtw is <= 0, reject from data set as implausible

  cams2$income <- cams2$paycheck + cams2$pensionIncome + cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2
  cams2$gtw <- cams2$portfolioAssetsTotal + cams2$paycheck + cams2$pensionIncome + cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2
  cams2 <- cams2[cams2$gtw > 0,]  # if gtw is <= 0, reject from data set as implausible (  # reject all households with zero income and zero savings)
  
  # separate 1- and 2-person households
  cams1P <- cams2[cams2$persons == 1,]  # CAMS data for one-person households
  cams2P <- cams2[cams2$persons == 2,]  # CAMS data for two-person households
  
  
  #########################################  
  # Predict missing consumer non-durable spending data for 1-person households
  #########################################  
  
  # build a data frame of normalized data (using gtw) 
  cams1Pgtw <- data.frame(cams1P$cndurWOR,cams1P$ageAtRetirement1, cams1P$gender1,cams1P$ssIncAtRetire1/cams1P$gtw,cams1P$debt/cams1P$gtw,cams1P$portfolioAssetsTotal/cams1P$gtw,cams1P$paycheck/cams1P$gtw,cams1P$pensionIncome/cams1P$gtw,cams1P$gtw)
  colnames(cams1Pgtw) <- c("cndurWOR","ageAtRetirement1","gender1","ssIncAtRetire1","debt","portfolioAssetsTotal","paycheck","pensionIncome","gtw")
  cams1Pgtw$norm <- cams1Pgtw$ssIncAtRetire1 +  cams1Pgtw$portfolioAssetsTotal + cams1Pgtw$paycheck + cams1Pgtw$pensionIncome
  cams1Pgtw$male <- rep(0,dim(cams1Pgtw)[1])
  cams1Pgtw$male[cams1Pgtw$gender1 == "1.male"] <- 1

  cat("\nObservations incorrectly normalized= ",sum((cams1Pgtw$norm < .99)))

# create columns =1 if retirment in that wave, else zero  
  r <- dim(cams1Pgtw)[1]
  cams1Pgtw$d5 <- rep(0,r)
  cams1Pgtw$d6 <- rep(0,r)
  cams1Pgtw$d7 <- rep(0,r)
  cams1Pgtw$d8 <- rep(0,r)
  cams1Pgtw$d9 <- rep(0,r)
  cams1Pgtw$d10 <- rep(0,r)
  cams1Pgtw$d11 <- rep(0,r)
  cams1Pgtw$d12 <- rep(0,r)
  cams1Pgtw$d5[cams1P$earliestRetire == 5] <- 1
  cams1Pgtw$d6[cams1P$earliestRetire == 6] <- 1
  cams1Pgtw$d7[cams1P$earliestRetire == 7] <- 1
  cams1Pgtw$d8[cams1P$earliestRetire == 8] <- 1
  cams1Pgtw$d9[cams1P$earliestRetire == 9] <- 1
  cams1Pgtw$d10[cams1P$earliestRetire == 10] <- 1
  cams1Pgtw$d11[cams1P$earliestRetire == 11] <- 1
  cams1Pgtw$d12[cams1P$earliestRetire == 12] <- 1
  
  fit <- lm(cams1Pgtw$cndurWOR ~  cams1Pgtw$d5 + cams1Pgtw$d6 + cams1Pgtw$d7 + cams1Pgtw$d8 + cams1Pgtw$d9 + cams1Pgtw$d10 + cams1Pgtw$d11 + cams1Pgtw$d12 + cams1Pgtw$male + cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$portfolioAssetsTotal + cams1Pgtw$pensionIncome + cams1Pgtw$ageAtRetirement1 -1)
  
  cat("\n\n")
  print(summary(fit))
  
  cams1Pgtw$cndurExpected <- (fit$coefficients[1] * cams1Pgtw$d5 + fit$coefficients[2] * cams1Pgtw$d6 +fit$coefficients[3] * cams1Pgtw$d7 +fit$coefficients[4] * cams1Pgtw$d8 + fit$coefficients[5] * cams1Pgtw$d9 + fit$coefficients[6] * cams1Pgtw$d10 +fit$coefficients[7] * cams1Pgtw$d11 + fit$coefficients[8] * cams1Pgtw$d12 + fit$coefficients[9] * cams1Pgtw$male +fit$coefficients[10] * cams1Pgtw$ssIncAtRetire1 + fit$coefficients[11] * cams1Pgtw$portfolioAssetsTotal + fit$coefficients[12] * cams1Pgtw$pensionIncome + fit$coefficients[13] * cams1Pgtw$ageAtRetirement1)

#########################################  
# Predict missing consumer non-durable spending data for 2-person households
#########################################  
  
  # build a data frame of normalized data (using gtw) 
  cams2Pgtw <- data.frame(cams2P$cndurWOR,cams2P$ageAtRetirement1,cams2P$ageAtRetirement2,cams2P$ssIncAtRetire1/cams2P$gtw,cams2P$ssIncAtRetire2/cams2P$gtw,cams2P$debt/cams2P$gtw,cams2P$portfolioAssetsTotal/cams2P$gtw,cams2P$paycheck/cams2P$gtw,cams2P$pensionIncome/cams2P$gtw,cams2P$gtw)
  colnames(cams2Pgtw) <- c("cndurWOR","ageAtRetirement1","ageAtRetirement2","ssIncAtRetire1","ssIncAtRetire2","debt","portfolioAssetsTotal","paycheck","pensionIncome","gtw")
  cams2Pgtw$norm <- cams2Pgtw$ssIncAtRetire1 +  cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$paycheck + cams2Pgtw$pensionIncome
  cams2Pgtw$male <- NA
  
  cat("\nObservations incorrectly normalized= ",sum((cams2Pgtw$norm < .99)))
  
  # create columns =1 if retirment in that wave, else zero  
  r <- dim(cams2Pgtw)[1]
  cams2Pgtw$d5 <- rep(0,r)
  cams2Pgtw$d6 <- rep(0,r)
  cams2Pgtw$d7 <- rep(0,r)
  cams2Pgtw$d8 <- rep(0,r)
  cams2Pgtw$d9 <- rep(0,r)
  cams2Pgtw$d10 <- rep(0,r)
  cams2Pgtw$d11 <- rep(0,r)
  cams2Pgtw$d12 <- rep(0,r)
  cams2Pgtw$d5[cams2P$earliestRetire == 5] <- 1
  cams2Pgtw$d6[cams2P$earliestRetire == 6] <- 1
  cams2Pgtw$d7[cams2P$earliestRetire == 7] <- 1
  cams2Pgtw$d8[cams2P$earliestRetire == 8] <- 1
  cams2Pgtw$d9[cams2P$earliestRetire == 9] <- 1
  cams2Pgtw$d10[cams2P$earliestRetire == 10] <- 1
  cams2Pgtw$d11[cams2P$earliestRetire == 11] <- 1
  cams2Pgtw$d12[cams2P$earliestRetire == 12] <- 1
  
  fit2 <- lm(cams2Pgtw$cndurWOR ~cams2Pgtw$d5 + cams2Pgtw$d6 + cams2Pgtw$d7 + cams2Pgtw$d8 + cams2Pgtw$d9 + cams2Pgtw$d10 + cams2Pgtw$d11 + cams2Pgtw$d12 + cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$pensionIncome + cams2Pgtw$ageAtRetirement1 + cams2Pgtw$ageAtRetirement2 -1) # "-1" tells lm to ignore the y-intercept. d-columns will be the y-intercept.
  
  cat("\n\n")
  print(summary(fit2))
  
  cams2Pgtw$cndurExpected <- (fit2$coefficients[1] * cams2Pgtw$d5 + fit2$coefficients[2] * cams2Pgtw$d6 + fit2$coefficients[3] * cams2Pgtw$d7 +fit2$coefficients[4] * cams2Pgtw$d8 + fit2$coefficients[5] * cams2Pgtw$d9 + fit2$coefficients[6] * cams2Pgtw$d10 +fit2$coefficients[7] * cams2Pgtw$d11 + fit2$coefficients[8] * cams2Pgtw$d12 + fit2$coefficients[9] * cams2Pgtw$ssIncAtRetire1 + fit2$coefficients[10] * cams2Pgtw$ssIncAtRetire2 + fit2$coefficients[11] * cams2Pgtw$portfolioAssetsTotal + fit2$coefficients[12] * cams2Pgtw$pensionIncome + fit2$coefficients[13] * cams2Pgtw$ageAtRetirement1 + fit2$coefficients[14] * cams2Pgtw$ageAtRetirement2)
  
  
  
  
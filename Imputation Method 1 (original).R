# Missing data imputation strategy 1.
# - Step 1. Run MLR on actual consumption data using dummy variables for waves 5-12. Replace the y-intercept 
#   calculated by the MLR with the appropriate dummy variable to impute missing consumption data
#   for retirements in waves 5-12. (These are the coefficients from "fit" for one-person households
#   and "fit2" for 2-person households objects calculated by Impute Missing Spending Data.R)
# - Step 2. Run a second MLR on actual consumption data using the MLR's predicted y-intercept 
#   instead of dummy variables. Use this second model to impute missing consumption data
#   for retirements in waves 1-4. 

# Develop a MLR model of predicted consumption for one-person households as a function of Social Security income at
# retirement, portfolio assets, pension income and age at retirement WITH DUMMY VARIABLES variables for
# wave of retirement. ("fit" object from previous script)

# Develop a MLR model of predicted consumption for two-person households as a function of Social Security income at
# retirement, portfolio assets, pension income and age at retirement WITH DUMMY VARIABLES variables for
# wave of retirement. ("fit2" object from previous script)

# Develop a MLR model of predicted consumption for one-person households as a function of Social Security income at
# retirement, portfolio assets, pension income and age at retirement WITH NO DUMMY VARIABLES for
# wave of retirement.

  fit3 <- lm(cams1Pgtw$cndurWOR ~ cams1Pgtw$male + cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$portfolioAssetsTotal + cams1Pgtw$pensionIncome + cams1Pgtw$ageAtRetirement1)
  
  cat("\n\nfit3: 1-person household, no dummy variables")
  print(summary(fit3))
  
  cams$noDummy1P <- rep(0,dim(cams)[1])
  cams$noDummy1P <- (fit3$coefficients[1] + fit3$coefficients[2] * cams$male + fit3$coefficients[3] * cams$ssIncAtRetire1 / cams$gtw + fit3$coefficients[4] * cams$portfolioAssetsTotal / cams$gtw + fit3$coefficients[5] * cams$pensionIncome / cams$gtw + fit3$coefficients[6] * cams$ageAtRetirement1)

# Develop a MLR model of predicted consumption for two-person households as a function of Social Security income at
# retirement, portfolio assets, pension income and age at retirement WITH NO DUMMY VARIABLES for
# wave of retirement.

  fit4 <- lm(cams2Pgtw$cndurWOR ~ cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$pensionIncome + cams2Pgtw$ageAtRetirement1 + cams2Pgtw$ageAtRetirement2) 
  
  cat("\n\nfit4: 2-person household, no dummy variables")
  print(summary(fit4))
  
  cams$noDummy2P <- rep(0,dim(cams)[1])
  cams$noDummy2P <- (fit4$coefficients[1] + fit4$coefficients[2] * cams$ssIncAtRetire1 / cams$gtw + fit4$coefficients[3] * cams$ssIncAtRetire2 / cams$gtw + fit4$coefficients[4] * cams$portfolioAssetsTotal / cams$gtw + fit4$coefficients[5] * cams$pensionIncome / cams$gtw + fit4$coefficients[6] * cams$ageAtRetirement1 + fit4$coefficients[7] * cams$ageAtRetirement2)

# When actual consumption data is available for a household, use it.
  
  cams$cndur <- rep(0,dim(cams)[1])
  savecndurWOR <- cams$cndurWOR   # save
  cams$cndurWOR[is.na(cams$cndurWOR)] <- 0
  cams$cndur[cams$cndurWOR > 0] <- cams$cndurWOR[cams$cndurWOR > 0]
  
# Impute missing consumption data for waves of retirement 5-12 1-person households using first model with dummy variables

  cams$cndur[cams$cndurWOR <= 0 & cams$persons == 1 & cams$earliestRetire >= 5] <- cams$dummy1P[cams$cndurWOR <= 0 & cams$persons == 1 & cams$earliestRetire >= 5]
  
# Impute missing consumption data for waves of retirement 1-4 1-person households using second model with dummy variables

  cams$cndur[cams$cndurWOR <= 0 & cams$persons == 1 & cams$earliestRetire < 5] <- cams$noDummy1P[cams$cndurWOR <= 0 & cams$persons == 1 & cams$earliestRetire < 5]
  
# Impute missing consumption data for waves of retirement 5-12 2-person households using first model with dummy variables

  cams$cndur[cams$cndurWOR <= 0 & cams$persons == 2 & cams$earliestRetire >= 5] <- cams$dummy2P[cams$cndurWOR <= 0 & cams$persons == 2 & cams$earliestRetire >= 5]
  
# Impute missing consumption data for waves of retirement 1-4 2-person households using second model with dummy variables

  cams$cndur[cams$cndurWOR <= 0 & cams$persons == 2 & cams$earliestRetire < 5] <- cams$noDummy2P[cams$cndurWOR <= 0 & cams$persons == 2 & cams$earliestRetire < 5]
  
  cams$cndurWOR <- savecndurWOR    # restore


  # create a column of predicted consumer non-durables for all CAMS observations, missing data or not.
  
  cams$predictedCndur <- rep(0,dim(cams)[1])
  cams$predictedCndur[cams$persons == 1 & cams$earliestRetire < 5] <- cams$noDummy1P[cams$persons == 1 & cams$earliestRetire < 5]
  
  cams$predictedCndur[cams$persons == 2 & cams$earliestRetire < 5] <- cams$noDummy2P[cams$persons == 2 & cams$earliestRetire < 5]
  
  cams$predictedCndur[cams$persons == 1 & cams$earliestRetire >= 5] <- cams$dummy1P[cams$persons == 1 & cams$earliestRetire >= 5]
  
  cams$predictedCndur[cams$persons == 2 & cams$earliestRetire >= 5] <- cams$dummy2P[cams$persons == 2 & cams$earliestRetire >= 5]
  
  # save results as spreadsheet
  
  xlSummary <- cams[,c(2:5,10:26,28,58,68,54,73,72)]
  
  colnames(xlSummary)[7] <- "WaveOfRetire"
  colnames(xlSummary)[26] <- "predictedCndur"
  colnames(xlSummary)[27] <- "selectedCndur"
  test <-xlSummary
  # if no consumption data and no predicted consumption, selectedCndur will equal NA. Drop these rows.
  # save these rows in CAMS so we can delete these HHID's from the HRS data 
  xlSummary <- xlSummary[!is.na(xlSummary$selectedCndur),] 

  write.csv(xlSummary,"Imputed CAMS.csv")
  
  
  
  
  
  
 # (fit4$coefficients[1][2] + fit4$coefficients[2] * cams$ssIncAtRetire1[2][2] + fit4$coefficients[3] * cams$ssIncAtRetire2[2][2] + fit4$coefficients[4] * cams$portfolioAssetsTotal[2][2] + fit4$coefficients[5] * cams$pensionIncome[2][2] + fit4$coefficients[6] * cams$ageAtRetirement1[2] + fit4$coefficients[7] * cams$ageAtRetirement2[2])
  
  
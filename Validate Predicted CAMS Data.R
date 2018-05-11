# Predicted consumption for HRS observations can exhibit extreme absolute errors (as % of reported consumption).
# This cross-validation is intended to determine if those households with extreme errors (a
# absolute error greater than 100%) are throwing off consumption predictions.

# HRS data is loaded from a previously consolidated Excel.csv file named "HRS Retired.csv", created by R-script XXXZ.
# CAMS data is loaded from a previously consolidated Excel.csv file named "CAMS data.csv", created by R-script XXXZ.

# Cross-validate results by setting aside all CAMS households with absolute prediction error as percent of 
# reported data greater than 100%. For each cross-validation run, also set aside an equal number of
# randomly-selected CAMS records with errors less than 100%. These two subsets constitute the validation set. 
# The remaining data in the CAMS data set are the training set.
# NOTE: For CAMS data that has associated wealth and income in HRS, transfer this HRS data to CAMS
# data with the same HHID. CAMS households with no data in HRS cannot be used to predict consumption.
# create the "bad prediction" half of the validation set from CrossValidationModel1.R results for 1-person households.

# This first of three chained R-scripts runs through the consumption prediction process a single
# time in order to identify consumption predictions with large errors compared to reported consumption.
# It then chains to "CrossValidate 1P HH Bad Predictions.R" to validate predictions for predictions
# with large errors and finally to "CrossValidate 1P HH BGood Predictions.R" to validate predictions for 
# predictions with smaller errors. Lrage error predictions are those that exceed 100%. Lastly, 
# "Validate Predicted CAM Data.RMD" can be run to generate a report. 

n <- 1 # run one time to identify predictions with error > 100%.

crossValOnePerson <- matrix(0,112,n)

cams <- read.csv('~/Dropbox/Robo Research/CAMS data.csv')
cams <- cams[cams$selectedBy != "notRetired",]  # remove household not retired
# save predicted data from first model
saveM1 <- hrs$cndurImputedM1
hrs <- read.csv('~/Dropbox/Robo Research/HRS Retired.csv',stringsAsFactors = FALSE)
hrs <- hrs[hrs$selectedBy != "notRetired",]  # remove household not retired

# remove any HRS households not in CAMS (all CAMS hhids are in HRS.)
hrs <- hrs[hrs$hhid %in% cams$hhid,]

hrs <- hrs[hrs$selectedBy != "notRetired",]  # remove household not retired
cams <- cams[cams$hhid %in% hrs$hhid,]

hrs$cndurImputedM1 <- saveM1
# normalizing variable for HRS
hrs$gtw <- rep(0,dim(hrs)[1])
hrs$gtw <- hrs$portfolioAssetsTotal + hrs$paycheck + hrs$ssIncAtRetire1 + hrs$ssIncAtRetire2 + hrs$pensionIncome

cams2 <- cams  # preserve original CAMS data
cams2$gtw <- cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2 + cams2$paycheck + cams2$pensionIncome + cams2$portfolioAssetsTotal
cams2 <- cams2[cams2$selectedBy != "notRetired" & cams2$cndurWOR > 0 & cams2$hhid %in% hrs$hhid & cams2$gtw > 0 ,]

retiredHRSinCams <- length(cams2$hhid %in% retiredHHID)

# test traing set and validation set

for(scenarios in 1:n) {
  
cat ("\n",scenarios," of ",n)
 
  cndurStart <- match("h5cndur",colnames(cams))  # first column of consumer non-durables for waves 5-12
  
  for (j in 1:dim(cams2)[1]) {    # select consumer non-durable spending for wave of retirement
    cams2$cndurWOR[j] <- cams2[j,cams2$earliestRetire[j] + (cndurStart - 5)]
  }
  
   cams2 <- cams2[cams2$hhid %in% hrs$hhid,]
  # ... and only use HRS HHID's
  hrs <- hrs[hrs$hhid %in% cams2$hhid,]
  # sort cams2 and hrs by HHID
  hrs <- hrs[order(hrs$hhid),] 
  cams2 <- cams2[order(cams2$hhid),] 
  # validation set (hrs) contains all records not in training set
  # hrs <- hrs[!(hrs$hhid %in% goodErrorHHID1),]   ################
  # hrs <- hrs[!(hrs$hhid %in% badErrorHHID1),]
  hrs <- cams2   # for one-time run to find bad predictions in CAMS, use CAMS as both trainging set and validation set

  
  # separate 1- and 2-person households
  cams1P <- cams2[cams2$persons == 1,]  # CAMS data for one-person households
  cams2P <- cams2[cams2$persons == 2,]  # CAMS data for two-person households
  
  cams1Psize <- dim(cams1P)[1]
  
  ##############################################
  # Impute missing consumption data for retirement in waves 5-12.
  # Regression models fit (one-person households) and fit2 (two-person households).
  ##############################################
  
  #########################################  
  # Predict missing consumer non-durable spending data for 1-person households
  #########################################  
  
  # build a data frame of normalized data (using gtw) 
  cams1Pgtw <- data.frame(cams1P$hhid,cams1P$cndurWOR,cams1P$ageAtRetirement1,cams1P$gender1,cams1P$ssIncAtRetire1/cams1P$gtw,cams1P$debt/cams1P$gtw,cams1P$portfolioAssetsTotal/cams1P$gtw,cams1P$paycheck/cams1P$gtw,cams1P$pensionIncome/cams1P$gtw,cams1P$gtw,cams1P$earliestRetire)
  colnames(cams1Pgtw) <- c("hhid","cndurWOR","ageAtRetirement1","gender1","ssIncAtRetire1","debt","portfolioAssetsTotal","paycheck","pensionIncome","gtw","earliestRetire")
  cams1Pgtw$norm <- cams1Pgtw$ssIncAtRetire1 +  cams1Pgtw$portfolioAssetsTotal + cams1Pgtw$paycheck + cams1Pgtw$pensionIncome
  cams1Pgtw$male <- rep(0,dim(cams1Pgtw)[1])
  cams1Pgtw$male[cams1Pgtw$gender1 == "1.male"] <- 1
  
  cat("\nObservations incorrectly normalized (1-Person Households) = ",sum(cams1Pgtw$norm < .99))
  
  # create columns =1 if retirement in that wave, else zero  
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
  
  # Build same dummy variable vectors for cams
  r <- dim(cams)[1]
  cams$d5 <- rep(0,r)
  cams$d6 <- rep(0,r)
  cams$d7 <- rep(0,r)
  cams$d8 <- rep(0,r)
  cams$d9 <- rep(0,r)
  cams$d10 <- rep(0,r)
  cams$d11 <- rep(0,r)
  cams$d12 <- rep(0,r)
  cams$d5[cams$earliestRetire == 5] <- 1
  cams$d6[cams$earliestRetire == 6] <- 1
  cams$d7[cams$earliestRetire == 7] <- 1
  cams$d8[cams$earliestRetire == 8] <- 1
  cams$d9[cams$earliestRetire == 9] <- 1
  cams$d10[cams$earliestRetire == 10] <- 1
  cams$d11[cams$earliestRetire == 11] <- 1
  cams$d12[cams$earliestRetire == 12] <- 1
  cams$dummy1P <- rep(0,dim(cams)[1])  # initialize vector to 0
  cams$male <- cams$dummy1P   # initialize vector to 0
  cams$male[cams$gender1 == "1.male" | cams$gender1 == ".m"] <- 1
  
  # Build same dummy variable vectors for hrs
  r <- dim(hrs)[1]
  hrs$d5 <- rep(0,r)
  hrs$d6 <- rep(0,r)
  hrs$d7 <- rep(0,r)
  hrs$d8 <- rep(0,r)
  hrs$d9 <- rep(0,r)
  hrs$d10 <- rep(0,r)
  hrs$d11 <- rep(0,r)
  hrs$d12 <- rep(0,r)
  hrs$d5[hrs$earliestRetire == 5] <- 1
  hrs$d6[hrs$earliestRetire == 6] <- 1
  hrs$d7[hrs$earliestRetire == 7] <- 1
  hrs$d8[hrs$earliestRetire == 8] <- 1
  hrs$d9[hrs$earliestRetire == 9] <- 1
  hrs$d10[hrs$earliestRetire == 10] <- 1
  hrs$d11[hrs$earliestRetire == 11] <- 1
  hrs$d12[hrs$earliestRetire == 12] <- 1
  hrs$dummy1P <- rep(0,dim(hrs)[1])  # initialize vector to 0
  hrs$male <- hrs$dummy1P   # initialize vector to 0
  hrs$male[hrs$gender1 == "1.male" | hrs$gender1 == ".m"] <- 1
  
  fit <- lm(cams1Pgtw$cndurWOR ~ 
              cams1Pgtw$d5 
            + cams1Pgtw$d6 
            + cams1Pgtw$d7 
            + cams1Pgtw$d8 
            + cams1Pgtw$d9 
            + cams1Pgtw$d10 
            + cams1Pgtw$d11 
            + cams1Pgtw$d12 
            + cams1Pgtw$male 
            + cams1Pgtw$ssIncAtRetire1 
            + cams1Pgtw$portfolioAssetsTotal 
            + cams1Pgtw$pensionIncome 
            + cams1Pgtw$ageAtRetirement1 
            + cams1Pgtw$male 
            -1)  # turns off y-intercept calculation in lm function
  
  print(summary(fit))
  
  cams1Pgtw$cndurExpected <- (fit$coefficients[1] * cams1Pgtw$d5 + fit$coefficients[2] * cams1Pgtw$d6 +fit$coefficients[3] * cams1Pgtw$d7 +fit$coefficients[4] * cams1Pgtw$d8 + fit$coefficients[5] * cams1Pgtw$d9 + fit$coefficients[6] * cams1Pgtw$d10 +fit$coefficients[7] * cams1Pgtw$d11 + fit$coefficients[8] * cams1Pgtw$d12 + fit$coefficients[9] * cams1Pgtw$male + fit$coefficients[10] * cams1Pgtw$ssIncAtRetire1 + fit$coefficients[11] * cams1Pgtw$portfolioAssetsTotal + fit$coefficients[12] * cams1Pgtw$pensionIncome + fit$coefficients[13] * cams1Pgtw$ageAtRetirement1)
  
  
  # create HRS column for wave-of-retirement consumption for households with available CAMS data (not imputed)
  hrs$cndurWOR <- rep(NA,dim(hrs)[1])
  for (j in 1:length(hrsInCams2)){
    hrs$cndurWOR[which(hrs$hhid == hrsInCams2[j])] <- cams2$cndurWOR[j]
  } 
  
  # predict consumption for HRS observations even if consumption data is in CAMS for
  # for 1-person households retiring wave 5 or later
  hrs$cndurImputed <- rep(NA,dim(hrs)[1])
  
  hrs$cndurImputed[hrs$earliestRetire >= 5 & hrs$persons == 1] <- 
    (    fit$coefficients[1]  * hrs$d5[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[2]  * hrs$d6[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[3]  * hrs$d7[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[4]  * hrs$d8[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[5]  * hrs$d9[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[6]  * hrs$d10[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[7]  * hrs$d11[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[8]  * hrs$d12[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[9]  * hrs$male[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[10] * hrs$ssIncAtRetire1[hrs$earliestRetire >= 5 & hrs$persons == 1] / hrs$gtw[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[11] * hrs$portfolioAssetsTotal[hrs$earliestRetire >= 5 & hrs$persons == 1] / hrs$gtw[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[12] * hrs$pensionIncome[hrs$earliestRetire >= 5 & hrs$persons == 1] / hrs$gtw[hrs$earliestRetire >= 5 & hrs$persons == 1] 
         + fit$coefficients[13] * hrs$ageAtRetirement1[hrs$earliestRetire >= 5 & hrs$persons == 1] 
    )
  
  # for cross-validation, store hrs imputed data for 1-person households
  crossValM11P <- data.frame(cbind(hrs$hhid,hrs$cndurWOR,round(hrs$cndurImputed),"percentAbsError"=rep(0,dim(hrs)[1]))) 
  colnames(crossValM11P) <- c("hhid","cndurWOR","cndurImputedM11P","percentAbsError") 
  # compute absolute value of the error of predicted value as percentage of reported consumption data 
  crossValM11P$percentAbsError <- abs(hrs$cndurImputed - hrs$cndurWOR) / hrs$cndurWOR
  crossValM11P$Error <- hrs$cndurImputed - hrs$cndurWOR
  
  hrs1P <- hrs[hrs$persons==1,]
  
  # remove any households with no consumption reported
 hrs1P <- hrs1P[hrs1P$cndurWOR > 0 & !is.na(hrs1P$cndurWOR),]
  
  #########################################  
  # Predict missing consumer non-durable spending data for 2-person households
  #########################################  
  
  # Merge any duplicated households in cams2P by summing CAMS spending in one and deleting the other
  
  # cat ("\nCAMS 2-person households data contains ", sum(duplicated(cams2P$hhid))," duplicated households that will be merged." )
  
  # for cross-validation, set aside 20% of CAMS2 data for testing and 80% to model
  
  
  # setAside2 <- cams2P[sample(nrow(cams2P),round(.2*dim(cams2P)[1])), ]
  # hrs <- setAside2
  # cams2P <- cams2P[!(cams2P$hhid %in% setAside2$hhid),]
  
  hrs$male <- rep(0,dim(hrs)[1])
  hrs$male[hrs$gender1 == ".m" | hrs$gender1 == "1.male"] <- 1
  
  r <- dim(hrs)[1]
  hrs$d5 <- rep(0,r)
  hrs$d6 <- rep(0,r)
  hrs$d7 <- rep(0,r)
  hrs$d8 <- rep(0,r)
  hrs$d9 <- rep(0,r)
  hrs$d10 <- rep(0,r)
  hrs$d11 <- rep(0,r)
  hrs$d12 <- rep(0,r)
  hrs$d5[hrs$earliestRetire == 5] <- 1
  hrs$d6[hrs$earliestRetire == 6] <- 1
  hrs$d7[hrs$earliestRetire == 7] <- 1
  hrs$d8[hrs$earliestRetire == 8] <- 1
  hrs$d9[hrs$earliestRetire == 9] <- 1
  hrs$d10[hrs$earliestRetire == 10] <- 1
  hrs$d11[hrs$earliestRetire == 11] <- 1
  hrs$d12[hrs$earliestRetire == 12] <- 1
  
  # build a data frame of normalized data (using gtw) 
  cams2Pgtw <- data.frame(cams2P$hhid,cams2P$cndurWOR,cams2P$ageAtRetirement1,cams2P$ageAtRetirement2,cams2P$ssIncAtRetire1/cams2P$gtw,cams2P$ssIncAtRetire2/cams2P$gtw,cams2P$portfolioAssetsTotal/cams2P$gtw,cams2P$paycheck/cams2P$gtw,cams2P$pensionIncome/cams2P$gtw,cams2P$gtw,cams2P$earliestRetire)
  colnames(cams2Pgtw) <- c("hhid","cndurWOR","ageAtRetirement1","ageAtRetirement2","ssIncAtRetire1","ssIncAtRetire2","portfolioAssetsTotal","paycheck","pensionIncome","gtw","earliestRetire")
  cams2Pgtw$norm <- cams2Pgtw$ssIncAtRetire1 +  cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$paycheck + cams2Pgtw$pensionIncome
  cams2Pgtw$male <- NA
  
  # cat("\nObservations incorrectly normalized (2-person households) = ",sum((cams2Pgtw$norm < .99)))
  
  # create columns =1 if retirement in that wave, else zero  
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
  
  cams$dummy2P <- rep(0,dim(cams)[1])
  
  fit2 <- lm(cams2Pgtw$cndurWOR ~ 
               cams2Pgtw$d5 
             + cams2Pgtw$d6 
             + cams2Pgtw$d7 
             + cams2Pgtw$d8 
             + cams2Pgtw$d9 
             + cams2Pgtw$d10 
             + cams2Pgtw$d11 
             + cams2Pgtw$d12 
             + cams2Pgtw$ssIncAtRetire1 
             + cams2Pgtw$ssIncAtRetire2 
             + cams2Pgtw$portfolioAssetsTotal 
             + cams2Pgtw$pensionIncome 
             + cams2Pgtw$ageAtRetirement1 
             + cams2Pgtw$ageAtRetirement2 
             -1) # "-1" tells lm to ignore the y-intercept. d-columns will be the y-intercept.
  
  # cat("\n\n")
  # print(summary(fit2))
  
  # cams2$portfolioAssetsTotal[cams2$portfolioAssetsTotal < 0] <- 0
  cams2Pgtw$cndurExpected <- (fit2$coefficients[1] * cams2Pgtw$d5 + fit2$coefficients[2] * cams2Pgtw$d6 + fit2$coefficients[3] * cams2Pgtw$d7 +fit2$coefficients[4] * cams2Pgtw$d8 + fit2$coefficients[5] * cams2Pgtw$d9 + fit2$coefficients[6] * cams2Pgtw$d10 +fit2$coefficients[7] * cams2Pgtw$d11 + fit2$coefficients[8] * cams2Pgtw$d12 + fit2$coefficients[9] * cams2Pgtw$ssIncAtRetire1 + fit2$coefficients[10] * cams2Pgtw$ssIncAtRetire2 + fit2$coefficients[11] * cams2Pgtw$portfolioAssetsTotal + fit2$coefficients[12] * cams2Pgtw$pensionIncome + fit2$coefficients[13] * cams2Pgtw$ageAtRetirement1 + fit2$coefficients[14] * cams2Pgtw$ageAtRetirement2)
  # cams$dummy2P <- (fit2$coefficients[1] * cams$d5 + fit2$coefficients[2] * cams$d6 + fit2$coefficients[3] * cams$d7 + fit2$coefficients[4] * cams$d8 + fit2$coefficients[5] * cams$d9 + fit2$coefficients[6] * cams$d10 + fit2$coefficients[7] * cams$d11 + fit2$coefficients[8] * cams$d12 + (fit2$coefficients[9] * cams$ssIncAtRetire1 / cams$gtw) + (fit2$coefficients[10] * cams$ssIncAtRetire2 / cams$gtw) + (fit2$coefficients[11] * cams$portfolioAssetsTotal / cams$gtw) + (fit2$coefficients[12] * cams$pensionIncome / cams$gtw) + (fit2$coefficients[13] * cams$ageAtRetirement1) + (fit2$coefficients[14] * cams$ageAtRetirement2))
  
  # predict consumption for HRS observations even if consumption data is in CAMS for
  # for 2-person households retiring wave 5 or later
  hrs$cndurImputed[hrs$earliestRetire >= 5 & hrs$persons == 2] <- 
    (  fit2$coefficients[1]  * hrs$d5[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[2]  * hrs$d6[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[3]  * hrs$d7[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[4]  * hrs$d8[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[5]  * hrs$d9[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[6]  * hrs$d10[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[7]  * hrs$d11[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[8]  * hrs$d12[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[9]  * hrs$ssIncAtRetire1[hrs$earliestRetire >= 5 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[10] * hrs$ssIncAtRetire2[hrs$earliestRetire >= 5 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[11] * hrs$portfolioAssetsTotal[hrs$earliestRetire >= 5 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[12] * hrs$pensionIncome[hrs$earliestRetire >= 5 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[13] * hrs$ageAtRetirement1[hrs$earliestRetire >= 5 & hrs$persons == 2] 
       + fit2$coefficients[14] * hrs$ageAtRetirement2[hrs$earliestRetire >= 5 & hrs$persons == 2]  
    ) 
  

  
  ##############################################
  # Impute missing consumption data for retirement in waves 1-4 (all consumption data is missing).
  # Regression models fit3 (one-person households) and fit4 (two-person households).
  ##############################################
  
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
  
  fit3 <- lm(cams1Pgtw$cndurWOR ~ 
               cams1Pgtw$male 
             + cams1Pgtw$ssIncAtRetire1 
             + cams1Pgtw$portfolioAssetsTotal 
             + cams1Pgtw$pensionIncome 
             + cams1Pgtw$ageAtRetirement1 
             # + cams1Pgtw$earliestRetire   ######################## 
  )
 
  # fit4 <- lm( cams2Pgtw$cndurWOR ~ 
  #               cams2Pgtw$ssIncAtRetire1 
  #             + cams2Pgtw$ssIncAtRetire2
  #             + cams2Pgtw$portfolioAssetsTotal 
  #             + cams2Pgtw$pensionIncome 
  #             + cams2Pgtw$ageAtRetirement1 
  #             + cams2Pgtw$ageAtRetirement2
  #             + cams2Pgtw$earliestRetire     ########################
  # )
  # 
  
  
  # for cross-validation, store hrs imputed data for 1-person households
  crossValM12P <- data.frame(cbind(hrs$hhid,hrs$cndurWOR,round(hrs$cndurImputed),"percentAbsError"=rep(0,dim(hrs)[1])))
  colnames(crossValM12P) <- c("hhid","cndurWOR","cndurImputedM12P","percentAbsError") 
  # compute absolute value of the error of predicted value as percentage of reported consumption data 
  crossValM12P$percentAbsError <- abs(hrs$cndurImputed - hrs$cndurWOR) / hrs$cndurWOR
  crossValM12P$Error <- hrs$cndurImputed - hrs$cndurWOR
  
  # Save error percents
  # crossValOnePerson[,scenarios] <- crossValM11P$percentAbsError
  # crossValTwoPerson[,scenarios] <- crossValM12P$percentAbsError
  # hrs$errorPercent <- abs(hrs$cndurImputed - hrs$cndurWOR) / hrs$cndurWOR
  
} # end for scenarios loop

write.csv(saveM12P,"~/desktop/saveM12P.csv")

# calculate absolute value of error as a percent of reported consumption
hrs1P$percentError <- abs(hrs1P$cndurImputed - hrs1P$cndurWOR) / hrs1P$cndurWOR
cat("\nFraction of 1-Person households with error > 100%",sum((hrs1P$percentError > 1)) / dim(hrs1P)[1])
# hrs2P$percentError <- abs(hrs2P$cndurImputed - hrs2P$cndurWOR) / hrs2P$cndurWOR
cat("\nFraction of 2-Person households with error > 100%",sum((hrs2P$percentError > 1)) / dim(hrs2P)[1])


# plot(density(hrs1P$percentError),main="One-Person Household Abs Error")
# plot(density(hrs2P$percentError),main="Two-Person Household Abs Error",xlim=c(0,10))

# create vectors of high prediction error HHID's

highError1Phhid <- hrs1P$hhid[hrs1P$percentError > 1]
# highError2Phhid <- hrs2P$hhid[hrs2P$percentError > 1.5]

# cross-validate CAMS predicted data with low errors


source('~/R Projects/HRS/CrossValidate 1P HH Good Predictions.R')
source('~/R Projects/HRS/CrossValidate 1P HH Bad Predictions.R')

save.image("CrossVal.RData")



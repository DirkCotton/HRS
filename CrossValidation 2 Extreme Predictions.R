# Predicted consumption for hrs observations can exhibit extreme absolute errors (as % of reported consumption).
# This cross-validation is intended to determine if those households with extreme errors (a
# absolute error greater than 100%) are throwing off consumption predictions.

# Cross-validate results by setting aside all CAMS households with absolute prediction error as percent of 
# reported data greater than 100%. For each cross-validation run, also set aside an equal number of
# randomly-selected CAMS records with errors less than 100%. These two subsets constitute the vallidation set. 
# The remaining data in the CAMS data set are the training set.
# NOTE: For CAMS data that has associated wealth and income in hrs, transfer this hrs data to CAMS
# data with the same HHID. CAMS households with no data in hrs cannot be used to predict consumption.

set.seed(1224)
# Loop through n cross-validation scenarios
n <- 100 # cross-validations to run

crossValTwoPerson <- matrix(NA,615,100)

# cams <- read.csv('~/Dropbox/Robo Research/Retirement-Selected CAMS data.csv')
cams <- read.csv('~/Dropbox/Robo Research/CAMS data.csv')
cams <- cams[cams$selectedBy != "notRetired",]  # remove household not retired
# save imputed data from first model
saveM1 <- hrs$cndurImputedM1
hrs <- read.csv('~/Dropbox/Robo Research/hrs Retired.csv',stringsAsFactors = FALSE)
hrs <- hrs[hrs$earliestRetire >= 5,]

# remove any hrs households not in CAMS (all CAMS hhids are in hrs.)
hrs <- hrs[hrs$hhid %in% cams$hhid,]

hrs <- hrs[hrs$selectedBy != "notRetired",]  # remove household not retired

cams <- cams[cams$hhid %in% hrs$hhid,]

hrs$cndurImputedM1 <- saveM1
# normalizing variable for hrs
hrs$gtw <- rep(0,dim(hrs)[1])
hrs$gtw <- hrs$portfolioAssetsTotal + hrs$paycheck + hrs$ssIncAtRetire1 + hrs$ssIncAtRetire2 + hrs$pensionIncome

savehrs1P <- hrs[hrs$persons == 1,]
savehrs <- hrs[hrs$persons == 2,]

cams2 <- cams  # preserve original CAMS data
cams2$gtw <- cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2 + cams2$paycheck + cams2$pensionIncome + cams2$portfolioAssetsTotal
cams2 <- cams2[cams2$persons == 2 & cams2$selectedBy != "notRetired" & cams2$cndurWOR > 0 & cams2$hhid %in% hrs$hhid & cams2$gtw > 0 ,]

 
badErrorHHID2 <- highError2Phhid  

# remove bad error households from cams2 training set.
# cams2 <- cams2[!(cams2$hhid %in% badErrorHHID1) & !(cams2$hhid %in% badErrorHHID2),]
# cat("\n",length(badErrorHHID2)," households with prediction errors > 100% constitute half of validation set and are removed from training set.")

retiredhrsinCams <- length(cams2$hhid %in% retiredHHID)
    
# create training sets (all cams records except bad estimation error households)

badError2 <- cams2[cams2$persons == 2 & (cams2$hhid %in% badErrorHHID2),]
hrsTrain2 <- cams2[cams2$persons == 2 & !(cams2$hhid %in% badErrorHHID2),]

cat("\nTraining set for Two-Person households consists of ",dim(hrsTrain2)[1]," households.")

for(scenarios in 1:n) {
  
   
  cat ("\n",scenarios," of ",n)
  
  # Impute spending data for all hrs observations using a model with age-of-retirement and age-of-retirement^2 instead of dummy variables 
  
  # retiredHHID <- hrs$hhid[hrs$selectedBy != "notRetired"]
  # retiredhrsinCams <- length(cams2$hhid %in% retiredHHID)
  # # cat("\nCAMS households available for retired hrs households ", retiredhrsinCams)
  
  cndurStart <- match("h5cndur",colnames(cams))  # first column of consumer non-durables for waves 5-12
  
  for (j in 1:dim(cams2)[1]) {    # select consumer non-durable spending for wave of retirement
    cams2$cndurWOR[j] <- cams2[j,cams2$earliestRetire[j] + (cndurStart - 5)]
  }
  
  hrs <- savehrs1P
  
#########
  
  # # reject all households with zero  consumer non-durable spending
  # noWOR <- sum(cams2$cndurWOR == 0)
  # # cat("\nReject ",noWOR," CAMS observations with spending = 0 for wave of retirement")
  # cams2 <- cams2[cams2$cndurWOR > 0,]
  # # hasWOR <- dim(cams2)[1]
  
  # # reject all households with zero income and zero savings
  # noIncomeSave <- sum(cams2$portfolioAssetsTotal + cams2$pensionIncome + cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2 + cams2$paycheck <= 0 )
  # cams2 <- cams2[cams2$portfolioAssetsTotal + cams2$pensionIncome + cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2 + cams2$paycheck > 0 ,]
  # # cat("\nCAMS households rejected because no Income or savings ",noIncomeSave)
  # afterNoInc <- dim(cams2)[1]
  
  # for cross-validation, only use CAMS data when there is associated HHID in hrs (otherwise, there is no hrs data to impute from) 
  cams2 <- cams2[cams2$hhid %in% hrs$hhid,]
 
  # separate 1- and 2-person households
  cams1P <- cams2[cams2$persons == 1,]  # CAMS data for one-person households

  
  # randomly select same number of training set observations from "good" households with smaller prediction error
  goodErrorHHID2 <- sample(hrsTrain2$hhid,length(badErrorHHID2))
  
  cat("\n",length(goodErrorHHID2)," 2-person households with prediction errors < 100% constitute half of validation set and are removed from training set.")
  
 
  #########################################  
  # Predict missing consumer non-durable spending data for 2-person households
  #########################################  

  savecams2Pl <- dim(cams2P)[1]
  # validation set (hrs2P) contains all records not in training set
  ts2 <- c(goodErrorHHID2,badErrorHHID2)
  
  cat("\nValidation set for Two-Person households consists of ",dim(hrs2P)[1]," households.")
  cams2P <- hrsTrain2   # training set is cams1P
  

  cat("\nCAMS2P is ",dim(cams2P)[1], "mean ", mean(cams2P$hhid))
  
  cat("\nhrs2P # 0 is ",dim(hrs2P)[1], "mean ", mean(hrs2P$hhid), " cams2P is ",dim(cams2P)[1])
  
  hrs2P <- cams2P[(cams2P$hhid %in% ts2),]  # validation set is hrs2P
  hrs2P <- rbind(hrs2P,badError2)

  cat("\nhrs2P # 1 is ",dim(hrs2P)[1], "mean ", mean(hrs2P$hhid), " cams2P is ",dim(cams2P)[1])
 #  cat("\n ts 2 is ",length(ts2)," mean is ",mean(ts2))

  hrs2P$gtw <- rep(0,dim(hrs2P)[1])
  hrs2P$gtw <- hrs2P$portfolioAssetsTotal + hrs2P$paycheck + hrs2P$ssIncAtRetire1 + hrs2P$ssIncAtRetire2 + hrs2P$pensionIncome
  hrs2P$male <- rep(0,dim(hrs2P)[1])   # initialize vector to 0
  hrs2P$male[hrs2P$gender1 == "1.male" | hrs2P$gender1 == ".m"] <- 1
  
  r <- dim(hrs2P)[1]
  hrs2P$d5 <- rep(0,r)
  hrs2P$d6 <- rep(0,r)
  hrs2P$d7 <- rep(0,r)
  hrs2P$d8 <- rep(0,r)
  hrs2P$d9 <- rep(0,r)
  hrs2P$d10 <- rep(0,r)
  hrs2P$d11 <- rep(0,r)
  hrs2P$d12 <- rep(0,r)
  hrs2P$d5[hrs2P$earliestRetire == 5] <- 1
  hrs2P$d6[hrs2P$earliestRetire == 6] <- 1
  hrs2P$d7[hrs2P$earliestRetire == 7] <- 1
  hrs2P$d8[hrs2P$earliestRetire == 8] <- 1
  hrs2P$d9[hrs2P$earliestRetire == 9] <- 1
  hrs2P$d10[hrs2P$earliestRetire == 10] <- 1
  hrs2P$d11[hrs2P$earliestRetire == 11] <- 1
  hrs2P$d12[hrs2P$earliestRetire == 12] <- 1
  
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
  # cat("\nhrs2P # 2 is ",dim(hrs2P)[1], "mean ", mean(hrs2P$hhid))
  
  # cams2$portfolioAssetsTotal[cams2$portfolioAssetsTotal < 0] <- 0
  cams2Pgtw$cndurExpected <- (fit2$coefficients[1] * cams2Pgtw$d5 + fit2$coefficients[2] * cams2Pgtw$d6 + fit2$coefficients[3] * cams2Pgtw$d7 +fit2$coefficients[4] * cams2Pgtw$d8 + fit2$coefficients[5] * cams2Pgtw$d9 + fit2$coefficients[6] * cams2Pgtw$d10 +fit2$coefficients[7] * cams2Pgtw$d11 + fit2$coefficients[8] * cams2Pgtw$d12 + fit2$coefficients[9] * cams2Pgtw$ssIncAtRetire1 + fit2$coefficients[10] * cams2Pgtw$ssIncAtRetire2 + fit2$coefficients[11] * cams2Pgtw$portfolioAssetsTotal + fit2$coefficients[12] * cams2Pgtw$pensionIncome + fit2$coefficients[13] * cams2Pgtw$ageAtRetirement1 + fit2$coefficients[14] * cams2Pgtw$ageAtRetirement2)
  # cams$dummy2P <- (fit2$coefficients[1] * cams$d5 + fit2$coefficients[2] * cams$d6 + fit2$coefficients[3] * cams$d7 + fit2$coefficients[4] * cams$d8 + fit2$coefficients[5] * cams$d9 + fit2$coefficients[6] * cams$d10 + fit2$coefficients[7] * cams$d11 + fit2$coefficients[8] * cams$d12 + (fit2$coefficients[9] * cams$ssIncAtRetire1 / cams$gtw) + (fit2$coefficients[10] * cams$ssIncAtRetire2 / cams$gtw) + (fit2$coefficients[11] * cams$portfolioAssetsTotal / cams$gtw) + (fit2$coefficients[12] * cams$pensionIncome / cams$gtw) + (fit2$coefficients[13] * cams$ageAtRetirement1) + (fit2$coefficients[14] * cams$ageAtRetirement2))
  
  # predict consumption for hrs2P observations even if consumption data is in CAMS for
  # for 2-person households retiring wave 5 or later
  hrs2P$cndurImputed <- rep(NA,dim(hrs2P)[1])
  hrs2P$cndurImputed[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] <- 
    (    fit2$coefficients[1]  * hrs2P$d5[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[2]  * hrs2P$d6[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[3]  * hrs2P$d7[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[4]  * hrs2P$d8[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[5]  * hrs2P$d9[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[6]  * hrs2P$d10[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[7]  * hrs2P$d11[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[8]  * hrs2P$d12[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[9]  * hrs2P$ssIncAtRetire1[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[10] * hrs2P$ssIncAtRetire2[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[11] * hrs2P$portfolioAssetsTotal[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[12] * hrs2P$pensionIncome[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[13] * hrs2P$ageAtRetirement1[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2] 
       + fit2$coefficients[14] * hrs2P$ageAtRetirement2[hrs2P$earliestRetire >= 5 & hrs2P$persons == 2]  
    ) 
  
  
  # x1 <- cbind(cams1Pgtw[,1:2],round(cams1Pgtw[,21]))
  # colnames(x1) <- c("hhid","cndWOR","cndurExpected")
  # x2 <- cbind(cams2Pgtw[,1:2],round(cams2Pgtw[,21]))
  # colnames(x2) <- c("hhid","cndWOR","cndurExpected")
  # x3 <- rbind(x1,x2)
  # x3 <- (x3[with(x3, order(x3$hhid)), ])
  # 
  # x3 <- (x3[with(x3, order(x3$hhid)), ])
  # z <- full_join(hrs2P,x3)
  
  # cat("\nhrs2P # 3 is ",dim(hrs2P)[1], "mean ", mean(hrs2P$hhid))
  
  
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
  
  # cat("\n\nfit3: 1-person household, no dummy variables")
  # print(summary(fit3))
  
  # cams$noDummy1P <- rep(0,dim(cams)[1])
  # cams$noDummy1P <- (fit3$coefficients[1] + fit3$coefficients[2] * cams$male + fit3$coefficients[3] * cams$ssIncAtRetire1 / cams$gtw + fit3$coefficients[4] * cams$portfolioAssetsTotal / cams$gtw + fit3$coefficients[5] * cams$pensionIncome / cams$gtw + fit3$coefficients[6] * cams$ageAtRetirement1)
  
  # predict consumption for hrs2P observations even if consumption data is in CAMS for
  # for 1-person households retiring wave 4 or earlier
  
  # hrs2P$cndurImputed[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] <- 
  #   (  fit3$coefficients[1] 
  #      + fit3$coefficients[2] * hrs2P$male[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] 
  #      + fit3$coefficients[3] * hrs2P$ssIncAtRetire1[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] 
  #      + fit3$coefficients[4] * hrs2P$portfolioAssetsTotal[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] 
  #      + fit3$coefficients[5] * hrs2P$pensionIncome[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] 
  #      + fit3$coefficients[6] * hrs2P$ageAtRetirement1[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1] 
  #      # + fit3$coefficients[7] * hrs2P$earliestRetire[hrs2P$earliestRetire <= 4 & hrs2P$persons == 1]    ######################
  #   ) 
  
  # Develop a MLR model of predicted consumption for two-person households as a function of Social Security income at
  # retirement, portfolio assets, pension income and age at retirement WITH NO DUMMY VARIABLES for
  # wave of retirement.
  # cams2Pgtw$ageAtRetirement2ms2Pgtw$ssIncAtRetire2 + cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$pensionIncome + cams2Pgtw$ageAtRetirement1 + cams2Pgtw$ageSquared1 + ) 
  
  fit4 <- lm( cams2Pgtw$cndurWOR ~ 
                cams2Pgtw$ssIncAtRetire1 
              + cams2Pgtw$ssIncAtRetire2
              + cams2Pgtw$portfolioAssetsTotal 
              + cams2Pgtw$pensionIncome 
              + cams2Pgtw$ageAtRetirement1 
              + cams2Pgtw$ageAtRetirement2
              + cams2Pgtw$earliestRetire     ########################
  )
  
  # cat("\n\nfit4: 2-person household, no dummy variables")
  # print(summary(fit4))
  
  # cams$noDummy2P <- rep(0,dim(cams)[1])
  # cams$noDummy2P <- (fit4$coefficients[1] + fit4$coefficients[2] * cams$ssIncAtRetire1 / cams$gtw + fit4$coefficients[3] * cams$ssIncAtRetire2 / cams$gtw + fit4$coefficients[4] * cams$portfolioAssetsTotal / cams$gtw + fit4$coefficients[5] * cams$pensionIncome / cams$gtw + fit4$coefficients[6] * cams$ageAtRetirement1 + fit4$coefficients[7] * cams$ageAtRetirement2)
  
  # predict consumption for hrs2P observations even if consumption data is in CAMS for
  # for 2-person households retiring wave 4 or earlier
  # hrs2P$cndurImputed[hrs2P$earliestRetire < 5 & hrs2P$persons == 2] <-  
  #   (  fit4$coefficients[1] 
  #      + fit4$coefficients[2] * hrs2P$ssIncAtRetire1[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] 
  #      + fit4$coefficients[3] * hrs2P$ssIncAtRetire2[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] 
  #      + fit4$coefficients[4] * hrs2P$portfolioAssetsTotal[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] 
  #      + fit4$coefficients[5] * hrs2P$pensionIncome[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] / hrs2P$gtw[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] 
  #      + fit4$coefficients[6] * hrs2P$ageAtRetirement1[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2] 
  #      + fit4$coefficients[7] * hrs2P$ageAtRetirement2[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2]
  #      # + fit4$coefficients[8] * hrs2P$earliestRetire[hrs2P$earliestRetire <= 4 & hrs2P$persons == 2]
  #   ) 
  
 #  cat("\nhrs2P # 4 is ",dim(hrs2P)[1], "mean ", mean(hrs2P$hhid))
  
  # for cross-validation, store hrs2P imputed data for 1-person households
  crossValM12P <- data.frame(cbind(hrs2P$hhid,hrs2P$cndurWOR,round(hrs2P$cndurImputed),"percentAbsError"=rep(0,dim(hrs2P)[1]),"Error"=rep(0,dim(hrs2P)[1])))
  colnames(crossValM12P) <- c("hhid","cndurWOR","cndurImputedM12P","percentAbsError","Error") 
  # compute absolute value of the error of predicted value as percentage of reported consumption data 
  crossValM12P$percentAbsError <- abs(hrs2P$cndurImputed - hrs2P$cndurWOR) / hrs2P$cndurWOR
  crossValM12P$Error <- hrs2P$cndurImputed - hrs2P$cndurWOR
 
   # Save error percents
  
  crossValTwoPerson[,scenarios] <- crossValM12P$Error
  hrs2P$errorPercent <- abs(hrs2P$cndurImputed - hrs2P$cndurWOR) / hrs2P$cndurWOR
  # cat("\nhrs2P at end of loop is ",dim(hrs2P)[1])
  
} # end for scenarios loop

write.csv(saveM12P,"~/desktop/saveM12P.csv")
write.csv(saveM11P,"~/desktop/saveM11P.csv")

plot(density(crossValTwoPerson[,1]),main="Density Plots for 2-Person Prediction Errors\nPredicted - Actual",ylim=c(0,.00005),xlim=c(-100000,50000),xlab="Prediction Error ($)")
for (i in 2:n){
   
  lines(density(crossValTwoPerson[,i]))
}

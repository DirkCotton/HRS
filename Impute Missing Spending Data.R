# The CAMS data set contains spending data for HRS respondents but only for about one-third of them.
# The purpose of this script is to impute CAMS spending data for all HRS repondents using dummy variables for wave of retirement as the constant for cHat.
  
  # cams <- read.csv('~/Dropbox/Robo Research/Retirement-Selected CAMS data.csv')
  cams <- read.csv('~/Dropbox/Robo Research/CAMS data.csv') 
  cams <- cams[cams$selectedBy != "notRetired",]  # remove household not retired
  hrs <- read.csv('~/Dropbox/Robo Research/HRS Retired.csv')
  hrs <- hrs[hrs$selectedBy != "notRetired",]  # remove household not retired
    
  cat("\nCAMS 1-person Household observations= ",dim(cams1P)[1])
  cat("\nCAMS 1-person households not retired= ",sum(cams1P$selectedBy == "notRetired"))
  cat("\nConsumer Non-Dur spending for Wave of Retirement in 1-Person households is missing for ",sum(is.na(cams1P$cndurWOR))," CAMS observations -- rejected")
  cat("\nCAMS observations for retired 1-Person households=",sum(!(is.na(cams1P$cndurWOR)) & cams1P$selectedBy != "notRetired"))
  
  cat("\nNumber of 1-person households not retired= ",sum(cams$earliestRetire == 0 & cams$persons == 1))
  cat("\nNumber of 2-person households not retired= ", sum(cams$earliestRetire == 0 & cams$persons == 2))
  cat("\n1-person and 2-person households with HRS assets plus income <= 0 and rejected is ",sum(cams$assets <= 0))
  
  # define Gross Total Wealth as all income and assets available to pay expenses in a give year
  # cams2 contains both 1- and 2-person households
  
  cams2 <- cams  # preserve original CAMS data
  retiredHHID <- hrs$hhid[hrs$selectedBy != "notRetired"]
  retiredHRSinCams <- length(cams2$hhid %in% retiredHHID)
  cat("\nCAMS households available for retired HRS households ", retiredHRSinCams)
  
  cndurStart <- match("h5cndur",colnames(cams))  # first column of consumer non-durables for waves 5-12

  cams2 <- cams2[cams2$selectedBy != "notRetired",]
  
  # reject all CAMS data when wave of retirement is < 5.
  beforeW5 <- sum (cams2$earliestRetire <= 5)
  cams2 <- cams2[cams2$earliestRetire >= 5,] # no CAMS data for retirement prior to wave 5
  afterW5 <- dim(cams2)[1]
  
  for (j in 1:dim(cams2)[1]) {    # select consumer non-durable spending for wave of retirement
    cams2$cndurWOR[j] <- cams2[j,cams2$earliestRetire[j] + (cndurStart - 5)]
  }
  
  # reject all households not retired
  notRet <- sum(cams2$selectedBy != "notRetired")
  cams2 <- cams2[cams2$selectedBy != "notRetired",]
  
  # reject all households with zero  consumer non-durable spending
  noWOR <- sum(cams2$cndurWOR == 0)
  cat("\nReject ",noWOR," CAMS observations with spending = 0 for wave of retirement")
  cams2 <- cams2[cams2$cndurWOR > 0,]
  hasWOR <- dim(cams2)[1]
  
  # reject all households with zero income and zero savings
  noIncomeSave <- sum(cams2$portfolioAssetsTotal + cams2$pensionIncome + cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2 + cams2$paycheck <= 0 )
  cams2 <- cams2[cams2$portfolioAssetsTotal + cams2$pensionIncome + cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2 + cams2$paycheck > 0 ,]
  cat("\nCAMS households rejected because no Income or savings ",noIncomeSave)
  afterNoInc <- dim(cams2)[1]
  
  cams2$gtw <- cams2$portfolioAssetsTotal + cams2$paycheck + cams2$pensionIncome + cams2$ssIncAtRetire1 + cams2$ssIncAtRetire2
 
  cams2 <- cams2[cams2$gtw > 0,]  # if gtw is <= 0, reject from data set as implausible (  # reject all households with zero income and zero savings)
  
  # create vector of HHIDS in HRS that are availailable in filtered CAMS2 data set
  hrsInCams2 <- hrs$hhid[hrs$hhid %in% cams2$hhid]
  
  # normalizing variable for all CAMS and HRS
  cams$gtw <- cams$portfolioAssetsTotal + cams$paycheck + cams$ssIncAtRetire1 + cams$ssIncAtRetire2 + cams$pensionIncome
  hrs$gtw <- hrs$portfolioAssetsTotal + hrs$paycheck + hrs$ssIncAtRetire1 + hrs$ssIncAtRetire2 + hrs$pensionIncome
  
  
  # separate 1- and 2-person households
  cams1P <- cams2[cams2$persons == 1,]  # CAMS data for one-person households
  cams2P <- cams2[cams2$persons == 2,]  # CAMS data for two-person households
  
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
  
  cat("\n\n")
  print(summary(fit))
  
  cams1Pgtw$cndurExpected <- (fit$coefficients[1] * cams1Pgtw$d5 + fit$coefficients[2] * cams1Pgtw$d6 +fit$coefficients[3] * cams1Pgtw$d7 +fit$coefficients[4] * cams1Pgtw$d8 + fit$coefficients[5] * cams1Pgtw$d9 + fit$coefficients[6] * cams1Pgtw$d10 +fit$coefficients[7] * cams1Pgtw$d11 + fit$coefficients[8] * cams1Pgtw$d12 + fit$coefficients[9] * cams1Pgtw$male + fit$coefficients[10] * cams1Pgtw$ssIncAtRetire1 + fit$coefficients[11] * cams1Pgtw$portfolioAssetsTotal + fit$coefficients[12] * cams1Pgtw$pensionIncome + fit$coefficients[13] * cams1Pgtw$ageAtRetirement1)
  
  cams$dummy1P <- (fit$coefficients[1] * cams$d5 + fit$coefficients[2] * cams$d6 +fit$coefficients[3] * cams$d7 + fit$coefficients[4] * cams$d8 + fit$coefficients[5] * cams$d9 + fit$coefficients[6] * cams$d10 + fit$coefficients[7] * cams$d11 + fit$coefficients[8] * cams$d12 + fit$coefficients[9] * cams$male + fit$coefficients[10] * cams$ssIncAtRetire1 / cams$gtw + fit$coefficients[11] * cams$portfolioAssetsTotal / cams$gtw + fit$coefficients[12] * cams$pensionIncome / cams$gtw + fit$coefficients[13] * cams$ageAtRetirement1) 
  
  # create HRS column for wave-of-retirement consumption for households with available CAMS data (not imputed)
  hrs$cndurWOR <- rep(NA,dim(hrs)[1])
  for (j in 1:length(hrsInCams2)){
    hrs$cndurWOR[which(hrs$hhid == hrsInCams2[j])] <- cams2$cndurWOR[j]
  } 
  
  # predict consumption for HRS observations even if consumption data is in CAMS for
  # for 1-person households retiring wave 5 or later
  hrs$cndurImputed <- rep(NA,dim(hrs)[1])
  

  hrs$cndurImputed[hrs$earliestRetire >= 5 & hrs$persons == 1] <- 
    (  fit$coefficients[1]  * hrs$d5[hrs$earliestRetire >= 5 & hrs$persons == 1] 
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
  
#########################################  
# Predict missing consumer non-durable spending data for 2-person households
#########################################  

  
  # Merge any duplicated households in cams2P by summing CAMS spending in one and deleting the other
  
  cat ("\nCAMS 2-person households data contains ", sum(duplicated(cams2P$hhid))," duplicated households that will be merged." )
  
  
  
  # build a data frame of normalized data (using gtw) 
  cams2Pgtw <- data.frame(cams2P$hhid,cams2P$cndurWOR,cams2P$ageAtRetirement1,cams2P$ageAtRetirement2,cams2P$ssIncAtRetire1/cams2P$gtw,cams2P$ssIncAtRetire2/cams2P$gtw,cams2P$portfolioAssetsTotal/cams2P$gtw,cams2P$paycheck/cams2P$gtw,cams2P$pensionIncome/cams2P$gtw,cams2P$gtw,cams2P$earliestRetire)
  colnames(cams2Pgtw) <- c("hhid","cndurWOR","ageAtRetirement1","ageAtRetirement2","ssIncAtRetire1","ssIncAtRetire2","portfolioAssetsTotal","paycheck","pensionIncome","gtw","earliestRetire")
  cams2Pgtw$norm <- cams2Pgtw$ssIncAtRetire1 +  cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$paycheck + cams2Pgtw$pensionIncome
  cams2Pgtw$male <- NA
  
  cat("\nObservations incorrectly normalized (2-person households) = ",sum((cams2Pgtw$norm < .99)))
  
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
  
  cat("\n\n")
  print(summary(fit2))
  
  # cams2$portfolioAssetsTotal[cams2$portfolioAssetsTotal < 0] <- 0
  cams2Pgtw$cndurExpected <- (fit2$coefficients[1] * cams2Pgtw$d5 + fit2$coefficients[2] * cams2Pgtw$d6 + fit2$coefficients[3] * cams2Pgtw$d7 +fit2$coefficients[4] * cams2Pgtw$d8 + fit2$coefficients[5] * cams2Pgtw$d9 + fit2$coefficients[6] * cams2Pgtw$d10 +fit2$coefficients[7] * cams2Pgtw$d11 + fit2$coefficients[8] * cams2Pgtw$d12 + fit2$coefficients[9] * cams2Pgtw$ssIncAtRetire1 + fit2$coefficients[10] * cams2Pgtw$ssIncAtRetire2 + fit2$coefficients[11] * cams2Pgtw$portfolioAssetsTotal + fit2$coefficients[12] * cams2Pgtw$pensionIncome + fit2$coefficients[13] * cams2Pgtw$ageAtRetirement1 + fit2$coefficients[14] * cams2Pgtw$ageAtRetirement2)
  cams$dummy2P <- (fit2$coefficients[1] * cams$d5 + fit2$coefficients[2] * cams$d6 + fit2$coefficients[3] * cams$d7 + fit2$coefficients[4] * cams$d8 + fit2$coefficients[5] * cams$d9 + fit2$coefficients[6] * cams$d10 + fit2$coefficients[7] * cams$d11 + fit2$coefficients[8] * cams$d12 + (fit2$coefficients[9] * cams$ssIncAtRetire1 / cams$gtw) + (fit2$coefficients[10] * cams$ssIncAtRetire2 / cams$gtw) + (fit2$coefficients[11] * cams$portfolioAssetsTotal / cams$gtw) + (fit2$coefficients[12] * cams$pensionIncome / cams$gtw) + (fit2$coefficients[13] * cams$ageAtRetirement1) + (fit2$coefficients[14] * cams$ageAtRetirement2))
  
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
  
  
  x1 <- cbind(cams1Pgtw[,1:2],round(cams1Pgtw[,21]))
  colnames(x1) <- c("hhid","cndWOR","cndurExpected")
  x2 <- cbind(cams2Pgtw[,1:2],round(cams2Pgtw[,21]))
  colnames(x2) <- c("hhid","cndWOR","cndurExpected")
  x3 <- rbind(x1,x2)
  x3 <- (x3[with(x3, order(x3$hhid)), ])
  
  x3 <- (x3[with(x3, order(x3$hhid)), ])
  z <- full_join(hrs,x3)


  # save.image ("saveCams")
  
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
  
  cat("\n\nfit3: 1-person household, no dummy variables")
  print(summary(fit3))
  
  cams$noDummy1P <- rep(0,dim(cams)[1])
  cams$noDummy1P <- (fit3$coefficients[1] + fit3$coefficients[2] * cams$male + fit3$coefficients[3] * cams$ssIncAtRetire1 / cams$gtw + fit3$coefficients[4] * cams$portfolioAssetsTotal / cams$gtw + fit3$coefficients[5] * cams$pensionIncome / cams$gtw + fit3$coefficients[6] * cams$ageAtRetirement1)
  
  # predict consumption for HRS observations even if consumption data is in CAMS for
  # for 1-person households retiring wave 4 or earlier
  hrs$cndurImputed[hrs$earliestRetire <= 4 & hrs$persons == 1] <- 
    (  fit3$coefficients[1] 
     + fit3$coefficients[2] * hrs$male[hrs$earliestRetire <= 4 & hrs$persons == 1] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 1] 
     + fit3$coefficients[3] * hrs$ssIncAtRetire1[hrs$earliestRetire <= 4 & hrs$persons == 1] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 1] 
     + fit3$coefficients[4] * hrs$portfolioAssetsTotal[hrs$earliestRetire <= 4 & hrs$persons == 1] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 1] 
     + fit3$coefficients[5] * hrs$pensionIncome[hrs$earliestRetire <= 4 & hrs$persons == 1] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 1] 
     + fit3$coefficients[6] * hrs$ageAtRetirement1[hrs$earliestRetire <= 4 & hrs$persons == 1] 
     # + fit3$coefficients[7] * hrs$earliestRetire[hrs$earliestRetire <= 4 & hrs$persons == 1]    ######################
      ) 
  
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
  
  cat("\n\nfit4: 2-person household, no dummy variables")
  print(summary(fit4))
  
  cams$noDummy2P <- rep(0,dim(cams)[1])
  cams$noDummy2P <- (fit4$coefficients[1] + fit4$coefficients[2] * cams$ssIncAtRetire1 / cams$gtw + fit4$coefficients[3] * cams$ssIncAtRetire2 / cams$gtw + fit4$coefficients[4] * cams$portfolioAssetsTotal / cams$gtw + fit4$coefficients[5] * cams$pensionIncome / cams$gtw + fit4$coefficients[6] * cams$ageAtRetirement1 + fit4$coefficients[7] * cams$ageAtRetirement2)
  
  # predict consumption for HRS observations even if consumption data is in CAMS for
  # for 2-person households retiring wave 4 or earlier
  hrs$cndurImputed[hrs$earliestRetire < 5 & hrs$persons == 2] <-  
    (  fit4$coefficients[1] 
     + fit4$coefficients[2] * hrs$ssIncAtRetire1[hrs$earliestRetire <= 4 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 2] 
     + fit4$coefficients[3] * hrs$ssIncAtRetire2[hrs$earliestRetire <= 4 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 2] 
     + fit4$coefficients[4] * hrs$portfolioAssetsTotal[hrs$earliestRetire <= 4 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 2] 
     + fit4$coefficients[5] * hrs$pensionIncome[hrs$earliestRetire <= 4 & hrs$persons == 2] / hrs$gtw[hrs$earliestRetire <= 4 & hrs$persons == 2] 
     + fit4$coefficients[6] * hrs$ageAtRetirement1[hrs$earliestRetire <= 4 & hrs$persons == 2] 
     + fit4$coefficients[7] * hrs$ageAtRetirement2[hrs$earliestRetire <= 4 & hrs$persons == 2]
     # + fit4$coefficients[8] * hrs$earliestRetire[hrs$earliestRetire <= 4 & hrs$persons == 2]
     ) 
  
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
 
   # create a column of predicted consumer non-durables for all CAMS2 observations, missing data or not.
  
  # cams2$predictedCndur <- rep(0,dim(cams2)[1])
  # cams2$predictedCndur[cams2$persons == 1 & cams2$earliestRetire < 5] <- cams2$noDummy1P[cams2$persons == 1 & cams2$earliestRetire < 5]
  # cams2$predictedCndur[cams2$persons == 2 & cams2$earliestRetire < 5] <- cams2$noDummy2P[cams2$persons == 2 & cams2$earliestRetire < 5]
  # cams2$predictedCndur[cams2$persons == 1 & cams2$earliestRetire >= 5] <- cams2$dummy1P[cams2$persons == 1 & cams2$earliestRetire >= 5]
  # cams2$predictedCndur[cams2$persons == 2 & cams2$earliestRetire >= 5] <- cams2$dummy2P[cams2$persons == 2 & cams2$earliestRetire >= 5]
  # 
   # save results as spreadsheet
  
  xlSummary <- cams[,c(2:5,10:26,28,58,68,54,73,72)]
  
  colnames(xlSummary)[7] <- "WaveOfRetire"
  colnames(xlSummary)[26] <- "predictedCndur"
  colnames(xlSummary)[27] <- "selectedCndur"
  test <-xlSummary
  # if no consumption data and no predicted consumption, selectedCndur will equal NA. Drop these rows.
  # save these rows in CAMS so we can delete these HHID's from the HRS data 
  xlSummary <- xlSummary[!is.na(xlSummary$selectedCndur),] 
  
  write.csv(xlSummary,"Imputed CAMS.csv")  # write a summary to Excel .csv file
  
  colnames(hrs)[40] <- "cndurImputedM1"




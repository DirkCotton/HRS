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

crossValOnePerson <- matrix(NA,length(badErrorHHID1) * 2,100)

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
cams2 <- cams2[cams2$selectedBy != "notRetired" & cams2$cndurWOR > 0 & cams2$hhid %in% hrs$hhid & cams2$gtw > 0 & cams2$persons == 1 ,]

# create the "bad prediction" half of the validation set from CrossValidationModel1.R results for 1-person households.
badErrorHHID1 <- highError1Phhid   

# Step 1: remove households with bad predictions and set aside.
  
badErrorHH <- cams2[cams2$hhid %in% badErrorHHID1,]
goodDataOnly <- cams2[!(cams2$hhid %in% badErrorHHID1),]

goodErrorHH <- cams2[!(cams2$hhid %in% badErrorHHID1),]

# remove bad error households from cams2 training set.
# cams2 <- cams2[!(cams2$hhid %in% badErrorHHID1) & !(cams2$hhid %in% badErrorHHID2),]
# cat("\n",length(badErrorHHID1)," households with prediction errors > 100% constitute half of validation set and are removed from training set.")

retiredhrsinCams <- length(cams2$hhid %in% retiredHHID)
    
# create training sets (all cams records except bad estimation error households)

badError1 <- cams2[cams2$persons == 1 & (cams2$hhid %in% badErrorHHID1),]
hrsTrain1 <- cams2[cams2$persons == 1 & !(cams2$hhid %in% badErrorHHID1),]
# cat("\nTraining set for One-Person households consists of ",dim(hrsTrain1)[1]," households.")

for(scenarios in 1:n) {
   
  # cat ("\n",scenarios," of ",n)
  
  # Impute spending data for all hrs observations using a model with age-of-retirement and age-of-retirement^2 instead of dummy variables 
  
  # retiredHHID <- hrs$hhid[hrs$selectedBy != "notRetired"]
  # retiredhrsinCams <- length(cams2$hhid %in% retiredHHID)
  # # cat("\nCAMS households available for retired hrs households ", retiredhrsinCams)
  
  cndurStart <- match("h5cndur",colnames(cams))  # first column of consumer non-durables for waves 5-12
  
  for (j in 1:dim(cams2)[1]) {    # select consumer non-durable spending for wave of retirement
    cams2$cndurWOR[j] <- cams2[j,cams2$earliestRetire[j] + (cndurStart - 5)]
  }
  
  # for cross-validation, only use CAMS data when there is associated HHID in hrs (otherwise, there is no hrs data to impute from) 
  cams2 <- cams2[cams2$hhid %in% hrs$hhid,]
 
  # separate 1- and 2-person households
  cams1P <- cams2[cams2$persons == 1,]  # CAMS data for one-person households
  
  # randomly select same number of training set observations from "good" households with smaller prediction error
  goodErrorHHID1 <- sample(hrsTrain1$hhid,length(badErrorHHID1))
  
  # cat("\n",length(goodErrorHHID1)," 1-person households with prediction errors < 100% constitute half of validation set and are removed from training set.")
   
  # ... and only use hrs HHID's
  hrs <- hrs[hrs$hhid %in% cams1P$hhid,]
  # sort cams2 and hrs by HHID
  hrs <- hrs[order(hrs$hhid),] 
  cams1P <- cams1P[order(cams1P$hhid),] 
  savecams1Pl <- dim(cams1P)[1]
  # validation set (hrs) contains all records not in training set
  
  randomGood <- goodDataOnly[sample(nrow(goodDataOnly), size=dim(badErrorHH)[1]), ] 

  cams1P <- goodDataOnly[!(goodDataOnly$hhid %in% randomGood$hhid),]   # model set is cams1P
  # cat("\nModel data on ",dim(cams1P)[1]," households with mean HHID ",mean(cams1P$hhid))
  
  hrs <- badErrorHH  # prediction set is hrs
  # cat("\nPredict data with ",dim(hrs)[1]," households with mean HHID ",mean(hrs$hhid))
  
  
  # cat("\nValidation set consists of ",sum(badErrorHHID1 %in% hrs$hhid)," bad households and ",sum(goodErrorHHID1 %in% hrs$hhid),"good.")
  
  hrs$gtw <- rep(0,dim(hrs)[1])
  hrs$gtw <- hrs$portfolioAssetsTotal + hrs$paycheck + hrs$ssIncAtRetire1 + hrs$ssIncAtRetire2 + hrs$pensionIncome
  hrs$male <- rep(0,dim(hrs)[1])   # initialize vector to 0
  hrs$male[hrs$gender1 == "1.male" | hrs$gender1 == ".m"] <- 1
  
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
  
  # cat("\nObservations incorrectly normalized (1-Person Households) = ",sum(cams1Pgtw$norm < .99))
  
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
  
  # cat("\n\n")
  # print(summary(fit))
  
  cams1Pgtw$cndurExpected <- (fit$coefficients[1] * cams1Pgtw$d5 + fit$coefficients[2] * cams1Pgtw$d6 +fit$coefficients[3] * cams1Pgtw$d7 +fit$coefficients[4] * cams1Pgtw$d8 + fit$coefficients[5] * cams1Pgtw$d9 + fit$coefficients[6] * cams1Pgtw$d10 +fit$coefficients[7] * cams1Pgtw$d11 + fit$coefficients[8] * cams1Pgtw$d12 + fit$coefficients[9] * cams1Pgtw$male + fit$coefficients[10] * cams1Pgtw$ssIncAtRetire1 + fit$coefficients[11] * cams1Pgtw$portfolioAssetsTotal + fit$coefficients[12] * cams1Pgtw$pensionIncome + fit$coefficients[13] * cams1Pgtw$ageAtRetirement1)
  
  # predict consumption for hrs observations even if consumption data is in CAMS for
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
  
  # Save error percents
  
  crossValOnePerson[,scenarios] <- crossValM11P$Error
  # cat("\nCrossVal1Person scenario ",scenarios)

} # end for scenarios loop


write.csv(saveM11P,"~/desktop/saveM11P.csv")

# graph density of all n scenarios for one-person households
# cat("\nRemoved ",sum(crossValOnePerson > 5)," outliers with greater than 500% error from ",length(crossValOnePerson)," samples.")
# crossValOnePerson[crossValOnePerson < -100000 ] <- 0
plot(density(crossValOnePerson[,1]),main="1-Person Household Bad Predictions\nPredicted - Actual",xlab="Prediction Error ($)")
for (i in 2:n){
  lines(density(crossValOnePerson[,i]))
}



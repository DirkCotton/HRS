# Cross-validate models by setting aside 20% of CAMS data, building models M1 and M2
# with the remaining 80% of the CAMS data and then comparing model predictions to actual data.



#-----------------
# Impute spending data for all HRS observations using a model with age-of-retirement and age-of-retirement^2 instead of dummy variables 


# cams <- read.csv('~/Dropbox/Robo Research/Retirement-Selected CAMS data.csv')
cams <- read.csv('~/Dropbox/Robo Research/CAMS data.csv')
cams <- cams[cams$selectedBy != "notRetired",]  # remove household not retired
# save imputed data from first model
saveM1 <- hrs$cndurImputedM1
hrs <- read.csv('~/Dropbox/Robo Research/HRS Retired.csv')
hrs <- hrs[hrs$selectedBy != "notRetired",]  # remove household not retired
hrs$cndurImputedM1 <- saveM1

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

# for cross-validation, only use CAMS data when there id associated HHID in HRS (otherwise, there is no HRS data to impute from) 
cams2 <- cams2[cams2$hhid %in% hrs$hhid,]
# ... and only use HRS HHID's
hrs <- hrs[hrs$hhid %in% cams2$hhid,]
# sort cams2 and hrs by HHID
hrs <- hrs[order(hrs$hhid),] 
cams2 <- cams2[order(cams2$hhid),] 

# separate 1- and 2-person households
cams1P <- cams2[cams2$persons == 1,]  # CAMS data for one-person households
cams2P <- cams2[cams2$persons == 2,]  # CAMS data for two-person households

# for cross-validation, set aside 20% of CAMS2 data for testing and 80% to model
setAside <- cams1P[sample(nrow(cams1P),round(.2*dim(cams1P)[1])), ]
hrs <- setAside
cams1P <- cams1P[!(cams1P$hhid %in% setAside$hhid),]


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
cams1Pgtw$EarliestSquared <- cams1Pgtw$earliestRetire^2
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

# fit5 is same as fit3 except WOR and WOR^2 added as variables
fit5 <- lm(cams1Pgtw$cndurWOR ~ 
             cams1Pgtw$male 
           + cams1Pgtw$ssIncAtRetire1 
           + cams1Pgtw$portfolioAssetsTotal 
           + cams1Pgtw$pensionIncome 
           + cams1Pgtw$ageAtRetirement1 
           + cams1Pgtw$earliestRetire
           # + cams1Pgtw$EarliestSquared
)

cat("\n\n")
print(summary(fit5))

cams1Pgtw$cndurExpected <- (fit5$coefficients[5] * cams1Pgtw$male + fit5$coefficients[1] * cams1Pgtw$ssIncAtRetire1 + fit5$coefficients[2] * cams1Pgtw$portfolioAssetsTotal + fit5$coefficients[3] * cams1Pgtw$pensionIncome + fit5$coefficients[4] * cams1Pgtw$ageAtRetirement1 + fit5$coefficients[6] * cams1Pgtw$earliestRetire + fit5$coefficients[7] * cams1Pgtw$EarliestSquared)

# cams$dummy1P <- (fit$coefficients[1] * cams$d5 + fit$coefficients[2] * cams$d6 +fit$coefficients[3] * cams$d7 + fit$coefficients[4] * cams$d8 + fit$coefficients[5] * cams$d9 + fit$coefficients[6] * cams$d10 + fit$coefficients[7] * cams$d11 + fit$coefficients[8] * cams$d12 + fit$coefficients[9] * cams$male + fit$coefficients[10] * cams$ssIncAtRetire1 / cams$gtw + fit$coefficients[11] * cams$portfolioAssetsTotal / cams$gtw + fit$coefficients[12] * cams$pensionIncome / cams$gtw + fit$coefficients[13] * cams$ageAtRetirement1) 

# create HRS column for wave-of-retirement consumption for households with available CAMS data (not imputed)
hrs$cndurWOR <- rep(NA,dim(hrs)[1])
for (j in 1:length(hrsInCams2)){
  hrs$cndurWOR[which(hrs$hhid == hrsInCams2[j])] <- cams2$cndurWOR[j]
} 

# predict consumption for HRS observations even if consumption data is in CAMS for
# for 1-person households retiring wave 5 or later
hrs$cndurImputedM2 <- rep(NA,dim(hrs)[1])

hrs$cndurImputedM2[hrs$persons == 1] <- 
  (    fit5$coefficients[1]
       + fit5$coefficients[2] * hrs$male[hrs$persons == 1] 
       + fit5$coefficients[3] * hrs$ssIncAtRetire1[hrs$persons == 1] / hrs$gtw[hrs$persons == 1] 
       + fit5$coefficients[4] * hrs$portfolioAssetsTotal[hrs$persons == 1] / hrs$gtw[hrs$persons == 1] 
       + fit5$coefficients[5] * hrs$pensionIncome[hrs$persons == 1] / hrs$gtw[hrs$persons == 1] 
       + fit5$coefficients[6] * hrs$ageAtRetirement1[hrs$persons == 1]
       + fit5$coefficients[7] * hrs$earliestRetire[hrs$persons == 1]
       # + fit5$coefficients[8] * hrs$earliestRetire[hrs$persons == 1] ^2
  ) 


# for cross-validation, store hrs imputed data for 1-person households
saveM21P <- cbind(hrs$hhid,hrs$cndurWOR,round(hrs$cndurImputedM2))
colnames(saveM21P) <- c("hhid","cndurWOR","cndurImputedM21P") 

cat("\nCorrelation for Model2 1-Person Households ",cor(hrs$cndurWOR,hrs$cndurImputedM2))

#########################################  
# Predict missing consumer non-durable spending data for 2-person households
#########################################  


# Merge any duplicated households in cams2P by summing CAMS spending in one and deleting the other

cat ("\nCAMS 2-person households data contains ", sum(duplicated(cams2P$hhid))," duplicated households that will be merged." )

# for cross-validation, set aside 20% of CAMS2P data for testing and 80% to model
setAside2 <- cams2P[sample(nrow(cams2P),round(.2*dim(cams2P)[1])), ]
hrs <- setAside2
cams2P <- cams2P[!(cams2P$hhid %in% setAside2$hhid),]

# build a data frame of normalized data (using gtw) 
cams2Pgtw <- data.frame(cams2P$hhid,cams2P$cndurWOR,cams2P$ageAtRetirement1,cams2P$ageAtRetirement2,cams2P$ssIncAtRetire1/cams2P$gtw,cams2P$ssIncAtRetire2/cams2P$gtw,cams2P$portfolioAssetsTotal/cams2P$gtw,cams2P$paycheck/cams2P$gtw,cams2P$pensionIncome/cams2P$gtw,cams2P$gtw,cams2P$earliestRetire)
colnames(cams2Pgtw) <- c("hhid","cndurWOR","ageAtRetirement1","ageAtRetirement2","ssIncAtRetire1","ssIncAtRetire2","portfolioAssetsTotal","paycheck","pensionIncome","gtw","earliestRetire")
cams2Pgtw$EarliestSquared <- cams2Pgtw$earliestRetire^2
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
cams2Pgtw$EarliestSquared <- cams2Pgtw$earliestRetire^2

fit6 <- lm(  cams2Pgtw$cndurWOR ~ 
               cams2Pgtw$ssIncAtRetire1
             + cams2Pgtw$ssIncAtRetire2
             + cams2Pgtw$portfolioAssetsTotal
             + cams2Pgtw$pensionIncome
             + cams2Pgtw$ageAtRetirement1
             + cams2Pgtw$ageAtRetirement2
             + cams2Pgtw$earliestRetire
             # + cams2Pgtw$EarliestSquared
) 

cat("\n\n")
print(summary(fit6))


cams2Pgtw$cndurExpected <- (  fit6$coefficients[1]
                              + fit6$coefficients[2]  * cams2Pgtw$ssIncAtRetire1
                              + fit6$coefficients[3]  * cams2Pgtw$ssIncAtRetire2 
                              + fit6$coefficients[4]  * cams2Pgtw$portfolioAssetsTotal 
                              + fit6$coefficients[5]  * cams2Pgtw$pensionIncome 
                              + fit6$coefficients[6]  * cams2Pgtw$ageAtRetirement1 
                              + fit6$coefficients[7]  * cams2Pgtw$ageAtRetirement2 
                              + fit6$coefficients[8]  * cams2Pgtw$earliestRetire 
                              + fit6$coefficients[9]  * cams2Pgtw$EarliestSquared 
)


# cams$dummy2P <- (fit2$coefficients[1] * cams$d5 +   2$coefficients[2] * cams$d6 + fit2$coefficients[3] * cams$d7 + fit2$coefficients[4] * cams$d8 + fit2$coefficients[5] * cams$d9 + fit2$coefficients[6] * cams$d10 + fit2$coefficients[7] * cams$d11 + fit2$coefficients[8] * cams$d12 + (fit2$coefficients[9] * cams$ssIncAtRetire1 / cams$gtw) + (fit2$coefficients[10] * cams$ssIncAtRetire2 / cams$gtw) + (fit2$coefficients[11] * cams$portfolioAssetsTotal / cams$gtw) + (fit2$coefficients[12] * cams$pensionIncome / cams$gtw) + (fit2$coefficients[13] * cams$ageAtRetirement1) + (fit2$coefficients[14] * cams$ageAtRetirement2))


# predict consumption for HRS observations even if consumption data is in CAMS for
# for 2-person households retiring wave 5 or later 

hrs$cndurImputedM2[hrs$persons == 2] <-
  (    fit6$coefficients[1]
       + fit6$coefficients[2]  * hrs$ssIncAtRetire1[hrs$persons == 2] / hrs$gtw[hrs$persons == 2] 
       + fit6$coefficients[3]  * hrs$ssIncAtRetire2[hrs$persons == 2] / hrs$gtw[hrs$persons == 2] 
       + fit6$coefficients[4]  * hrs$portfolioAssetsTotal[hrs$persons == 2] / hrs$gtw[hrs$persons == 2] 
       + fit6$coefficients[5]  * hrs$pensionIncome[hrs$persons == 2] / hrs$gtw[hrs$persons == 2] 
       + fit6$coefficients[6]  * hrs$ageAtRetirement1[hrs$persons == 2] 
       + fit6$coefficients[7]  * hrs$ageAtRetirement2[hrs$persons == 2] 
       + fit6$coefficients[8]  * hrs$earliestRetire[hrs$persons == 2] 
       #  + fit6$coefficients[9]  * hrs$earliestRetire[hrs$persons == 2]^2
  )  

write.csv(hrs,"~/desktop/compare.csv")

cat("\nCorrelation for Model2 2-Person Households ",cor(hrs$cndurWOR,hrs$cndurImputedM2))

# for cross-validation, store hrs imputed data for 1-person households
saveM22P <- cbind(hrs$hhid,hrs$cndurWOR,round(hrs$cndurImputedM2))
colnames(saveM22P) <- c("hhid","cndurWOR","cndurImputedM21P") 

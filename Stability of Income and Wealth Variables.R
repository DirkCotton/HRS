# check stability of model variables across waves of retirement

# NOTE:  RUN Impute Missing Spending Data.R FIRST!

# following is the consumption model
# fit <- lm(cams1Pgtw$cndurWOR ~ cams1Pgtw$d5 + cams1Pgtw$d6 + cams1Pgtw$d7 + cams1Pgtw$d8 + cams1Pgtw$d9 + cams1Pgtw$d10 + cams1Pgtw$d11 + cams1Pgtw$d12 + cams1Pgtw$male + cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$portfolioAssetsTotal + cams1Pgtw$pensionIncome + cams1Pgtw$ageAtRetirement1 + cams1Pgtw$male -1)

# rebuild the model "fit" with every dollar-variable multiplied by the wave of retirement dummy variable

fitStable <- lm(cams1Pgtw$cndurWOR ~ cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$portfolioAssetsTotal + cams1Pgtw$pensionIncome + cams1Pgtw$ageAtRetirement1  -1)


fitStable1P <- lm(cams1Pgtw$cndurWOR ~ cams1Pgtw$d5 + cams1Pgtw$d6 + cams1Pgtw$d7 + cams1Pgtw$d8 + cams1Pgtw$d9 + cams1Pgtw$d10 + cams1Pgtw$d11 + cams1Pgtw$d12 + cams1Pgtw$male + cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$portfolioAssetsTotal + cams1Pgtw$pensionIncome + cams1Pgtw$ageAtRetirement1 + cams1Pgtw$male +cams1Pgtw$d5 * cams1Pgtw$pensionIncome + cams1Pgtw$d5 * cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$d5 * cams1Pgtw$portfolioAssetsTotal
                  + cams1Pgtw$d5 * cams1Pgtw$pensionIncome + cams1Pgtw$d6 * cams1Pgtw$pensionIncome + cams1Pgtw$d6 * cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$d6 * cams1Pgtw$portfolioAssetsTotal
                  + cams1Pgtw$d6 * cams1Pgtw$pensionIncome + cams1Pgtw$d7 * cams1Pgtw$pensionIncome + cams1Pgtw$d7 * cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$d7 * cams1Pgtw$portfolioAssetsTotal
                  + cams1Pgtw$d7 * cams1Pgtw$pensionIncome + cams1Pgtw$d8 * cams1Pgtw$pensionIncome + cams1Pgtw$d8 * cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$d8 * cams1Pgtw$portfolioAssetsTotal
                  + cams1Pgtw$d8 * cams1Pgtw$pensionIncome + cams1Pgtw$d9 * cams1Pgtw$pensionIncome + cams1Pgtw$d9 * cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$d9 * cams1Pgtw$portfolioAssetsTotal
                  + cams1Pgtw$d9 * cams1Pgtw$pensionIncome + cams1Pgtw$d10 * cams1Pgtw$pensionIncome + cams1Pgtw$d10 * cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$d10 * cams1Pgtw$portfolioAssetsTotal
                  + cams1Pgtw$d10 * cams1Pgtw$pensionIncome + cams1Pgtw$d11 * cams1Pgtw$pensionIncome + cams1Pgtw$d11 * cams1Pgtw$ssIncAtRetire1 + cams1Pgtw$d11 * cams1Pgtw$portfolioAssetsTotal
                  + cams1Pgtw$d11 * cams1Pgtw$pensionIncome  -1)

fitStable2P  <- lm(cams2Pgtw$cndurWOR ~ cams2Pgtw$d5 + cams2Pgtw$d6 + cams2Pgtw$d7 + cams2Pgtw$d8 + cams2Pgtw$d9 + cams2Pgtw$d10 + cams2Pgtw$d11 + cams2Pgtw$d12 + cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$pensionIncome + cams2Pgtw$ageAtRetirement1 + cams2Pgtw$ageAtRetirement2  + cams2Pgtw$d5 * cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$d5 * cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$d5 * cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$d5 * cams2Pgtw$pensionIncome + cams2Pgtw$d6 * cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$d6 * cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$d6 * cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$d6 * cams2Pgtw$pensionIncome + cams2Pgtw$d7 * cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$d7 * cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$d7 * cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$d7 * cams2Pgtw$pensionIncome + cams2Pgtw$d8 * cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$d8 * cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$d8 * cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$d8 * cams2Pgtw$pensionIncome + cams2Pgtw$d9 * cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$d9 * cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$d9 * cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$d9 * cams2Pgtw$pensionIncome + cams2Pgtw$d10 * cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$d10 * cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$d10 * cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$d10 * cams2Pgtw$pensionIncome + cams2Pgtw$d11 * cams2Pgtw$ssIncAtRetire1 + cams2Pgtw$d11 * cams2Pgtw$ssIncAtRetire2 + cams2Pgtw$d11 * cams2Pgtw$portfolioAssetsTotal + cams2Pgtw$d11 * cams2Pgtw$pensionIncome  -1)

print(summary(fitStable1P))
print(summary(fitStable2P))


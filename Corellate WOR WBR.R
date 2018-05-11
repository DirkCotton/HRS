wor1P <- cbind(cams$cndurWBR[cams$persons == 1],cams$cndurWOR[cams$persons == 1])

wor1P <- wor1P[!is.na(wor1P[,1]) & !is.na(wor1P[,2]),]
wor1P <- wor1P[wor1P[,1] > 0 & wor1P[,2] > 0,]

cat ("\nCorrelation of 1-person spending wave of retirement versus previous wave ",cor(wor1P))

wor2P <- cbind(cams$cndurWBR[cams$persons == 2],cams$cndurWOR[cams$persons == 2])

wor2P <- wor2P[!is.na(wor2P[,1]) & !is.na(wor2P[,2]),]
wor2P <- wor2P[wor2P[,1] > 0 & wor2P[,2] > 0,]

cat ("\nCorrelation of 2-person spending wave of retirement versus previous wave ",cor(wor2P))

cat("\n",mean(wor1P[,1]))
cat("\n",mean(wor1P[,2]))
cat("\n",mean(wor2P[,1]))
cat("\n",mean(wor2P[,2]))
cat("\n",median(wor2P[,1]))
cat("\n",median(wor2P[,2]))

# Compare my R findings with HRS Summary Statistics. Is the count of r1ptyp1 when r1ptyp1 >= 1 4,354?

firstPTYP<- which(colnames(hrs_df) =="r1ptyp1")  # find first column number using full HRS database
# lastPTYP <- which(colnames(hrs_df) =="r12ptyp1")  # find last column number using full HRS database

cat("\nNumber of r1ptyp1 >0 and not NA: ",sum(hrs_df$r1ptyp1 > 0,na.rm = TRUE))

cat("\nNumber of r1ptyp1 = DB or DB+DC excluding NA: ",sum(hrs_df$r1ptyp1 == 1 | (hrs_df$r1ptyp1 == 3),na.rm = TRUE))

psum <- 0
for (wave in 1:12) {
  for (ptype in 1:1) {
    varname = paste("r",wave,"ptyp",ptype,sep="")
    
    #sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 1 | hrs_df[,which(colnames(hrs_df) == varname)] == 3,na.rm = TRUE)
    
    sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 1,na.rm = TRUE)
    psum <- psum + sumcol
    cat("\n",varname," total is ",sumcol)
  }
}

cat("\nNumber of respondents with DB: ",psum)
psum <- 0
for (wave in 1:12) {
  for (ptype in 1:1) {
    varname = paste("r",wave,"ptyp",ptype,sep="")
    
    #sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 1 | hrs_df[,which(colnames(hrs_df) == varname)] == 3,na.rm = TRUE)
    
    sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 3,na.rm = TRUE)
    psum <- psum + sumcol
    cat("\n",varname," total is ",sumcol)
  }
}

cat("\nNumber of respondents with DB and DC plan: ",psum)

psum <- 0
for (wave in 1:12) {
  for (ptype in 1:1) {
    varname = paste("s",wave,"ptyp",ptype,sep="")
    
    #sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 1 | hrs_df[,which(colnames(hrs_df) == varname)] == 3,na.rm = TRUE)
    
    sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 1,na.rm = TRUE)
    psum <- psum + sumcol
    cat("\n",varname," total is ",sumcol)
  }
}

cat("\nNumber of spouses with DB: ",psum)
psum <- 0
for (wave in 1:12) {
  for (ptype in 1:1) {
    varname = paste("s",wave,"ptyp",ptype,sep="")
    
    #sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 1 | hrs_df[,which(colnames(hrs_df) == varname)] == 3,na.rm = TRUE)
    
    sumcol <- sum(hrs_df[,which(colnames(hrs_df) == varname)] == 3,na.rm = TRUE)
    psum <- psum + sumcol
    cat("\n",varname," total is ",sumcol)
  }
}

cat("\nNumber of spouses with DB and DC plan: ",psum)


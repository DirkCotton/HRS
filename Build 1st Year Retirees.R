# Build a data frame of all HRS records who claim retirement for the first time. This is their first year of retirement.


# Retired in wave 11 (2012)
r1 <- sum(hrs_df$r11sayret  == 1,na.rm = TRUE) # completely retired
r2 <- sum(hrs_df$r11sayret   == 2,na.rm = TRUE) # partly retired
r0 <- sum(hrs_df$r11sayret   == 0,na.rm = TRUE)  # not retired
r3 <- sum(hrs_df$r11sayret   == 3,na.rm = TRUE)  # irrelevant question
totRecs2012 <- r1 + r2 + r3 + r0
cat("\nTotal records 2012",totRecs2012)

# Retired in wave 11 (2011)
r1A <- sum(hrs_df$r10sayret  == 1,na.rm = TRUE) # completely retired
r2A <- sum(hrs_df$r10sayret   == 2,na.rm = TRUE) # partly retired
r0A <- sum(hrs_df$r10sayret   == 0,na.rm = TRUE)  # not retired
r3A <- sum(hrs_df$r10sayret   == 3,na.rm = TRUE)  # irrelevant question
totRecs2011 <- r1A + r2A + r3A + r0A
cat("\nTotal records 2011",totRecs2011)

count <- sum( (hrs_df$r11sayret == 1 | hrs_df$r11sayret == 2) & (hrs_df$r10sayret == 0 | hrs_df$r10sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2012 but not in 2010: ",sum( (hrs_df$r11sayret == 1 | hrs_df$r11sayret == 2) & (hrs_df$r10sayret == 0 | hrs_df$r10sayret == 3),na.rm = TRUE ) )
retired1st2012 <- hrs_df[(!is.na(hrs_df$r11sayret) & !is.na(hrs_df$r10sayret)),]
retired1st2012 <- retired1st2012[ (retired1st2012$r11sayret == 1 | retired1st2012$r11sayret == 2) & (retired1st2012$r10sayret == 0 | retired1st2012$r10sayret == 3), ]
if (count != length(retired1st2012$hhidpn)) cat("\n***ERROR - counts don't match for 2012 ",count," ",length(retired1st2012$hhidpn))

count <- sum( (hrs_df$r10sayret == 1 | hrs_df$r10sayret == 2) & (hrs_df$r9sayret == 0 | hrs_df$r9sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2011 but not in 2010: ",count )
retired1st2011 <- hrs_df[(!is.na(hrs_df$r10sayret) & !is.na(hrs_df$r9sayret)),]
retired1st2011 <- retired1st2011[ (retired1st2011$r10sayret == 1 | retired1st2011$r10sayret == 2) & (retired1st2011$r9sayret == 0 | retired1st2011$r9sayret == 3), ]
if (count != length(retired1st2011$hhidpn)) cat("\n***ERROR - counts don't match for 2011 ",count," ",length(retired1st2011$hhidpn))

count <- sum( (hrs_df$r9sayret == 1 | hrs_df$r9sayret == 2) & (hrs_df$r8sayret == 0 | hrs_df$r8sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2010 but not in 2009: ",count )
retired1st2010 <- hrs_df[(!is.na(hrs_df$r9sayret) & !is.na(hrs_df$r8sayret)),]
retired1st2010 <- retired1st2010[ (retired1st2010$r9sayret == 1 | retired1st2010$r9sayret == 2) & (retired1st2010$r8sayret == 0 | retired1st2010$r8sayret == 3), ]
if (count != length(retired1st2010$hhidpn)) cat("\n***ERROR - counts don't match for 2010 ",count," ",length(retired1st2010$hhidpn))

count <- sum( (hrs_df$r8sayret == 1 | hrs_df$r8sayret == 2) & (hrs_df$r7sayret == 0 | hrs_df$r7sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2009 but not in 2008: ",count )
retired1st2009 <- hrs_df[(!is.na(hrs_df$r8sayret) & !is.na(hrs_df$r7sayret)),]
retired1st2009 <- retired1st2009[ (retired1st2009$r8sayret == 1 | retired1st2009$r8sayret == 2) & (retired1st2009$r7sayret == 0 | retired1st2009$r7sayret == 3), ]
if (count != length(retired1st2009$hhidpn)) cat("\n***ERROR - counts don't match for 2009 ",count," ",length(retired1st2009$hhidpn))

count <- sum( (hrs_df$r7sayret == 1 | hrs_df$r7sayret == 2) & (hrs_df$r6sayret == 0 | hrs_df$r6sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2008 but not in 2006: ",count )
retired1st2008 <- hrs_df[(!is.na(hrs_df$r7sayret) & !is.na(hrs_df$r6sayret)),]
retired1st2008 <- retired1st2008[ (retired1st2008$r7sayret == 1 | retired1st2008$r7sayret == 2) & (retired1st2008$r6sayret == 0 | retired1st2008$r6sayret == 3), ]
if (count != length(retired1st2008$hhidpn)) cat("\n***ERROR - counts don't match for 2008 ",count," ",length(retired1st2008$hhidpn))

count <- sum( (hrs_df$r7sayret == 1 | hrs_df$r7sayret == 2) & (hrs_df$r4sayret == 0 | hrs_df$r4sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2007 but not in 2005: ",count )
retired1st2007 <- hrs_df[(!is.na(hrs_df$r7sayret) & !is.na(hrs_df$r4sayret)),]
retired1st2007 <- retired1st2007[ (retired1st2007$r7sayret == 1 | retired1st2007$r7sayret == 2) & (retired1st2007$r4sayret == 0 | retired1st2007$r4sayret == 3), ]
if (count != length(retired1st2007$hhidpn)) cat("\n***ERROR - counts don't match for 2007 ",count," ",length(retired1st2007$hhidpn))

count <- sum( (hrs_df$r5sayret == 1 | hrs_df$r5sayret == 2) & (hrs_df$r4sayret == 0 | hrs_df$r4sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2006 but not in 2005: ",count )
retired1st2006 <- hrs_df[(!is.na(hrs_df$r5sayret) & !is.na(hrs_df$r4sayret)),]
retired1st2006 <- retired1st2006[ (retired1st2006$r5sayret == 1 | retired1st2006$r5sayret == 2) & (retired1st2006$r4sayret == 0 | retired1st2006$r4sayret == 3), ]
if (count != length(retired1st2006$hhidpn)) cat("\n***ERROR - counts don't match for 2006 ",count," ",length(retired1st2006$hhidpn))

count <- sum( (hrs_df$r4sayret == 1 | hrs_df$r4sayret == 2) & (hrs_df$r3sayret == 0 | hrs_df$r3sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2005 but not in 2004: ",count )
retired1st2005 <- hrs_df[(!is.na(hrs_df$r4sayret) & !is.na(hrs_df$r3sayret)),]
retired1st2005 <- retired1st2005[ (retired1st2005$r4sayret == 1 | retired1st2005$r4sayret == 2) & (retired1st2005$r3sayret == 0 | retired1st2005$r3sayret == 3), ]
if (count != length(retired1st2005$hhidpn)) cat("\n***ERROR - counts don't match for 2005 ",count," ",length(retired1st2005$hhidpn))

count <- sum( (hrs_df$r3sayret == 1 | hrs_df$r3sayret == 2) & (hrs_df$r2sayret == 0 | hrs_df$r2sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2004 but not in 2003: ",count )
retired1st2004 <- hrs_df[(!is.na(hrs_df$r3sayret) & !is.na(hrs_df$r2sayret)),]
retired1st2004 <- retired1st2004[ (retired1st2004$r3sayret == 1 | retired1st2004$r3sayret == 2) & (retired1st2004$r2sayret == 0 | retired1st2004$r2sayret == 3), ]
if (count != length(retired1st2004$hhidpn)) cat("\n***ERROR - counts don't match for 2004 ",count," ",length(retired1st2004$hhidpn))

count <- sum( (hrs_df$r2sayret == 1 | hrs_df$r2sayret == 2) & (hrs_df$r1sayret == 0 | hrs_df$r1sayret == 3),na.rm = TRUE )
cat ("\nSay they're retired in 2003 but not in 2002: ",count )
retired1st2003 <- hrs_df[(!is.na(hrs_df$r2sayret) & !is.na(hrs_df$r1sayret)),]
retired1st2003 <- retired1st2003[ (retired1st2003$r2sayret == 1 | retired1st2003$r2sayret == 2) & (retired1st2003$r1sayret == 0 | retired1st2003$r1sayret == 3), ]
if (count != length(retired1st2003$hhidpn)) cat("\n***ERROR - counts don't match for 2003 ",count," ",length(retired1st2003$hhidpn))

firstYrRet.df <- data.frame( rbind(retired1st2003,retired1st2004,retired1st2005,retired1st2006,retired1st2007,retired1st2008,retired1st2009,retired1st2010,retired1st2011,retired1st2012))

saveRDS(firstYrRet.df, file="firstyearretired.rds")  # save as data frame

# readRDS(file="firstyearretired.rds") # restore


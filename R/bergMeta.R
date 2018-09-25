bergMeta <- function(dat,method="DL"){


packages <- (.packages())
if (!"metafor" %in% packages) require(metafor,quietly=T)


df <- dat
df <- df[!is.na(df$RR),]
names.data <- names(df)

# Calculate standard error of the estimates
df$Cochrane.SE <- (log(df$UL)-log(df$LL))/3.92

# Run fixed and random effects meta analysis
fixed <- rma(yi=df$RR,sei=df$Cochrane.SE,method="FE")
random <- rma(yi=df$RR,sei=df$Cochrane.SE,method=method)

# Pull out estimates
fixed.rr <- format(round(fixed$beta,2),nsmall=2)
fixed.ll <- format(round(fixed$ci.lb,2),nsmall=2)
fixed.ul <- format(round(fixed$ci.ub,2),nsmall=2)
fixed.final <- c(fixed.rr,fixed.ll,fixed.ul)
rm(fixed.rr,fixed.ll,fixed.ul)

random.rr <- format(round(random$beta,2),nsmall=2)
random.ll <- format(round(random$ci.lb,2),nsmall=2)
random.ul <- format(round(random$ci.ub,2),nsmall=2)
random.final <- c(random.rr,random.ll,random.ul)
rm(random.rr,random.ll,random.ul)

I2 <- format(round(random$I2,2),nsmall=2)

estimates <- data.frame(c("Fixed effects","Random effects"),
                        rbind(fixed.final,random.final),
                        stringsAsFactors=F)
names(estimates) <- c("Model","RR","LL","UL")
estimates$Estimate <- paste0(estimates$RR," (",
                             estimates$LL,", ",
                             estimates$UL,")")
row.names(estimates) <- NULL
estimates$I2 <- c(NA,I2)

return(estimates)
}





prepJoint <- function(dat,primarySubtype,otherSubtypes){
     df <- dat
     subtypeVars <- c(primarySubtype,otherSubtypes)
     outputList <- list()
for (i in 1:length(subtypeVars)){
     foo <- df
     foo$OUTCOME <- foo[[subtypeVars[i]]]
     foo$EVENT2 <- i-1
     outputList[[i]] <- foo
}
final <- Reduce(function(x,y) rbind(x,y), outputList)
return(final)
     }


jointCox <- function(dat,primarySubtype,otherSubtypes,start,stop,expo,age,idvar,covariates=NULL){

# Check loaded packages
packages <- (.packages())
if (!"survival" %in% packages) require(survival,quietly=T)
if (!"dplyr" %in% packages) require(dplyr,quietly=T)


# Prepare dataset
# Typically this would be done outside the function
# But I want this function to be self-contained, which means it is a little inefficient
joint <- prepJoint(dat,primarySubtype,otherSubtypes)

# Set primary outcome variable (first element of subtypeVars)

myoutcome <- primarySubtype

     # Pull out my case numbers
     cases <- joint[joint[[myoutcome]]==1 & joint$EVENT2==1,]
     cases <- cases[!is.na(cases[[expo]]),]

     # Make sure start and stop times are numeric
     joint[,c("start","stop")] <- lapply(joint[,c(start,stop)],as.numeric)

# formula for models: one interaction and one base model

# Interaction model - this will provide me with the estimates
     y1 <- formula(paste("Surv(start,stop,OUTCOME)~",
                         paste(c(expo,"strata(EVENT2)"),collapse="*"), "+",
                         paste0("cluster(",idvar,")"), "+",
                         paste(c(covariates),collapse="+"), "+",
                         paste0("strata(",age,")")))
     y2 <- formula(paste("Surv(start,stop,OUTCOME)~",
                         paste(c(expo,"strata(EVENT2)"),collapse="+"), "+",
                         paste0("cluster(",idvar,")"), "+",
                         paste(c(covariates),collapse="+"), "+",
                         paste0("strata(",age,")")))

     # Fit the interaction and base models
     fit1 <- coxph(y1,data=joint)
     fit2 <- coxph(y2,data=joint)

     # Extract liklihood ratio values to calculate a p-tumor heterogeneity
     fit1.logtest <- summary(fit1)$logtest[c("test","df")]
     fit2.logtest <- summary(fit2)$logtest[c("test","df")]

     # Calculate the p-tumor heterogeneity
     fit1.test <- fit1.logtest["test"]
     fit1.df <- fit1.logtest["df"]
     fit2.test <- fit2.logtest["test"]
     fit2.df <- fit2.logtest["df"]
     p.het <- pchisq(abs(fit1.test-fit2.test),
                     abs(fit1.df-fit2.df),
                     lower.tail=F)
     rm(fit1.test,fit1.df,fit2.test,fit2.df,fit1.logtest,fit2.logtest)

     # Create an output table
     results <- data.frame(summary(fit1)$conf.int[,c("exp(coef)", "lower .95", "upper .95")],
                           pval=summary(fit1)$coefficients[,"Pr(>|z|)"])
     names(results) <- c("RR", "LL", "UL","p")
     results$RR <- format(round(results$RR,2),2)
     results$LL <- format(round(results$LL,2),2)
     results$UL <- format(round(results$UL,2),2)
     results$estimate <- paste0(results$RR, " (",
                                results$LL, ", ",
                                results$UL, ")")
     results <- results[,c("estimate","p")]

     # Subset the results so only my exposures are there
     # Also creating a table of case numbers here
     if (class(joint[[expo]])=="factor"){
          keep=paste0(expo,levels(joint[[expo]]))
          results <- results[keep,]
          results$Category <- factor(levels(joint[[expo]]))
          expoCases <- data.frame(table(cases[[expo]]))
          names(expoCases) <- c("Category","Cases")
          results <- dplyr::full_join(expoCases,results,"Category")
          results$estimate[is.na(results$estimate)] <- "1.00"
     } else {
          results <- results[expo,] # continuous variables
          expoCases <- data.frame(Category="Continuous",
                                  cases=nrow(cases),
                                  stringsAsFactors=F)
          results <- cbind(expoCases,results)

     }

     # Add the tumor hetergeneity p-value
     results$P_het <- c(rep(NA,(nrow(results)-1)),p.het)

     # Format the output all pretty and stuff
     results <- data.frame(Exposure=c(expo,rep(NA,nrow(results))),
                           rbind(NA,results),stringsAsFactors=F)
     names(results) <- c("Exposure","Category","Cases","Estimate","Pval","P_Het")
     row.names(results) <- NULL

     # Add an indicator for the subtype
     results <- data.frame(Subtype=c(myoutcome,rep(NA,(nrow(results)-1))),
                           results,stringsAsFactors=F)

     # Prepare all my results
     final <- list(final=results,
                   interaction=fit1,
                   base=fit2)
     return(final)
}




cox_models <- function(dat,failtime,outcome,expo,dtint,birthday,covariates=NULL,agedist,agegrps=NULL){

packages <- (.packages())
if (!"survival" %in% packages) require(survival,quietly=T)
if (!"dplyr" %in% packages) require(dplyr,quietly=T)


     df <- dat

# Calculate age at baseline based on dtint and birthday
     df$BASELINE_AGE <- floor((as.numeric(df[[dtint]])-as.numeric(df[[birthday]]))/365.25)
# Calculate rates for categorical variables
# For continuous variables, I'll output a blank dataset to merge later with estimates
# Pull in the incrate function


if (class(df[[expo]]) != "numeric"){
     spec.rates <- incrate(dat=df, agedist, agegrps, dtint, birthday,
                    failtime, outcome, expo)
     rates <- spec.rates$Std.Rates[spec.rates$Std.Rates$Exposure != "Total",c("Exposure", "Deaths","Person-years","Std rate")]
     names(rates) <- c("Exposure","Cases","Person-years","Std rate")
} else {
     rates <- data.frame(Exposure="Continuous",
                         Cases=NA,
                         "Person-years" = NA,
                         "Std rate"=NA)
     spec.rates <- "Not run for continuous exposures"
}

# Modeling
age.y <- formula(paste0("Surv(",failtime,",",outcome,")~",
                        paste0(c(expo,"strata(BASELINE_AGE)"),collapse="+")))
multi.y <- formula(paste0("Surv(",failtime,",",outcome,")~",
                        paste0(c(expo,"strata(BASELINE_AGE)",covariates),collapse="+")))
age.fit <- survival::coxph(age.y,data=df)
multi.fit <- survival::coxph(multi.y,data=df)

summarize <- function(fit){
     myfit <- get(fit)
     results <- data.frame(summary(myfit)$conf.int)
     results <- data.frame(results[,names(results) != "exp..coef."],
                          pval=summary(myfit)$coefficients[,"Pr(>|z|)"])
    names(results) <- c("RR", "LL", "UL","p-val")
    results$RR <- format(round(results$RR,2),2)
    results$LL <- format(round(results$LL,2),2)
    results$UL <- format(round(results$UL,2),2)
    results$answer <- paste0(results$RR, " (",
                             results$LL, ", ",
                             results$UL, ")")
    results <- results[substr(row.names(results),1,nchar(expo))==expo,c("answer","p-val")]
}
age.results <- summarize("age.fit")
multi.results <- summarize("multi.fit")
results <- cbind(age.results,multi.results)

# Add referent group and combine with the rates
  if (class(df[[expo]])=="factor"){
    results <- rbind(rep(c("1.00",NA),2),results)
    results <- cbind(rates,results)
  } else {
    results <- cbind(rates,results)
  }
names(results) <- c("Level","Cases","Person_years","Std.Rate","Age_adjusted","Age_P","Multi_adjusted","Multi_P")
results <- data.frame(Exposure=c(expo,rep(NA,nrow(results))),rbind(NA,results))

# Create my documentation output
age.fit <- summary(age.fit)
multi.fit <- summary(multi.fit)

final <- list(final=results,rates=spec.rates,age=age.fit,multi=multi.fit)
return(final)
}


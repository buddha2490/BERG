# Stratified interaction models -------------------------------------------

stratified_cox <- function(dat,start,stop,outcome,expoVar,strataVar,age,covariates=NULL){

# load packages if neceessary
packages <- (.packages())
if (!"dplyr" %in% packages) require(dplyr,quietly=T)
if (!"survival" %in% packages) require(survival,quietly=T)


# data file
df <- dat

# convert start and stop times to numeric
df[,c(start,stop)] <- lapply(df[,c(start,stop)],as.numeric)

# start time mus != stop time - adjusting if that is true (similar to adding 1 to fail time)
df[[stop]] <- df[[stop]] + 1/365.25

# Data checks - stop the program with error if some things are not correct
class.strataVar <- class(df[[strataVar]]) # must be factor
class.expoVar <- class(df[[expoVar]]) # must NOT be character, can be factor/numeric/int

if (class.strataVar != "factor") stop("strataVar is not a factor variable")
if (class.expoVar == "character") stop("expoVar must be numeric or factor")



# The formula for these models is different for continuous vs categorical expoVar
# if expoVar is continuous, this is just a normal interaction model
# if expoVar is categorical, then I need to code it with multiple referent groups in mind

# interaction model
     y <- formula(paste(
     paste0("Surv(",start,",",stop,",",outcome,")~",
            strataVar, "+", strataVar,":",expoVar,"+",
            paste(c(covariates,paste0("strata(",age,")")),collapse="+"))))


# Reduced model is the same for continuous/categorical variables, just additive
z <- formula(paste(
     paste0("Surv(",start,",",stop,",",outcome,")~",
            strataVar, "+",expoVar,"+",
            paste(c(covariates,paste0("strata(",age,")")),collapse="+"))))

fit <- survival::coxph(y,data=df) # interaction model
base <- survival::coxph(z,data=df) # reduced model
p.int <- anova(fit,base)$"P(>|Chi|)"[2] # p-interaction
rm(y,z)

strat.levels <- levels(df[[strataVar]])
expo.levels <- levels(df[[expoVar]]) # its ok if this is NULL for continuous variables

# The data management portion of the function must run a bit different
# for categorical vs continuous variables.
# I've split categorica/continuous results organization up into two function
# So they will be easier to review

categorical <- function(){

# Create an interaction variable for calculating case numbers
# This interaction() categories will match the stratified output format, so I can cbind() later
df$interaction <- interaction(df[[strataVar]],df[[expoVar]])


# Create a vector of my model variable names
#  they all look like  "smoke92Never:bmicat9218.5-24.9"
#  I can automate their creation and then subset the model output
strat <- paste0(strataVar,levels(df[[strataVar]]))
expo <- paste0(expoVar,levels(df[[expoVar]]))
keep <- expand.grid(strat,expo)

# keep$keep is the final vector for subsetting model output
keep$keep <- paste(keep$Var1,keep$Var2,sep=":")
keep$Var1 <- sub(strataVar,"",keep$Var1)  # matching smoking categories
keep$Var2 <- sub(expoVar,"",keep$Var2) # matching BMI categories


# Generate my case numbers - will be merged with model output
levels(df$interaction) <- keep$keep  # change the level names so I can merge
case.numbers <- data.frame(table(df$interaction[df[[outcome]]==1]),stringsAsFactors=F)
names(case.numbers) <- c("keep","Cases")
case.numbers$keep <- as.character(case.numbers$keep)

# Subset and prepare my model output
results <- data.frame(summary(fit)$conf.int)
results <- data.frame(results[,names(results) != "exp..coef."],
                          pval=summary(fit)$coefficients[,"Pr(>|z|)"])
names(results) <- c("RR", "LL", "UL","p-val")
results$RR <- format(round(results$RR,2),2)
results$LL <- format(round(results$LL,2),2)
results$UL <- format(round(results$UL,2),2)
results$answer <- paste0(results$RR, " (",
                             results$LL, ", ",
                             results$UL, ")")
results$keep <- row.names(results)
results <- results[row.names(results) %in% keep$keep,c("keep","answer","p-val")]

# Merge case numbers, model results, and my "keep" data frame
results <- Reduce(function(x,y) full_join(x,y,"keep"), list(keep,results,case.numbers))
results$answer[is.na(results$answer)] <- "1.00"

# Create a stratified result table
strata <- unique(results$Var1)
out <- lapply(strata,function(x){
     df <- dplyr::filter(results,Var1==x)
     df <- dplyr::select(df,Var2,Cases,answer,"p-val")
     names(df) <- c("expo","Cases",x,"p-val")
     return(df)
})
final <- Reduce(function(x,y) dplyr::full_join(x,y,"expo"),out)
return(final)
}
continuous <- function(){

# Subset and prepare my model output
results <- data.frame(summary(fit)$conf.int)
results <- data.frame(results[,names(results) != "exp..coef."],
                          pval=summary(fit)$coefficients[,"Pr(>|z|)"])
names(results) <- c("RR", "LL", "UL","P")
results$RR <- format(round(results$RR,2),2)
results$LL <- format(round(results$LL,2),2)
results$UL <- format(round(results$UL,2),2)
results$Estimate <- paste0(results$RR, " (",
                             results$LL, ", ",
                             results$UL, ")")
results <- results[grep(expoVar,row.names(results)),]
results$strataVar <- levels(df[[strataVar]])

# Get my case numbers
# It will not include cases where expoVar is missing
cases <- data.frame(strataVar=strat.levels,
                         cases=tapply(df[[outcome]][!is.na(df[[expoVar]])],
                                      df[[strataVar]][!is.na(df[[expoVar]])],
                                      sum),
                         stringsAsFactors=F)
# Join cases with main results
     results <- dplyr::full_join(cases,results,"strataVar")
# Loop to cbind() all the results
     final <- data.frame(results[results$strataVar==strat.levels[1],
                                 c("cases","Estimate","P")],
                         stringsAsFactors=F)
     for (i in 2:length(strat.levels)){
          final <- cbind(final,results[results$strataVar==strat.levels[i],c("cases","Estimate","P")])
     }
# Structuring the dataset so that it can be eventually rbind() with categorical results
     final <- data.frame(Category="Continuous",
                         final,
                         stringsAsFactors=F)

     return(final)
}

if (class(df[[expoVar]]) == "factor") final <- categorical()
if (class(df[[expoVar]]) != "factor") final <- continuous()

# Add a column for the expoVar variable name, so it can be rbind() with out model output
final <- data.frame(Variable=c(expoVar,rep(NA,nrow(final))),
                    rbind(NA,final),stringsAsFactors=F)

# Name my output variables so that they are descriptive
# I assume the analyst will change them at some point, I just want results clearly identified
col.names <- expand.grid(c("Cases","Estimate","P"),strat.levels)
col.names <- paste(col.names$Var1,col.names$Var2,sep="_")
names(final) <- c("Variable","Category",col.names)

# Add the p-value for interaction
final$P_Interaction <- c(rep(NA,nrow(final)-1),p.int)


# Final list of results
output <- list(final=final,
               int.model=fit,
               base.model=base)

return(output)
}

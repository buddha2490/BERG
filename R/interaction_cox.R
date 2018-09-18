
# Single reference group interaction models ------------------------------------------------------


interaction_cox <- function(dat,failtime,outcome,expoVar,strataVar,age,covariates=NULL){
require(survival)
require(dplyr)
# data file
df <- dat

# Data checks - stop the program with error if some things are not correct
class.strataVar <- class(df[[strataVar]]) # must be factor
class.expoVar <- class(df[[expoVar]]) # must NOT be character, can be factor/numeric/int

if (class.strataVar != "factor") stop("strataVar is not a factor variable")
if (class.expoVar == "character") stop("expoVar must be numeric or factor")
rm(class.strataVar,class.expoVar)

# Coding this interaction is different for categorical vs continuous expoVar
if (class(df[[expoVar]]) !="numeric"){
df$INTERACTION <- interaction(df[[expoVar]],df[[strataVar]])

# Formula for the categorical model
y <- formula(paste(
     paste0("Surv(",failtime,",",outcome,")~",
            paste(c("INTERACTION",covariates,paste0("strata(",age,")")),collapse="+"))))

} else {
# Formula for the continuous model
y <- formula(paste(
     paste0("Surv(",failtime,",",outcome,")~",
            paste0(expoVar,"*",strataVar," + "),
            paste(c(covariates,paste0("strata(",age,")")),collapse="+"))))
}
# Formula for the base model for P-interaction - same for categorical or continuous expoVar - just additive
z <- formula(paste(
     paste0("Surv(",failtime,",",outcome,")~",
            strataVar, "+",expoVar,"+",
            paste(c(covariates,paste0("strata(",age,")")),collapse="+"))))


# Fit the models
fit <- survival::coxph(y,data=df)
base <- survival::coxph(z,data=df)
p.int <- anova(fit,base)$"P(>|Chi|)"[2] # p-interaction
rm(y,z)

# Data management section - pull together and format my results

# First: format the estimates and p-values into something nice
# Then dump the remaining trash to make it more manageable

results <- data.frame(summary(fit)$conf.int,P=data.frame(summary(fit)$coef)[["Pr...z.."]])
     results$Expo <- row.names(results)
     results <- results[,c("Expo","exp.coef.","lower..95","upper..95","P")]
     names(results) <- c("Expo","RR","LL","UL","P")
results[,c("RR","LL","UL")] <- lapply(results[,c("RR","LL","UL")],function(x){
     format(round(x,2),nsmall=2)
})
results$Estimate <- paste0(results$RR," (",
                           results$LL,", ",
                           results$UL,")")
results <- results[,c("Expo","Estimate","P")]

# Subset the results, get rid of the covariates
if (class(df[[expoVar]])=="factor") {
     results <- results[grep("INTERACTION",results$Expo),]
} else {
     results <- results[grep(expoVar,results$Expo),]
}

strat.levels <- levels(df[[strataVar]])
expo.levels <- levels(df[[expoVar]]) # if continuous, will be NULL, that's ok

# Case numbers and formatting ------------------------------------------------------------

# Continuous vs categorical expoVar have to go through a different set of formatting
# These functions will do the work and produce an organized table, complete with case numbers

continuous <- function(){

# I need to cbind() by the strataVar levels
# Adding the levels into the dataset so I can sort them out
     results$strataVar <- strat.levels
# Summing cases within each strata
# If there are missing expoVar observations, they will not be included in case numbers
     cases <- data.frame(strataVar=strat.levels,
                         cases=tapply(df[[outcome]][!is.na(df[[expoVar]])],
                                      df[[strataVar]][!is.na(df[[expoVar]])],
                                      sum),
                         stringsAsFactors=F)
# Join with the model estimates
     results <- dplyr::left_join(cases,results,"strataVar")
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
categorical <- function(){
     results$Expo <- sub("INTERACTION","",results$Expo)
     cases <- data.frame(table(df$INTERACTION[df[[outcome]]==1]),stringsAsFactors=F) # case numbers
     names(cases) <- c("Expo","Cases")
     cases$Expo <- as.character(cases$Expo)
     results <- dplyr::full_join(cases,results,"Expo") # merge with model estimates
     rm(cases)
     results$Estimate[is.na(results$Estimate)] <- "1.00" # add referent group
# separate out the interaction levels into the two variables
# This will allow me to format the table more easily
     grid <- expand.grid(expo.levels,strat.levels)
     grid$Expo <- paste(grid$Var1,grid$Var2,sep=".")
     results <- dplyr::full_join(grid,results,"Expo")

# Now I want to cbind() the estimates for each level of strataVar
final <- results[results$Var2 == strat.levels[1],c("Var1","Cases","Estimate","P")]
for (i in 2:length(strat.levels)){
     final <- cbind(final,
                    results[results$Var2== strat.levels[i],c("Cases","Estimate","P")]
     )
}
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


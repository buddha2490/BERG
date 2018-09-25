#### Table 1 function.
#### Author:  Brian Carter
#### Date:  February 5th 2018
#### Last updated: September 4th 2018




# Table 1 function
# Will calculate frequencies (Formatted as "N (%)") for categorical variables
# Or "Mean (SD)" for continuous variables
# Then format the output as a data.frame that can be rbind() with others to create a concise Table 1.

# Function inputs:
# dat:  Dataset name
# variable: Main frequency variable. In an N*N table, this would be the rows
# stratvar:  Strata variable.  In an N*N table, this would be the columns
# percents:  1= Row percents;   2= Column percents (default); 0 = Overall percents
# freq.type: 0= Both (default);  1= N only;  2= Percentages only;

# Notes:
# Categorical variables must be character or factor variables
# Continuous variables should be "numeric", "dbl", or "integer"




tbl1 <- function(dat,variable,stratvar,percents=2,freq.type=0){

# load packages if neceessary
packages <- (.packages())
if (!"dplyr" %in% packages) require(dplyr,quietly=T)


# Rename the dataframe
df <- dat

# Split up the variable vector into continuous vs categorical variables
type <- lapply(variable,function(x){
     data.frame(var=x,class=class(df[[x]]))})
type <- Reduce(function(x,y) rbind(x,y), type)

categorical.vars <- as.character(type[type$class %in% c("character","factor"),"var"])
continuous.vars <- as.character(type[!type$class %in% c("character","factor"),"var"])

# Function to run the frequencies and format the table
littletbl1 <- function(x){

var <- df[[x]]  # Main exposure variable
strat <- df[[stratvar]]  # Strata variable
col.formats <- names(table(strat)) # labels for the columns (based on strata variable)

# Categorical variables
if (class(var) %in% c("character","factor")){
freq.tbl <- data.frame(table(var,strat,useNA="ifany"))
     if (percents==2){
          prop.tbl <- data.frame(prop.table(table(var,strat,useNA="ifany"),2))  # Column percents
     } else if (percents==1) {
          prop.tbl <- data.frame(prop.table(table(var,strat,useNA="ifany"),1))  # Row percents
     } else if (percents==0){
          prop.tbl <- data.frame(prop.table(table(var,strat,useNA="ifany")))  # total percents
     }

     # Format my output
     freq.tbl$Freq <- format(freq.tbl$Freq,big.mark=",")
     prop.tbl$Freq <- 100*prop.tbl$Freq
     prop.tbl$Freq <- format(round(prop.tbl$Freq,1),nsmall=1)

     # user defined output: 1= N-only, 2=Percents only, 3= Both

     if (freq.type==1) {tbl <- freq.tbl;  tbl$Answer <- tbl$Freq}
     if (freq.type==2) {tbl <- prop.tbl;  tbl$Answer <- tbl$Freq}
     if (freq.type==0) {
          tbl <- data.frame(freq.tbl,Prop=prop.tbl$Freq,stringsAsFactors=-F)
          tbl$Answer <- paste0(tbl$Freq, " (", tbl$Prop, ")")
     }


     # Rearrange the table to have the "strat" variable as columns, "x" as rows
     #  This should work for both character and factor variables
     cat.table <- tbl[tbl$strat==col.formats[1],c("var","Answer")]
     for (i in 2:length(col.formats)){
          cat.table <- cbind(cat.table,tbl$Answer[tbl$strat==col.formats[i]])
     }
     cat.table <- data.frame(Variable=c(x,rep(NA,nrow(cat.table))),
                             rbind(NA,cat.table))
     cat.table[,names(cat.table)] <- lapply(cat.table[,names(cat.table)],as.character)
     names(cat.table) <- c("Variable","Levels",as.character(col.formats))
     return(cat.table)
}

# Calculate strata-specific means and standard deviations

if (!class(var) %in% c("character","factor")){

means <- data.frame(t((tapply(var,strat,function(x) mean(x,na.rm=T)))))
sd <- data.frame(t((tapply(var,strat,function(x) sd(x,na.rm=T)))))

# format the means/sd and combine into "mean (SD)" format
means <- format(round(means,1),big.mark=",",nsmall=1)
sd <- format(round(sd,1),big.mark=",",nsmall=1)
means.sd <- paste0(means, " (",sd,")")


# Create a dataframe with the results
tbl <- data.frame(Variable=c(x),
                  Levels=c("Continuous"),
                  t(means.sd))
tbl[,names(tbl)] <- lapply(tbl[,names(tbl)],as.character)
names(tbl) <- c("Variable","Levels",as.character(col.formats))
row.names(tbl) <- NULL
return(tbl)
}
}
# Run the function for continuous and categorical variables
if (length(continuous.vars) != 0){
mycontinuous <- lapply(continuous.vars,littletbl1)
     mycontinuous <- Reduce(function(x,y) rbind(x,y),mycontinuous)
} else {
     mycontinuous <- NULL
}
if (length(categorical.vars) != 0){
mycategorical <- lapply(categorical.vars,littletbl1)
     mycategorical <- Reduce(function(x,y) rbind(x,y),mycategorical)
} else {
     mycategorical <- NULL
}

# Bind the results and return the table
final.table <- rbind(mycontinuous,mycategorical)
return(final.table)
}


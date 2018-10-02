prophazCheck <- function(dat,start,stop,outcome,age,expo,outcome.title=NULL,expo.title=NULL){

     # Check loaded packages
packages <- (.packages())
if (!"survival" %in% packages) require(survival,quietly=T)
if (!"dplyr" %in% packages) require(dplyr,quietly=T)
if (!"gridGraphics" %in% packages) require(gridGraphics,quietly=T)


if (is.null(outcome.title)) outcome.title=outcome
if (is.null(expo.title)) expo.title=expo

     df <- dat
     df$failmonth <- (as.numeric(df[[stop]]) - as.numeric(df[[start]]) + 1) / 30.4375

     # survival function
     surv <- paste0("Surv(failmonth,",outcome,")")
     y <- formula(paste(surv,"~",paste(c(expo,
                                         paste0("strata(",age,")")),collapse="+")))

     # get the p-value for proportional hazards test
     fit <- cox.zph(coxph(y,data=df))
     p <- fit$table[row.names(fit$table)=="GLOBAL","p"]
     p <- format(round(p,4),nsmall=4)

     mycolors <- c("black","red","blue","green","purple","orange","yellow")
     mycolors <- mycolors[1:length(levels(df[[expo]]))]

     # Create a graph in base graphics
     plot(survfit(Surv(df$failmonth,df[[outcome]]) ~ df[[expo]]),fun="cloglog",
          cex=0.1, xlab="Fail time (months)",
          col=mycolors,
          main=paste0(outcome.title," and ",expo.title,"\n","p=",p))
     a <- recordPlot()
     plot.new()

     # Format some output
     final.p <- data.frame(outcome=outcome.title,expo=expo.title,p=p,stringsAsFactors=F)

     return(list(pval=final.p,plots=a))
     }

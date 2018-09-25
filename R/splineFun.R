
splineFun <- function(dat,expo,covariates=NULL,reference=NULL,knots,
                      failtime,outcome,agestrat,
                      expo.label=NULL,
                      outcome.text=NULL){

# load packages if neceessary
packages <- (.packages())
if (!"rms" %in% packages) require(rms,quietly=T)
if (!"survival" %in% packages) require(survival,quietly=T)

     df <- dat
     df$EXPOSURE <- df[[expo]]
     dd <<- datadist(df)
     options(datadist="dd")
     scaleFUN <- function(x) sprintf("%.1f",x)

if (is.null(expo.label)) expoLabel <- expo
if (!is.null(expo.label)) expoLabel <- expo.label
if (is.null(outcome.text)) outcomeText <- outcome
if (!is.null(outcome.text)) outcomeText <- outcome.text

# Data checks - stop the program with error if some things are not correct
if (class(df[[expo]]) != "numeric") stop("Exposure variable must be numeric")
if ( (!class(df[[outcome]]) %in% c("integer","numeric"))) stop("Outcome variable must be numeric")
if (is.null(reference)) warning("Reference point will be computed as the median unless you specify a reference point")

# modeling
     y <- formula(paste0("Surv(",failtime,",",outcome,")~ ",paste0("rcs(EXPOSURE, ",knots,")"), "+",
                         paste(c(paste("strat(",agestrat,")"),covariates),collapse="+")))
     fit <- cph(y,data=df,x=T,y=T)
     nonlin <- anova(fit)

     # set reference group for our exposure
     # set to median if not specified
     if (is.null(reference)) reference <- median(df$EXPOSURE,na.rm=T)
     dd$limits$EXPOSURE[2] <<- reference
     fit <- update(fit)
     p <- Predict(fit, EXPOSURE, ref.zero=TRUE, fun=exp)


# random stuff to format the graphics

     # text of p-value
     p.lin <- paste("P-linearlity = ",
                       format(nonlin["EXPOSURE","P"],scientific=T,digits=2))
     p.nonlin <- paste("P-nonlinearlity = ",
                       format(nonlin[" Nonlinear","P"],scientific=T,digits=2))
     # Title text
     title <- paste0("Restricted cubic splines\n",
                outcomeText,"\n",
                p.lin,"\n",
                p.nonlin)
# graphics
      myplot1 <- ggplot(p,
                   ylim=c(0, 4.0),
                   xlim=c(quantile(df$EXPOSURE,0.01,na.rm=T), quantile(df$EXPOSURE,0.99,na.rm=T)),
                   xlab=expoLabel,
                   ylab="Hazard Ratio", adj.subtitle=FALSE) +
      geom_hline(yintercept=1,linetype="dashed", color="gray21") +
      scale_y_continuous(labels = scaleFUN) +
           ggtitle(title)
 rm(dd,envir=.GlobalEnv)

     return(myplot1)



}

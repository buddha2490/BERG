
dxdateClean <- function(data,d){
  df <- data

# 1997
x <- df[[d]]
df$DTINT97n <- as.Date(ifelse(df$report97==1 & df$DTINT97=="1998-01-01" & !is.na(df$DXDATE),
                                 x+1,df$DTINT97), origin="1970-01-01")
x <- as.Date(ifelse(df$report97==1 & !is.na(df$DTINT97n) & x>df$DTINT97n &
                                      (x-df$DTINT97n) < 180, df$DTINT97n-1, ifelse(
                                           df$report97==1 & !is.na(df$DTINT97) & x>df$DTINT97n &
                                                (x-df$DTINT97n) >= 180, as.Date("1900-01-01"), x)), origin="1970-01-01")
# 1999
x <- as.Date(ifelse(df$report99==1 & !is.na(df$DTINT99) & x>df$DTINT99 &
                                      (x-df$DTINT99)<180, df$DTINT99-1, ifelse(
                                           df$report99==1 & !is.na(df$DTINT99) & x>df$DTINT99 &
                                                (x-df$DTINT99)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")

#2001
x <- as.Date(ifelse(df$report01==1 & !is.na(df$DTINT01) & x>df$DTINT01 &
                                      (x-df$DTINT01)<180, df$DTINT01-1, ifelse(
                                           df$report01==1 & !is.na(df$DTINT01) & x>df$DTINT01 &
                                                (x-df$DTINT01)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")
#2003
x <- as.Date(ifelse(df$report03==1 & !is.na(df$DTINT03) & x>df$DTINT03 &
                                      (x-df$DTINT03)<180, df$DTINT03-1, ifelse(
                                           df$report03==1 & !is.na(df$DTINT03) & x>df$DTINT03 &
                                                (x-df$DTINT03)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")

#2005
x <- as.Date(ifelse(df$report05==1 & !is.na(df$DTINT05) & x>df$DTINT05 &
                                      (x-df$DTINT05)<180, df$DTINT05-1, ifelse(
                                           df$report05==1 & !is.na(df$DTINT05) & x>df$DTINT05 &
                                                (x-df$DTINT05)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")
#2007
x <- as.Date(ifelse(df$report07==1 & !is.na(df$DTINT07) & x>df$DTINT07 &
                                      (x-df$DTINT07)<180, df$DTINT07-1, ifelse(
                                           df$report07==1 & !is.na(df$DTINT07) & x>df$DTINT07 &
                                                (x-df$DTINT07)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")
#2009
x <- as.Date(ifelse(df$report09==1 & !is.na(df$DTINT09) & x>df$DTINT09 &
                                      (x-df$DTINT09)<180, df$DTINT09-1, ifelse(
                                           df$report09==1 & !is.na(df$DTINT09) & x>df$DTINT09 &
                                                (x-df$DTINT09)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")
#2011
x <- as.Date(ifelse(df$report11==1 & !is.na(df$DTINT11) & x>df$DTINT11 &
                                      (x-df$DTINT11)<180, df$DTINT11-1, ifelse(
                                           df$report11==1 & !is.na(df$DTINT11) & x>df$DTINT11 &
                                                (x-df$DTINT11)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")

#2013
 x <- as.Date(ifelse(df$report13==1 & !is.na(df$DTINT13) & x>df$DTINT13 &
                          (x-df$DTINT13)<180, df$DTINT13-1, ifelse(
                               df$report13==1 & !is.na(df$DTINT13) & x>df$DTINT13 &
                                   (x-df$DTINT13)>=180, as.Date("1900-01-01"), x)), origin="1970-01-01")

return(x)
}

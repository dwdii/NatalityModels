library(reshape2)
library(dplyr)
library(lubridate)

rotateXaxisLabels45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
rotateXaxisLabels90 <- theme(axis.text.x = element_text(angle = 90, hjust = 1))

#
#### FUNCTION: loadBirthData ####
#
loadBirthData <- function(filename, path="./data/%s", sumByYrMonth=TRUE)
{
  # Load the Natality data
  birthFile <- sprintf(path, filename)
  birthData <- read.table(birthFile, 
                          header=TRUE, 
                          sep="\t", 
                          fill=TRUE, 
                          stringsAsFactors=FALSE,
                          colClasses=c('character', # Notes
                                       'character', # Year
                                       'character',   # Year.Code
                                       'character', # Month
                                       'character',   # Month.Code
                                       'character', # Age of Mother
                                       'character', # Age of Mother Code
                                       'character', # Marital Status
                                       'character', # Marital Status Code
                                       'character', # Education
                                       'character', # Education Code
                                       'numeric'))  # Births
  
  # Eliminate rows with no birth data (some rows have comments only)
  birthDataWoNa <- subset(birthData, !is.na(birthData$Births)) 
  
  # Transform raw year/month columns into a Date column
  birthDataWoNa <- dplyr::mutate(birthDataWoNa, 
                                 Date = lubridate::parse_date_time(sprintf("%s-%s-01", 
                                                                           Year.Code, 
                                                                           Month.Code), 
                                                                   orders="ymd"))
  # Data type conversion
  birthDataWoNa$Year.Code <- as.numeric(birthDataWoNa$Year.Code)  
  birthDataWoNa$Month.Code <- as.numeric(birthDataWoNa$Month.Code)
  birthDataWoNa$Age.of.Mother <- as.factor(birthDataWoNa$Age.of.Mother)
  birthDataWoNa$Marital.Status <- as.factor(birthDataWoNa$Marital.Status)
  birthDataWoNa$Education <- as.factor(birthDataWoNa$Education)
  # Remove the extra effectively duplicated columns
  birthDataWoNa <- subset(birthDataWoNa, select = - c(Notes, Year, Month, Marital.Status.Code, Education.Code))
  
  if(sumByYrMonth)
  {
    #by_yrmon <- group_by(birthDataWoNa, Year.Code, Month.Code, Date)
    #birthDataWoNa <- summarise(by_yrmon, sum(Births))
    
    birthDataWoNa <- aggregate(Births ~ Year.Code + Month.Code + Date, birthDataWoNa,
                               FUN=sum)
  }
  
  return (birthDataWoNa)
}

#
#### FUNCTION: loadCensusNnData ####
#
# Example:
#     df <- loadCensusNnData("US-EST00INT-01.csv")
#     summary(df)
#
loadCensusNnData <- function(filename, path)
{
  # Load the monthly totals Census data
  filepath <- sprintf(path, filename)
  df <- read.table(filepath, 
                   header=TRUE, 
                   sep=",", 
                   fill=TRUE, 
                   stringsAsFactors=FALSE)
  
  # Transform raw year/month columns into a Date column
  df <- dplyr::mutate(df, 
                      Date = lubridate::parse_date_time(sprintf("%s-%s-01", 
                                                                YEAR, 
                                                                MONTH), 
                                                        orders="ymd"))  
  return (df)
}

#
#### FUNCTION: loadCensus00Data ####
#
# Example:
#     dfC00 <- loadCensus00Data()
#     summary(dfC00)
#     dfC00[dfC00$YEAR == 2003,]
#
loadCensus00Data <- function(path="./data/%s")
{
  fnMonthlyTotals <- "US-EST00INT-TOT.csv"
  fnAnnualTotals <- "US-EST00INT-ALLDATA.csv"
  
  # Load the monthly totals Census data
  df <- loadCensusNnData(fnMonthlyTotals, path)
  
  # Load the annual age/gender data  
  filepath <- sprintf(path, fnAnnualTotals)
  dfAT <- read.table(filepath, 
                   header=TRUE, 
                   sep=",", 
                   fill=TRUE, 
                   stringsAsFactors=FALSE)

  
  dfAT <- dfAT[dfAT$MONTH == 7 & dfAT$AGE == 999, c("YEAR", "AGE", "TOT_POP", "TOT_FEMALE", "TOT_MALE")]
  dfAT$GenderRatio <- dfAT$TOT_FEMALE / (dfAT$TOT_POP)
  
  # Join the gender ratio to the monthly data
  # and generate the female and male sub totals
  df <- join(df, dfAT[,c("YEAR", "GenderRatio")])
  df$TOT_FEMALE <- df$GenderRatio * df$TOT_POP
  df$TOT_MALE <- df$TOT_POP - df$TOT_FEMALE
  
  # Remove April 2010, it is part of the 2010 census estimates
  df <- df[!(df$YEAR == 2010 & df$MONTH == 4), ]
  # Remove July 2010, it is part of the 2010 census estimates
  df <- df[!(df$YEAR == 2010 & df$MONTH == 7), ]
  
  # show me  
  #print(summary(dfAT))
  
  return (df)  
}

#
#### FUNCTION: loadCensus10Data ####
#
#
# Example:
#    dfC10 <- loadCensus10Data("NC-EST2014-ALLDATA-R-File%02d.csv", 12)
#    summary(dfC10)
#   dfC10[dfC10$YEAR == 2010,]
#
loadCensus10Data <- function(filenameFmt, count, path="./data/%s", verbose=FALSE, fertileAgeRange=c(15,49))
{
  # Load the Census data
  fn <- sprintf(filenameFmt, 1)
  if(verbose)
  {
    print(paste("Loading", fn))  
  }
  
  df <- loadCensusNnData(fn, path)  
  for(i in 2:count)
  {
    fn <- sprintf(filenameFmt, i)
    if(verbose)
    {
      print(paste("Loading", fn))  
    }
    
    df <- rbind(df, loadCensusNnData(fn, path))
  }
  
  # Subset to Monthly totals with subset of columns
  dfRet <- df[df$AGE == 999 ,c("MONTH", "YEAR", "TOT_POP", "TOT_FEMALE", "TOT_MALE", "Date")]
  
  # Convert Census to regular April
  dfRet[dfRet$MONTH == 4.1, ]$MONTH <- 4
  
  # Remove April 1, 2010 estimates base 
  dfRet <- dfRet[dfRet$MONTH != 4.2, ]
  
  # Generate Gender Ratio column
  dfRet$GenderRatio <- dfRet$TOT_FEMALE / (dfRet$TOT_POP)
  
  # Transform raw year/month columns into a Date column one final time, now that 
  # we fixed up the 4.1 month
  dfRet <- dplyr::mutate(dfRet, 
                         Date = lubridate::parse_date_time(sprintf("%s-%s-01", 
                                                                YEAR, 
                                                                MONTH), 
                                                        orders="ymd"))
  # Starting to bucket the fertile females count
  # This is basically done, but need to do something similar to the 2000-2010 data
  # ... Also might be good to bucket in smaller sets rather than one big one.
  if(FALSE)
  {
    # Pull out the # of females ages X - Y
    dfFxy <- df[fertileAgeRange[1] <= df$AGE & df$AGE <= fertileAgeRange[2], c("TOT_FEMALE", "Date", "AGE")]
    
    dfFxySum <- aggregate(TOT_FEMALE ~ Date, dfFxy, FUN=sum)
    colnames(dfFxySum) <- c("Date", "FERTILE_FEMALE")
    if(verbose)
    {
      print(summary(dfFxySum))  
    }
    # Join to our result set ** April 2010 is NA ** 
    dfRet <- plyr::join(dfRet, dfFxySum, by="Date")
  }
  
  return (dfRet)  
}

#
#### FUNCTION: scaleCensusTotalPop ####
#
scaleCensusTotalPop <- function(data)
{
  scalar <- 1000.0
  data$TOT_POP <- data$TOT_POP / scalar
  data$TOT_FEMALE <- data$TOT_POP / scalar
  data$TOT_MALE <- data$TOT_POP / scalar
  
  return(data)
}

#
#### FUNCTION: loadEarningsData ####
#
# Women's weekly earnings
#
# Example:
#    df <- loadEarningsData("Earnings-2003-2015.csv")
#    summary(df)
#
loadEarningsData <- function(filename, path="./data/%s", verbose=FALSE)
{
  # Load the monthly totals Census data
  filepath <- sprintf(path, filename)
  df <- read.table(filepath, 
                   header=TRUE, 
                   sep=",", 
                   fill=TRUE, 
                   stringsAsFactors=FALSE)
  # Melt to querterly long form
  df <- reshape2::melt(df, measure.vars=c("Qtr1","Qtr2","Qtr3","Qtr4"), variable.name="Qtr")
  # Simplify to numeric
  df$Qtr  <- as.character(df$Qtr)
  df$Qtr[df$Qtr == "Qtr1"] <- 1
  df$Qtr[df$Qtr == "Qtr2"] <- 2
  df$Qtr[df$Qtr == "Qtr3"] <- 3
  df$Qtr[df$Qtr == "Qtr4"] <- 4
  df$Qtr  <- as.numeric(df$Qtr)
  colnames(df) <- c("Year", "Qtr", "Earnings")
  
  # Convert to wide monthly format
  df$M1 <- 0
  df$M2 <- 0
  df$M3 <- 0
  for(i in 1:4)
  {
    df$M1[df$Qtr == i] <- 1 + ((i - 1) * 3)
    df$M2[df$Qtr == i] <- 2 + ((i - 1) * 3)
    df$M3[df$Qtr == i] <- 3 + ((i - 1) * 3)
  }
  
  # Melt to monthly long form
  df <- reshape2::melt(df, measure.vars=c("M1","M2","M3"), variable.name="MonthVar")
  colnames(df) <- c("Year", "Qtr", "Earnings", "MonthVar", "Month")
  df <- subset(df, select=-c(MonthVar))
  df <- df[order(df$Year, df$Month), ]
  
  # Transform raw year/month columns into a Date column 
  df <- dplyr::mutate(df, 
                         Date = lubridate::parse_date_time(sprintf("%s-%s-01", 
                                                                   Year, 
                                                                   Month), 
                                                           orders="ymd"))      
  
  return (df)  
}

#
#### FUNCTION: loadUnemploymentData ####
#
# Example:
#    df <- loadUnemploymentData("UnemploymentRate-2003-2015.csv")
#    summary(df)
#
loadUnemploymentData <- function(filename, path="./data/%s")
{
  # Load the Unemployment data
  dataFile <- sprintf(path, filename)
  data <- read.table(dataFile, 
                     header=TRUE, 
                     sep=",", 
                     fill=TRUE, 
                     stringsAsFactors=FALSE) 
  
  # Melt the data to a long format
  data <- melt(data, 
               id.vars=c("Year"), 
               variable.name="Month", 
               value.name="UnemploymentRate")
  
  # Transform raw year/month columns into a Date column, sorted
  data <- mutate(data, 
                 Date = lubridate::parse_date_time(sprintf("%s-%s-01", 
                                                           Year, 
                                                           Month), 
                                                   orders="ybd"))
  data <- data[order(data$Date), ]
  
  return (data)
}


#### exploreVar Function ####
exploreVar <- function(data, varName, respName, jitter=FALSE, 
                       binwidth=diff(range(data[,varName], na.rm=TRUE))/sqrt(nrow(data)), 
                       na.rm=TRUE,
                       rotateLabels=FALSE,
                       xunits=NA)
{
  xlabel <- varName
  if(!is.na(xunits))
  {
    xlabel <- paste(varName, "(", xunits, ")")
  }
  
  varStats <- data.frame(min=min(data[,varName], na.rm=na.rm),
                         mean=mean(data[,varName], na.rm=na.rm), 
                         stdev=sd(data[,varName], na.rm=na.rm), 
                         median=median(data[,varName], na.rm=na.rm),
                         max=max(data[,varName], na.rm=na.rm))
  
  g1 <- ggplot(data) + 
    geom_histogram(aes_string(x=varName), binwidth = binwidth) + 
    labs(title=paste("Distribution of\n", varName, "Variable"),
         x=xlabel) +
    myTheme

  position <- "identity"
  if(jitter)
  {
    position <- "jitter"
  }
  
  g2 <- ggplot(data) + 
    geom_point(aes_string(x=varName, y=respName), alpha=0.4, position=position) + 
    labs(title=paste("Scatter plot of\n", varName, "vs", respName),
         x=xlabel) +
    myTheme
  
  g3 <- ggplot(data) + 
    geom_boxplot(aes_string(rep(varName, nrow(data)), varName)) + 
    labs(title=paste("Box plot of\n", varName), x="", y="") +
    myTheme
  
  if(rotateLabels)
  {
    g1 <- g1 + rotateXaxisLabels45
    g2 <- g2 + rotateXaxisLabels45
    g3 <- g3 + rotateXaxisLabels45
  }  
  
  
  return (list(varStats, g1, g2, g3))
}
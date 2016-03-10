### Daniel's Raw Code Sandbox...

#
# FUNCTION: loadBirthData
#
loadBirthData <- function(filename)
{
  # Load the Natality data
  birthFile <- sprintf("./data/%s", filename)
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
  
  birthDataWoNa$Month.Code <- as.numeric(birthDataWoNa$Month.Code)
  birthDataWoNa$Age.of.Mother <- as.factor(birthDataWoNa$Age.of.Mother)
  birthDataWoNa$Marital.Status <- as.factor(birthDataWoNa$Marital.Status)
  birthDataWoNa$Education <- as.factor(birthDataWoNa$Education)
  
  return (birthDataWoNa)
}


birthData <- loadBirthData("Natality, 2007-2014.txt")
summary(birthData)

birthData2 <- loadBirthData("Natality, 2003-2006.txt")
summary(birthData2)

allBirthData <- rbind(birthData, birthData2)

lmCdc <- lm(Births ~ Month.Code + Age.of.Mother + Marital.Status + Education, data=allBirthData)
summary(lmCdc)

#library(leaps)
#lmSubsCdc <- leaps::regsubsets(Births ~ Month.Code + Age.of.Mother + Marital.Status + Education, data=allBirthData)
#summary(lmSubsCdc)

#step(lmCdc)

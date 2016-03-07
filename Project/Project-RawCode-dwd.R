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
                                       'numeric',   # Year.Code
                                       'character', # Month
                                       'numeric',   # Month.Code
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
  return (birthDataWoNa)
}


birthData <- loadBirthData("Natality, 2007-2014.txt")
summary(birthData)

birthData2 <- loadBirthData("Natality, 2003-2006.txt")
summary(birthData2)



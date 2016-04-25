library(ggplot2)
library(plyr)
dlfFile <- "./Project/NatalityModels-DataLoadFuncs.R"
if(!file.exists(dlfFile))
{
  dlfFile <- paste0("../", dlfFile)
}
source(dlfFile)

### Daniel's Raw Code Sandbox...
dataPath <- "./data/"
if(!dir.exists(dataPath))
{
  dataPath <- paste0("../", dataPath)
}
dataPath <- paste0(dataPath, "%s")

# Load the 2007-2014 birth data
birthData <- loadBirthData("Natality, 2007-2014.txt", path=dataPath)
#summary(birthData)

# Load the 2003-2006 birth data
birthData2 <- loadBirthData("Natality, 2003-2006.txt", path=dataPath)
#summary(birthData2)

# Combine the 2 sets of birth data
allBirthData <- rbind(birthData, birthData2)
#summary(allBirthData)

# Load 2000s census data
censusData0010 <- loadCensus00Data(path=dataPath)
censusData0010 <- scaleCensusTotalPop(censusData0010)
#summary(censusData0010)

# Load 2010s census data
censusData1015 <- loadCensus10Data("NC-EST2014-ALLDATA-R-File%02d.csv", 12, path=dataPath)
censusData1015 <- scaleCensusTotalPop(censusData1015)
#summary(censusData1015)

g1 <- ggplot(censusData1015) + geom_line(aes(x=Date, y=GenderRatio))
g1

# Combine the 2000-2010 census with the 2010-2015 estimates 
allCensusData <- rbind(censusData0010, censusData1015)
g1 <- ggplot(allCensusData) + geom_line(aes(x=Date, y=TOT_FEMALE))
g1

# Load the women's earnings data
earningsData <- loadEarningsData("Earnings-2003-2015.csv", path=dataPath)
g1 <- ggplot(earningsData) + 
  geom_line(aes(x=Date, y=Earnings)) 
g1

# Load unemployment rate
urateData <- loadUnemploymentData("UnemploymentRate-2003-2015.csv", path=dataPath)
g1 <- ggplot(urateData) + 
  geom_line(aes(x=Date, y=UnemploymentRate)) 
g1


# Combine all together
allData <- plyr::join(allBirthData, allCensusData, by="Date")
allData <- plyr::join(allData, earningsData, by="Date")
allData <- plyr::join(allData, urateData, by="Date")
allData <- allData[,c("Year", 
                      "Month", 
#                       "Age.of.Mother", 
#                       "Age.of.Mother.Code", 
#                       "Marital.Status", 
#                       "Education", 
                      "Births", 
                      "Date", 
                      "TOT_POP", 
                      "GenderRatio", 
                      "TOT_FEMALE", 
                      "TOT_MALE", 
                      "Earnings", 
                      "UnemploymentRate")]
summary(allData)

ncolAllData <- ncol(allData)
nrowAllData <- nrow(allData)

# Data Exploration
missingVals <- sapply(allData, function(x) sum(is.na(x)))
missingValsPerc <- sapply(allData, function(x) sum(is.na(x))/length(x)*100)
dfMissingVals <- data.frame(Missing=missingVals, Percent=missingValsPerc)
#summary(missingVals)

# Correlation
colsForCor <- c("Year", "Month",  "Births", "TOT_POP", "GenderRatio", "TOT_FEMALE", 
                "TOT_MALE", "Earnings", "UnemploymentRate")
corMatrix <- cor(allData[,colsForCor], use="complete.obs")


# Initial linear model with just the birth data.
#lmCdc <- lm(Births ~ Month.Code + Age.of.Mother + Marital.Status + Education, data=allBirthData)
#summary(lmCdc)


#library(leaps)
#lmSubsCdc <- leaps::regsubsets(Births ~ Month.Code + Age.of.Mother + Marital.Status + Education, data=allBirthData)
#summary(lmSubsCdc)

#step(lmCdc)

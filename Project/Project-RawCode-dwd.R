library(ggplot2)
library(plyr)
### Load the data helper functions
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

# Combine the 2000-2010 census with the 2010-2015 estimates 
allCensusData <- rbind(censusData0010, censusData1015)

g1 <- ggplot(allCensusData) + 
  geom_line(aes(x=Date, y=GenderRatio)) +
  labs(title="Gender Ratio 2000 - 2015") +
  myTheme
g1

g1 <- ggplot(allCensusData) + 
  geom_line(aes(x=Date, y=TOT_FEMALE)) +
  labs(title="Female Population 2000 - 2015") +
  myTheme
g1

# Load the women's earnings data
earningsData <- loadEarningsData("Earnings-2003-2015.csv", path=dataPath)
g1 <- ggplot(earningsData) + 
  geom_line(aes(x=Date, y=Earnings)) +
  labs(title="Women's Weekly Earnings 2003 - 2015") +
  myTheme
g1

# Load unemployment rate
urateData <- loadUnemploymentData("UnemploymentRate-2003-2015.csv", path=dataPath)
g1 <- ggplot(urateData) + 
  geom_line(aes(x=Date, y=UnemploymentRate))  +
  labs(title="Unemployment Rate 2003 - 2015") +
  myTheme
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
                      #"FEMALE_15_30",
                      #"FEMALE_30_50",
                      "FEMALE_15_24",
                      "FEMALE_25_34",
                      "FEMALE_35_44",
                      "Earnings", 
                      "UnemploymentRate")]
summary(allData)
# Row and column counts
ncolAllData <- ncol(allData)
nrowAllData <- nrow(allData)

# Data Exploration
missingVals <- sapply(allData, function(x) sum(is.na(x)))
missingValsPerc <- sapply(allData, function(x) sum(is.na(x))/length(x)*100)
dfMissingVals <- data.frame(Missing=missingVals, Percent=missingValsPerc)
#summary(missingVals)

# Correlation
colsForCor <- c("Year", "Month",  "Births", "TOT_POP", "GenderRatio", "TOT_FEMALE", 
                "TOT_MALE", "FEMALE_15_24","FEMALE_25_34",
                "FEMALE_35_44", "Earnings", "UnemploymentRate")
corMatrix <- cor(allData[,colsForCor], use="complete.obs")


# Subset all data into training and validation data sets
cvSample <- sample(nrow(allData) * 0.20)
# Validation set
crossValData <- allData[cvSample,]
#crossValData <- fillInMissingWithMedian(crossValData, FALSE)
# Model set is subset to only complete cases.
modelData <- allData[-cvSample,]
completeModelData <- modelData[complete.cases(modelData), ]


# Initial linear model with all variables
lmAllVars <- lm(Births ~ Month + . - Year - Date, data=modelData)
smLmAllVars <- summary(lmAllVars)
vfAllVars <- faraway::vif(lmAllVars)

pdAllVars <- predict(lmAllVars, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdAllVars$fit)
gPdAllVars <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme
#gPdAllVars

# Significant variables model 
lmSigVars <- lm(Births ~ TOT_POP + GenderRatio + TOT_FEMALE + FEMALE_15_24 + FEMALE_25_34 + FEMALE_35_44 + Earnings, 
                data=modelData)
smLmSigVars <- summary(lmSigVars)
vfSigVars <- faraway::vif(lmSigVars)

pdSigVars <- predict(lmSigVars, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdSigVars$fit)
gPdSigVars <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme
#gPdSigVars

# High Cor variables model 
lmHighCorVars <- lm(Births ~ FEMALE_25_34 + UnemploymentRate + FEMALE_35_44 + Earnings + Month + TOT_FEMALE, 
                data=modelData)
smLmHighCorVars <- summary(lmHighCorVars)
vfHighCorVars <- faraway::vif(lmHighCorVars)

pdHighCorVars <- predict(lmHighCorVars, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdHighCorVars$fit)
gPdHighCorVars <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme
#gPdHighCorVars

# Signif-HighVIFS variables model 
lmSigVifVars <- lm(Births ~ Month + GenderRatio + FEMALE_25_34 + FEMALE_35_44 + Earnings, 
                data=modelData)
smLmSigVifVars <- summary(lmSigVifVars)
vfSigVifVars <- faraway::vif(lmSigVifVars)

pdSigVifVars <- predict(lmSigVifVars, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdSigVars$fit)
gPdSigVifVars <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Signif Vars - High VIFs Model")
gPdSigVifVars
smLmSigVifVars

# Step model 
lmStep <- step(lmAllVars)
smLmStep <- summary(lmStep)
vfStep <- faraway::vif(lmStep)

pdStepVars <- predict(lmStep, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdStepVars$fit)
gPdStep <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Step Model")
gPdStep
smLmStep

# Validation
showSummary <- FALSE
responseCol <- "Births"
cvAllLm <- crossValidate(lmAllVars, crossValData, responseCol, showSummary)
cvSignifLm <- crossValidate(lmSigVars, crossValData, responseCol, showSummary)
cvHighCorLm <- crossValidate(lmHighCorVars, crossValData, responseCol, showSummary)
cvStep <- crossValidate(lmStep, crossValData, responseCol, showSummary)

# 
# 
# 
cvLmResults <- data.frame(Model=c("All Variables", 
                                  "Significant",
                                  "High Cor",
                                  "Step"),
                          Val.Error=c(cvAllLm, cvSignifLm, cvHighCorLm, cvStep),
                          R2=c(smLmAllVars$adj.r.squared, 
                               smLmSigVars$adj.r.squared,
                               smLmHighCorVars$adj.r.squared,
                               smLmStep$adj.r.squared),
                          Variables=c(length(lmAllVars$coefficients) - 1,
                                      length(lmSigVars$coefficients) - 1,
                                      length(lmHighCorVars$coefficients) - 1,
                                      length(lmStep$coefficients) - 1),
                          VIF=c("TBD", "TBD","TBD", "TBD"))

#library(leaps)
#lmSubsCdc <- leaps::regsubsets(Births ~ Month.Code + Age.of.Mother + Marital.Status + Education, data=allBirthData)
#summary(lmSubsCdc)

#step(lmCdc)

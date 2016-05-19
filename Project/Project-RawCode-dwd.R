library(ggplot2)
library(plyr)
set.seed(020275)
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

gGenderRatio <- ggplot(allCensusData) + 
  geom_line(aes(x=Date, y=GenderRatio)) +
  labs(title="Gender Ratio 2000 - 2015") +
  myTheme
#gGenderRatio

gFmPop <- ggplot(allCensusData) + 
  geom_line(aes(x=Date, y=TOT_FEMALE)) +
  labs(title="Female Population 2000 - 2015") +
  myTheme
#gFmPop

# Load the women's earnings data
earningsData <- loadEarningsData("Earnings-2003-2015.csv", path=dataPath)
gEarnings <- ggplot(earningsData) + 
  geom_line(aes(x=Date, y=Earnings)) +
  labs(title="Women's Weekly Earnings 2003 - 2015") +
  myTheme
#gEarnings

# Load unemployment rate
urateData <- loadUnemploymentData("UnemploymentRate-2003-2015.csv", path=dataPath)
gUnemployment <- ggplot(urateData) + 
  geom_line(aes(x=Date, y=UnemploymentRate))  +
  labs(title="Unemployment Rate 2003 - 2015") +
  myTheme
#gUnemployment


# Combine all together
allData <- plyr::join(allBirthData, allCensusData, by="Date")
allData <- plyr::join(allData, earningsData, by="Date")
allData <- plyr::join(allData, urateData, by="Date")
allData$Month9Ago <- month(allData$Date - months(9))
allData <- allData[order(allData$Date),]
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
                      "UnemploymentRate",
                      "Month9Ago")]
#summary(allData)
# Row and column counts
ncolAllData <- ncol(allData)
nrowAllData <- nrow(allData)

#allData$moIndex <- month.abb[allData$Month]

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
cvSample <- sample(nrow(allData),  nrow(allData) * 0.20)
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

# Step model 
lmStep <- step(lmAllVars, trace=0)
smLmStep <- summary(lmStep)
vfStep <- faraway::vif(lmStep)

pdStepVars <- predict(lmStep, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdStepVars$fit)
gPdStep <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Step Model")
#gPdStep
#smLmStep

# Signif-HighVIFS variables model 
lmSigVifVars <- lm(Births ~ Month + GenderRatio + FEMALE_25_34 + FEMALE_35_44 + Earnings, 
                   data=modelData)
smLmSigVifVars <- summary(lmSigVifVars)
vfSigVifVars <- faraway::vif(lmSigVifVars)

pdSigVifVars <- predict(lmSigVifVars, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdSigVifVars$fit)
gPdSigVifVars <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Signif Vars - High VIFs Model")
#gPdSigVifVars
#smLmSigVifVars

# Signif-Limited variables model 
lmSigLimVars <- lm(Births ~ Month + Month9Ago + FEMALE_25_34 + UnemploymentRate, 
                   data=modelData)
smLmSigLimVars <- summary(lmSigLimVars)
vfSigLimVars <- faraway::vif(lmSigLimVars)

pdSigLimVars <- predict(lmSigLimVars, se.fit=TRUE)
pdModelData <- cbind(modelData, model=pdSigLimVars$fit)
gPdSigLimVars <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Signif Limited Model")
#gPdSigLimVars
#smLmSigLimVars
#vfSigLimVars

# Signif-Limited with Interaction variables model 
lmSigLimInterVars <- lm(Births ~ Month + Month9Ago + FEMALE_25_34 + UnemploymentRate + Month9Ago:FEMALE_25_34, 
                   data=modelData)
smLmSigLimInterVars <- summary(lmSigLimInterVars)
vfSigLimInterVars <- faraway::vif(lmSigLimInterVars)


# Poison Count Regression
pmSigLimVars <- glm(Births ~ Month + Month9Ago + FEMALE_25_34 + UnemploymentRate,
                    family=poisson, modelData)
smPmSigLimVars <- summary(pmSigLimVars)
vifPmSigLimVars <- faraway::vif(pmSigLimVars)

pdPmSigLimVars <- predict(pmSigLimVars, type="response")
pdModelData <- cbind(modelData, model=pdPmSigLimVars)
gPdSigLimVars <- ggplot(pdModelData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Signif Limited Count Model")

# Poisson Regression via StepAIC
# Poison Count Regression
pmAllVars <- glm(Births ~ Month + . - Year - Date,
                    family=poisson, modelData)

smpmAllVars <- summary(pmAllVars)

pmStep <- stepAIC(pmAllVars, direction="backward", trace=0)
smPmStep <- summary(pmStep)
#smPmStep

pmStepAICSuggested <- glm(Births ~ Month + TOT_POP + GenderRatio + TOT_FEMALE + FEMALE_15_24 + 
                            FEMALE_25_34 + FEMALE_35_44 + Earnings + UnemploymentRate + 
                            Month9Ago, family=poisson, modelData)

smPmStepAICSuggested <-summary(pmStepAICSuggested)
#smPmStepAICSuggested
vifPmStep <- faraway::vif(pmStepAICSuggested)

## Negative Binomial Models
nbm <- glm.nb(Births ~ Month + TOT_POP + GenderRatio + TOT_FEMALE + FEMALE_15_24 + 
                 FEMALE_25_34 + FEMALE_35_44 + Earnings + UnemploymentRate + 
                 Month9Ago, data=modelData)
smNbm <- summary(nbm)
vifNbm <- faraway::vif(nbm)

stepNbm <- stepAIC(nbm, direction="backward", trace=0)
#stepNbm$anova 


nbmStepAICSuggested <- glm.nb(Births ~ Month + TOT_POP + GenderRatio + FEMALE_25_34 + UnemploymentRate + 
                                Month9Ago, data=modelData)
vifStepNbm <- faraway::vif(nbmStepAICSuggested)
smNbmStepAIC <- summary(nbmStepAICSuggested)

# AR
#arModel <- ar(ts(modelData), method="burg")
#arModel

# Validation
showSummary <- FALSE
responseCol <- "Births"
cvAllLm <- crossValidate(lmAllVars, crossValData, responseCol, showSummary)
cvSignifLm <- crossValidate(lmSigVars, crossValData, responseCol, showSummary)
cvHighCorLm <- crossValidate(lmHighCorVars, crossValData, responseCol, showSummary)
cvStep <- crossValidate(lmStep, crossValData, responseCol, showSummary)
cvSigMinus <- crossValidate(lmSigVifVars, crossValData, responseCol, showSummary)
cvSigLim <- crossValidate(lmSigLimVars, crossValData, responseCol, showSummary)
cvPmSigLim <- crossValidateGLM(pmSigLimVars, crossValData, responseCol, showSummary)
cmPmStep <- crossValidateGLM(pmStepAICSuggested, crossValData, responseCol, showSummary)
cvNbm <- crossValidateGLM(nbm, crossValData, responseCol, showSummary)
cvSigLimInterVars <- crossValidate(lmSigLimInterVars, crossValData, responseCol, showSummary)

# 
# 
# 
cvLmResults <- data.frame(Model=c("All Variables", 
                                  "Significant",
                                  "High Cor",
                                  "Step",
                                  "Significant Minus",
                                  "Significant Limited",
                                  "Poisson Signif Ltd",
                                  "Poisson Step",
                                  "Neg Binomial Step",
                                  "Signif Ltd w/ Interaction"),
                          Val.Error=c(cvAllLm, 
                                      cvSignifLm, 
                                      cvHighCorLm, 
                                      cvStep, 
                                      cvSigMinus, 
                                      cvSigLim,
                                      cvPmSigLim,
                                      cmPmStep,
                                      cvNbm,
                                      cvSigLimInterVars),
                          R2=c(smLmAllVars$adj.r.squared, 
                               smLmSigVars$adj.r.squared,
                               smLmHighCorVars$adj.r.squared,
                               smLmStep$adj.r.squared,
                               smLmSigVifVars$adj.r.squared,
                               smLmSigLimVars$adj.r.squared,
                               NA,
                               NA,
                               NA,
                               smLmSigLimInterVars$adj.r.squared),
                          AIC=c(AIC(lmAllVars),
                                AIC(lmSigVars),
                                AIC(lmHighCorVars),
                                AIC(lmStep),
                                AIC(lmSigVifVars),
                                AIC(lmSigLimVars),
                                pmSigLimVars$aic,
                                pmStep$aic,
                                nbm$aic,
                                AIC(lmSigLimInterVars)),
                          Variables=c(length(lmAllVars$coefficients) - 1,
                                      length(lmSigVars$coefficients) - 1,
                                      length(lmHighCorVars$coefficients) - 1,
                                      length(lmStep$coefficients) - 1,
                                      length(lmSigVifVars$coefficients) - 1,
                                      length(lmSigLimVars$coefficients) - 1,
                                      length(pmSigLimVars$coefficients) - 1,
                                      length(pmStep$coefficients) - 1,
                                      length(nbm$coefficients) - 1,
                                      length(lmSigLimInterVars$coefficients) - 1),
                          VIF=c("BAD", "BAD","BAD", "BAD", "BAD", "OK", "OK", "BAD", "BAD", "BAD"))

# Significant Limited Model
pdSigLimVarsCV <- predict(lmSigLimVars, se.fit=TRUE, newdata=crossValData)
pdCVData <- cbind(crossValData, model=pdSigLimVarsCV$fit)

gPdSigLimVarsCV <- ggplot(pdCVData) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Signif Limited Model vs Validation Set", y="Births")
#gPdSigLimVarsCV

# Poisson Significant Limited Model
pdPmSigLimVarsAll <- predict(pmSigLimVars, type="response", newdata=allData)
pdAllDataPm <- cbind(allData, model=pdPmSigLimVarsAll)

gPdPmSigLimVarsAll <- ggplot(pdAllDataPm) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Signif Limited Model vs Full Data Set", y="Births")


# Poisson Significant Limited Model
pdPmSigLimVarsCV <- predict(pmSigLimVars, type="response", newdata=crossValData)
pdCVDataPm <- cbind(crossValData, model=pdPmSigLimVarsCV)

gPdPmSigLimVarsCV <- ggplot(pdCVDataPm) + 
  geom_line(aes(x=Date, y=model), colour="pink", size=1) + 
  geom_line(aes(x=Date, y=Births), colour="lightgreen", size=1) + myTheme +
  labs(title="Poisson Signif Limited Model vs Validation Set", y="Births")


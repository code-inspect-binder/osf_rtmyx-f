#####################################################################
## Perception analysis script for danger rating analysis           ##
## April 1, 2023                                                   ##
#####################################################################

## ---- 1) PREPARATION ----

## Load necessary packages
require(lcmm)               ## for regression analysis
require(SarpBulletinSurvey) ## for plotting results of regression analysis
require(party)              ## for CTrees
require(RColorBrewer)       ## Colors for CTrees
require(vcd)                ## for mosaic plots

## Clean workspace
rm(list = ls())

## Model file name
ModelFileName <- "Models_Perc_All.RData"

## ---- 2) GET DATA -----

## Load data
load("DangerRatingAnalysisData.RData")

## Select relevant table
Data <- Tbl_Perc

## Number of responses
nrow(Data)


## ---- 3) FORMATTING DATA FOR REGRESSION ANALYSIS ----

if (!file.exists(ModelFileName)) {

  ## Set up data frame
  DataReg <- data.frame(Id = character(),
                        DR = numeric(),
                        RngL = numeric(),
                        RngM = numeric(),
                        RngC = numeric(),
                        RngH = numeric(),
                        RngE = numeric(),
                        Sev = numeric())
  
  ## Processing of data
  for (i in 1:nrow(Data)) {
    Temp <- data.frame(Id = rep(Data$Id[i], 10),
                       DR = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4), 
                       RngL = c(-0.5,0.5, 0, 0, 0, 0, 0, 0, 0, 0),
                       RngM = c(0, 0, -0.5,0.5, 0, 0, 0, 0, 0, 0),
                       RngC = c(0, 0, 0, 0, -0.5,0.5, 0, 0, 0, 0),
                       RngH = c(0, 0, 0, 0, 0, 0, -0.5,0.5, 0, 0),
                       RngE = c(0, 0, 0, 0, 0, 0, 0, 0, -0.5,0.5),
                       Sev = c(Data$DRSeverity_L_low[i], Data$DRSeverity_L_up[i], 
                               Data$DRSeverity_M_low[i], Data$DRSeverity_M_up[i], 
                               Data$DRSeverity_C_low[i], Data$DRSeverity_C_up[i], 
                               Data$DRSeverity_H_low[i], Data$DRSeverity_H_up[i], 
                               Data$DRSeverity_E_low[i], Data$DRSeverity_E_up[i]))
    DataReg <- rbind(DataReg, Temp)  
  }
  
  rm(Temp, i)


## ---- 4) ESTIMATING REGRESSION MODELS ----

  ## Parameter settings
  NumClustMax <- 9
  DegPoly <- 2
  OutputFile <- "Models_Perc_All"
  
  ## Output list
  Models_Perc <- list()
  
  ## Loop for model estimation
  for (i in 1:NumClustMax) {
    # for (i in 8:9) {  
    
    message(paste0(Sys.time(), ": Processing model with ", i, " clusters ..."))
    
    if (i == 1) {
      
      set.seed(1234)
      fit <- hlme(Sev ~ poly(DR, degree = DegPoly, raw = TRUE) + RngL + RngM + RngC + RngH + RngE, 
                  random = ~ poly(DR, degree = DegPoly, raw = TRUE), subject = "Id", 
                  ng = 1, data = DataReg)
      
      fitstart <- fit
      
    } else {
      
      set.seed(1234)
      fit <- hlme(Sev ~ poly(DR, degree = DegPoly, raw = TRUE) + RngL + RngM + RngC + RngH + RngE, 
                  random = ~ poly(DR, degree = DegPoly, raw = TRUE) , subject = "Id", 
                  mixture = ~ poly(DR, degree = DegPoly, raw = TRUE) + RngL + RngM + RngC + RngH + RngE,
                  ng = i, data = DataReg, B = random(fitstart))
      
    }
    
    Models_Perc[[length(Models_Perc) + 1]] <- fit
    if (i > 1) names(Models_Perc) <- paste0("Class", c(1:i))
    save(Models_Perc, file = ModelFileName)
    save(fit, file = paste0("Model_Perc_", i, "Clusters.RData"))
    rm(fit)
    
  }
  
  rm(DegPoly, i, NumClustMax, fitstart)
  
} else {
  
  load (ModelFileName)
  
}

## ---- 5) EXAMINING REGRESSION MODELS ----

## AIC and BIC
AIC <- sapply(Models_Perc, function (x) x$AIC)
BIC <- sapply(Models_Perc, function (x) x$BIC)

par(mfrow=c(1,2))
plot(c(1:length(Models_Perc)), AIC, xlab = "Models_Perc", main = "AIC")
plot(c(1:length(Models_Perc)), BIC, xlab = "Models_Perc", main = "BIC")
par(mfrow=c(1,1))
rm(AIC, BIC)
## -> Continuous decrease

## Iterations
niter <- sapply(Models_Perc, function (x) x$niter)
plot(c(1:length(Models_Perc)), niter, xlab = "Models_Perc", main = "niter")
rm(niter)
## -> None of the model maxed out the iterations


## ---- 6) EXAMINING MODEL WITH 7 CLASSES ----

Model_Perc <- Models_Perc[[7]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Simple wrapper function for plotting model
plotModel <- function(Model, Tbl = Data, Order = NA, BasePlot = T, ModelPlot = T, mfrow = c(4,2), main = "") {
  
  if (is.na(Order[1])) {Order <- seq(1:Model$ng)}
  
  ClassDF <- Model$pprob
  
  par(mfrow = mfrow)
  
  for (i in 1:Model$ng) {
    if (ModelPlot) {plotDRSeverityHLME2(Model = Model, Tbl = Tbl, Class = Order[i], main = main[i])}
  }
  
  par(mfrow = c(1,1))
  
}

## Plotting
Ord <- c(7, 2, 4, 1, 5, 6, 3)
Main <- c("a) Concave, wider", "b) Linear, narrower", "c) Linear, wider", "d) Convex, narrower", "e) Convex, widest", 
         "f) Convex, high start, narrowing", "g) Convex, low end, widening")
png(filename = "Fig_05_PerceptionCharts_07Cl.png", width = 17, height = 15, units = "cm", res = 300, pointsize = 8)
mfrow <- c(3,3)
plotModel(Model_Perc, Order = Ord, main = Main, mfrow = mfrow)
dev.off()

## Exreacting model parameters
FixEff <- summary(Model_Perc)
FixEff[grep("class1",  rownames(FixEff)),]
FixEff[grep("class2",  rownames(FixEff)),]
FixEff[grep("class3",  rownames(FixEff)),]
FixEff[grep("class4",  rownames(FixEff)),]
FixEff[grep("class5",  rownames(FixEff)),]
FixEff[grep("class6",  rownames(FixEff)),]
FixEff[grep("class7",  rownames(FixEff)),]

## Clean up
rm(Ord, Main, mfrow, plotModel, FixEff)


## ---- 7) CTREE ANALYSIS LINKING CLASS MEMBERSHIP TO BACKGROUND ----

## New data frame for CTree analysis
CTreeData <- Data
nrow(CTreeData)

## Transferring model class to Data
CTreeData$ModelClass <- factor(Model_Perc$pprob$class)
(temp <- table(CTreeData$ModelClass, useNA = "always"))
round(100*prop.table(temp), 1)

## Labelling and ordering of classes
Ord <- c(7, 2, 4, 1, 5, 6, 3)
Main <- c("Concave, wider", "Linear, narrower", "Linear, wider", "Convex, narrower", "Convex, widest", 
          "Convex, high start, narrowing", "Convex, low end, widening")

CTreeData$ModelClassN <- factor(CTreeData$ModelClass, levels = Ord, labels = Main)
table(CTreeData$ModelClass, CTreeData$ModelClassN)

## Color scale for CTree
ScaleCol <- RColorBrewer::brewer.pal(Model_Perc$ng, "Set1")
ScaleCol <- RColorBrewer::brewer.pal(Model_Perc$ng, "Paired")
ScaleCol <- c("#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")

## Eliminate NA from predictors
table(CTreeData$BackgrAvTraining, useNA = "always")
CTreeData <- CTreeData[!is.na(CTreeData$BackgrAvTraining),]
table(CTreeData$BackgrYrsOfExp, useNA = "always")
CTreeData <- CTreeData[!is.na(CTreeData$BackgrYrsOfExp),]
table(CTreeData$BackgrDaysPerYr, useNA = "always")
CTreeData <- CTreeData[!is.na(CTreeData$BackgrDaysPerYr),]
table(CTreeData$DemogrCountry, useNA = "always")
CTreeData <- CTreeData[!is.na(CTreeData$DemogrCountry),]
table(CTreeData$BackgrActivity_1, useNA = "always")
CTreeData <- CTreeData[!is.na(CTreeData$BackgrActivity_1),]

nrow(CTreeData)

## Estimating CTree with activity
CTree <- ctree(ModelClassN ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry + BackgrActivity_1,
               data = CTreeData)

## Plot CTree
plot(CTree, terminal_panel = node_barplot(CTree, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))
## -> Activity has no impact

## Estimating CTree without activity
CTree <- ctree(ModelClassN ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry,
               data = CTreeData)
plot(CTree, terminal_panel = node_barplot(CTree, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))

## Plot CTree
png(filename = "Fig_06_PercTree_hires_template.png", width = 12, height = 10, units = "cm", res = 300, pointsize = 8)
plot(CTree, terminal_panel = node_barplot(CTree, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))
dev.off()

rm(ScaleCol)

## Transferring ctree node to Data
CTreeData$CTreeNode <- predict(CTree, type = "node")
table(CTreeData$CTreeNode)
## -> Needs to match with what is shown in CTree plot

## Showing differences with mosaic plot
(test <- chisq.test(CTreeData$CTreeNode, CTreeData$ModelClassN))
mosaic(test$observed, shade = T)

## Clean up
rm(test, temp)

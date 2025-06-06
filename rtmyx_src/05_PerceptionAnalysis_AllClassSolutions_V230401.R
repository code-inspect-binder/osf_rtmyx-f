#####################################################################
## Examines all class solutions of perrception question            ##
## April 1, 2023                                                   ##
#####################################################################

## ---- 1) PREPARATION ----

## Load necessary packages
require(lcmm)               ## for regression analysis
require(SarpBulletinSurvey) ## for plotting results of regression analysis
require(RColorBrewer)       ## Colors for CTrees
require(igraph)             ## for network plot


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


# ## ---- 3) FORMATTING DATA FOR REGRESSION ANALYSIS ----
# 
# if (!file.exists(ModelFileName)) {
# 
#   ## Set up data frame
#   DataReg <- data.frame(Id = character(),
#                         DR = numeric(),
#                         RngL = numeric(),
#                         RngM = numeric(),
#                         RngC = numeric(),
#                         RngH = numeric(),
#                         RngE = numeric(),
#                         Sev = numeric())
#   
#   ## Processing of data
#   for (i in 1:nrow(Data)) {
#     Temp <- data.frame(Id = rep(Data$Id[i], 10),
#                        DR = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4), 
#                        RngL = c(-0.5,0.5, 0, 0, 0, 0, 0, 0, 0, 0),
#                        RngM = c(0, 0, -0.5,0.5, 0, 0, 0, 0, 0, 0),
#                        RngC = c(0, 0, 0, 0, -0.5,0.5, 0, 0, 0, 0),
#                        RngH = c(0, 0, 0, 0, 0, 0, -0.5,0.5, 0, 0),
#                        RngE = c(0, 0, 0, 0, 0, 0, 0, 0, -0.5,0.5),
#                        Sev = c(Data$DRSeverity_L_low[i], Data$DRSeverity_L_up[i], 
#                                Data$DRSeverity_M_low[i], Data$DRSeverity_M_up[i], 
#                                Data$DRSeverity_C_low[i], Data$DRSeverity_C_up[i], 
#                                Data$DRSeverity_H_low[i], Data$DRSeverity_H_up[i], 
#                                Data$DRSeverity_E_low[i], Data$DRSeverity_E_up[i]))
#     DataReg <- rbind(DataReg, Temp)  
#   }
#   
#   rm(Temp, i)
# 
# 
# ## ---- 4) ESTIMATING REGRESSION MODELS ----
# 
#   ## Parameter settings
#   NumClustMax <- 9
#   DegPoly <- 2
#   OutputFile <- "Models_Perc_All"
#   
#   ## Output list
#   Models_Perc <- list()
#   
#   ## Loop for model estimation
#   for (i in 1:NumClustMax) {
#     # for (i in 8:9) {  
#     
#     message(paste0(Sys.time(), ": Processing model with ", i, " clusters ..."))
#     
#     if (i == 1) {
#       
#       set.seed(1234)
#       fit <- hlme(Sev ~ poly(DR, degree = DegPoly, raw = TRUE) + RngL + RngM + RngC + RngH + RngE, 
#                   random = ~ poly(DR, degree = DegPoly, raw = TRUE), subject = "Id", 
#                   ng = 1, data = DataReg)
#       
#       fitstart <- fit
#       
#     } else {
#       
#       set.seed(1234)
#       fit <- hlme(Sev ~ poly(DR, degree = DegPoly, raw = TRUE) + RngL + RngM + RngC + RngH + RngE, 
#                   random = ~ poly(DR, degree = DegPoly, raw = TRUE) , subject = "Id", 
#                   mixture = ~ poly(DR, degree = DegPoly, raw = TRUE) + RngL + RngM + RngC + RngH + RngE,
#                   ng = i, data = DataReg, B = random(fitstart))
#       
#     }
#     
#     Models_Perc[[length(Models_Perc) + 1]] <- fit
#     if (i > 1) names(Models_Perc) <- paste0("Class", c(1:i))
#     save(Models_Perc, file = ModelFileName)
#     save(fit, file = paste0("Model_Perc_", i, "Clusters.RData"))
#     rm(fit)
#     
#   }
#   
#   rm(DegPoly, i, NumClustMax, fitstart)
#   
# } else {
  
  load (ModelFileName)
  
# }

## ---- 5) EXAMINING REGRESSION MODELS ----

## AIC and BIC
AIC <- sapply(Models_Perc, function (x) x$AIC)
BIC <- sapply(Models_Perc, function (x) x$BIC)

png(filename = "Fig_S01_PerceptionAICBIC.png", width = 15, height = 12, units = "cm", res = 300, pointsize = 8)
par(mfrow=c(1,2))
plot(c(1:length(Models_Perc)), AIC, main = "AIC",  t = "b", xlab = "Number of classes in solution")
plot(c(1:length(Models_Perc)), BIC, main = "BIC", t = "b", xlab = "Number of classes in solution")
par(mfrow=c(1,1))
dev.off()
rm(AIC, BIC)
## -> Continuous decrease

## Iterations
niter <- sapply(Models_Perc, function (x) x$niter)
plot(c(1:length(Models_Perc)), niter, xlab = "Models_Perc", main = "niter")
rm(niter)
## -> None of the model maxed out the iterations


## ---- 6) EXAMINING MODEL WITH DIFFERENT CLASSES ----

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

# Output data frame prep
Poly2Param <- data.frame(coef = numeric(),
                         Se = numeric(),
                         Wald = numeric(),
                         p = numeric(),
                         classID = character(),
                         stringsAsFactors = F)


## ---- 6.1) 1-class solution ----
ClassSol <- 1
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord1 <- c(1)
Main1 <- c("a) Linear (entire dataset)")
png(filename = "Fig_S03_PerceptionCharts_01Cl.png", width = 17, height = 5, units = "cm", res = 300, pointsize = 8)
mfrow <- c(1,3)
plotModel(Model_Perc, Order = Ord1, main = Main1, mfrow = mfrow)
dev.off()

## Extracting model parameters
FixEff <- summary(Model_Perc)

## Extracting second polynomial parameter
Temp <- as.array(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp <- data.frame(coef = Temp[1], Se = Temp[2], Wald = Temp[3], p = Temp[4],
                   ClassID = "1-1", stringsAsFactors = F)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Data$Class1 <- 1

## Clean up
rm(mfrow, Model_Perc, Temp)


## ---- 6.2) 2-class solutions ----
ClassSol <- 2
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord2 <- c(1, 2)
Main2 <- c("a) Linear, narrower", "b) Linear, wider")
png(filename = "Fig_S04_PerceptionCharts_02Cl.png", width = 17, height = 5, units = "cm", res = 300, pointsize = 8)
mfrow <- c(1,3)
plotModel(Model_Perc, Order = Ord2, main = Main2, mfrow = mfrow)
dev.off()

## Extracting model parameters
FixEff <- summary(Model_Perc)
FixEff[grep("class1",  rownames(FixEff)),]
FixEff[grep("class2",  rownames(FixEff)),]

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class2")
Data <- merge(Data, Memb)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ----- 6.3) 3-class solutions ----
ClassSol <- 3
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord3 <- c(2, 3, 1) 
Main3 <- c("a) Linear, narrower", "b) Linear, wider", "c) Linear, low end, widest")
png(filename = "Fig_S05_PerceptionCharts_03Cl.png", width = 17, height = 5, units = "cm", res = 300, pointsize = 8)
mfrow <- c(1,3)
plotModel(Model_Perc, Order = Ord3, main = Main3, mfrow = mfrow)
dev.off()

## Extracting model parameters
FixEff <- summary(Model_Perc)
FixEff[grep("class1",  rownames(FixEff)),]
FixEff[grep("class2",  rownames(FixEff)),]
FixEff[grep("class3",  rownames(FixEff)),]

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class3")
Data <- merge(Data, Memb)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ----- 6.4) 4-class solutions ----
ClassSol <- 4
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord4 <- c(1, 4, 2, 3)
Main4 <- c("a) Linear, wider", "b) Linear, narrower", "c) Convex, wide", "d) Convex, low end, widest")
png(filename = "Fig_S06_PerceptionCharts_04Cl.png", width = 17, height = 10, units = "cm", res = 300, pointsize = 8)
mfrow <- c(2,3)
plotModel(Model_Perc, Order = Ord4, main = Main4, mfrow = mfrow)
dev.off()

## Extracting model parameters
FixEff <- summary(Model_Perc)
FixEff[grep("class1",  rownames(FixEff)),]
FixEff[grep("class2",  rownames(FixEff)),]
FixEff[grep("class3",  rownames(FixEff)),]
FixEff[grep("class4",  rownames(FixEff)),]

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class4")
Data <- merge(Data, Memb)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ----- 6.5) 5-class solutions ----
ClassSol <- 5
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord5 <- c(2, 3, 1, 5, 4)
Main5 <- c("a) Slightly concave, wider", "b) Linear, narrower", "c) Convex, wider", "d) Convex, narrower", "e) Convex, low end, widest")
png(filename = "Fig_S07_PerceptionCharts_05Cl.png", width = 17, height = 10, units = "cm", res = 300, pointsize = 8)
mfrow <- c(2,3)
plotModel(Model_Perc, Order = Ord5, main = Main5, mfrow = mfrow)
dev.off()

## Extracting model parameters
FixEff <- summary(Model_Perc)
FixEff[grep("class1",  rownames(FixEff)),]
FixEff[grep("class2",  rownames(FixEff)),]
FixEff[grep("class3",  rownames(FixEff)),]
FixEff[grep("class4",  rownames(FixEff)),]
FixEff[grep("class5",  rownames(FixEff)),]

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class5")
Data <- merge(Data, Memb)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ----- 6.6) 6-class solutions ----
ClassSol <- 6
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord6 <- c(4, 3, 2, 1, 6, 5)
Main6 <- c("a) Concave, wider", "b) Linear, narrower", "c) Convex, wider", "d) Convex, narrower", "e) Convex, high start, widest", "f) Convex, low end, widest")
png(filename = "Fig_S08_PerceptionCharts_06Cl.png", width = 17, height = 10, units = "cm", res = 300, pointsize = 8)
mfrow <- c(2,3)
plotModel(Model_Perc, Order = Ord6, main = Main6, mfrow = mfrow)
dev.off()

## Extracting model parameters
FixEff <- summary(Model_Perc)
FixEff[grep("class1",  rownames(FixEff)),]
FixEff[grep("class2",  rownames(FixEff)),]
FixEff[grep("class3",  rownames(FixEff)),]
FixEff[grep("class4",  rownames(FixEff)),]
FixEff[grep("class5",  rownames(FixEff)),]
FixEff[grep("class6",  rownames(FixEff)),]

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class6")
Data <- merge(Data, Memb)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ----- 6.7) 7-class solutions ----
ClassSol <- 7
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord7 <- c(7, 2, 4, 1, 5, 6, 3)
Main7 <- c("a) Concave, wider", "b) Linear, narrower", "c) Linear, wider", "d) Convex, narrower", "e) Convex, widest", 
         "f) Convex, high start, narrowing", "g) Convex, low end, widening")
png(filename = "Fig_S09_PerceptionCharts_07Cl.png", width = 17, height = 15, units = "cm", res = 300, pointsize = 8)
mfrow <- c(3,3)
plotModel(Model_Perc, Order = Ord7, main = Main7, mfrow = mfrow)
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

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class7")
Data <- merge(Data, Memb)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ----- 6.8) 8-class solutions ----
ClassSol <- 8
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord8 <- c(6, 4, 3, 5, 8, 7, 2, 1)
Main8 <- c("a) Concave, wider", "b) Linear, narrower", "c) Linear, wider", "d) Convex, wider", "e) Convex, narrower", 
           "f) Convex, widest", "g) Convex, high start, narrowing", "h) Convex, low end, widening")
png(filename = "Fig_S10_PerceptionCharts_08Cl.png", width = 17, height = 15, units = "cm", res = 300, pointsize = 8)
mfrow <- c(3,3)
plotModel(Model_Perc, Order = Ord8, main = Main8, mfrow = mfrow)
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
FixEff[grep("class8",  rownames(FixEff)),]

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class8")
Data <- merge(Data, Memb)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ----- 6.9) 9-class solutions ----
ClassSol <- 9
Model_Perc <- Models_Perc[[ClassSol]]

## Summary
summary(Model_Perc)
postprob(Model_Perc)

## Plotting
Ord9 <- c(3, 1, 6, 2, 9, 4, 7, 5, 8)
Main9 <- c("a) Concave, wider", "b) Linear, narrower", "c) Linear, widest", "d) Linear, wider", "e) Convex, narrower", 
           "f) Convex, wider","g) Convex, widest", "h) Convex, high start, narrowing", "i) Convex, low,end, widening")
png(filename = "Fig_S11_PerceptionCharts_09Cl.png", width = 17, height = 15, units = "cm", res = 300, pointsize = 8)
mfrow <- c(3,3)
plotModel(Model_Perc, Order = Ord9, main = Main9, mfrow = mfrow)
dev.off()

## Extracting model parameters
FixEff <- summary(Model_Perc)
FixEff[grep("class1",  rownames(FixEff)),]
FixEff[grep("class2",  rownames(FixEff)),]
FixEff[grep("class3",  rownames(FixEff)),]
FixEff[grep("class4",  rownames(FixEff)),]
FixEff[grep("class5",  rownames(FixEff)),]
FixEff[grep("class6",  rownames(FixEff)),]
FixEff[grep("class7",  rownames(FixEff)),]
FixEff[grep("class8",  rownames(FixEff)),]

## Extracting class membership
Memb <- Model_Perc$pprob[,c("Id", "class")]
table(Memb$class)
names(Memb) <- c("Id", "Class9")
Data <- merge(Data, Memb)

## Extracting second polynomial parameter
Temp <- as.data.frame(FixEff[grep("poly",  rownames(FixEff)),][(ClassSol+1):(2*ClassSol),])
Temp$ClassID <- paste(ClassSol, 1:ClassSol, sep = "-")
names(Temp) <- names(Poly2Param)
Poly2Param <- rbind(Poly2Param, Temp)

## Clean up
rm(mfrow, FixEff, Model_Perc, Memb, Temp)


## ---- 7) SECOND POLYMONIAL PARAMETER ----

## Overview
rownames(Poly2Param) <- Poly2Param$ClassID
Poly2Param <- Poly2Param[order(Poly2Param$coef),]
hist(Poly2Param$coef, breaks = seq(-2.25, 1.25, by = 0.25), col = "gold", main = "Distribution of second polynomial parameter estimates",
     xlab = "Parameter estimates")

## Assigning labels
Poly2Param$Shp <- "Linear"
# Poly2Param$Shp[Poly2Param$coef < -0.50] <- "Convex-ish"
Poly2Param$Shp[Poly2Param$coef < -0.75] <- "Convex"
# Poly2Param$Shp[Poly2Param$coef > 0.50] <- "Concave-ish"
Poly2Param$Shp[Poly2Param$coef > 0.75] <- "Concave"
Poly2Param$Shp <- ordered(Poly2Param$Shp, c("Convex", "Convex-ish", "Linear", "Concave-ish", "Concave"))
table(Poly2Param$Shp)


## ---- 8) PLOT NETWORK ----

## Check Data data frame
names(Data)

## Convert and concatenate order arrays
convertOrderForNetwork <- function(Ord) {
  Output <- integer()
  for (i in 1:length(Ord)) {
    Output[i] <- which(Ord == i)
  }
  return(Output)
}

Ord1N <- convertOrderForNetwork(Ord1)
Ord2N <- convertOrderForNetwork(Ord2)
Ord3N <- convertOrderForNetwork(Ord3)
Ord4N <- convertOrderForNetwork(Ord4)
Ord5N <- convertOrderForNetwork(Ord5)
Ord6N <- convertOrderForNetwork(Ord6)
Ord7N <- convertOrderForNetwork(Ord7)
Ord8N <- convertOrderForNetwork(Ord8)
Ord9N <- convertOrderForNetwork(Ord9)

Order <- c(Ord1N, Ord2N+1, Ord3N+3, Ord4N+6, Ord5N+10, Ord6N+15, Ord7N+21, Ord8N+28, Ord9N+36)

## NODES
## *****
## Create node dataframe
model <- numeric()
class <- numeric()
for (i in 1:9) {
  model <- c(model, rep(i, i))
  class <- c(class, seq(1:i))
}

AbbyNodes <- data.frame(id = paste(model, class, sep = "-"), model = model, class = class, name = class, n = NA,
                        stringsAsFactors = F)
rm(i, model, class)

## Add number of members
for (i in 1:9) {
  for (j in 1:i) {
    AbbyNodes$n[AbbyNodes$model == i & AbbyNodes$class == j] <- sum(Data[,paste0("Class", i)] == j)
  }
}
rm(i, j)

## Add Shp attribute 
AbbyNodes <- merge(AbbyNodes, Poly2Param[,c("ClassID", "Shp")], by.x = "id", by.y = "ClassID")
AbbyNodes$ShpCol <- as.numeric(AbbyNodes$Shp)

# AbbyNodes$Shp <- 3 ## Convex
# AbbyNodes$Shp[AbbyNodes$id %in% c("1-1", "2-1", "3-2", "4-4", "5-3", "6-3", "7-2", "8-4", "9-1")] <- 2 ## Linear
# AbbyNodes$Shp[AbbyNodes$id %in% c("4-1", "5-2", "6-4", "7-7", "8-6", "9-3")] <- 1  ## Concave
# table(AbbyNodes$Shp)

head(AbbyNodes, 10)

## EDGES
## *****
## Create links dataframe
from_model <- numeric()
from_class <- numeric()
to_model <- numeric()
to_class <- numeric()
n <- numeric()

for (i in 1:8) {
  
  from_Tbl <- AbbyNodes[AbbyNodes$model == i,]
  to_Tbl <- AbbyNodes[AbbyNodes$model == i + 1,]
  
  for (from in 1:nrow(from_Tbl)) {
    for (to in 1:nrow(to_Tbl)) {
      from_model <- c(from_model, from_Tbl$model[from]) 
      from_class <- c(from_class, from_Tbl$class[from])
      to_model <- c(to_model, to_Tbl$model[to]) 
      to_class <- c(to_class, to_Tbl$class[to]) 
      
      n <- c(n, nrow(Data[(Data[paste0("Class", i)] == from) & (Data[paste0("Class", i+1)] == to),]))
      
    }
  }  
}

AbbyLinks <- data.frame(from = paste(from_model, from_class, sep = "-"),
                        to = paste(to_model, to_class, sep = "-"),
                        n = n,
                        stringsAsFactors = F)

rm(i, from, from_model, from_class, from_Tbl, to, to_model, to_class, to_Tbl, n)

## Calculate to and from percentages
from_id <- unique(AbbyLinks$from)
for (i in 1:length(from_id)) {
  AbbyLinks$perc_from[AbbyLinks$from == from_id[i]] <- AbbyLinks$n[AbbyLinks$from == from_id[i]]/sum(AbbyLinks$n[AbbyLinks$from == from_id[i]])
}
to_id <- unique(AbbyLinks$to)
for (i in 1:length(to_id)) {
  AbbyLinks$perc_to[AbbyLinks$to == to_id[i]] <- AbbyLinks$n[AbbyLinks$to == to_id[i]]/sum(AbbyLinks$n[AbbyLinks$to == to_id[i]])
}
rm(i, from_id, to_id)

head(AbbyLinks, 10)

## Order nodes
AbbyNodes$Order <- Order
AbbyNodes <- AbbyNodes[order(AbbyNodes$Order),]
rm(Order)


## TURN INTO NETWORK
## *****************
AbbyNet <- graph_from_data_frame(d=AbbyLinks, vertices=AbbyNodes, directed=T) 


## PRESENTATION
## ************

## Size nodes and links
SizeMax <- 25
SizeMin <- 5

V(AbbyNet)$size <- (V(AbbyNet)$n - min(V(AbbyNet)$n))/(max(V(AbbyNet)$n) - min(V(AbbyNet)$n))*(SizeMax - SizeMin) + SizeMin
E(AbbyNet)$width <- E(AbbyNet)$n/100
rm(SizeMin, SizeMax)

## Color
col <- brewer.pal(5, "RdYlBu")
# col <- c("gray50", "tomato", "gold")
V(AbbyNet)$color <- col[V(AbbyNet)$ShpCol]

## Layout
lx <- numeric()
ly <- numeric()

ydiff <- 1/8
xdiff <- 1/8

for (i in 1:9) {
  xstart <- 0.5 - (i-1)/2*xdiff
  for (j in 1:i) {
    ly <- c(ly, 1 - (i-1)*ydiff)
    lx <- c(lx, xstart + (j-1)*xdiff)
  }
}

l <- matrix(c(lx, ly), ncol = 2)
rm(lx, ly, xdiff, ydiff, xstart, i, j)


## Liminate edges
cut.off <- 0.1
# cut.off <- 0
if (cut.off > 0) {AbbyNet <- delete.edges(AbbyNet, E(AbbyNet)[perc_to <= cut.off])}

# edge.label <- round(100*E(AbbyNet)$perc_to)
# edge.label <- round(100*E(AbbyNet)$perc_from)
edge.label <- E(AbbyNet)$n

vertex.label <- V(AbbyNet)$name
# vertex.label <- V(AbbyNet)$n


op <- par(mfrow = c(1,1), mar=c(1,1,1,1))
plot(AbbyNet, layout = l, 
     vertex.label = vertex.label, vertex.label.family	= "TT Arial", vertex.label.cex = 0.75, 
     edge.label.family	= "TT Arial", edge.label.cex = 0.5, edge.arrow.size=0, edge.label = edge.label)
par(op)

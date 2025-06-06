#####################################################################
## Danger rating use analysis script for danger rating analysis    ##
## April 1, 2023                                                   ##
#####################################################################

## ---- 1) PREPARATION ----

## Load necessary packages
require(poLCA)              ## for latent class analysis
require(SarpBulletinSurvey) ## for plotting results of latent class analysis
require(party)              ## for CTrees
require(vcd)                ## for mosaic plots

## Clean workspace
rm(list = ls())


## ---- 2) GET DATA -----

## Load data
load("DangerRatingAnalysisData.RData")

## Select relevant table
Data <- Tbl_Use

## Number of responses
nrow(Data)


## ---- 3) FORMATTING DATA FOR LATENT CLASS ANALYSIS ----

## Convert Threshold question to ordinal variable
str(Data)
Data$DRThresh_L <- ordered(Data$DRThresh_L)
Data$DRThresh_M <- ordered(Data$DRThresh_M)
Data$DRThresh_C <- ordered(Data$DRThresh_C)
Data$DRThresh_H <- ordered(Data$DRThresh_H)
Data$DRThresh_E <- ordered(Data$DRThresh_E)
str(Data)

## ---- 4) BASIC STATISTICS -----

## How many people do not enter the backcountry when the danger rating is extreme
(temp <- table(Data$DRThresh_E))
round(100*prop.table(temp), 1) ## -> 91%

## How many people do not enter the backcountry when the danger rating is high
(temp <- table(Data$DRThresh_H))
round(100*prop.table(temp), 1) ## -> 62%
## Because we required the answers to grow monotonically, these peoples also do not go out at Extreme

## How many people enter the backcountry primarily based on the danger rating when the rating is low
(temp <- table(Data$DRThresh_L))
round(100*prop.table(temp), 1) ## -> 33%

## How many people enter the backcountry primarily based on the danger rating when the rating is moderate
(temp <- table(Data$DRThresh_M))
round(100*prop.table(temp), 1) ## -> 6%


## ---- 5) LATENT CLUSTER ANALYSIS ----

formula <- cbind(DRThresh_L, DRThresh_M, DRThresh_C, DRThresh_H, DRThresh_E) ~ 1
NumClustMax <- 9

Models_Use <- list()

## Estimating Models
for (i in 1:NumClustMax) {
  set.seed(12345)
  fit <- poLCA(formula, data = Data, nclass = i, nrep = 3)
  Models_Use[[length(Models_Use) + 1]] <- fit
  if (i > 1) names(Models_Use) <- paste0("Class", c(1:i))
}

## Model evaluation
BIC <- sapply(Models_Use, function (x) x$bic)
AIC <- sapply(Models_Use, function (x) x$aic)

par(mfrow=c(1,2))
plot(c(1:length(Models_Use)), AIC, t = "b", xlab = "Models", main = "AIC")
plot(c(1:length(Models_Use)), BIC, t = "b", xlab = "Models", main = "BIC")
par(mfrow=c(1,1))
## --> 6 clusters seem to be best according to BIC

## Clean up
rm(fit, i, AIC, BIC, NumClustMax, formula)


## ---- 6) EXAMINING 6 CLUSTER SOLUTION -----

Model_Use <- Models_Use[[6]]

## Summary
Model_Use

## Transferring class membership to Data
Data$Class <- as.factor(Model_Use$predclass)
(temp <- table(Data$Class))
round(100*prop.table(temp), 1)
## -> Should match with predicted class membership

## Transferring posterior probabilities
Post <- as.data.frame(Model_Use$posterior)
names(Post) <- c("Post1", "Post2", "Post3", "Post4", "Post5", "Post6")
head(Post)

Data <- cbind(Data, Post)
head(Data)
rm(Post)

## Calculating assignment probability
summary(Data$Post1[Data$Class == 1])
summary(Data$Post2[Data$Class == 2])
summary(Data$Post3[Data$Class == 3])
summary(Data$Post4[Data$Class == 4])
summary(Data$Post5[Data$Class == 5])
summary(Data$Post6[Data$Class == 6])

## Plotting
# ClassOrder <- c(1:6)
ClassOrder <- c(2, 4, 3, 6, 1, 5) ## Based on relationship to Bull User Type
Data$Class <- factor(Data$Class, levels = ClassOrder)
table(Data$Class)

Titles <- c("a) Class 1: Rely exclusively on DR at L & M",
            "b) Class 2: Rely mainly on DR at L & M; H & E prevent trips",
            "c) Class 3: Rely mainly on DR at L, M & C; E prevents trips",
            "d) Class 4: Use mainly on other info; H & E prevents trips",
            "e) Class 5: Use mainly on other info; E prevents trips",
            "f) Class 6: Use other info at all DR levels")

## Patterns
png(filename = "Fig_07_UseClusters_hires_template.png", width = 15, height = 15, units = "cm", res = 300, pointsize = 8)
par(mfrow=c(3, 2))
for (i in 1:length(ClassOrder)) {
  plotDRThresh1(Data[Data$Class == ClassOrder[i],], main = Titles[i], ClassID = ClassOrder[i])
}
dev.off()
par(mfrow=c(1,1))
rm(i)

## ---- 7) UNIVARIATE RELATIONSHIP BETWEEN BULLETIN USER TYPE AND USE CLASS ----

## Mosaic plot
(test <- chisq.test(Data$Class, Data$BullUseType))
names(dimnames(test$observed)) <- c("Latent Class", "Bulletin User Type")
mosaic(test$observed, shade = T)
## -> Strong relationship
##    Used to order classes in plot higher up

(temp <- table(Data$Class, Data$BullUseType))
round(100*prop.table(temp, 1), 1)
round(100*prop.table(temp, 2), 1)

## Speaman's rank correlation
cor.test(as.numeric(Data$Class), as.numeric(Data$BullUseType), method = "spearman")

## -> Bulletin user type is not included in CTree becasue how participants use 
##    the danger scale is very much a direct representation of the bulletin user type

## Clean up
rm(test)

## ---- 8) FIRST CTREE ANALYSIS TO EXAMINE RELATION TO PREDICTORS WITH PERCEPTION ----

## New data frame for CTree analysis
CTreeData1 <- Data
nrow(CTreeData1)

## Test dependent variable
CTreeData1$Class <- factor(CTreeData1$Class)
(temp <- table(CTreeData1$Class, useNA = "always"))
round(100*prop.table(temp), 1)

## Retrieve perception class
load ("Models_Perc_All.RData")
Model_Perc <- Models_Perc[[7]]
PercClass <- Model_Perc$pprob[,c("Id", "class")]
names(PercClass) <- c("Id", "PercClass")
PercClass$PercClass <- factor(PercClass$PercClass)

## Merge into CTreeData1 data frame
CTreeData1 <- merge(CTreeData1, PercClass) ## Only keeps records that have both analyses
nrow(CTreeData1)
rm(Models__Perc, Model_Perc, PercClass)

## Eliminate NA from predictors
table(CTreeData1$BackgrAvTraining, useNA = "always")
CTreeData1 <- CTreeData1[!is.na(CTreeData1$BackgrAvTraining),]
table(CTreeData1$BackgrYrsOfExp, useNA = "always")
CTreeData1 <- CTreeData1[!is.na(CTreeData1$BackgrYrsOfExp),]
table(CTreeData1$BackgrDaysPerYr, useNA = "always")
CTreeData1 <- CTreeData1[!is.na(CTreeData1$BackgrDaysPerYr),]
table(CTreeData1$DemogrCountry, useNA = "always")
CTreeData1 <- CTreeData1[!is.na(CTreeData1$DemogrCountry),]
table(CTreeData1$BackgrActivity_1, useNA = "always")
CTreeData1 <- CTreeData1[!is.na(CTreeData1$BackgrActivity_1),]
nrow(CTreeData1)

## Color scale for CTree plots
ScaleCol <- RColorBrewer::brewer.pal(6, "Set1")

## Estimate CTree with activity
CTree1 <- ctree(Class ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry + PercClass + BackgrActivity_1,
               data = CTreeData1)
plot(CTree1, terminal_panel = node_barplot(CTree1, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))

## Estimate CTree without activity
CTree1 <- ctree(Class ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry + PercClass,
                data = CTreeData1)
plot(CTree1, terminal_panel = node_barplot(CTree1, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))

rm(ScaleCol)

## --> Danger rating perception does not seems to have a singificant impact on how 
##     participants use the scale. SO, let's do the Ctree analysis again without 
##     the perception class and all participants from the use question.

rm(temp)

## ---- 8) SECOND CTREE ANALYSIS TO EXAMINE RELATION TO PREDICTORS WITHOUT PERCEPTION ----

## New data frame for CTree analysis
CTreeData2 <- Data
nrow(CTreeData2)

## Test dependent variable
CTreeData2$Class <- factor(CTreeData2$Class)
(temp <- table(CTreeData2$Class, useNA = "always"))
round(100*prop.table(temp), 1)

## Eliminate NA from predictors
table(CTreeData2$BackgrAvTraining, useNA = "always")
CTreeData2 <- CTreeData2[!is.na(CTreeData2$BackgrAvTraining),]
table(CTreeData2$BackgrYrsOfExp, useNA = "always")
CTreeData2 <- CTreeData2[!is.na(CTreeData2$BackgrYrsOfExp),]
table(CTreeData2$BackgrDaysPerYr, useNA = "always")
CTreeData2 <- CTreeData2[!is.na(CTreeData2$BackgrDaysPerYr),]
table(CTreeData2$DemogrCountry, useNA = "always")
CTreeData2 <- CTreeData2[!is.na(CTreeData2$DemogrCountry),]
table(CTreeData2$BackgrActivity_1, useNA = "always")
CTreeData2 <- CTreeData2[!is.na(CTreeData1$BackgrActivity_1),]

nrow(CTreeData2)

## Color scale for CTree
ScaleCol = RColorBrewer::brewer.pal(6, "Set1")
ScaleCol <- RColorBrewer::brewer.pal(6, "Paired")

## Estimate CTree with activity
CTree2 <- ctree(Class ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry + BackgrActivity_1,
                data = CTreeData2)
plot(CTree2, terminal_panel = node_barplot(CTree2, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))

# ## Estimate CTree without activity
# CTree2 <- ctree(Class ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry,
#                data = CTreeData2)
# 
# plot(CTree2, terminal_panel = node_barplot(CTree2, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))


png(filename = "Fig_08_UseTreeCorrect_hires_template.png", width = 22.86, height = 14, units = "cm", res = 300, pointsize = 8)
plot(CTree2, terminal_panel = node_barplot(CTree2, fill = ScaleCol, col = ScaleCol, beside = F, reverse = F))
dev.off()

## -> CTree2 is included in results of paper!

rm(temp)
rm(ScaleCol)


## ---- 5) EXAMINING INDIVIDUAL NODES ----

## Calculate terminal nodes for all participants
CTreeData2$node <- predict(CTree2, type = "node")
table(CTreeData2$node)
## -> Needs to match with number in terminal nodes in plot

## Node 3 vs 4 (6, 7, 9 & 10)
Temp <- CTreeData2
Temp$NodeNew <- Temp$node
Temp <- Temp[Temp$NodeNew %in% c(3, 6, 7, 9, 10),] 
Temp$NodeNew <- factor(Temp$NodeNew, labels = c(3, 4, 4, 4, 4))
table(Temp$NodeNew, Temp$node)
table(Temp$NodeNew, Temp$Class)
round(100*prop.table(table(Temp$NodeNew, Temp$Class), 1), 1)

(test <- chisq.test(Temp$NodeNew, Temp$Class))
mosaic(test$observed, shade = T)

rm(Temp)

## Node 13 (14, 16 & 17) vs. 18
Temp <- CTreeData2
Temp$NodeNew <- Temp$node
Temp <- Temp[Temp$NodeNew %in% c(14, 16, 17, 18),] 
Temp$NodeNew <- factor(Temp$NodeNew, labels = c(13, 13, 13, 18))
table(Temp$NodeNew, Temp$node)
table(Temp$NodeNew, Temp$Class)
round(100*prop.table(table(Temp$NodeNew, Temp$Class), 1), 1)

(test <- chisq.test(Temp$NodeNew, Temp$Class))
mosaic(test$observed, shade = T)

rm(Temp)

## Node 6 vs. 7
Temp <- CTreeData2
Temp$NodeNew <- Temp$node
Temp <- Temp[Temp$NodeNew %in% c(6, 7),] 
table(Temp$NodeNew, Temp$node)
table(Temp$NodeNew, Temp$Class)
round(100*prop.table(table(Temp$NodeNew, Temp$Class), 1), 1)

(test <- chisq.test(Temp$NodeNew, Temp$Class))
mosaic(test$observed, shade = T)

rm(Temp)
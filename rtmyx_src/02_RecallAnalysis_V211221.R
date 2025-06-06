##########################################################
## Recall analysis script for danger rating analysis    ##
## December 21, 2021                                    ##
##########################################################

## ---- 1) PREPARATION ----

## Load necessary packages
require(party)       ## for CTrees
require(vcd)         ## for mosaic plots

## Clean workspace
rm(list = ls())

## ---- 2) GET DATA -----

## Load data
load("DangerRatingAnalysisData.RData")

## Select relevant table
Data <- Tbl_Recall

## Number of responses
nrow(Data)

## ---- 3) DANGER RATING SCALE TERMS ANALYSIS ----

## Type of response
(temp <- table(Data$DRLevelsRespType))
round(100*prop.table(temp), 1)

## Frequency of response patterns (clean)
temp <- as.data.frame(table(Data$DRLevelsClean))
temp <- temp[order(temp$Freq, decreasing = T),]
temp$perc <- round(100*temp$Freq/sum(temp$Freq),3)
temp
sum(temp$perc)

## Number of terms/levels provided
table(Data$DRLevelsCleanNum)
## -> There are 18 individuals who explicilty said that they
##    do not remember any levels!

## Frequency of response patterns among individual who provided four levels
temp <- as.data.frame(table(Data$DRLevelsClean[Data$DRLevelsCleanNum == 4]))
temp <- temp[order(temp$Freq, decreasing = T),]
temp$perc <- round(100*temp$Freq/sum(temp$Freq),3)
temp
sum(temp$perc)

## Recall of individual levels (not necessarily in the right spot)
## Overview
table(Data$DRLevelsLow)
table(Data$DRLevelsMod)
table(Data$DRLevelsCons)
table(Data$DRLevelsHigh)
table(Data$DRLevelsExt)

## Stacking answers in a single data frame
temp <- rbind(data.frame(id = Data$Id, level = "Low", Recalled = Data$DRLevelsLow),
              data.frame(id = Data$Id, level = "Mod", Recalled = Data$DRLevelsMod),
              data.frame(id = Data$Id, level = "Cons", Recalled = Data$DRLevelsCons),
              data.frame(id = Data$Id, level = "High", Recalled = Data$DRLevelsHigh),
              data.frame(id = Data$Id, level = "Ext", Recalled = Data$DRLevelsExt))

table(temp$level, temp$Recalled)
round(100*prop.table(table(temp$level, temp$Recalled),1), 1)

test <- chisq.test(temp$level, temp$Recalled)
mosaic(test$observed, shade = T)
rm(temp, test)

## Number of correctly recalled levels
table(Data$DRLevelsCorrCount)

## Levels in correct order
table(Data$DRLevelsInOrder)
table(Data$DRLevelsInOrder, Data$DRLevelsCorrCount)

## Fully correctly recalled scale
table(Data$DRLevelsCorrScale)
round(100*prop.table(table(Data$DRLevelsCorrScale)), 1)

## ---- 4) CTREE ANALYSIS FOR IDENTIFYING CORRECT SCALE ----

## New data frame for CTree analysis
CTreeData <- Data
nrow(CTreeData)

## Test dependent variable
CTreeData$DRLevelsCorrScale <- factor(CTreeData$DRLevelsCorrScale)
(temp <- table(CTreeData$DRLevelsCorrScale, useNA = "always"))
round(100*prop.table(temp), 1)

## Eliminate NA from predictors
table(CTreeData$BullUseType, useNA = "always")
CTreeData <- CTreeData[!is.na(CTreeData$BullUseType),]
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

## Color scale for CTree
ColScale <- c("#66bd63", "#f46d43")

## Estimate CTree with activity
CTree <- ctree(DRLevelsCorrScale ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry + BackgrActivity_1,
               data = CTreeData)

## Plot CTree
plot(CTree, terminal_panel = node_barplot(CTree, fill = ColScale))
## -> Only included in two final splits
##    1) No training & > 10 days in the backcountry each year: BC, IC, OB (209) > SM, SS (85)
##    2) Prof training & > 20 days in backcountry each year: IC (10) < BC, OB, SM, SS (384)

# ## Estimate CTree without activity
# CTree <- ctree(DRLevelsCorrScale ~ BackgrAvTraining + BackgrDaysPerYr + BackgrYrsOfExp + DemogrCountry,
#                       data = CTreeData)
# 
# ## Plot CTree
# plot(CTree, terminal_panel = node_barplot(CTree, fill = ColScale))

## HighRes plot
# png(filename = "Fig_04_RecallTreeWithActivity_hires_template.png", width = 16.5, height = 12, units = "cm", res = 300, pointsize = 8)
png(filename = "Fig_04_RecallTreeWithActivity_hires_template.png", width = 22.86, height = 14, units = "cm", res = 300, pointsize = 8)
plot(CTree, terminal_panel = node_barplot(CTree, fill = ColScale))
dev.off()

## Clean up
rm(temp)


## ---- 5) EXAMINING INDIVIDUAL NODES ----

## Calculate terminal nodes for all participants
CTreeData$node <- predict(CTree, type = "node")
table(CTreeData$node)
## -> Needs to match with number in terminal nodes in plot

## @ABBY: THE REST OF THE CODE NEEDS TO BE UPDATED!!

## Node 4
Temp <- CTreeData
Temp$NodeNew <- Temp$node
Temp <- Temp[Temp$NodeNew %in% c(4),] 
table(Temp$NodeNew, Temp$node)
table(Temp$NodeNew, Temp$DRLevelsCorrScale)
round(100*prop.table(table(Temp$NodeNew, Temp$DRLevelsCorrScale), 1), 1)
rm(Temp)

## Node 7
Temp <- CTreeData
Temp$NodeNew <- Temp$node
Temp <- Temp[Temp$NodeNew %in% c(7),] 
table(Temp$NodeNew, Temp$node)
table(Temp$NodeNew, Temp$DRLevelsCorrScale)
round(100*prop.table(table(Temp$NodeNew, Temp$DRLevelsCorrScale), 1), 1)
rm(Temp)

## Node 19
Temp <- CTreeData
Temp$NodeNew <- Temp$node
Temp <- Temp[Temp$NodeNew %in% c(19),] 
table(Temp$NodeNew, Temp$node)
table(Temp$NodeNew, Temp$DRLevelsCorrScale)
round(100*prop.table(table(Temp$NodeNew, Temp$DRLevelsCorrScale), 1), 1)
rm(Temp)


## TERMS
## @ABBY: I AM NOT SURE WHAT YOU ARE USING THIS CODE FOR!

## Danger scale terms - Terms provided for each level *1 in DRLevelsClean1 is lowest level in the response
Temp <- as.data.frame(table(Tbl$DRLevelsClean1))
Temp <- Temp[order(Temp$Freq, decreasing = T),]
Temp
rm(Temp)

Temp <- as.data.frame(table(Tbl$DRLevelsClean2))
Temp <- Temp[order(Temp$Freq, decreasing = T),]
Temp
rm(Temp)

Temp <- as.data.frame(table(Tbl$DRLevelsClean3))
Temp <- Temp[order(Temp$Freq, decreasing = T),]
Temp
rm(Temp)

Temp <- as.data.frame(table(Tbl$DRLevelsClean4))
Temp <- Temp[order(Temp$Freq, decreasing = T),]
Temp
rm(Temp)

Temp <- as.data.frame(table(Tbl$DRLevelsClean5))
Temp <- Temp[order(Temp$Freq, decreasing = T),]
Temp
rm(Temp)
#####################################################################
## Danger scale term analysis script for danger rating analysis    ##
## July 30, 2021                                                   ##
#####################################################################

## ---- 1) PREPARATION ----

## Load necessary packages
require(party)       ## for CTrees
require(tidyverse)   ## for summarizing patterns
require(tidyr)
require(vcd)         ## for mosaic plots

## Clean workspace
rm(list = ls())

## ---- 2) GET DATA -----

## Load data
load("DangerRatingAnalysisData.RData")

## Select relevant table
Data <- Tbl_Term

## Number of responses
nrow(Data)

## ---- 3) DANGER RATING SCALE TERMS ANALYSIS ----

## Extract patterns in order
Temp <- Data[, c("DROrder_L", "DROrder_M", "DROrder_C", "DROrder_H", "DROrder_E")]

## Assess status of answer
Temp$status <- "Complete"
Temp$status <- sapply(1:nrow(Temp), function(x) ifelse(sum(duplicated(c(Temp[x,]))) > 0, "Duplicates", Temp$status[x]))
Temp$status[is.na(Temp$DROrder_L) | is.na(Temp$DROrder_M) | is.na(Temp$DROrder_C) | is.na(Temp$DROrder_H) | is.na(Temp$DROrder_E)] <- "Incomplete"
table(Temp$status)
## -> 35 completed the question, 4 assigned multiple labels to the same level, 
##    and 2 did not complete the question

## What are the response patterns
Temp2 <- Temp[Temp$status == "Complete",] %>% group_by_all %>% count
Temp2 <- Temp2[order(Temp2$n, decreasing = T), ]
Temp2$Perc <- round(100*Temp2$n/sum(Temp2$n), 1)
Temp2
## -> Of the people who completed the question, 26 (74.3%) got the order right, 
##    and in the 9 other responses considerable was never at the right spot. 
##    Five participants (14.3%) reversed high and considerable,
##    and two participants (6%) reversed considerable and moderate. 
rm(Temp2)

## Ranks of each level for participants who completed the question
(Temp2 <- table(Temp$DROrder_L[Temp$status == "Complete"]))
round(100*prop.table(Temp2), 1)
(Temp2 <- table(Temp$DROrder_M[Temp$status == "Complete"]))
round(100*prop.table(Temp2), 1)
(Temp2 <- table(Temp$DROrder_C[Temp$status == "Complete"]))
round(100*prop.table(Temp2), 1)
(Temp2 <- table(Temp$DROrder_H[Temp$status == "Complete"]))
round(100*prop.table(Temp2), 1)
(Temp2 <- table(Temp$DROrder_E[Temp$status == "Complete"]))
round(100*prop.table(Temp2), 1)
## -> Shows the same pattern as the previous analysis, wich different statistics

## Clean up 
remove(Temp)
remove(Temp2)

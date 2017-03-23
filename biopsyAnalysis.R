library(sqldf)
library(plyr)

setwd("/Users/rivkahcarl/Desktop/")

biop1 <-read.csv("ProstateBiopsyData.csv", header=TRUE, stringsAsFactors = FALSE)
biop1$biopsyDate <- as.Date(biop1$biopsyDate, "%m/%d/%y")
biop1$dateOfBirth <- as.Date(biop1$dateOfBirth, "%m/%d/%y")
biop1$mriDate <- as.Date(biop1$mriDate, "%m/%d/%y")

# Need to validate data in LLM, LLB, LLA etc columns --- ASK PETER

# Rename ASAP, HGPIN to "benign" - no differentiation for this analysis
biop1$LLB <- gsub("ASAP","benign", biop1$LLB)
biop1$LLB <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$LLB)
biop1$LLB <- gsub("HGPIN","benign", biop1$LLB)
biop1$LLB <- gsub("atypical (not benign)","benign", biop1$LLB)

biop1$LLM <- gsub("ASAP","benign", biop1$LLM)
biop1$LLM <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$LLM)
biop1$LLM <- gsub("HGPIN","benign", biop1$LLM)
biop1$LLM <- gsub("atypical (not benign)","benign", biop1$LLM)

biop1$LLA <- gsub("ASAP","benign", biop1$LLA)
biop1$LLA <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$LLA)
biop1$LLA <- gsub("HGPIN","benign", biop1$LLA)
biop1$LLA <- gsub("atypical (not benign)","benign", biop1$LLA)

biop1$LMB <- gsub("ASAP","benign", biop1$LMB)
biop1$LMB <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$LMB)
biop1$LMB <- gsub("HGPIN","benign", biop1$LMB)
biop1$LMB <- gsub("atypical (not benign)","benign", biop1$LMB)

biop1$LMM <- gsub("ASAP","benign", biop1$LMM)
biop1$LMM <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$LMM)
biop1$LMM <- gsub("HGPIN","benign", biop1$LMM)
biop1$LMM <- gsub("atypical (not benign)","benign", biop1$LMM)

biop1$LMA <- gsub("ASAP","benign", biop1$LMA)
biop1$LMA <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$LMA)
biop1$LMA <- gsub("HGPIN","benign", biop1$LMA)
biop1$LMA <- gsub("atypical (not benign)","benign", biop1$LMA)

biop1$RLB <- gsub("ASAP","benign", biop1$RLB)
biop1$RLB <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$RLB)
biop1$RLB <- gsub("HGPIN","benign", biop1$RLB)
biop1$RLB <- gsub("atypical (not benign)","benign", biop1$RLB)

biop1$RLM <- gsub("ASAP","benign", biop1$RLM)
biop1$RLM <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$RLM)
biop1$RLM <- gsub("HGPIN","benign", biop1$RLM)
biop1$RLM <- gsub("atypical (not benign)","benign", biop1$RLM)

biop1$RLA <- gsub("ASAP","benign", biop1$RLA)
biop1$RLA <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$RLA)
biop1$RLA <- gsub("HGPIN","benign", biop1$RLA)
biop1$RLA <- gsub("atypical (not benign)","benign", biop1$RLA)

biop1$RMB <- gsub("ASAP","benign", biop1$RMB)
biop1$RMB <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$RMB)
biop1$RMB <- gsub("HGPIN","benign", biop1$RMB)
biop1$RMB <- gsub("atypical (not benign)","benign", biop1$RMB)

biop1$RMM <- gsub("ASAP","benign", biop1$RMM)
biop1$RMM <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$RMM)
biop1$RMM <- gsub("HGPIN","benign", biop1$RMM)
biop1$RMM <- gsub("atypical (not benign)","benign", biop1$RMM)

biop1$RMA <- gsub("ASAP","benign", biop1$RMA)
biop1$RMA <- gsub("ASAP, cannot r/o HGPIN","benign", biop1$RMA)
biop1$RMA <- gsub("HGPIN","benign", biop1$RMA)
biop1$RMA <- gsub("atypical (not benign)","benign", biop1$RMA)

biop1$psaDensity <- biop1$psa/biop1$prostateSizeCC

# Number of Targeted vs Number of Not Targeted
targetedBiop <- subset(biop1, biop1$isTargeted == '1')
nonTargetedBiop <- subset(biop1, biop1$isTargeted == '0')

targetedCount <- nrow(targetedBiop)
cat("Total Number of Patients with  Targeted Biopsies", targetedCount)

nonTargetedCount <- nrow(nonTargetedBiop)
cat("Total Number of Patients with Non Targeted Biopsies", nonTargetedCount)

# Breakout of Number of Cases per Radiologist that were Targeted Vs Non Targeted
radi1 <- biop1[, c("radiologist", "isTargeted")]
table(radi1)

xx <- aggregate(radi1, by=list(radi1$radiologist, radi1$isTargeted), FUN=length)

xx$radiologist <- NULL
xx <- rename(xx, c("Group.1"="radiologist", "isTargeted"="CountOfCases",  "Group.2"="isTargeted" ))
xx$isTargeted <- gsub("0", "nonTargeted", xx$isTargeted)
xx$isTargeted <- gsub("1", "Targeted", xx$isTargeted)
library(reshape)
yy <- as.data.frame(cast(xx, radiologist ~ isTargeted, sum))
yy$totalCases <- yy$Targeted + yy$nonTargeted
yy$prctNonTargeted <- yy$nonTargeted / yy$totalCases
yy$prctTargeted <- yy$Targeted / yy$totalCases


# List of cases where nonTargeted and >= 6
biop2 <- subset(biop1, biop1$isTargeted == '0' & 
                 ((biop1$pathologyLLALarger + biop1$pathologyLLASmaller) >= 6 |
                  (biop1$pathologyLLMLarger + biop1$pathologyLLMSmaller) >= 6 | 
                  (biop1$pathologyLLBLarger + biop1$pathologyLLBSmaller) >= 6 | 
                  (biop1$pathologyLMBLarger + biop1$pathologyLMBSmaller) >= 6 | 
                  (biop1$pathologyLMMLarger + biop1$pathologyLMMSmaller) >= 6 | 
                  (biop1$pathologyLMALarger + biop1$pathologyLMASmaller) >= 6 | 
                  (biop1$pathologyRLALarger + biop1$pathologyRLASmaller) >= 6 |
                    (biop1$pathologyRLMLarger + biop1$pathologyRLMSmaller) >= 6 | 
                    (biop1$pathologyRLBLarger + biop1$pathologyRLBSmaller) >= 6 | 
                    (biop1$pathologyRMBLarger + biop1$pathologyRMBSmaller) >= 6 | 
                    (biop1$pathologyRMMLarger + biop1$pathologyRMMSmaller) >= 6 | 
                    (biop1$pathologyRMALarger + biop1$pathologyRMASmaller) >= 6))

write.csv(biop2, "AllDataNonTargetedGreaterThan6.csv", row.names=F)


# Breakout of nonTargeted = 0 but >= by radiologist
biop2Radi <- aggregate(biop2, by=list(biop2$radiologist), FUN=length)[, c("Group.1", "msr")]
biop2Radi <- rename(biop2Radi, c("Group.1"="radiologist", "msr"="nonTargetedButGreaterThan6"))

biopFalseNonTarget <- merge(yy, biop2Radi, by=c("radiologist"))
biopFalseNonTarget$prctNonTargetedErrorAsPrctOfNonTargetedCases <- biopFalseNonTarget$nonTargetedButGreaterThan6/biopFalseNonTarget$nonTargeted
biopFalseNonTarget$prctNonTargetedErrorAsPrctOfTotalCases <- biopFalseNonTarget$nonTargetedButGreaterThan6/biopFalseNonTarget$totalCases

write.csv(biopFalseNonTarget, "PerRadiologistCasesOfNonTargetButGreaterThan6.csv", row.names = F)

biop3 <- biop2[, c("radiologist", "biopsyDate", "psa", "prostateSizeCC",
                   "pathologyLLALarger", "pathologyLLASmaller", 
                   "pathologyLLMLarger", "pathologyLLMSmaller", 
                   "pathologyLLBLarger", "pathologyLLBSmaller",
                   "pathologyLMBLarger", "pathologyLMBSmaller",
                   "pathologyLMMLarger", "pathologyLMMSmaller", 
                   "pathologyLMALarger", "pathologyLMASmaller",
                   "pathologyRLALarger", "pathologyRLASmaller",
                   "pathologyRLMLarger", "pathologyRLMSmaller", 
                   "pathologyRLBLarger", "pathologyRLBSmaller",
                   "pathologyRMALarger", "pathologyRMASmaller",
                   "pathologyRMMLarger", "pathologyRMMSmaller", 
                   "pathologyRMBLarger", "pathologyRMBSmaller"
                   )]

#xx <- sqldf("Select isTargeted
#              , radiologist
#              , count(*)
#      from radi1 
#      Group By isTargeted")


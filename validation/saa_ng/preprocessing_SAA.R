#load packages
library(plyr)
library(dplyr)
library(gtools)
library(ggplot2)
library(tidyr)

source("./validation/saa_ng/clean_validation_trialData_SAA.R")

# # reading data submissions for rice
# dataR<-read.csv("./validation/saa_ng/data/SG_dataVAL_Rice.csv")
# dataR<-unique(dataR)
# names(dataR)[grepl('\\.', names(dataR))] <- sub('.*\\.', '', names(dataR)[grepl('\\.', names(dataR))])
# 
# # removing duplicated column headers after above exercise
# names(dataR)[74]<-"betweenRowR_FarmFH"
# names(dataR)[75]<-"withinRowR_FarmFH"
# 
# dataR<-subset(dataR, select=-c(rateNote))
# 
# #removing duplicate submissions of data submissions
# source("C:/Users/ckreye/OneDrive - CGIAR/WorkSpace_R/ONA/SAA/uploadCheck/clean_validation_trialData_SAA.R")
# dataR<-as.data.frame(filterSingleSubmission(dataR, c("HHID", "event"), recent=TRUE)) 
# 
# dataR<-dataR[!dataR$username=="ckreye",] # results from dummy set
# 
# dataR<-dataR[!dataR$EAID %in% c("SGEANG000081","SGEANG000082",
#                                 "SGEANG000110", "SGEANG000111", "SGEANG000112", "SGEANG000000",
#                                 "SGEANG123456"),] # test IDs



# introduced a second data file thru the harvest training
# reading and cleaning the second data file

dataR2<-read.csv("./validation/saa_ng/data/SG_dataVAL_RiceV2.csv")
dataR2<-unique(dataR2)
names(dataR2)[grepl('\\.', names(dataR2))] <- sub('.*\\.', '', names(dataR2)[grepl('\\.', names(dataR2))])

# removing duplicated column headers after above exercise
names(dataR2)[74]<-"betweenRowR_FarmFH"
names(dataR2)[75]<-"withinRowR_FarmFH"

dataR2<-subset(dataR2, select=-c(rateNote))

#removing duplicate submissions of data submissions

dataR2<-as.data.frame(filterSingleSubmission(dataR2, c("HHID", "event"), recent=TRUE)) 

dataR2<-dataR2[!dataR2$username=="ckreye",] # results from dummy set

dataR2<-dataR2[!dataR2$EAID %in% c("SGEANG000081","SGEANG000082",
                                   "SGEANG000110", "SGEANG000111", "SGEANG000112", "SGEANG000000",
                                   "SGEANG123456"),] # test IDs

#dataRH<-smartbind(dataR,dataR2)

dataRH<-dataR2

# # reading data submissions for maize
# dataM<-read.csv("C:/Users/ckreye/Desktop/ONA/SG_dataVAL_Maize.csv")
# dataM<-unique(dataM)
# names(dataM)[grepl('\\.', names(dataM))] <- sub('.*\\.', '', names(dataM)[grepl('\\.', names(dataM))])
# 
# #removing duplicate submissions of data submissions
# source("C:/Users/ckreye/OneDrive - CGIAR/WorkSpace_R/ONA/SAA/uploadCheck/clean_validation_trialData_SAA.R")
# dataM<-as.data.frame(filterSingleSubmission(dataM, c("HHID", "event"), recent=TRUE)) 
# 
# dataM<-dataM[!dataM$EAID %in% c("SGEANG000081","SGEANG000082",
#                                 "SGEANG000110", "SGEANG000111", "SGEANG000112", "SGEANG000000",
#                                 "SGEANG123456"),] # test IDs


dataM2<-read.csv("./validation/saa_ng/data/SG_dataVAL_MaizeV2.csv")
dataM2<-unique(dataM2)
names(dataM2)[grepl('\\.', names(dataM2))] <- sub('.*\\.', '', names(dataM2)[grepl('\\.', names(dataM2))])

#removing duplicate submissions of data submissions
dataM2<-as.data.frame(filterSingleSubmission(dataM2, c("HHID", "event"), recent=TRUE)) 

dataM2<-dataM2[!dataM2$EAID %in% c("SGEANG000081","SGEANG000082",
                                   "SGEANG000110", "SGEANG000111", "SGEANG000112", "SGEANG000000",
                                   "SGEANG123456"),] # test IDs

#dataMH<-smartbind(dataM,dataM2)
dataMH<-dataM2



#~~~~

EA<-read.csv("./validation/saa_ng/data/SG_Register_EA.csv")
EA<-unique(EA)
names(EA)[grepl('\\.', names(EA))] <- sub('.*\\.', '', names(EA)[grepl('\\.', names(EA))])

# removing duplicate submissions of EA submissions
if(is.character(EA$end) | is.factor(EA$end)) 
{EA$end <- as.POSIXlt(as.character(EA$end), format="%b %d, %Y %I:%M:%S %p", tz="Africa/Lagos")}
tmp <- subset(EA, select=c("EAID", "end", "KEY"))
tmp$end <- as.numeric(julian(tmp$end))
EA <- EA[EA$KEY %in% (tmp %>% group_by_at("EAID") %>% filter(end == max(end)))$KEY,] # not sure if this captures "ID" correctly

EA<-subset(EA, select=c(EAID, state, lga, Latitude, Longitude,
                        firstNameEA, surNameEA, phoneNrEA, genderEA
))
names(EA)[names(EA)=="state"]<-"stateEA"
EA<-EA[!EA$EAID %in% c("SGEANG000081","SGEANG000082",
                       "SGEANG000110", "SGEANG000111", "SGEANG000112", "SGEANG000000",
                       "SGEANG123456"),] # test IDs
#~~~

HH<-read.csv("./validation/saa_ng/data/SG_Register_HH.csv")
HH<-unique(HH)
names(HH)[grepl('\\.', names(HH))] <- sub('.*\\.', '', names(HH)[grepl('\\.', names(HH))])

HH$today<-strptime(HH$today, format= "%b %d, %Y")

# removing duplicate submissions of HH submissions
if(is.character(HH$end) | is.factor(HH$end)) 
{HH$end <- as.POSIXlt(as.character(HH$end), format="%b %d, %Y %I:%M:%S %p", tz="Africa/Lagos")}
tmp <- subset(HH, select=c("HHID", "end", "KEY"))
tmp$end <- as.numeric(julian(tmp$end))
HH <- HH[HH$KEY %in% (tmp %>% group_by_at("HHID") %>% filter(end == max(end)))$KEY,] #  not sure if this captures "ID" correctly

HH<-subset(HH, select=c(HHID, state, EAID,
                        firstNameHH, surNameHH, phoneNrHH, today,
                        genderHH, educationHH, maritalStatusHH, HHtype,
                        occupationHH,relationHH, ageHH
))
HH<-HH[!HH$HHID %in% c("SGHHNG000348","SGHHNG000349", "SGHHNG000350","SGHHNG000000" ),]
HH<-HH[!HH$EAID %in% c("SGEANG000110","SGEANG000111", "SGEANG000112", "SGEANG000000",
                       "SGEANG123456"),]

HH$HHID<-as.character(HH$HHID)
HH$HHID[HH$HHID == "+GHHNG000467"] <- "SGHHNG000467"
HH$HHID<-as.factor(HH$HHID)

#~~~~

HHfert22<-subset(HH, select =c(HHID, state, EAID,firstNameHH, surNameHH, phoneNrHH, today))
names(HHfert22)[names(HHfert22)=="state"]<-"stateHH"
HHfert22$Year<-substr(HH$today, 1,4)
HHfert22<-HHfert22[HHfert22$Year=="2022",]

EAHH22<-merge(HHfert22,EA, all.x = TRUE)
EAHH22<-subset(EAHH22, select=-c(Latitude, Longitude, today))

EAHH22<-subset(EAHH22, select=-c(genderEA))

# merging data with HH and EA info
# rice  
dataRH<-merge(dataRH,EAHH22, all.x=TRUE)
dataRH<-dataRH[!dataRH$HHID=="SGHHNG000000",]

# maize  
dataMH<-merge(dataMH,EAHH22, all.x=TRUE)
dataMH<-dataMH[!dataMH$HHID=="SGHHNG000000",]


#Fertilizer rates - season 2


# MAIZE - NE
#~~~~~~~~~~~~~~~

NE0<-read.csv("./validation/saa_ng/data/SG_VAL_NE2.csv")
NE0<-unique(NE0)
names(NE0)[grepl('\\.', names(NE0))] <- sub('.*\\.', '', names(NE0)[grepl('\\.', names(NE0))])
NE0<-NE0[!NE0$HHID %in% c("SGHHNG000000"),]
NE0<-NE0[!NE0$EAID %in% c("SGEANG000000", "SGEANG000110", "SGEANG000111", "SGEANG000112" ),]

# removing duplicate submissions
if(is.character(NE0$end) | is.factor(NE0$end)) 
{NE0$end <- as.POSIXlt(as.character(NE0$end), format="%b %d, %Y %I:%M:%S %p", tz="Africa/Lagos")}
tmp <- subset(NE0, select=c("HHID", "end", "KEY"))
tmp$end <- as.numeric(julian(tmp$end))
NE <- NE0[NE0$KEY %in% (tmp %>% group_by_at("HHID") %>% filter(end == max(end)))$KEY,]

NEfert<-subset(NE, select=c(VAL, EAID, HHID, state, Latitude, Longitude, Altitude, Accuracy,
                            lat, lon, plantingDate, unit, sizeFarm, orgApply1, orgApply2, 
                            applyCowDung, applyPoultryManure, applyFYM, 
                            costUrea, costNPK, priceProduce,riskAtt, 
                            ridging, variety, plantingDensity, betweenRow, withinRow,
                            amountCowDung, amountPoultryManure, amountFYM, 
                            NPK, ureaTotal, plotSize,
                            ureaBRR, NPKBRR,ureaBRR_BE, NPKBRR_BE, 
                            plotNPK, plotUreaSplit1, plotUreaSplit2,
                            plotNPKBRR, plotUreaSplit1BRR, plotUreaSplit2BRR, 
                            plotNPKBRR_BE, plotUreaBRR_BE, plotUreaSplit1BRR_BE,plotUreaSplit2BRR_BE,
                            NPKApply,ureaApply2,ureaApply3,
                            call, confirmVAL))


HH22<-subset(EAHH22, select=-c(EAID))
NEFert<-merge(NEfert, HH22, all.x = TRUE)

MaizeDST<-NEFert
Maize<-dataMH

MaizeE1<-Maize[Maize$event=="event1M",]
MaizeE2<-Maize[Maize$event=="event2M",]
MaizeE3<-Maize[Maize$event=="event3M",]
MaizeE4<-Maize[Maize$event=="event4M",]
MaizeE5<-Maize[Maize$event=="event5M",]
MaizeE6<-Maize[Maize$event=="event6M",]

missMaizeE1<-MaizeDST[!MaizeDST$HHID %in% MaizeE1$HHID,]
missMaizeE2<-MaizeDST[!MaizeDST$HHID %in% MaizeE2$HHID,]
missMaizeE3<-MaizeDST[!MaizeDST$HHID %in% MaizeE3$HHID,]
missMaizeE4<-MaizeDST[!MaizeDST$HHID %in% MaizeE4$HHID,]
missMaizeE5<-MaizeDST[!MaizeDST$HHID %in% MaizeE5$HHID,]
missMaizeE6<-MaizeDST[!MaizeDST$HHID %in% MaizeE6$HHID,]

missMaizeE1$Event<-"E1"
missMaizeE2$Event<-"E2"
missMaizeE3$Event<-"E3"
missMaizeE4$Event<-"E4"
missMaizeE5$Event<-"E5"
missMaizeE6$Event<-"E6"

missMaizeE1<-missMaizeE1[!duplicated(missMaizeE1$HHID),]
missMaizeE2<-missMaizeE2[!duplicated(missMaizeE2$HHID),]
missMaizeE3<-missMaizeE3[!duplicated(missMaizeE3$HHID),]
missMaizeE4<-missMaizeE4[!duplicated(missMaizeE4$HHID),]
missMaizeE5<-missMaizeE5[!duplicated(missMaizeE5$HHID),]
missMaizeE6<-missMaizeE6[!duplicated(missMaizeE6$HHID),]

missMaize<-rbind(missMaizeE1, missMaizeE2, missMaizeE3, missMaizeE4, missMaizeE5, missMaizeE6 )

missMaizeb<-merge(missMaize, EAHH22)
missHH<-missMaize[!missMaize$HHID %in% missMaizeb$HHID,]


# RICE - RAL
#~~~~~~~~~~~~~~~~

RAL0<-read.csv("./validation/saa_ng/data/SG_VAL_RAL.csv")
RAL0<-unique(RAL0)
names(RAL0)[grepl('\\.', names(RAL0))] <- sub('.*\\.', '', names(RAL0)[grepl('\\.', names(RAL0))])
RAL0<-RAL0[!RAL0$HHID %in% c("SGHHNG000000"),]
RAL0<-RAL0[!RAL0$EAID %in% c("SGEANG000000", "SGEANG000110", "SGEANG000111", "SGEANG000112",
                             "SGEANG123456"),]

# removing duplicate submissions
if(is.character(RAL0$end) | is.factor(RAL0$end)) 
{RAL0$end <- as.POSIXlt(as.character(RAL0$end), format="%b %d, %Y %I:%M:%S %p", tz="Africa/Lagos")}
tmp <- subset(RAL0, select=c("HHID", "end", "KEY"))
tmp$end <- as.numeric(julian(tmp$end))
RAL <- RAL0[RAL0$KEY %in% (tmp %>% group_by_at("HHID") %>% filter(end == max(end)))$KEY,]

RALfert<-subset(RAL, select=c(VAL,bund, EAID, HHID, state, Latitude, Longitude, Altitude, Accuracy,
                              plantingDate, unit, sizeFarm, orgApply1, orgApply2, 
                              applyCowDung, applyPoultryManure, applyFYM,applyHHWaste,
                              riceField, strawHarvest, stubble, stubbleManage, prodSystem, 
                              season, establish, targetYield, riskAtt, 
                              costUrea, costNPK, priceProduce,
                              variety, plantingDensity, betweenRow, withinRow,
                              amountCowDung, amountPoultryManure, amountFYM, amountHHWaste_m2,
                              rateNPK, rateUrea, ureaBRR, NPKBRR, plotSize,
                              plotNPK, NPKApply,
                              plotUreaSplit1,ureaApply1,
                              plotUreaSplit2,ureaApply2,
                              plotUreaSplit3,ureaApply3,
                              plotUreaSplit4,ureaApply4,
                              plotNPKBRR, plotUreaSplit1BRR,
                              call, confirmVAL))


HH22<-subset(EAHH22, select=-c(EAID))
RALFert<-merge(RALfert, HH22, all.x = TRUE)

write.csv(RALfert,"./validation/saa_ng/data/output/Ralfert.csv")

RiceDST<-RALFert
Rice<-dataRH

RiceE1<-Rice[Rice$event=="event1R",]
RiceE2<-Rice[Rice$event=="event2R",]
RiceE3<-Rice[Rice$event=="event3R",]
RiceE4<-Rice[Rice$event=="event4R",]
RiceE5<-Rice[Rice$event=="event5R",]
RiceE6<-Rice[Rice$event=="event6R",]
RiceE7<-Rice[Rice$event=="event7R",]

missRiceE1<-RiceDST[!RiceDST$HHID %in% RiceE1$HHID,]
missRiceE2<-RiceDST[!RiceDST$HHID %in% RiceE2$HHID,]
missRiceE3<-RiceDST[!RiceDST$HHID %in% RiceE3$HHID,]
missRiceE4<-RiceDST[!RiceDST$HHID %in% RiceE4$HHID,]
missRiceE5<-RiceDST[!RiceDST$HHID %in% RiceE5$HHID,]
missRiceE6<-RiceDST[!RiceDST$HHID %in% RiceE6$HHID,]
missRiceE7<-RiceDST[!RiceDST$HHID %in% RiceE7$HHID,]

missRiceE1$Event<-"E1"
missRiceE2$Event<-"E2"
missRiceE3$Event<-"E3"
missRiceE4$Event<-"E4"
missRiceE5$Event<-"E5"
missRiceE6$Event<-"E6"
missRiceE7$Event<-"E7"

missRiceE1<-missRiceE1[!duplicated(missRiceE1$HHID),]
missRiceE2<-missRiceE2[!duplicated(missRiceE2$HHID),]
missRiceE3<-missRiceE3[!duplicated(missRiceE3$HHID),]
missRiceE4<-missRiceE4[!duplicated(missRiceE4$HHID),]
missRiceE5<-missRiceE5[!duplicated(missRiceE5$HHID),]
missRiceE6<-missRiceE6[!duplicated(missRiceE6$HHID),]
missRiceE7<-missRiceE7[!duplicated(missRiceE7$HHID),]

missRice<-rbind(missRiceE1, missRiceE2, missRiceE3, missRiceE4, 
                missRiceE5, missRiceE6, missRiceE7 )

missRiceb<-merge(missRice, EAHH22)
missHH<-missRice[!missRice$HHID %in% missRiceb$HHID,] # missing HH uploaded Jan 09


###########################################
# checking uploads for the monitoring form
###########################################


dataM0<-read.csv("./validation/saa_ng/data/SGmonitorVAL.csv")
dataM1<-read.csv("./validation/saa_ng/data/SGmonitorVAL-installCorrectDetails.csv")
dataM2<-read.csv("./validation/saa_ng/data/SGmonitorVAL-plotLayout.csv")
dataM3<-read.csv("./validation/saa_ng/data/SGmonitorVAL-trialQuality_Some.csv")
dataM4<-read.csv("./validation/saa_ng/data/SGmonitorVAL-trialRating_All.csv")
dataM5<-read.csv("./validation/saa_ng/data/SGmonitorVAL-trialRating_Some.csv")
dataM6<-read.csv("./validation/saa_ng/data/SGmonitorVAL-problemPlot_Some.csv")

dataM0<-unique(dataM0)

# add cleaning of duplicated uploads

dataM1<-unique(dataM1)
dataM2<-unique(dataM2)
dataM3<-unique(dataM3)
dataM4<-unique(dataM4)
dataM5<-unique(dataM5)
dataM6<-unique(dataM6)

names(dataM0)[names(dataM0)=="KEY"]<-"KEY1"

names(dataM1)[names(dataM1)=="PARENT_KEY"]<-"KEY1"
names(dataM1)[names(dataM1)=="KEY"]<-"KEY2"

names(dataM2)[names(dataM2)=="PARENT_KEY"]<-"KEY1"
names(dataM2)[names(dataM2)=="KEY"]<-"KEY3"

names(dataM3)[names(dataM3)=="PARENT_KEY"]<-"KEY1"
names(dataM3)[names(dataM3)=="KEY"]<-"KEY4"

names(dataM4)[names(dataM4)=="PARENTt_KEY"]<-"KEY1"
names(dataM4)[names(dataM4)=="KEY"]<-"KEY5"

names(dataM5)[names(dataM5)=="PARENT_KEY"]<-"KEY1"
names(dataM5)[names(dataM5)=="KEY"]<-"KEY6"

names(dataM6)[names(dataM6)=="PARENT_KEY"]<-"KEY6"
names(dataM6)[names(dataM6)=="KEY"]<-"KEY7"


# merger across all sets really helpful?
m1<-merge(dataM0,dataM1, all.x = TRUE)
m2<-merge(m1,dataM2, all.x = TRUE)
m3<-merge(m2,dataM3, all.x = TRUE)
m4<-merge(m3,dataM4, all.x = TRUE)
m5<-merge(m4,dataM5, all.x = TRUE)
m6<-merge(m5,dataM6, all.x = TRUE)


# merge logical pairs

Mon0<-dataM0

MonDetail<-merge(dataM0,dataM1)
MonPlot<-merge(dataM0,dataM2)
MonQualSome<-merge(dataM0,dataM3)
MonRateAll<-merge(dataM0,dataM4)
MonRateSome<-merge(dataM0,dataM5)
MonProbSome<-merge(dataM6,MonRateSome)

names(Mon0)[grepl('\\.', names(Mon0))] <- sub('.*\\.', '', names(Mon0)[grepl('\\.', names(Mon0))])

# 2022 submissions

Mon0$end <- as.POSIXlt(as.character(Mon0$end), format="%b %d, %Y", tz="Africa/Lagos")
Mon0$year<-substr(Mon0$today,8,12)
Mon0<-Mon0[Mon0$year==" 2022" | Mon0$year=="2022",]

HH<-subset(HH, select=c(HHID, state))
Mon0<-merge(Mon0,HH)

Mon0<-subset(Mon0, select=c(monEvent, firstVisit, EAID,HHID, Latitude, Longitude,
                            crop, maizeVarietySelect, maizeVarietyOther, maizeVarietyDuration,
                            maizeVariety, riceVarietySelect, riceVarietyOther, riceVarietyDuration,
                            riceVariety, cassavaVarietySelect, cassavaVarietyOther, cassavaVariety,
                            plots, nrPlots, knowledgeHH, knowledgeEA, installCorrect, installCorrectDetails_count,
                            nrPlotsCorrect, correctScore, heterogeneities, intercrops, borderEffects,
                            trialQualityIncidence_All, trialQualitySeverity_All, trialQualityScore_All,
                            trialQuality_Some_count, trialQualityScore, management,
                            embedded, bufferRows, problems_All, trialRating_All_count, problems_Some,
                            trialRating_Some_count, pest_All, pest_Some, disease, trialValid,plotLayout_count,
                            positionLandscape, slope, conservation, ridges, irrigated,waterManagement, 
                            waterManagementPlot, fallow, fertilizer, organicInput, cropGrownMonocrop, cropGrownIntercrop,
                            conservationMeasures, distanceBetweenRidges, widthRidges, heightRidges,
                            waterStructureField, heightBundsF, widthBundsF, depthDitchesF, widthDitchesF,
                            waterStructurePlot, heightBundsP, widthBundsP, depthDitchesP, widthDitchesP,
                            riceEnviron, yearsFallow, yearsSinceCropMonocrop, yearsSinceCropIntercrop, 
                            predominantIntercrops, predominantIntercrops_other,previousCrop,
                            previousCrop_other, previousOtherCrops, previousOtherCrops_other,fertilizerApplied,
                            fertilizerApplied_other,localSoilName,localSoilNameTranslated,soilOtherIssue, textSoilOtherIssueDetails,
                            distance,  fertilityFarmer, fertilityResearcher, drainage , soilDepth,
                            state))

MonC<-Mon0[Mon0$crop=="cassava",]
MonM<-Mon0[Mon0$crop=="maize",]
MonR<-Mon0[Mon0$crop=="rice",]

# plot Details, working with MonPlot
names(MonPlot)[grepl('\\.', names(MonPlot))] <- sub('.*\\.', '', names(MonPlot)[grepl('\\.', names(MonPlot))])

HH<-subset(HH, select=c(HHID, state))
MonPlot<-merge(MonPlot,HH)

MonPlotM<-MonPlot[MonPlot$crop=="maize",]
#write.csv(MonPlotM, "MonPlotM.csv", row.names = FALSE)

MonPlotR<-MonPlot[MonPlot$crop=="rice",]
#write.csv(MonPlotR, "MonPlotR.csv", row.names = FALSE)



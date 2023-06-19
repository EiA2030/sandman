
# 2022 validations QA for maize 
# uses dataM created under SAA/upload Check
# uses results of the monitoring form - SAA/upload Check
# uses monitoring report information from the team (word file under: C:\Users\ckreye\OneDrive - CGIAR\EiA\SAAuseCase\SAA_2022\Monitoring )


library(ggplot2)
library(tidyr)

source("./validation/saa_ng/preprocessing_SAA.R")

#MonR
MonRinvalid<-MonR[MonR$trialValid=="no",]

# creating data set of HHIDs that were manually identified as not suitable
# (may overlap with monitoring form)
HHID<-c("SGHHNG000509","SGHHNG000451," )
valid<-c("no", "no")
excludeHH<-data.frame(HHID,valid)

dataR<-Rice
dataR<-dataR[!dataR$HHID %in% MonRinvalid$HHID,]
dataR<-dataR[!dataR$HHID %in% excludeHH$HHID,]

# checking data set at harvest

dataH<-dataR[dataR$event=="event6R",]

dH<-subset(dataH, select = c(EAID, HHID,
                                variety_bluePlot,
                                intercrop_bluePlot,
                                variety_yellowPlot,            
                                intercrop_YellowPlot,          
                                variety_redPlot,             
                                intercrop_redPlot,
                                plantingDensity_bluePlot,
                                betweenRowR_bluePlot,
                                withinRowR_bluePlot,
                                plantingDensity_yellowPlot,
                                betweenRowR_yellowPlot,        
                                withinRowR_yellowPlot,
                                plantingDensity_redPlot,
                                plotL1_BRR, plotL2_BRR, 
                                plotW1_BRR, plotW2_BRR,
                                plotL1_SSR, plotL2_SSR, 
                                plotW1_SSR, plotW2_SSR,
                                plotL1_ZCC, plotL2_ZCC, 
                                plotW1_ZCC, plotW2_ZCC,
                             riceGrainMoisture_plot1_1_FFP,
                             riceGrainMoisture_plot1_2_FFP,
                             riceGrainMoisture_plot1_3_FFP,
                             riceGrainMoisture_plot2_1_FFP,
                             riceGrainMoisture_plot2_2_FFP,
                             riceGrainMoisture_plot2_3_FFP,
                             riceGrainMoisture_plot3_1_FFP,
                             riceGrainMoisture_plot3_2_FFP,
                             riceGrainMoisture_plot3_3_FFP,
                             riceMoisture_plot1_FFP,
                             riceMoisture_plot2_FFP,
                             riceMoisture_plot3_FFP,
                             plantNumber_plot1_FFP,
                             plantNumber_plot2_FFP,
                             plantNumber_plot3_FFP,
                             plotL1_FFP1,                  
                             plotW1_FFP1,                  
                             plotL2_FFP1,                  
                             plotW2_FFP1,                 
                             diagonale1,
                             plotL1_FFP2,                  
                             plotW1_FFP2,                  
                             plotL2_FFP2,                  
                             plotW2_FFP2,                 
                             diagonale2,
                             plotL1_FFP3,                  
                             plotW1_FFP3,                  
                             plotL2_FFP3,                  
                             plotW2_FFP3,                 
                             diagonale3
                          ))

dH$sameVariety<-ifelse(dH$variety_bluePlot==dH$variety_yellowPlot &
                         dH$variety_redPlot== dH$variety_yellowPlot, "YES", "NO")

#table(dH$sameVariety)
dDiffV<-dH[dH$sameVariety=="NO",]
# HHID 488: other local variety in the red plot

dH$sameDensity<-ifelse(dH$plantingDensity_bluePlot ==dH$plantingDensity_yellowPlot &
                         dH$plantingDensity_redPlot== dH$plantingDensity_yellowPlot, "YES", "NO")

#table(dH$sameDensity)
dDiffDen<-dH[dH$sameDensity=="NO",]
# different planting density in red plot

dH$IC<-ifelse(dH$intercrop_bluePlot=="yes"| 
                dH$intercrop_YellowPlot=="yes" | 
                dH$intercrop_redPlot=="yes","YES","NO")

#table(dH$IC)
dIC<-dH[dH$IC=="YES",] # HH384: intercrop in red plot


dH$plotSizeBRR<-((dH$plotL1_BRR + dH$plotL2_BRR)/2) * 
  ((dH$plotW1_BRR + dH$plotW2_BRR)/2)

dH$plotSizeSSR<-((dH$plotL1_SSR + dH$plotL2_SSR)/2) * 
  ((dH$plotW1_SSR + dH$plotW2_SSR)/2)

dH$plotSizeZCC<-((dH$plotL1_ZCC + dH$plotL2_ZCC)/2) * 
  ((dH$plotW1_ZCC + dH$plotW2_ZCC)/2)

melt1<-gather(dH, plot, size, plotSizeBRR:plotSizeZCC)
melt1$plot<-revalue(melt1$plot, c(plotSizeSSR="SSR",
                                  plotSizeBRR="BRR",
                                  plotSizeZCC="ZCC")) 
# ggplot(melt1, aes(plot, size))+
#   geom_boxplot()

dH$samePlotSize<-ifelse(dH$plotSizeBRR==dH$plotSizeSSR, "YES", "NO")

#table(dH$samePlotSize)
dDiffPlot<-dH[dH$samePlotSize=="NO",]

dH$halfPlot<-ifelse((dH$plotSizeSSR/2)==dH$plotSizeZCC, "YES", "NO")

#table(dH$halfPlot)
dDiffHalf<-dH[dH$halfPlot=="NO",]

# => calculate plot size for yield estimation

# create cleaned file for maize to be read into yield and revenue
# assume that all ZCC plots have dim around 5m width and 10 m lenght
# => correct all plot width 1 and 2 to 5 m if listed with 10m

dataR$plotW1_ZCC<-ifelse(dataR$event=="event6R" & dataR$plotW1_ZCC==10,5,dataR$plotW1_ZCC)
dataR$plotW2_ZCC<-ifelse(dataR$event=="event6R" & dataR$plotW2_ZCC==10,5,dataR$plotW1_ZCC)

# exclude the non valid trials as already done at the 
# top of the script; continue with dataR
# for further cleaning, remove the invalid HHID that are identified 
# in dH from dataR

# => issues of the red plot; HHIDs collected here,
# read into yield and revenue to set grain yield in the red plot to NA
dRed<-dH[dH$HHID %in% c(dDiffV$HHID, dDiffDen$HHID, dIC$HHID),]

write.csv(dRed, "./validation/saa_ng/data/output/dRed.csv", row.names = FALSE)

dHClean<-dataR
write.csv(dHClean,"./validation/saa_ng/data/output/riceClean.csv", row.names = FALSE)


##############################################################
# Looking at grain moisture content
##############################################################

dGH<-dataR[dataR$event=="event6R",]

dGH<-subset(dGH, select=c(HHID,
                          riceGrainMoisture1_SSR,  
                          riceGrainMoisture2_SSR,
                          riceGrainMoisture3_SSR,
                          riceGrainMoisture1_BRR,  
                          riceGrainMoisture2_BRR,
                          riceGrainMoisture3_BRR,
                          riceGrainMoisture1_ZCC,  
                          riceGrainMoisture2_ZCC,
                          riceGrainMoisture3_ZCC,
                          riceMoisture_ZCC,
                          riceMoisture_BRR,
                          riceMoisture_SSR))


  
meltGH<-gather(dGH, plot, moist, riceGrainMoisture1_SSR:riceGrainMoisture3_ZCC)

meltGH$plot<-revalue(meltGH$plot, c( riceGrainMoisture1_SSR="SSR",  
                                     riceGrainMoisture2_SSR="SSR",
                                     riceGrainMoisture3_SSR="SSR",
                                     riceGrainMoisture1_BRR="BRR",  
                                     riceGrainMoisture2_BRR="BRR",
                                     riceGrainMoisture3_BRR="BRR",
                                     riceGrainMoisture1_ZCC="ZCC",  
                                     riceGrainMoisture2_ZCC="ZCC",
                                     riceGrainMoisture3_ZCC="ZCC")) 

# ggplot(meltGH, aes(HHID, moist, colour = plot))+
#   geom_point()
# 
# ggplot(meltGH[meltGH$moist>0,], aes(HHID, moist, colour = plot))+
#   geom_boxplot()


meltGH<-aggregate(meltGH$moist, by =list (meltGH$HHID,
                                          meltGH$plot,
                                          meltGH$riceMoisture_ZCC,
                                          meltGH$riceMoisture_BRR,
                                          meltGH$riceMoisture_SSR),
                  FUN=mean)

names(meltGH)[names(meltGH)=="Group.1"]<-"HHID"
names(meltGH)[names(meltGH)=="Group.2"]<-"plot"
names(meltGH)[names(meltGH)=="x"]<-"moisture"
names(meltGH)[names(meltGH)=="Group.3"]<-"moistZCC"
names(meltGH)[names(meltGH)=="Group.4"]<-"moistBRR"
names(meltGH)[names(meltGH)=="Group.5"]<-"moistSSR"

spreadGH<-meltGH %>% spread (plot, moisture)

m2GH<-gather(spreadGH,plot, moisture, BRR:SSR)

# ggplot(m2GH[m2GH$ZCC>0,], aes(ZCC, moisture, colour=plot))+
#   geom_point()
# 
# ggplot(spreadGH[spreadGH$moistBRR>0 & spreadGH$moistZCC>0,], 
#        aes(BRR, SSR))+
#   geom_point()+
#   geom_point(aes (moistBRR, moistSSR), colour="red")+
#   geom_point(aes (moistZCC, moistSSR), colour="green")+
#   geom_point(aes (moistZCC, moistBRR), colour="blue")+
#   #geom_smooth()+
#   stat_smooth(method="lm", colour="grey")

spreadGH$SSR_BRR<-spreadGH$SSR-spreadGH$BRR
spreadGH$SSR_ZCC<-spreadGH$SSR-spreadGH$ZCC
spreadGH$BRR_ZCC<-spreadGH$BRR-spreadGH$ZCC

# ggplot(spreadGH[spreadGH$ZCC>0,], aes(BRR, SSR_BRR))+
#   geom_point()+
#   geom_point(aes (ZCC, SSR_ZCC), colour="green")+
#   geom_point(aes (ZCC, BRR_ZCC), colour="blue")+
#   geom_abline(slope=0,intercept=0)
# 
# ggplot(meltGH[meltGH$moisture>0,], aes(plot,moisture))+
#   geom_boxplot()

# write data set to combine with Yield and Revenue scripts
# ds spreadGH; rename columns to avoid duplication of names
names(spreadGH)[names(spreadGH)=="BRR"]<-"moistureBRR"
names(spreadGH)[names(spreadGH)=="SSR"]<-"moistureSSR"
names(spreadGH)[names(spreadGH)=="ZCC"]<-"moistureZCC"

write.csv(spreadGH,"./validation/saa_ng/data/output/grainMoistureR.csv")

############################################
# plant numbers
############################################

# dataE5<-dataR[dataR$event=="event5R",]
# 
# d5<-subset(dataE5, select=c(HHID,
#                             nrPlants_BRR_row1,
#                             nrPlants_BRR_row2,
#                             nrPlants_BRR_row3,
#                             nrPlants_SSR_row1,
#                             nrPlants_SSR_row2,
#                             nrPlants_SSR_row3,
#                             nrPlants_ZCC_row1,
#                             nrPlants_ZCC_row2,
#                             nrPlants_ZCC_row3))
# 
# meltd5<-gather(d5, plot, plantNr, nrPlants_BRR_row1:nrPlants_ZCC_row3)
# meltd5$plot<-revalue(meltd5$plot, c(nrPlants_BRR_row1="BRR",
#                                 nrPlants_BRR_row2="BRR",
#                                 nrPlants_BRR_row3="BRR",
#                                 nrPlants_SSR_row1="SSR",
#                                 nrPlants_SSR_row2="SSR",
#                                 nrPlants_SSR_row3="SSR",
#                                 nrPlants_ZCC_row1="ZCC",
#                                 nrPlants_ZCC_row2="ZCC",
#                                 nrPlants_ZCC_row3="ZCC"))
# 
# # ggplot(meltd5, aes(HHID, plantNr, colour = plot))+
# #   geom_point()
# 
# # => consider removing HHIDs with very low plantNr; e.g <10...
# # or < 25 plants...
# # see further down rows ~ 265
# 
# # ggplot(meltd5[meltd5$plantNr>0,], aes(plot, plantNr))+
# #   geom_boxplot()+
# #   geom_abline(slope = 0, intercept = 50, colour="green")+
# #   geom_abline(slope = 0, intercept = 25, colour="red")
# 
# meltd5<-aggregate(meltd5$plantNr, by =list (meltd5$HHID,
#                                           meltd5$plot
#                                           ),
#                   FUN=mean)
# 
# names(meltd5)[names(meltd5)=="Group.1"]<-"HHID"
# names(meltd5)[names(meltd5)=="Group.2"]<-"plot"
# names(meltd5)[names(meltd5)=="x"]<-"plantNr"
# 
# spread5<-meltd5 %>% spread (plot, plantNr)
# 
# # picking this up:
# # => consider removing HHIDs with very low plantNr; e.g <10...
# # or < 25 plants...
# # identify HHIDs where SSR is < 10 plants
# 
# lowPl<-spread5[spread5$SSR<10,]
# 
# # checking if these HHs have been monitored
# lowPlmon<-MonR[MonR$HHID %in% lowPl$HHID,]
# # => HHID (404) yes and rated as valid
# 
# # checking affliations of HHIDS
# EAHH<-EAHH22
# lowPl<-merge(lowPl, EAHH)
# # => sent email to Kamal and Bello to check
# 
# # checking if low plant numbers equal minimum yield => does not seem to be the case
# lowPli<-dataR[dataR$HHID %in% lowPl$HHID,]
# summary(dataR$riceGrainFW_plot_SSR)
# summary(lowPli$riceGrainFW_plot_SSR)
# summary(lowPli$riceGrainFW_plot_BRR)
# summary(dataR$riceGrainFW_plot_BRR)


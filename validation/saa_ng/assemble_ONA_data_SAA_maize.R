
# 2022 validations QA for maize 
# uses dataM created under SAA/upload Check
# uses results of the monitoring form - SAA/upload Check
# uses monitoring report information from preprocessing_SAA.R

library(ggplot2)
library(tidyr)

source("./validation/saa_ng/preprocessing_SAA.R")


MonMinvalid<-MonM[MonM$trialValid=="no",]

# reading plot details from monitoring
#MonPlotM

# creating data set of HHIDs that were manually identified as not suitable
# (may overlap with monitoring form)
HHID<-c("SGHHNG000412","SGHHNG000445" )
valid<-c("no", "no")
excludeHH<-data.frame(HHID,valid)

dataM<-Maize
dataM<-dataM[!dataM$HHID %in% MonMinvalid$HHID,]
dataM<-dataM[!dataM$HHID %in% excludeHH$HHID,]

# checking data set at harvest

dataH<-dataM[dataM$event=="event5M",]

dH<-subset(dataH, select = c(EAID, HHID,
                                variety_BluePlot,
                                plantingDensity_BluePlot,
                                intercrop_BluePlot,
                                betweenRowM_BluePlot,
                                withinRowM_BluePlot,
                                variety_yellowPlot,            
                                plantingDensity_yellowPlot,   
                                intercrop_YellowPlot,          
                                betweenRowM_yellowPlot,        
                                withinRowM_yellowPlot,        
                                variety_redPlot,             
                                plantingDensity_redPlot,      
                                intercrop_redPlot,
                                nrPlants_BRR, nrPlants_SSR, nrPlants_ZCC,
                                plotL1_BRR, plotL2_BRR, 
                                plotW1_BRR, plotW2_BRR,
                                plotL1_SSR, plotL2_SSR, 
                                plotW1_SSR, plotW2_SSR,
                                plotL1_ZCC, plotL2_ZCC, 
                                plotW1_ZCC, plotW2_ZCC
                                ))

dH$sameVariety<-ifelse(dH$variety_BluePlot==dH$variety_yellowPlot &
                         dH$variety_redPlot== dH$variety_yellowPlot, "YES", "NO")

#table(dH$sameVariety)


dH$sameDensity<-ifelse(dH$plantingDensity_BluePlot ==dH$plantingDensity_yellowPlot &
                         dH$plantingDensity_redPlot== dH$plantingDensity_yellowPlot, "YES", "NO")

#table(dH$sameDensity)
 
dH$IC<-ifelse(dH$intercrop_BluePlot=="yes"| 
                dH$intercrop_YellowPlot=="yes" | 
                dH$intercrop_redPlot=="yes","YES","NO")

#table(dH$IC)

# plot size: averages of L and W 1 and 2 not reliable.
# too often just the exact 10 m recorded - lokely copy paste from protocol
# sometimes wrong entry for L or W 1 and corrected for L or W 2
# discussion with Bello, Kamal and Helen:
# In harvest training, we agreed that we plant 13 rows in BRR and SSR and 6 rows in ZCC
# during the monitoring the team measured from plant to plant
# What apparently happened is that the farmers ridged without the EAs present 
# and distributed 13 rows across 10 m and 6 rows across 5 m
# in Kaduna and Kano, ZCC has 13 rows, but these rows are only 5 m long
# => different approach for plot area calculation
# farms that were monitored, use the plot measurements by the monitoring team
# devide by 13 for BRR and SSR plots to get average inter row distance
# add average inter row distance to measured lengths. 
# for plot length, use measured length plus default inter plant spacing
# for the ZCC plot in Kaduna and Kano, use the 13 rows for the plot width
# for the ZCC plot in Beneue use the 6 rows for the plot width

# subsetting the plot dimensions from MonM
plotDim<-subset(MonPlotM, select=c(HHID, state, plotDimPlot, 
                               plotL1, plotL2, plotW1, plotW2))

# rename the plot column
names(plotDim)[names(plotDim)=="plotDimPlot"]<-"plot"

plotDim<-plotDim[plotDim$plot %in% c("BRR", "SSR", "ZCC"),]

# add identifier of monitored HH
plotDim$mon<-"M"

plotDim$avW<-(plotDim$plotW1+plotDim$plotW2)/2
plotDim$avL<-(plotDim$plotL1+plotDim$plotL2)/2

plotDim$interRow<-ifelse(plotDim$state=="benue" & plotDim$plot=="ZCC",
                         plotDim$avW/6,
                         plotDim$avW/13)

# adding intra and inter row spaces to plot length and width
# for length: 0.25 as default plant to plant distance
# for width as calculated
plotDim$avLc<-plotDim$avL+0.25
plotDim$avWc<-plotDim$avW + plotDim$interRow

plotDim$plotSize<-plotDim$avLc*plotDim$avWc

plotDim<-subset(plotDim, select = c(HHID, mon, state, plot, plotSize))

plotDimS<-plotDim %>% spread (plot,plotSize)

names(plotDimS)[names(plotDimS)=="BRR"]<-"plotSizeBRR"
names(plotDimS)[names(plotDimS)=="SSR"]<-"plotSizeSSR"
names(plotDimS)[names(plotDimS)=="ZCC"]<-"plotSizeZCC"


write.csv(plotDimS,"./validation/saa_ng/data/output/plotDimS.csv", row.names = FALSE)


# Plant numbers
melt2<-gather(dH, plot, plantNr, nrPlants_BRR :nrPlants_ZCC)
melt2$plot<-revalue(melt2$plot, c(nrPlants_SSR="SSR",
                                  nrPlants_BRR="BRR",
                                  nrPlants_ZCC="ZCC")) 

melt2<-subset(melt2, select=c(HHID, plot, plantNr))

# merging melt2 - plant Nr with plotDim - plotSize
# only plots that were monitored

melt2<-merge(plotDim, melt2)

# target numberof plants
# blue and yellow plots: planted at 0.25 X 0.75 m on 10 x 10 m plot: 533 plants per plot
# => 5.3 plants/m2
# red plot: planted at 0.25 X 0.75 m on 10 x 5 m plot: 266 plants per plot
# => 5.3 plants/m2

melt2$plantNr_m2<-melt2$plantNr/melt2$plotSize

melt2$plNrRow<-ifelse(!melt2$state=="benue", melt2$plantNr/13,
                      ifelse(melt2$plot=="ZCC" & melt2$state=="benue", melt2$plantNr/6, 
                             melt2$plantNr/13))

# ggplot(melt2, aes(plot, plantNr_m2))+
#   geom_boxplot()+
#   geom_abline(slope=0, intercept = 5.33, colour="green")+
#   geom_abline(slope=0, intercept = 2.6, colour="red")
# 
# ggplot(melt2, aes(state, plNrRow,fill=plot))+
#   geom_boxplot()+
#   geom_abline(slope=0, intercept = 20, colour="green")+
#   geom_abline(slope=0, intercept = 10, colour="blue")
#   

# plant numbers in all farms (also those not monitored)

dH$diffPlNR<-dH$nrPlants_BRR - dH$nrPlants_SSR

dH$plNrDiff<-ifelse(dH$diffPlNR>40 | dH$diffPlNR<(-40),"YES","NO" )

#table(dH$plNrDiff)

dH$avRowBRR<-dH$nrPlants_BRR/13
dH$avRowSSR<-dH$nrPlants_SSR/13

dH$rowDiff<-dH$avRowBRR - dH$avRowSSR

dH$rowDiff_low<-ifelse(dH$rowDiff>dH$avRowBRR |
                     dH$rowDiff<(-1) * dH$avRowSSR, "NO", "YES") 
                     
# table(dH$rowDiff_low)  
# summary(dH$rowDiff)

# => fertilizer and plant numbers may interact, 
# => no cleaning based on low plant numbers in the red plot
# => consider cleaning on the basis of low plant numbers in the 
# yellow and blue plots
excludeLowPlNR<-as.data.frame(dH[dH$nrPlants_BRR/dH$plotSizeBRR<2.6 & 
     dH$nrPlants_SSR/dH$plotSizeSSR<2.6,])

# create cleaned file for maize to be read into yield and revenue

dHClean<-dataM
# for now only exclude the non valid trials as already done at the 
# top of the script
# for further cleaning, remove the invalid HHID that are identified 
# in dH from dataM

write.csv(dHClean,"./validation/saa_ng/data/output/maizeClean.csv", row.names = FALSE)


##############################################################
# Looking at grain moisture content
##############################################################

dGH<-dataM[dataM$event=="event5M",]

dGH<-subset(dGH, select=c(HHID,
                          maizeGrainMoisture1_plot_SSR,  
                          maizeGrainMoisture2_plot_SSR,
                          maizeGrainMoisture3_plot_SSR,
                          maizeGrainMoisture1_plot_BRR,  
                          maizeGrainMoisture2_plot_BRR,
                          maizeGrainMoisture3_plot_BRR,
                          maizeGrainMoisture1_plot_ZCC,  
                          maizeGrainMoisture2_plot_ZCC,
                          maizeGrainMoisture3_plot_ZCC,
                          maizeMoisture_ZCC,
                          maizeMoisture_BRR,
                          maizeMoisture_SSR))


  
meltGH<-gather(dGH, plot, moist, maizeGrainMoisture1_plot_SSR:maizeGrainMoisture3_plot_ZCC)

meltGH$plot<-revalue(meltGH$plot, c( maizeGrainMoisture1_plot_SSR="SSR",  
                                     maizeGrainMoisture2_plot_SSR="SSR",
                                     maizeGrainMoisture3_plot_SSR="SSR",
                                     maizeGrainMoisture1_plot_BRR="BRR",  
                                     maizeGrainMoisture2_plot_BRR="BRR",
                                     maizeGrainMoisture3_plot_BRR="BRR",
                                     maizeGrainMoisture1_plot_ZCC="ZCC",  
                                     maizeGrainMoisture2_plot_ZCC="ZCC",
                                     maizeGrainMoisture3_plot_ZCC="ZCC")) 

# ggplot(meltGH, aes(HHID, moist, colour = plot))+
#   geom_point()
# 
# ggplot(meltGH[meltGH$moist>0,], aes(HHID, moist, colour = plot))+
#   geom_boxplot()


meltGH<-aggregate(meltGH$moist, by =list (meltGH$HHID,
                                          meltGH$plot,
                                          meltGH$maizeMoisture_ZCC,
                                          meltGH$maizeMoisture_BRR,
                                          meltGH$maizeMoisture_SSR),
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

write.csv(spreadGH,"./validation/saa_ng/data/output/grainMoistureM.csv")

##################################
# overview of scoring data
##################################
score<-dataM[dataM$event=="event4M" | dataM$event=="event5M",]
score$end<-as.POSIXct(score$end)
scoreM<-gather(score, category, score, rateDrought:rateTheftMissing)

# ggplot(scoreM, aes(category, score, fill=category))+
#   geom_boxplot()+
#   facet_wrap(~event)+
#   theme_bw()+
#   #theme(plot.title = element_text(size = 20, face = "bold"))+
#   theme(axis.title = element_text(size=14, face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.text = element_text(),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size=14),
#         strip.text = element_text(size=14))+
#   theme(legend.position = "none")
#   
#  
# # correlation overview
# ggplot(scoreM[scoreM$event=="event5M",], aes(score, maizeGrainFW_plot_BRR, colour=category))+
#   geom_point()+
#   facet_wrap(~category)+
#   theme_bw()+
#   #theme(plot.title = element_text(size = 20, face = "bold"))+
#   theme(axis.title = element_text(size=14, face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.text = element_text(),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size=14),
#         strip.text = element_text(size=14))+
#   theme(legend.position = "none")

scoreLow<-scoreM[scoreM$score<4,]
#write.csv(scoreLow,"scoreLow.csv", row.names = FALSE)

scoreHigh<-scoreM[scoreM$score>3,]
scoreHighU<-scoreHigh[!duplicated(scoreHigh$HHID),]
write.csv(scoreHighU,"./validation/saa_ng/data/output/scoreHigh.csv", row.names = FALSE)

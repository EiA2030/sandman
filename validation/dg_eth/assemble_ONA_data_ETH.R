
#######################################
# 1. Get the potato data from SAnDMan #
#######################################

#downloading the data
wd <- "/home/jovyan/TRANSFORM/eia2030/sandman/validation/dg_eth/"
setwd(wd)
creds <- scan(paste0(wd, "../../pwd.txt"), what = "character")
user <- creds[1]
pw   <- creds[2]

# crop <- "Cassava"

#get the list of all datasets of user...
dss <- findONAdatasets(user = user, pw = pw)

forms <- c("fieldData_wheat_DG",
           "dataVAL_wheat_DG_V2",
           "dataVAL_wheat_DG",
           "AgroadvisoryDG",
           "Register_HH_DG")
forms <- dss[dss$id_string %in% forms,]

#download and decompose the assign field/trial/plot data:
data <- list()
for(i in forms$id_string){
  ds <- getONAdata(user = user, pw = pw, id = forms[forms$id_string == i,]$id)
  ds <- decomposeONAdata(ds)
  data[[i]] <- ds
}



d <- data.frame("biomass_cntrl" = as.numeric(data$fieldData_wheat_DG[[1]]$grainFW_CON),
                "biomass_loc" = as.numeric(data$fieldData_wheat_DG[[1]]$grainFW_Local),
                "biomass_ssr" = as.numeric(data$fieldData_wheat_DG[[1]]$grainFW_SSR))
d$c_s <- as.numeric(data$fieldData_wheat_DG[[1]]$grainFW_SSR)-as.numeric(data$fieldData_wheat_DG[[1]]$grainFW_CON)
d$l_s <- as.numeric(data$fieldData_wheat_DG[[1]]$grainFW_SSR)-as.numeric(data$fieldData_wheat_DG[[1]]$grainFW_Local)
d <- d[complete.cases(d),]
plot(ecdf(d$c_s))
plot(ecdf(d$l_s))

ecdf(d$l_s)



# tmp<-(arrange(d[!is.na(d$biomass_ssr),], stateEA, eSSR))
# tmp.ecdf1<-ddply(tmp,.(stateEA), transform, ecdf=ecdf(eSSR)(eSSR))
# tmp.ecdf2<-(ddply(tmp,.(stateEA), transform,lower=ecdf.ks.CI(eSSR)$lower))
# tmp.ecdf3<-ddply(tmp,.(stateEA), transform, upper=ecdf.ks.CI(eSSR)$upper)
# 
# tmp.ecdfi<-merge(tmp.ecdf1, tmp.ecdf2)
# tmp.ecdf.pp1<-merge(tmp.ecdfi, tmp.ecdf3)
# 
# 
# p<-ggplot(tmp.ecdf.pp1, aes(eSSR, ecdf, colour=riceSystem))
# p+geom_point(size=3)+
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper,
#   ),
#   alpha=.2)+
#   geom_vline(xintercept = 0)+
#   ggtitle("SSR over BRR")+
#   xlab("Difference in fresh grain weight (t/ha)") +
#   ylab("cumulative probability") +
#   theme_bw()+
#   facet_wrap(~stateEA)+
#   #facet_grid(~riceSystem)+
#   #xlim(-2.5,2.5)+ # attention at least 2 outliers!!! - removed
#   theme(plot.title = element_text(size = 20, face = "bold"))+
#   theme(axis.title = element_text(size=20, face="bold"),
#         axis.text = element_text(size=18),
#         legend.title = element_text(size=18, face="bold"),
#         legend.text = element_text(size=18),
#         strip.text = element_text(size=18))

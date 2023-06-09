
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
`dss <- findONAdatasets(user = user, pw = pw)

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

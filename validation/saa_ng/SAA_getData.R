       

# lost script Aug 8 2022; partial reconstruction

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if I need to install odkr again:

# require(devtools)
# devtools::install_github("validmeasures/odkr") # Rtools required
# library (odkr)
# 
get_briefcase(destination = "./validation/saa_ng/data")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
creds <- scan(("./pwd.txt"), what = "character")
user <- creds[1]
pw   <- creds[2]

#https://odk.ona.io/iita_nrm # alternative url try in case of issues


require(odkr)

# forms
forms<-c("SG_Register_EA","SG_Register_HH","SGmonitorVAL","FIP_Survey_2020","SG_dataVAL_MaizeV2","SG_dataVAL_RiceV2",
         "SG_VAL_NE2","SG_dataVAL_FIP","SG_VAL_RAL","SG_dataVAL_Maize","SG_dataVAL_Rice")


for (form in forms) {
  pull_remote(target = "./validation/saa_ng/data",
              id = form,
              to = "./validation/saa_ng/data",
              from = "https://odk.ona.io/iita_nrm",
              username = user,
              password = pw)
  
  
  export_data(target = "./validation/saa_ng/data",
              id = form,
              from = "./validation/saa_ng/data",
              to = "./validation/saa_ng/data",
              filename = form)
}



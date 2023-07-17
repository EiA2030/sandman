
rm(list=ls())
setwd("D:/IITA")
wd<-"D:/IITA"

# Load required libraries
library(httr)
library(jsonlite)
library(tidyr)
library(purrr)
library(dplyr)
library(readr)
library(stringr)

creds <- scan(paste0(wd, "/pws.txt"), what = "character")
username <- creds[1]
password   <- creds[2]


# a function to read in data from ONA
getONAdatasets <- function(username, password,form_id){
  
  require(httr)
  # Create the authentication token
  url <- "https://api.ona.io/api/v1"
  token <- paste0(username, ":", password)
  auth_token <- base64enc::base64encode(charToRaw(token))
  # Make the API request to retrieve data
  response <- httr::GET(
    url = paste0(url, "/data/", form_id),
    httr::add_headers(Authorization = paste0("Basic ", auth_token)),
    query = list(format = "json")
  )
  # Parse the JSON response into a data frame with the nesting in list
  r <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
  
  return(r)
  
}
# Function to unnest the data

unnest_variable <- function(data, unnest_col) {
  data_unnested <- data.frame()
  # unnest the first level contained in plot variable
  for(i in 1:nrow(data)){
    #print(i)
    df <- data %>%
      slice(i) %>%
      unnest(unnest_col, keep_empty = TRUE, names_sep = "_")
    
    df$row_id = i
    
    df <- df %>%
      select(row_id, everything()) 
    
    # add unnested columns to the original dataset
    data_unnested <- plyr::rbind.fill(data_unnested, df)
  }
  return(data_unnested)
}


# reading and cleaning the data

#1. register/verify data
Register<- getONAdatasets(username = username, password = password, form_id = "527985")

Register<- select(Register,ENID,country,HHID,projectCode,grep("detailsHH.*|.*geopoint.*|.*coopassoc.*",names(Register),value = T))

Register<- select(Register,-grep("detailsHH_qd.*",names(Register),value = T))

#2.describe household
d_HH<-getONAdatasets(username = username, password = password, form_id = "632119")

HH_unnested <- unnest_variable(d_HH, "survey/cropGroup/cropRepeat")

HH_unnested1=unnest_variable(HH_unnested, "survey/populationGroup/popRepeat")
HH_unnested2=unnest_variable(HH_unnested1, "survey/foodSecurity/dietaryDiversity/dietRepeat")


#clean describe data to remove duplicates
Vars1<- c("barcodehousehold","respondentName","phoneNrRE","respondentGender","timeStartAuto","timeStartManual","geopoint","currency",
          "nameHH","maritalStatusHH","educationHH","farmHistory","phone","electricity","radio","transport",
          "roofing","walls","credit","creditSource","farmSize","unitsFarmSize","landOwned","genderLand","farmLabor",
          "marketTravel","training","inputsAny","inputsUsed","offFarmAny","farmProportion","genderOffFarm","timeEnd","respondentBalance","enumeratorComments")
other_var<- HH_unnested2 %>% 
  dplyr::select(grep(paste(Vars1,collapse = "|"), names(HH_unnested2), value = TRUE))

# drop rows that are entirely missing
other_var <- other_var[rowSums(is.na(other_var)) <= ncol(other_var)-3-1,]

# drop duplicated values
other_var <- other_var[!duplicated(other_var), ]

#a) livestock owned data 
livestock<- HH_unnested2 %>% 
  dplyr::select(`survey/barcodehousehold`,grep("survey/livestockGroup/livestockOwned.*|.*livestock.*",names(HH_unnested2),value = T))

# drop rows that are entirely missing
livestock <- livestock[rowSums(is.na(livestock)) <= ncol(livestock)-2-1,]

# drop duplicated values
livestock <- livestock[!duplicated(livestock), ]

#b) Crop repeat data 
crops<- HH_unnested2 %>%
  dplyr::select(`survey/barcodehousehold`,grep(".*crop.*",names(HH_unnested2),value = T)) %>% 
  select(-c(`survey/cropGroup/cropRepeat`,`survey/cropGroup/cropRepeat_count`)) %>% 
  filter(!is.na(`survey/cropGroup/cropRepeat_survey/cropGroup/cropRepeat/cropPosition`))
# drop rows that are entirely missing
crops <- crops[rowSums(is.na(crops)) <= ncol(crops)-2-1,]

# for some crops harvest is zero but crop income is given??

# drop duplicated values
crops <- crops[!duplicated(crops), ]

##c. Population

pop<- HH_unnested2 %>% 
  dplyr::select(`survey/barcodehousehold`,grep(".*population.*",names(HH_unnested2),value = T))

pop <- pop[rowSums(is.na(pop)) <= ncol(pop)-2-1,]
pop <- pop[!duplicated(pop), ] # to confirm ID RSENRW000001 so many repeats

##d. Diet and other household characteristics
hh_char_diet<- HH_unnested2 %>%
  dplyr::select(`survey/barcodehousehold`,grep(paste(".*food.*",collapse = "|"), names(HH_unnested2), value = TRUE)) 

# drop rows that are entirely missing
hh_char_diet <- hh_char_diet[rowSums(is.na(hh_char_diet)) <= ncol(hh_char_diet)-2-1,]

# drop duplicated values
hh_char_diet <- hh_char_diet[!duplicated(hh_char_diet), ]

# #spread the  data to have values for each diet produced to be confirmed on the values to spread
# hh_char_diet= tidyr::pivot_wider(hh_char_diet, names_from = `survey/foodSecurity/dietaryDiversity/dietRepeat_survey/foodSecurity/dietaryDiversity/dietRepeat/foodGroup`,
# values_from = c("survey/foodSecurity/dietaryDiversity/dietRepeat_survey/foodSecurity/dietaryDiversity/dietRepeat/dietDetails/foodProduced","survey/foodSecurity/dietaryDiversity/dietRepeat_survey/foodSecurity/dietaryDiversity/dietRepeat/dietDetails/foodFrequency"))


#merge the 3 datasets (needs to find a way to reduce duplicates so we can do 1:1 merge)

df_list<-  list(other_var,pop,livestock,crops,hh_char_diet)      
HH_Describe<-df_list %>% reduce(full_join, by=c("survey/barcodehousehold")) 

# drop duplicated values
HH_Describe <- HH_Describe[!duplicated(HH_Describe), ]

#3. Assign Treatment data
assign_ftp<-getONAdatasets(username = username, password = password, form_id = "523975") 
assign_ftp=assign_ftp %>% 
select("ENID","country","projectCode","lat","lon","latr","lonr","FDID2","FD_name_new","FDID2_new",
       "FD_owner","HHID","FDID2","expCode","expCode_label","trial","nrTreats","TL_name1_new","season",
       "plantingDate","nrBlocks")

# unnest trial column
assign_ftp_unnested <- unnest_variable(assign_ftp, "trial")
assign_ftp_unnested1 <- unnest_variable(assign_ftp_unnested, "trial_trial/plot")

#Drop rows that have missing values entirely
assign_ftp_clean <- assign_ftp_unnested1[rowSums(is.na(assign_ftp_unnested1)) <= ncol(assign_ftp_unnested1)-12-1, ]

# check duplicates in treatment ID
duplicates <- assign_ftp_clean %>%
  filter(duplicated(`trial_trial/plot_trial/plot/POID2_new`) | duplicated(`trial_trial/plot_trial/plot/POID2_new`), fromLast = TRUE)

# check why some data don't have plot ID

#4. soil sample data
soilsample<-getONAdatasets(username = username, password = password, form_id = "526510") 
#unnest soil sample data
soilsample_unnested <- unnest_variable(soilsample, "soilSample")


vars1<- c("ENID","country","projectCode","lat","lon","latr","lonr","FDID","TLID2","TLID","POID2","POID",
"SSID","placing","depth","depthOther","upperLimit","lowerLimit")


soilsample_clean=soilsample_unnested %>% 
select(grep(paste(vars1,collapse = "|"),names(soilsample_unnested),value = T))


#5. Process plant sample
plant_samples<-getONAdatasets(username = username, password = password, form_id = "528337")
#unnest soil plant data
plant_samples_unnested <- unnest_variable(plant_samples, "plantSample")

vars2<- c("ENID","country","projectCode","crop","PSID","PSFW","PSss","PSssFW","PSDW","IDnr",
          "PSID_old","PSID_new","nrLabels","nrPooled","plantParts","nr","PSID_child",
          "fraction","fractionLabel","PSID_fraction","PSID_parent","PSID_pooled")

plantsample_clean=plant_samples_unnested %>% 
  select(grep(paste(vars2,collapse = "|"),names(plant_samples_unnested),value = T))


#6. record trial management

trial_management<-getONAdatasets(username = username, password = password, form_id = "526549")

#unnest data
trial_management_unnested <- unnest_variable(trial_management, "fieldbookSectionDetails")
#Unnest the second level
trial_management_unnested1 <- unnest_variable(trial_management_unnested, "fieldbookSectionDetails_fieldbookSectionDetails/activityRepeat")
trial_management_unnested2 <- unnest_variable(trial_management_unnested1, "fieldbookSectionDetails_fieldbookSectionDetails/activityRepeat_fieldbookSectionDetails/activityRepeat/varietyRepeat")
trial_management_unnested3 <- unnest_variable(trial_management_unnested2, "fieldbookSectionDetails_fieldbookSectionDetails/activityRepeat_fieldbookSectionDetails/activityRepeat/organicInputQuantityRepeat")
trial_management_unnested4 <- unnest_variable(trial_management_unnested3, "fieldbookSectionDetails_fieldbookSectionDetails/activityRepeat_fieldbookSectionDetails/activityRepeat/limeQuantityRepeat")
trial_management_unnested5 <- unnest_variable(trial_management_unnested4, "fieldbookSectionDetails_fieldbookSectionDetails/activityRepeat_fieldbookSectionDetails/activityRepeat/fertilizerQuantityRepeat")

vars3<- c("ENID","country","projectCode", "lat","lon","latr","lonr","lookup","limeRate","organicInputRate","fertilizerRate","organicInput","organicInputLabel",
          "FDID2","FDID","TLID2","TLID","FBID","fieldbookSections","fieldbookSection","date","fallowType","clearingMethod","fieldbookSectionLabel",
          "activities","activity","activityLabel","limeApplication","organicInputApplication","fallowResidues","ploughingMethod",
          "ploughingDepth","harrowingMethod","ridgingMethod","distanceBetweenRidges","widthRidges","heightRidges","crops","productPesticide","descriptionPesticide",
          "weedingMethod","performed","crop","cropLabel","variety","variety_other","limes","lime","limeLabel","limeQuantity","limeQuantitySpecified","organicInputs",
          "organicInputQuantity","organicInputQuantitySpecified","fertilizers","fertilizer","fertilizerLabel","fertilizerQuantity","fertilizerQuantitySpecified")

trial_management_clean=trial_management_unnested5 %>% 
  select(grep(paste(vars3,collapse = "|"),names(trial_management_unnested5),value = T))

#Drop rows that have missing values entirely
trial_management_clean <- trial_management_clean[rowSums(is.na(trial_management_clean)) <= ncol(trial_management_clean)-10-1, ]

trial_management_clean <- trial_management_clean[!duplicated(trial_management_clean), ]

#4. describe field data

d_field<-getONAdatasets(username = username, password = password, form_id = "526492")

# unnest field data

d_field_unnest <- unnest_variable(d_field, "fieldHistory")
d_field_unnest1=unnest_variable(d_field_unnest, "previousYield")

vars4<- c("ENID","country","projectCode","lat","lon","latr","lonr","FDID2","FDID","locationType","ownership",
          "HHID","positionLandscape","slope","slopeClass","erosion","conservation","rockOutcrop",
          "surfaceStoniness","surfaceGravelliness","drainage","soilDepth","nrSeasonsPerYear",
          "nrSeasons","fieldHistory","season","fallow","previousCrop","previousOtherCrops","yearsSinceFallow",
          "clearing","ploughing","ridging","irrigation","fallowType","clearingMethod","fallowResidues","irrigationMethod",
          "nrPloughing","ploughingMethod","ploughingDepth","ridgingMethod","distanceBetweenRidges","widthRidges","heightRidges","erosionMonth",
          "erosionTypes","erosionTypesSel","erosionTypeLab","erosionCauses","erosionImpacts","erosionNoCause","erosionControlPresent",
          "erosionControlTypes","erosionControlSel","erosionControlLab","fertilizer","organicInput",
          "lime","fertilizerApplied","fertilizerQuantity","organicInputApplied",
          "organicInputQuantity","limeApplied","limeQuantity","localSoilName","localSoilNameTranslated",
          "distance","fertilityFarmer","fertilityResearcher","previousYield_crops",
          "previousYield","previousYield_crop","previousYield_cropLabel","previousYield_yieldClass","previousYield_yield")
d_field_clean<- d_field_unnest1 %>% 
  select(grep(paste(vars4,collapse = "|"),names(d_field_unnest1),value = T))

#Drop duplicates
d_field_clean <- d_field_clean[rowSums(is.na(d_field_clean)) <= ncol(d_field_clean)-10-1, ]

d_field_clean <- d_field_clean[!duplicated(d_field_clean), ]


#----------------------------------------------------------------------------------------------------
#merging all the datasets
  
  

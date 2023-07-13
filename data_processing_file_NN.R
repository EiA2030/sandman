
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

Register<- select(Register,HHID,projectCode,grep("detailsHH.*",names(Register),value = T))

#2.describe household
d_HH<-getONAdatasets(username = username, password = password, form_id = "632119")

HH_unnested <- unnest_variable(d_HH, "survey/cropGroup/cropRepeat")

HH_unnested1=unnest_variable(HH_unnested, "survey/populationGroup/popRepeat")
HH_unnested2=unnest_variable(HH_unnested1, "survey/foodSecurity/dietaryDiversity/dietRepeat")

## clean  unique identifier
HH_unnested2<- rename(HH_unnested2,"ENID"="startGroup/barcodeenumerator")

#clean describe data to remove duplicates

#a) livestock owned data 
livestock<- HH_unnested2 %>% 
  dplyr::select(ENID,`survey/close/phoneNrRE`,grep("survey/livestockGroup/livestockOwned.*",names(HH_unnested2),value = T))

# drop rows that are entirely missing
livestock <- livestock[rowSums(is.na(livestock)) <= ncol(livestock)-3-1,]

# drop duplicated values
livestock <- livestock[!duplicated(livestock), ]

#b) Crop repeat data 
crop_names=c("cropPosition","cropUnits","cropIncome","cropHarvest","cropProp","cropSold","marketType","cropYearsExperience_hh")

crops<- HH_unnested2 %>%
  dplyr::select(ENID,`survey/close/phoneNrRE`,grep(paste(crop_names,collapse = "|"), names(HH_unnested2), value = TRUE)) %>% 
  #filter if crop harvest=999
  filter(`survey/cropGroup/cropRepeat_survey/cropGroup/cropRepeat/focusCrop/cropAmount/cropHarvest`!=999) %>% 
  filter(!is.na(`survey/cropGroup/cropRepeat_survey/cropGroup/cropRepeat/cropPosition`))
# drop rows that are entirely missing
crops <- crops[rowSums(is.na(crops)) <= ncol(crops)-2-1,]

# for some crops harvest is zero but crop income is given??

# drop duplicated values
crops <- crops[!duplicated(crops), ]

# confirm enumerator ID 39222701 to be dropped

##c. Diet and other household characteristics

Diet=c("farmSize","landOwned","walls","roofing","phone","radio","electricity",
       "transport","offFarmAny","credit","worstMonth","foodGroup","foodProduced","foodFrequency")


hh_char_diet<- HH_unnested2 %>%
  dplyr::select(ENID,`survey/close/phoneNrRE`,grep(paste(Diet,collapse = "|"), names(HH_unnested2), value = TRUE)) 

# drop rows that are entirely missing
hh_char_diet <- hh_char_diet[rowSums(is.na(hh_char_diet)) <= ncol(hh_char_diet)-2-1,]

# drop duplicated values
hh_char_diet <- hh_char_diet[!duplicated(hh_char_diet), ]

#spread the  data to have values for each diet produced
hh_char_diet= tidyr::pivot_wider(hh_char_diet, names_from = `survey/foodSecurity/dietaryDiversity/dietRepeat_survey/foodSecurity/dietaryDiversity/dietRepeat/foodGroup`,
                                 values_from = c("survey/foodSecurity/dietaryDiversity/dietRepeat_survey/foodSecurity/dietaryDiversity/dietRepeat/dietDetails/foodProduced","survey/foodSecurity/dietaryDiversity/dietRepeat_survey/foodSecurity/dietaryDiversity/dietRepeat/dietDetails/foodFrequency"))
# confirn phone number 0789893750 not unique

#merge the 3 datasets

df_list<-  list(livestock,crops,hh_char_diet)      
HH_Describe<-df_list %>% reduce(full_join, by=c("ENID","survey/close/phoneNrRE")) 

# drop duplicated values
HH_Describe <- HH_Describe[!duplicated(HH_Describe), ]

#3. Assign Treatment data
assign_ftp<-getONAdatasets(username = username, password = password, form_id = "523975") %>% 
  # keep only relevant variables
  select(ENID,HHID,today,FDID2_new,phonenumber,FDID2,season,expCode,trial,expCode_label,plantingDate)

# unnest trial column
assign_ftp_unnested <- unnest_variable(assign_ftp, "trial")
assign_ftp_unnested1 <- unnest_variable(assign_ftp_unnested, "trial_trial/plot")

#Drop rows that have missing values entirely
assign_ftp_clean <- assign_ftp_unnested1[rowSums(is.na(assign_ftp_unnested1)) <= ncol(assign_ftp_unnested1)-5-1, ]

# check duplicates in treatment ID
duplicates <- assign_ftp_clean %>%
  filter(duplicated(`trial_trial/plot_trial/plot/POID2_new`) | duplicated(`trial_trial/plot_trial/plot/POID2_new`), fromLast = TRUE)

# check why some data don't have plot ID

#4. soil sample data
soilsample<-getONAdatasets(username = username, password = password, form_id = "526510") %>% 
  select(FDID2,soilSample,projectCode)

#unnest soil sample data
soilsample_unnested <- unnest_variable(soilsample, "soilSample")

#5. plant sample
plant_samples<-getONAdatasets(username = username, password = password, form_id = "528337") %>% 
  select(ENID,plantSample,projectCode)
#unnest soil plant data
plant_samples_unnested <- unnest_variable(plant_samples, "plantSample")

#6. trial management

trial_management<-getONAdatasets(username = username, password = password, form_id = "526549") %>% 
  select(ENID,FDID2,TLID2,fieldbookSections,fieldbookSectionDetails)

#unnest data
trial_management_unnested <- unnest_variable(trial_management, "fieldbookSectionDetails")
#Unnest the second level
trial_management_unnested1 <- unnest_variable(trial_management_unnested, "fieldbookSectionDetails_fieldbookSectionDetails/activityRepeat")
trial_management_unnested2 <- unnest_variable(trial_management_unnested1, "fieldbookSectionDetails_fieldbookSectionDetails/activityRepeat")

#4. describe field data

d_field<-getONAdatasets(username = username, password = password, form_id = "526492")

d_field1<- d_field %>% 
  select(HHID,FDID2,ENID,fieldHistory,previousYield,nrSeasonsPerYear,previousYield_crops,`localSoilFertility/localSoilNameTranslated`,yearsSinceFallow,
         `nutrientInputDetails/fertilizerApplied`,`nutrientInputDetails/organicInputApplied`,`nutrientInputDetails/limeApplied`,`erosionFields/erosionTypes`,
         `erosionFields/erosionControlTypes`,`irrigationDetails/irrigationMethod`)

# unnest field data

d_field_unnest <- unnest_variable(d_field1, "fieldHistory")
d_field_unnest1=unnest_variable(d_field_unnest, "previousYield")

#mering the datasets


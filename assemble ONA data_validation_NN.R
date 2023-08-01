
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

# Define the ONA server URL, username, password, and form ID
url <- "https://api.ona.io/api/v1"

creds <- scan(paste0(wd, "/pws.txt"), what = "character")
username <- creds[1]
password   <- creds[2]

form_id <- "752967" #cassava


# Create the authentication token
token <- paste0(username, ":", password)
auth_token <- base64enc::base64encode(charToRaw(token))

# Make the API request to retrieve data
response <- httr::GET(
  url = paste0(url, "/data/", form_id),
  httr::add_headers(Authorization = paste0("Basic ", auth_token)),
  query = list(format = "json")
)


# Parse the JSON response into a data frame with the nesting in list
data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

#remove ystem variables
system_var<- c("_tags","_uuid","start","_notes" ,"_edited","_status" ,"_version","_duration" ,"_xform_id",
               "intro/event","_attachments","_geolocation","_media_count","_total_media","formhub/uuid",
               "_id","_media_count","_total_media","_submitted_by","_date_modified",
               "intro/altitude","intro/latitude","intro/longitude","meta/instanceID","_submission_time",
               "_xform_id_string","_bamboo_dataset_id","intro/in_the_field","_media_all_received")

data<- data %>% 
  select(-system_var)

#------------------------------------------------------------------------------------------
  #data cleaning

# plant stand data

Plant_stand_data<- data %>% 
  dplyr::select(`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("planting.*", names(data), value = TRUE))

#Plot data

plot_data<- data %>% 
  dplyr::select(`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("plotDescription.*", names(data), value = TRUE))

plot1<- plot_data %>% 
  gather(v, value, 5:26) %>% 
  mutate(treat=ifelse(v %in% grep("*.AEZ.*",v, value=T),"AEZ",
                      ifelse(v %in% grep("*.BR.*",v, value=T),"BR",
                             ifelse(v %in% grep("*.SSR.*",v, value=T),"SSR", NA)))) %>% 
  separate(v, c("details","var", "col"),"/") %>% 
  select(-details) %>% 
  mutate(col1=gsub("\\_aez|\\_BR|\\_ssr|\\_control", "", col)) %>% 
  mutate(col1=gsub("_SSR","",col1)) %>% select(-c(col,var))

# clean col to reshape wide

reshaped_data <- plot1 %>% 
  pivot_wider(
  id_cols = c("intro/enumerator_ID", "intro/barcodehousehold", "crop", "plotDescription/plotSizeDetails/row_number","treat"),
  names_from = col1,
  values_from = value
)

#drop rows that are entirely missing

reshaped_data <- reshaped_data[rowSums(is.na(reshaped_data)) <= ncol(reshaped_data)-5-1, ]


# land preparation data
land_prep_data<- data %>% 
  dplyr::select(`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("LandPreparation*", names(data), value = TRUE))


# crop management data

crop_mgt_data<- data %>% 
  dplyr::select(`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("cropManagement*", names(data), value = TRUE))

# merge all the datasets

df_list<- list(reshaped_data,Plant_stand_data,land_prep_data,crop_mgt_data) 

full_data<-df_list %>% reduce(full_join, by=c("intro/enumerator_ID", "intro/barcodehousehold","crop")) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, c("plot_plot/"), ""))



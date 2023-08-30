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

# Define the ONA server URL, username, password, and form ID
url <- "https://api.ona.io/api/v1"

creds <- scan(paste0(wd, "/pws.txt"), what = "character")
username <- creds[1]
password   <- creds[2]

form_id <- "755562" #form 5


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

# create a function to unnest the data 

data_unnested <- data.frame()
# unnest the first level contained in plot information variable that has several levels of nesting
for(i in 1:nrow(data)){
  #print(i)
  df <- data %>%
    slice(i) %>%
    unnest(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_3/eia_addon_plot_information_repeat`, keep_empty = TRUE, names_sep = "_") %>% 
    unnest(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_crop_production_title/eia_addon_crop_repeat`,keep_empty = TRUE, names_sep = "_") %>% 
    unnest(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_2/eia_addon_ag_extension_title/eia_addon_extension_repeat`, keep_empty = TRUE, names_sep = "_") %>%
    unnest(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat`, keep_empty = TRUE, names_sep = "_") %>%
    unnest(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_roster/eia_addon_hh_members_names_repeat`, keep_empty = TRUE, names_sep = "_") %>%
    unnest(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_ag_inputs_title/eia_addon_Agri_Inputs/fertilizers_grp/Fertilizer_use_subsidytype_bought`, keep_empty = TRUE, names_sep = "_") %>%
    unnest(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_land_use_title/eia_addon_plot_roster_grp/eia_addon_hh_plots_details/eia_addon_hh_plots_repeat`, keep_empty = TRUE, names_sep = "_")
  df$row_id = i
  
  df <- df %>%
    select(row_id, everything()) 
  
  # add unnested columns to the original dataset
  data_unnested <- plyr::rbind.fill(data_unnested, df)
}

# drop duplicated records

sys_names<- c("row_id","_id", "_tags", "_uuid","_notes", "_edited", "_status","_version", "_duration", "_xform_id","_attachments",     
              "_media_count","_total_media","formhub/uuid","_submitted_by","_date_modified","meta/instanceID",    
              "_submission_time","_xform_id_string","_bamboo_dataset_id","_media_all_received")


#Cleaning nested columns to remove duplicates and merge back
  # drop nested columns from original data

 original_data1 <-data %>% 
   select(-c(`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_3/eia_addon_plot_information_repeat`, 
               `eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_crop_production_title/eia_addon_crop_repeat`,
              `eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_2/eia_addon_ag_extension_title/eia_addon_extension_repeat`,
               `eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat`,
               `eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_roster/eia_addon_hh_members_names_repeat`,
               `eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_ag_inputs_title/eia_addon_Agri_Inputs/fertilizers_grp/Fertilizer_use_subsidytype_bought`,
              `eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_land_use_title/eia_addon_plot_roster_grp/eia_addon_hh_plots_details/eia_addon_hh_plots_repeat`))


 #---------------------------------------------------------------------------------------------------------
 
#1. clean crop eia_addon_extension_repeat
 extension_repeat<- data_unnested %>% 
   dplyr::select(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
                 grep(paste("eia_addon_extension_repeat",collapse = "|"), names(data_unnested), value = TRUE)) %>% 
  #shorten names
    rename_with(
     ~stringr::str_replace_all(.x, "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/", "")) %>% 
   filter(!is.na(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`)) # drop records thar dont have HHID
   #Drop rows that have missing values entirely
  extension_repeat <- extension_repeat[rowSums(is.na(extension_repeat)) <= ncol(extension_repeat)-2-1, ]
 
 # drop duplicated values
extension_repeat <- extension_repeat[!duplicated(extension_repeat), ]

#rename extension index variable for merging purposes

extension_repeat=rename(extension_repeat,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_2/eia_addon_ag_extension_title/eia_addon_extension_repeat_count"="eia_addon_part_2/eia_addon_ag_extension_title/eia_addon_extension_repeat_count")

#---------------------------------------------------------------------------------------------------------

#2. clean crop eia_addon_hh_members_details_repeat

hh_members_details_repeat<- data_unnested %>% 
  dplyr::select(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
grep(paste("eia_addon_hh_members_details_repeat",collapse = "|"), names(data_unnested), value = TRUE)) %>% 
  #shorten names
  rename_with(
    ~stringr::str_replace_all(.x, "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/", "")) %>% 
  filter(!is.na(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`)) %>%  # drop records that dont have HHID
  rename("hh_member_name"="eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat_eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat/eia_addon_person_name")

#Drop rows that have missing values entirely
hh_members_details_repeat <- hh_members_details_repeat[rowSums(is.na(hh_members_details_repeat)) <= ncol(hh_members_details_repeat)-2-1, ]

# drop duplicated values
hh_members_details_repeat <- hh_members_details_repeat[!duplicated(hh_members_details_repeat), ]

#---------------------------------------------------------------------------------------------------------

#3. clean eia_addon_hh_members_names_repeat

hh_members_names_repeat<- data_unnested %>% 
  dplyr::select(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
                grep(paste("eia_addon_hh_members_names_repeat",collapse = "|"), names(data_unnested), value = TRUE)) %>% 
  #shorten names
  rename_with(
    ~stringr::str_replace_all(.x, "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/", "")) %>% 
  filter(!is.na(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`)) %>%  # drop records thar dont have HHID
rename("hh_member_name"="eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_roster/eia_addon_hh_members_names_repeat_eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_roster/eia_addon_hh_members_names_repeat/eia_addon_hh_member_name")

#Drop rows that have missing values entirely
hh_members_names_repeat <- hh_members_names_repeat[rowSums(is.na(hh_members_names_repeat)) <= ncol(hh_members_names_repeat)-2-1, ]

# drop duplicated values
hh_members_names_repeat <- hh_members_names_repeat[!duplicated(hh_members_names_repeat), ]


#merge Household details

HH_data = merge(hh_members_details_repeat, hh_members_names_repeat,by=c("eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1","eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1","hh_member_name"))

#Rename person name index for merging purpose

HH_data=rename(HH_data,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat_count"="eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat_eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat/eia_addon_person_name_index")


#---------------------------------------------------------------------------------------------------------

#4. clean crop repeat data 
crop_repeat<- data_unnested %>% 
  dplyr::select(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
                `eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_crop_production_title/eia_addon_crops`,grep(paste("eia_addon_crop_repeat",collapse = "|"), names(data_unnested), value = TRUE)) %>% 
  #shorten names
  rename_with(
    ~stringr::str_replace_all(.x, "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/", "")) %>% 
  filter(!is.na(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`)) # drop records thar dont have HHID
#Drop rows that have missing values entirely
crop_repeat <- crop_repeat[rowSums(is.na(crop_repeat)) <= ncol(crop_repeat)-2-1, ]

# drop duplicated values
crop_repeat <- crop_repeat[!duplicated(crop_repeat), ]

crop_repeat <- crop_repeat %>% 
  #shorten names
  rename_with(
    ~stringr::str_replace_all(.x, "eia_addon_part_1/eia_addon_crop_production_title/eia_addon_crop_repeat_eia_addon_part_1/eia_addon_crop_production_title/eia_addon_crop_repeat/eia_addon_crop_rep_grp/|eia_addon_part_1/eia_addon_crop_production_title/", ""))

#rename crop index variable for merging purpose

crop_repeat<-rename(crop_repeat,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_crop_production_title/eia_addon_crop_repeat_count"="eia_addon_crop_repeat_eia_addon_crop_repeat/eia_addon_crop_rep_number")

#---------------------------------------------------------------------------------------------------------


#5. clean Fertilizer_use_subsidytype_bought

Fertilizer_use_subsidytype_bought<- data_unnested %>% 
  dplyr::select(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
                grep(paste("Fertilizer_use_subsidytype_bought",collapse = "|"), names(data_unnested), value = TRUE)) %>% 
  #shorten names
  rename_with(
    ~stringr::str_replace_all(.x, "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/", "")) %>% 
  filter(!is.na(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`)) %>% # drop records thar dont have HHID
#Drop rows that dontt have fertiliser data missing values entirely
filter(!is.na(`eia_addon_part_1/eia_addon_ag_inputs_title/eia_addon_Agri_Inputs/fertilizers_grp/Fertilizer_use_subsidytype_bought_eia_addon_part_1/eia_addon_ag_inputs_title/eia_addon_Agri_Inputs/fertilizers_grp/Fertilizer_use_subsidytype_bought/specific_Fertilizer_use_subsidytype`))

# drop duplicated values
Fertilizer_use_subsidytype_bought <- Fertilizer_use_subsidytype_bought[!duplicated(Fertilizer_use_subsidytype_bought), ]

#rename index variable for merging purpose
Fertilizer_use_subsidytype_bought= rename(Fertilizer_use_subsidytype_bought,
"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_ag_inputs_title/eia_addon_Agri_Inputs/fertilizers_grp/Fertilizer_use_subsidytype_bought_count"="eia_addon_part_1/eia_addon_ag_inputs_title/eia_addon_Agri_Inputs/fertilizers_grp/Fertilizer_use_subsidytype_bought_count")


#---------------------------------------------------------------------------------------------------------

 #6. clean eia_addon_hh_plots_repeat
hh_plots_repeat<- data_unnested %>% 
   dplyr::select(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
                 grep(paste("eia_addon_hh_plots_repeat",collapse = "|"), names(data_unnested), value = TRUE)) %>% 
   #shorten names
   rename_with(
     ~stringr::str_replace_all(.x, "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/", "")) %>% 
  filter(!is.na(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`)) # drop records thar dont have HHID
 #Drop rows that have missing values entirely
 hh_plots_repeat <- hh_plots_repeat[rowSums(is.na(hh_plots_repeat)) <= ncol(hh_plots_repeat)-2-1, ]
 
 # drop duplicated values
 hh_plots_repeat <- hh_plots_repeat[!duplicated(hh_plots_repeat), ]

 #rename index varible for merging purposes
 
 hh_plots_repeat=rename(hh_plots_repeat,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_land_use_title/eia_addon_plot_roster_grp/eia_addon_hh_plots_details/eia_addon_hh_plots_repeat_count"="eia_addon_part_1/eia_addon_land_use_title/eia_addon_plot_roster_grp/eia_addon_hh_plots_details/eia_addon_hh_plots_repeat_eia_addon_part_1/eia_addon_land_use_title/eia_addon_plot_roster_grp/eia_addon_hh_plots_details/eia_addon_hh_plots_repeat/eia_addon_index_plot")
 
#---------------------------------------------------------------------------------------------------------------------------------
 
#7. Clean plot information data
 
 plot_info1<- data_unnested %>% 
   dplyr::select(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
                 grep(paste("eia_addon_plot_information_repeat",collapse = "|"), names(data_unnested), value = TRUE)) %>% 
   #shorten names
   rename_with(
     ~stringr::str_replace_all(.x, "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/", "")) %>% 
   filter(!is.na(`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`)) # drop records thar dont have HHID
 #Drop rows that have missing values entirely
 plot_info1 <- plot_info1[rowSums(is.na(plot_info1)) <= ncol(plot_info1)-2-1, ]
 
 # drop duplicated values
 plot_info1 <- plot_info1[!duplicated(plot_info1), ]
 
 
 ## create a function to unnest the plot data 

 
 plot_unnested <- data.frame()
 # unnest the first level contained in plot information variable that has several levels of nesting
 for(i in 1:nrow(plot_info1)){
   #print(i)
   df <- plot_info1 %>%
     slice(i) %>%
     unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_weeding_title/eia_addon_weeding_info`, keep_empty = TRUE, names_sep = "_") %>% 
     unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_inorganic_inputs_title/eia_addon_inorganic_inputs_repeat`,keep_empty = TRUE,.drop = TRUE, names_sep = "_") %>% 
     unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_organic_inputs_title/eia_addon_organic_inputs_repeat`, keep_empty = TRUE, names_sep = "_") %>% 
     unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_land_preparation_title/eia_addon_tillage/eia_addon_tillage_info`, keep_empty = TRUE, names_sep = "_")
   
   df$row_id = i
   
   df <- df %>%
     select(row_id, everything()) 
   
   # add unnested columns to the original dataset
   plot_unnested <- plyr::rbind.fill(plot_unnested, df)
 }
 
 #Unnest third level of plot information
 plot_unnested2 <- data.frame()
 # unnest the first level contained in plot information variable that has several levels of nesting
 for(i in 1:nrow(plot_unnested)){
   #print(i)
   df <- plot_unnested %>%
     slice(i) %>%
     unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_organic_inputs_title/eia_addon_organic_inputs_repeat_eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_organic_inputs_title/eia_addon_organic_inputs_repeat/eia_addon_organic_input_details/eia_addon_organic_inputs_details_repeat`,keep_empty = TRUE,.drop = TRUE, names_sep = "_") %>%  
   unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_insect_pest_control_title/eia_addon_insect_info`,keep_empty = TRUE,.drop = TRUE, names_sep = "_") %>% 
          unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_weeding_title/eia_addon_weeding_info`,keep_empty = TRUE,.drop = TRUE, names_sep = "_") %>% 
                 unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_bird_pest_control_title/eia_addon_bird_info`,keep_empty = TRUE,.drop = TRUE, names_sep = "_") %>% 
                        unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_disease_pest_control_title/eia_addon_disease_info`,keep_empty = TRUE,.drop = TRUE, names_sep = "_") %>% 
                               unnest(`eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_title/eia_addon_rodent_pest_control_title/eia_addon_rodent_info`,keep_empty = TRUE,.drop = TRUE, names_sep = "_")
   
   df$row_id = i
   
   df <- df %>%
     select(row_id, everything()) 
   
   # add unnested columns to the original dataset
   plot_unnested2 <- plyr::rbind.fill(plot_unnested2, df)
 }
 
 #Drop rows that have missing values entirely
 plot_unnested2 <- plot_unnested2[rowSums(is.na(plot_unnested2)) <= ncol(plot_unnested2)-2-1, ]
 
 # drop duplicated values
 plot_unnested2 <- plot_unnested2[!duplicated(plot_unnested2), ]
 
 #rename index variable for merging purposes
 
 plot_unnested2=rename(plot_unnested2,
      "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_3/eia_addon_plot_information_repeat_count"="eia_addon_part_3/eia_addon_plot_information_repeat_eia_addon_part_3/eia_addon_plot_information_repeat/eia_addon_plot_numberid")
 

#merge all the data into 1
 
list1<-list(original_data1,hh_plots_repeat,crop_repeat,HH_data,extension_repeat,Fertilizer_use_subsidytype_bought,plot_unnested2)

#Merge original data with household member data
data_1 = merge(original_data1, HH_data,by=c("eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1","eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1","eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_members_details_repeat_count"))
  
#Merge data1  with hh_plots_repeat data

data_2 = merge(data_1, hh_plots_repeat,by=c("eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1","eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1",
                                            "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_land_use_title/eia_addon_plot_roster_grp/eia_addon_hh_plots_details/eia_addon_hh_plots_repeat_count"))


#Merge data2 with crop_repeat data
data_3 = merge(data_2, crop_repeat,by=c("eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1","eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1",
                                            "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_crop_production_title/eia_addon_crop_repeat_count"))


#Merge data3 with Fertilizer_use_subsidytype_bought data
data_4 = merge(data_3, Fertilizer_use_subsidytype_bought,by=c("eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1","eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1",
                                        "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_ag_inputs_title/eia_addon_Agri_Inputs/fertilizers_grp/Fertilizer_use_subsidytype_bought_count"))


#Merge data4 with extension_repeat data
data_5 = merge(data_4, extension_repeat,by=c("eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1","eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1",
                                                              "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_2/eia_addon_ag_extension_title/eia_addon_extension_repeat_count"))

#Merge data5 with plot_unnested2 data
data_6 = merge(data_5, plot_unnested2,by=c("eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1","eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1",
                                             "eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_3/eia_addon_plot_information_repeat_count"))

 
 # drop duplicated values
data_6 <- data_6[!duplicated(data_6), ]
 
 # export the data to xlsx

 writexl::write_xlsx(data_6,
                     "D:/IITA/data/farmer_seg_data_malawi.xlsx")
 
 
 







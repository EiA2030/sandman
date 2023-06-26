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

form_id <- "627372" #Maize


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
# unnest the first level contained in plot variable
for(i in 1:nrow(data)){
  #print(i)
  df <- data %>%
    slice(i) %>%
    unnest(plot, keep_empty = TRUE, names_sep = "_")
  
  df$row_id = i
  
  df <- df %>%
    select(row_id, everything()) 
  
  # add unnested columns to the original dataset
  data_unnested <- plyr::rbind.fill(data_unnested, df)
}
# unnest the second level contained in plot_plot/PD variable
data_unnested <- data_unnested %>%
  unnest(`plot_plot/PD`, keep_empty = TRUE, names_sep = "_")

# review the plot variables - missing/data types
plot_df <- data_unnested %>% select(contains("plot"))
sapply(plot_df, function(x) sum(is.na(x)))

#------------------------------------------------------------------------------------------
  #data cleaning

######################################
# 2. FIELD, TRIAL & PLOT IDENTIFIERS #
######################################
# field identifiers=FDID2
# treatment identifier=TLID2
#plot identifier=`plot_plot/POID2`

# Summarise plot data at a parameter level

#1) PD data 
PD<-c("plot_plot/PD_plot/PD/pestDisease",	
      "plot_plot/PD_plot/PD/PD_parameters/score_severity",
"plot_plot/PD_plot/PD/PD_parameters/score_incidence")
  

PD_data<- data_unnested %>% 
  dplyr::select(FDID2,TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`,PD) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, "plot_plot/PD_plot/PD/PD_parameters/", "")) %>%  #shorten variable names
  mutate(score_severity=ifelse(score_severity == "NaN",NA,score_severity),
        score_incidence=ifelse(score_incidence == "NaN",NA,score_incidence))

# drop rows that are entirely missing
PD_data <- PD_data[rowSums(is.na(PD_data)) <= ncol(PD_data)-4-1, ]

# convert score severity and incidence variable to numeric
PD_data<-PD_data %>% dplyr::mutate_at(c("score_severity","score_incidence"), as.numeric) 

#Drop any duplicates
PD_data <- PD_data[!duplicated(PD_data), ]

# get average values of the score severity and score incidence 
PD_data<- PD_data %>% 
  group_by(FDID2,TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`,`plot_plot/PD_plot/PD/pestDisease`) %>% 
  summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE)))) %>% 
  rename_with(~gsub("_mean", "", .), contains("mean"))

#spread the  data to have values for each diseases
PD_data= tidyr::pivot_wider(PD_data, names_from = `plot_plot/PD_plot/PD/pestDisease`, 
                     values_from = c("score_severity","score_incidence"))
#-----------------------------------------------------------------------------------------------------------------------------

# 2)  plant stand
plantStand<-c("nrPlants",	"plotWidthPlantStand","plotLengthPlantStand")

# Check if variables exist in the dataframe
matching_elements <- names(data_unnested)[grepl(paste(plantStand, collapse = "|"), names(data_unnested))] 
matching_elements= gsub("plot_plot/plantStand_parameters/|plot/plant_branching/branching_parameters/","",matching_elements)

missing_variables <- setdiff(plantStand, matching_elements)

# Create missing variables and fill with NAs
if (length(missing_variables) > 0) {
  for (variable in missing_variables) {
    data_unnested[[variable]] <- NA
  }
}

# subset plant stand data
Plant_stand_data<- data_unnested %>% 
  dplyr::select(FDID2, TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`,today,grep(paste(plantStand,collapse = "|"), names(data_unnested), value = TRUE)) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, "plot_plot/plantStand_parameters/|plot/plant_branching/branching_parameters/", "")) %>%  #shorten variable names
  mutate(today = as.Date(today, format="%Y-%m-%d")) %>%
  group_by(FDID2, TLID2, `plot_plot/POID2`) %>%
  filter(today == min(today)) %>% #selecting first assessment of plant stand
  dplyr::select(-today)
  
#Drop rows that have missing values entirely
Plant_stand_data <- Plant_stand_data[rowSums(is.na(Plant_stand_data)) <= ncol(Plant_stand_data)-4-1, ]

# drop duplicated values
Plant_stand_data <- Plant_stand_data[!duplicated(Plant_stand_data), ]

# get average  values (Use ID-RSPORW155897139875 to confirm any difference between sample data average and script 
Plant_stand_data_summ<- Plant_stand_data %>% 
  dplyr::mutate_at(plantStand, as.numeric) %>% 
  group_by(FDID2, TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE))))%>% 
  rename_with(~gsub("_mean", "", .), contains("mean"))

#-------------------------------------------------------------------------------------------------------------------

# 3) Tuber yield data

tuberYield<- c("NrProlificPlants","plotLengthBiomass","plotWidthBiomass","biomassFW",
               "biomassFWss","biomassPSID","plotLengthGrainYield","plotWidthGrainYield",
               "NrCobs","cobsFW","grainsFW","grainsMC")

# Check if variables exist in the dataframe
matching_elements <- names(data_unnested)[grepl(paste(tuberYield, collapse = "|"), names(data_unnested))] 
matching_elements= gsub("plot_plot/grainYield[0-9]_parameters/","",matching_elements)

missing_variables <- setdiff(tuberYield, matching_elements)

# Create missing variables and fill with NAs
if (length(missing_variables) > 0) {
  for (variable in missing_variables) {
    data_unnested[[variable]] <- NA
  }
}

tuberYield_data<- data_unnested %>% 
  dplyr::select(FDID2, TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`,grep(paste(tuberYield,collapse = "|"), names(data_unnested), value = TRUE)) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, c("plot_plot/grainYield[0-9]_parameters/"), "")) #shorten variable names

#Drop rows that have missing values entirely
tuberYield_data <- tuberYield_data[rowSums(is.na(tuberYield_data)) <= ncol(tuberYield_data)-4-1, ]

# drop duplicated values
tuberYield_data <- tuberYield_data[!duplicated(tuberYield_data), ]

# get average  values since some plots have multiple harvest measurements (Use ID- to confirm if there are any difference between sample data average and script 
tuberYield_data_summ<-tuberYield_data %>% 
dplyr::mutate_at(c("NrProlificPlants","plotLengthBiomass","plotWidthBiomass","biomassFW",
                   "biomassFWss","plotLengthGrainYield","plotWidthGrainYield",
                   "NrCobs","cobsFW","grainsFW","grainsMC"), as.numeric) %>% 
  group_by(FDID2, TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE))))%>% 
  rename_with(~gsub("_mean", "", .), contains("mean"))

#-------------------------------------------------------------------------------------------------------------------

# 4) plant vigor and weeds data
plantVigor_weeds<-c("plot_plot/weeds_parameters/score_weeds","plot_plot/plantVigor_parameters/score_vigor")

plantVigor_weeds_data<- data_unnested %>% 
  dplyr::select(FDID2,TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`,plantVigor_weeds) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, c("plot_plot/weeds_parameters/|plot_plot/plantVigor_parameters/"), ""))  #shorten variable names

#Drop rows that have missing values entirely
plantVigor_weeds_data <- plantVigor_weeds_data[rowSums(is.na(plantVigor_weeds_data)) <= ncol(plantVigor_weeds_data)-4-1, ]

# drop duplicated values
plantVigor_weeds_data <- plantVigor_weeds_data[!duplicated(plantVigor_weeds_data), ]

# calculate the mean score per plot and indicate the number of measurements:
plantVigor_weeds_data_summ<-plantVigor_weeds_data %>% 
dplyr::mutate_at(c("score_vigor","score_weeds"), as.numeric) %>% 
  group_by(FDID2, TLID2,`plot_plot/POID2`,`plot_plot/POID2_label`) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE),sum = ~sum(., na.rm = TRUE)))) %>% 
  rename_with(~gsub("sum", "nr", .), contains("sum"))

#-------------------------------------------------------------------------------------------------------------------

# merge all the plot data
df_list <- list(PD_data,Plant_stand_data_summ,tuberYield_data_summ,plantVigor_weeds_data_summ)      
combined_data<-df_list %>% reduce(full_join, by=c("FDID2", "TLID2","plot_plot/POID2","plot_plot/POID2_label")) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, c("plot_plot/"), "")) 
  
combined_data <- apply(combined_data, 2, function(x) {
    replace(x, is.null(x) | x == "NAN", NA)
  })

#Drop rows that have missing values entirely
combined_data <- as.data.frame(combined_data[rowSums(is.na(combined_data)) <= ncol(combined_data)-4-1, ])

# confirm that there are no duplicates in the data in terms of plot Id
duplicates <- combined_data %>%
  filter(duplicated(POID2) | duplicated(POID2), fromLast = TRUE)

#save clean data as xlsx
writexl::write_xlsx(combined_data,"./data/summary_Maize_plot_data.xlsx")

  
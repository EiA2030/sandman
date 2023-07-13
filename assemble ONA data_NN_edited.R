wd <- "~/TRANSFORM/eia2030/sandman/"
setwd(wd)

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

creds <- scan(paste0(wd, "/pwd.txt"), what = "character")
user <- creds[1]
pw   <- creds[2]


# username <- "naominganga"
# password <-"Nyahururu1"
form_id <- "526553" #potato
#form_id <- "627372" #maize

# Create the authentication token
# token <- paste0(username, ":", password)
token <- paste0(user, ":", pw)
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

sample_soil_data <- data_unnested %>% 
  select(`ENID`,`latr`,`lonr`,`_uuid`,`lookup`,`deviceid`,`geopoint`,`_duration`,`_xform_id`,`parameters`,`_geolocation`,`_submission_time`,`_date_modified`)


# field identifiers=FDID2
# treatment identifier=TLID2
#plot identifier=`plot_plot/POID2`

# List down key variables within plots to be analysed 

PD<-c("plot_plot/PD_count","plot_plot/pestDiseases","plot_plot/PD_plot/PD/pestDisease",	
      "plot_plot/PD_plot/PD/pestDiseaseLabel","plot_plot/PD_plot/PD/PD_parameters/score_severity",
      "plot_plot/PD_plot/PD/PD_parameters/score_incidence")	

plantStand<-c("plot_plot/plantStand_parameters/nrPlants",	"plot_plot/plantStand_parameters/plotWidth",
              "plot_plot/plantStand_parameters/plotLength")

tuberYield2<- c("plot_plot/tuberYield1_parameters/tubersNr", "plot_plot/tuberYield2_parameters/tubersFW",            
                "plot_plot/tuberYield3_parameters/tubersFWss",          
                "plot_plot/tuberYield1_parameters/tubersSmallNr","plot_plot/tuberYield2_parameters/tubersSmallFW",       
                "plot_plot/tuberYield1_parameters/tubersDiseasedNr","plot_plot/tuberYield2_parameters/tubersDiseasedFW",   
                "plot_plot/tuberYield1_parameters/tubersMarketableNr","plot_plot/tuberYield2_parameters/tubersMarketableFW",  
                "plot_plot/tuberYield3_parameters/tubersSmallFWss","plot_plot/tuberYield3_parameters/tubersDiseasedFWss", 
                "plot_plot/tuberYield3_parameters/tubersMarketableFWss",
                "tuberYield_parDetails/tuberSampling","tuberYield_parDetails/PD_tubers","tuberYield_parDetails/tuberQuality",
                "tuberYield_parDetails/tuberSampling")

plantVigor_weeds<-c("plot_plot/weeds_parameters/score_weeds","plot_plot/plantVigor_parameters/score_vigor",
                    "pesticides", "weeding")


#Change type from character to numeric for all number variables

numeric_var<- c("score_severity","score_incidence","PD_count","Pdtubers_count","nrPlants","plotWidth","tubersNr",
                "tubersFW","plotLength","tubersFWss","PS_count","tubersSmallNr","tubersSmallFW",
                "plot_plot/tuberYield1_parameters/tubersDiseasedNr","tubersDiseasedFW","tubersMarketableNr",
                "tubersMarketableFW","tubersSmallFWss","tubersDiseasedFWss","tubersMarketableFWss")

data_unnested <-data_unnested %>% 
  dplyr::mutate_at(grep(paste(numeric_var, collapse = "|"), names(data_unnested), value = TRUE), as.numeric) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, "plot_plot/", "")) #shorten variable names by removing plot_plot 

#str(data_unnested)

## Check for duplicates and keep all instances
duplicated_rows <- data_unnested[duplicated(data_unnested) | duplicated(data_unnested, fromLast = TRUE), ]

#drop duplicated records
clean_data <- data_unnested[!duplicated(data_unnested), ]


#summarise all numeric variables at plot level

dta_summary <- clean_data%>% 
  dplyr::select(FDID2, TLID2,POID2,POID2_label,grep(paste(numeric_var,collapse = "|"), names(clean_data), value = TRUE))%>%
  group_by(FDID2, TLID2,POID2,POID2_label) %>%
  summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE))))

#save clean data as xlsx
writexl::write_xlsx(dta_summary,"./data/summary_potato_plot_data.xlsx")
writexl::write_xlsx(clean_data,"./data/clean_potato_plot_data.xlsx")

require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggExtra)


#source("C:/Users/ckreye/OneDrive - CGIAR/WorkSpace_R/ONA/reviewDST/ODK_functions.R")
#wd<-"C:/Users/ckreye/OneDrive - CGIAR/WorkSpace_R/ONA/reviewDST/dwld"
#wd<-"C:/Users/ckreye/OneDrive - CGIAR/WorkSpace_R/ONA/reviewDST"

#setwd(wd)


dropGroupNames <- function(ds){
  names(ds)[grepl('\\.', names(ds))] <- sub('.*\\.', '', names(ds)[grepl('\\.', names(ds))])
  return(ds)
}

filterSingleSubmission <- function(ds, ID, recent=TRUE) {
  #ds: dataset to filter (must contain the end variable)
  #ID: vector of ID variables for which a unique combination must be retained
  #recent: if TRUE, the most recent submission is retained, else the first submission is retained
  if(is.character(ds$end) | is.factor(ds$end)) {ds$end <- as.POSIXlt(as.character(ds$end), format="%b %d, %Y %I:%M:%S %p", tz="Africa/Lagos")}
  tmp <- subset(ds, select=c(ID, "end", "KEY"))
  tmp$end <- as.numeric(julian(tmp$end))
  res <- ds[ds$KEY %in% (tmp %>% group_by_at(ID) %>% filter(end == ifelse(recent,max(end), min(end))))$KEY,]
  return(res)
}







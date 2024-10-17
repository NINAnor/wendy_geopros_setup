
library(stringi)
library(tidyverse)
library(fs)
library(ahpsurvey)
library(bigrquery)
library(DBI)

project_id<-"eu-wendy"

#the wendy project is wrongly named for downloading data
if(project_id == "eu-wendy"){
  bqprojID<-"wendy"
}else{
  bqprojID<-project_id
}


#site_id<-"NO06_1" c("ITA","ESP","GRC")
studyID<-"GRC"
env<-"dev" #c("dev","prod")
var_lang<-"en" #c("grk","en","ita","esp")

### gcs bucket and bq settings
cred_path<-paste0("gcs_keys/",project_id,"_key.json")

### perform an AHP for ecosystem services based on the individual rankings of ES
userID<-



es_pair <- tbl(con, "es_pair")
es_pair <- select(es_pair, ES_left,ES_right,selection_text,selection_val,userID,siteID,ahp_section) %>%filter(siteID == studyID)%>% collect()

##

atts4 <- c("cultural","regulating","provisioning")
atts1 <- c("atmo","nat_haz","biodiv")
atts2 <- c("aes","cult","recr")
atts3 <- c("farm","wild","drink_wat","fibres")
atts_list<-list(atts1,atts2,atts3,atts4)
user_vec<-es_pair%>%distinct(userID)


all_groups<-es_pair%>%filter(siteID == studyID)
pref_list<-list() #list to store tmp pref
cons_list<-list() #list to store tmp consistencies
group_vec<-all_groups%>%distinct(ahp_section)

for(g in 1:nrow(group_vec)){
  data<-es_pair%>%filter(ahp_section %in% group_vec[g,])%>%select(userID,selection_val, ES_left, ES_right)
  data$pair<-paste0(data$ES_left,"_",data$ES_right)
  tDat<-data%>%select(selection_val, pair, userID)%>%
    pivot_wider(id_cols = userID, id_expand = F,  names_from = pair, values_from = selection_val)
  tDat<-tDat[, -1]
  
  ## preferences
  atts<-atts_list[[g]]
  
  ahp_all <- ahp.mat(df = tDat, atts = atts, negconvert = TRUE)
  pref<-ahp.aggpref(ahp_all, atts, method = "geometric", aggmethod = "eigen", qt = 0)
  
  
  a<-as.data.frame(pref)
  pref_list[[g]]<-a
  # cons_list[group_vec[g,]]<-ahp.cr(ahp_all, atts, ri = NULL)
}

main<-as.data.frame(t(pref_list[[4]]))
reg<-as.data.frame(t(pref_list[[1]]))
cul<-as.data.frame(t(pref_list[[2]]))
prov<-as.data.frame(t(pref_list[[3]]))

## final ranking of es per respondent
reg<-t(reg*main$regulating)
cul<-t(cul*main$cultural)
prov<-t(prov*main$provisioning)

all<-as.data.frame(rbind(reg,prov,cul))
all<-all%>%rownames_to_column(var = "esID")
all$siteID<-rep(studyID,nrow(all))

# insert_upload_job("rgee-381312", "data_base", "ahp_all", all)
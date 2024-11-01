perform_ahp_update_db <- function(es_pair, con_admin, studyID){
  #make analysis groups
  att_main <- c("Cultural","Provisioning","Regulation")
  att_gr1 <-  c("atmo","nat_haz","biodiv")
  att_gr2 <- c("aes","cult","recr")
  att_gr3<-c("farm","wild_hunt","wild_col","drink_wat","mat")
  att_list<-list(att_main,att_gr1,att_gr2,att_gr3)

  grp_names_vec<-c("main","regulating","cultural","provisioning")

  #vector with individual decision makers (userIDs)
  dec_makers<-es_pair%>%distinct(userID)

  pref_list<-list() #list to store tmp pref
  agree_list<-list() #list to store tmp consistencies
  #the same order as atts_list
  group_vec<-es_pair%>%distinct(ahp_section)

  ## store preference and agreement

  for(g in 1:nrow(group_vec)){
    tmp_dat<-es_pair%>%filter(ahp_section %in% group_vec[g,])%>%select(userID,selection_val, ES_left, ES_right)
    tmp_dat$pair<-paste0(tmp_dat$ES_left,"_",tmp_dat$ES_right)
    tDat<-tmp_dat%>%select(selection_val, pair, userID)%>%
      pivot_wider(id_cols = userID, id_expand = F,  names_from = pair, values_from = selection_val)
    tDat<-tDat[, -1]

    ## preferences
    tmp_attr<-att_list[[g]]

    ahp_tmp <- ahp.mat(df = tDat, atts = tmp_attr, negconvert = TRUE)
    pref_tmp<-ahp.aggpref(ahp_tmp, tmp_attr, method = "eigen", aggmethod = "eigen", qt = 0)

    ## and the

    # store these:
    df_cr<-ahp.cr(ahp_tmp, tmp_attr, ri = NULL)

    a<-as.data.frame(pref_tmp)
    pref_list[[g]]<-a
    # cons_list[group_vec[g,]]<-ahp.cr(ahp_all, atts, ri = NULL)

    ## consensus
    icc_res<-icc_res<-icc(as.data.frame(tDat), model = "twoway", type = "agreement", unit = "average")
    agree_list[[g]]<-c(icc_res$value,grp_names_vec[g],studyID)

  }

  main<-as.data.frame(t(pref_list[[1]]))
  reg<-as.data.frame(pref_list[[2]])
  reg$global<-main$Regulation
  reg$class<-rep("reg",nrow(reg))
  cul<-as.data.frame(pref_list[[3]])
  cul$global<-main$Cultural
  cul$class<-rep("cul",nrow(cul))
  prov<-as.data.frame(pref_list[[4]])
  prov$global<-main$Provisioning
  prov$class<-rep("prov",nrow(prov))

  ## final ranking of es per respondent

  all<-as.data.frame(rbind(reg,prov,cul))
  n_es_grp<-table(all$class)
  sum_glob<-sum(all$global)

  reg$adj_glob<-n_es_grp[3]*reg$global/sum_glob
  cul$adj_glob<-n_es_grp[1]*cul$global/sum_glob
  prov$adj_glob<-n_es_grp[2]*prov$global/sum_glob
  all_fin<-rbind(reg,cul,prov)
  all_fin$pref_adj<-all_fin$pref*all_fin$adj_glob
  # all<-t(reg*main$Regulation)



  all_fin<-all_fin%>%rownames_to_column(var = "esID")
  all_fin$siteID<-rep(studyID,nrow(all_fin))

  # upload all_fin (ranking of es)
  ranking_es = bq_table(project = "eu-wendy", dataset = dataset, table = 'es_ranking')
  bq_table_upload(x = ranking_es, values = all_fin, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

  # upload agreement among participants
  # Convert list to data frame
  data_df <- as.data.frame(do.call(rbind, agree_list))

  # Rename columns
  colnames(data_df) <- c("icc_val", "es_Category")

  # Convert Value column to numeric
  data_df$icc_val <- as.numeric(data_df$icc_val)

  # Display the result
  agreement_es = bq_table(project = "eu-wendy", dataset = dataset, table = 'es_agreement')
  bq_table_upload(x = agreement_es, values = data_df, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')


}



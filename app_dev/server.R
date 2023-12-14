function(input, output, session) {

  output$cond_b1<-renderUI({
    validate(
      need(input$projtype != "", 'Provide a projtype'),
      need(input$proj_nat_name != '', 'Provide a project name'),
      need(input$proj_descr != '', 'Provide a proj description')
    )
    tagList(
      actionButton('sub1', 'confirm', class='btn-primary'),
      uiOutput("type_dep")
    )

  })

  observeEvent(input$sub1,{

    if(input$projtype == "onshore"){
      output$type_dep<-renderUI(
        tagList(
          textOutput("cntr_text"),
          mapedit::selectModUI("map_sel_cntry"),
          uiOutput("cond_cntry")
        )
      )
    }else if(input$projtype == "offshore"){
      output$type_dep<-renderUI(
        tagList(
          mapedit::editModUI("sel_offshore"),
          htmlOutput("overlay_result2"),
          uiOutput("btn2"),
        )
      )
    }
    removeUI(selector = "#projtype")
    removeUI(selector = "#proj_nat_name")
    removeUI(selector = "#proj_nat_name-label")
    removeUI(selector = "#proj_descr")
    removeUI(selector = "#proj_descr-label")
    removeUI(selector = "#sub1")
  })

  output$cntr_text<-renderText("Select your country of interest")
  #country map

      cntry_sel <- callModule(module=selectMod,
                        leafmap=map_cntr,
                        id="map_sel_cntry")

      # cntry_sel<-mapedit::selectMap(map_cntr)
  observe({
    cntry_sel<-cntry_sel()
    if(nrow(cntry_sel==1)){
      ## render save country btn
      output$cond_cntry<-renderUI(
        tagList(
          actionButton("save_countr","save country"),
          uiOutput("cntry_dep")
        )
      )
    }else{
      ## render text min  / max 1 cntry
      output$cond_cntry<-renderUI(
        tagList(
          h5("please select min and max 1 country")
        )
      )
    }
  })

      #reactive values to store mapping
      rv<-reactiveValues(
        onshore_sel = reactive({}),
        offshore_sel = reactive({})
      )

      rv$offshore_sel<-callModule(module = editMod,
                             leafmap=map_coast,
                             id="sel_offshore")

   # offshore_sel<-mapedit::editMap(map_coast)




  sel_country<-eventReactive(input$save_countr,{
    cntry_sel<-cntry_sel()
    sel_country<-st_sf(cntr%>%filter(CNTR_ID==cntry_sel[which(cntry_sel$selected==TRUE),"id"]))
  })

  observeEvent(input$save_countr,{
    sel_country<-sel_country()

    # display only the selected country
    map_onshore<- leaflet(sel_country) %>%
      addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0)%>%
      addProviderTiles(provider= "CartoDB.Positron")%>%
      addDrawToolbar(targetGroup='drawPoly',
                     polylineOptions = F,
                     polygonOptions = T,
                     circleOptions = F,
                     markerOptions = F,
                     circleMarkerOptions = F,
                     rectangleOptions = T,
                     singleFeature = FALSE,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))

    rv$onshore_sel<-callModule(module = editMod,
                               leafmap=map_onshore,
                               id="sel_onshore")

    # onshore_sel<-mapedit::editMap(map_onshore)

    output$cntry_dep<-renderUI(
      tagList(
        "Draw your region of interest within the country borders",
        mapedit::editModUI("sel_onshore"),
        htmlOutput("overlay_result"),
        uiOutput("btn1"),
      )
    )
    removeUI(
      selector = paste0("#map_sel_cntry","-map"))
    removeUI(
      selector = "#save_countr")
    removeUI(
      selector = "#cntr_text")
    removeUI(
      selector = "#es_descr")



  })

  ## for onshore:: a helper function to check if poly is inside country and correct size
  observe({
    req(rv$onshore_sel)
    req(sel_country)
    sel_country<-sel_country()


    rectangles <- rv$onshore_sel()$finished

    n_poly<-nrow(as.data.frame(rectangles))

    if(n_poly==1){
      n_within<-nrow(as.data.frame(st_within(rectangles,sel_country)))

      if(n_within<n_poly){
        output$overlay_result <- renderText({
          paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely within your selected country<li/></font>")
        })
        removeUI(
          selector = paste0("#savepoly"))
      }else{
        area<-round(as.numeric(st_area(rectangles))/1000000,0)

        if(area>on_max){
          output$overlay_result <- renderText({
            paste0("<font color=\"#FF0000\"> <li>Your area is ",area ," km2, and thus too big, please draw a smaller area of max ",on_max," km2<li/></font>")
          })
          removeUI(
            selector = paste0("#savepoly"))

        }else if(area<on_min){
          output$overlay_result <- renderText({
            paste0("<font color=\"#FF0000\"> <li>Your area is ",area, " km2, and thus too small, please draw a bigger area of min ",on_min," km2<li/></font>")
          })
          removeUI(
            selector = paste0("#savepoly"))

        }else{
          output$btn1<-renderUI(
            actionButton("savepoly","save area")
          )
          output$overlay_result <- renderText({
            paste0("Your area is ",area, " km2, Save your area now")

          })
        }

      }

    }else if(n_poly>1){
      output$overlay_result <- renderText({
        paste("<font color=\"#FF0000\"> <li>Remove areas, just one area allowed<li/></font>")
      })
      removeUI(
        selector = paste0("#savepoly"))

    }else if(n_poly==0){
      output$overlay_result <- renderText({
        paste("<font color=\"#FF0000\"><li>Please draw one area<li/></font>")
      })
      removeUI(
        selector = paste0("#savepoly"))

    }

  })

  ## for offshore:: a helper function to check poly area is outside coast line and correct size
  observe({
    req(rv$offshore_sel)

    rectangles <- rv$offshore_sel()$finished

    n_poly<-nrow(as.data.frame(rectangles))

    if(n_poly==1){
      n_inter<-nrow(as.data.frame(st_intersects(rectangles,coast)))

      if(n_inter==n_poly){
        output$overlay_result2 <- renderText({
          paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely inside offshore areas<li/></font>")
        })
        removeUI(
          selector = paste0("#savepoly"))
      }else{
        area<-round(as.numeric(st_area(rectangles))/1000000,0)

        if(area>off_max){
          output$overlay_result2 <- renderText({
            paste0("<font color=\"#FF0000\"> <li>Your area is ",area ," km2, and thus too big, please draw a smaller area of max ", off_max ," km2<li/></font>")
          })
          removeUI(
            selector = paste0("#savepoly"))

        }else if(area<off_min){
          output$overlay_result2 <- renderText({
            paste0("<font color=\"#FF0000\"> <li>Your area is ",area, " km2, and thus too small, please draw a bigger area of min ",off_min ," km2<li/></font>")
          })
          removeUI(
            selector = paste0("#savepoly"))

        }else{
          output$btn2<-renderUI(
            actionButton("savepoly","save area")
          )
          output$overlay_result2 <- renderText({
            paste0("Your area is ",area, " km2, Save your area now")

          })
        }

      }

    }else if(n_poly>1){
      output$overlay_result2 <- renderText({
        paste("<font color=\"#FF0000\"> <li>Remove areas, just one area allowed<li/></font>")
      })
      removeUI(
        selector = paste0("#savepoly"))

    }else if(n_poly==0){
      output$overlay_result2 <- renderText({
        paste("<font color=\"#FF0000\"><li>Please draw one area<li/></font>")
      })
      removeUI(
        selector = paste0("#savepoly"))

    }

  })

  ## save poly in wendy gee asset

  siteID<-eventReactive(input$savepoly,{
    siteID<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
  })

  study_area<-eventReactive(input$savepoly,{
    req(siteID)
    siteID<-siteID()

    if(input$projtype=="onshore"){
      study_area<-rv$onshore_sel()$finished
      sel_country<-sel_country()
      study_area<-study_area%>%select()
      study_area$cntrID<-sel_country$ISO3_CODE
    }else{
      study_area<-rv$offshore_sel()$finished
      study_area<-study_area%>%select()
      study_area$cntrID<-"off"
    }

    study_area$siteID<-siteID
    study_area$siteAREAkm2<-as.integer(round(as.numeric(st_area(study_area))/1000000,0))

    study_area$siteTYPE <-input$projtype
    study_area$siteNAME <-input$proj_nat_name
    study_area$siteDESCR <-input$proj_descr

    study_area$siteCREATOR <-Sys.getenv("USERNAME")

    study_area$siteCREATETIME<-Sys.time()
    study_area


  })

  observeEvent(input$savepoly,{
    req(study_area)
    study_area<-study_area()
    if(input$projtype=="onshore"){

      removeUI(selector = paste0("#sel_onshore","-map"))
      removeUI(
        selector = "#overlay_result")

    }else{
      removeUI(selector = paste0("#sel_offshore","-map"))
      removeUI(
        selector = "#overlay_result2")
    }

    ee_study<-study_area
    ee_study$siteCREATETIME<-as.character(ee_study$siteCREATETIME)
    ee_study<-sf_as_ee(ee_study)


    ## save geom on gee
    assetId<-paste0("projects/eu-wendy/assets/study_sites/",as.character(study_area$siteID))
    # ee_study <- ee_study$set('siteID', as.character(study_area$siteID),
    #                              'cntrID', as.character(study_area$cntrID))
    start_time<-Sys.time()
    task_tab <- ee_table_to_asset(
      collection = ee_study,
      description = "test upload study area",
      assetId = assetId
    )

    task_tab$start()

    ## upload as bq spatial table to WENDY google cloud
    # geo <- sf_geojson(study_area, atomise = TRUE)
    # st_write(study_area,"test.geojson")
    # geo1 <- sf_geojson(study_area, atomise = FALSE)
    # geo_js_df <- as.data.frame(geojson_wkt(geo))
    # str(geo)
    #
    # players_table = bq_table(project = "rgee-381312", dataset = "data_base", table = "test_new")


    #

    insertUI(selector = "#savepoly", where = "afterEnd",
             ui=tagList(
               # textOutput("proj_id"),
               br(),
               h5("Click on the ecosystem services that are relevant to map in your study area"),
               br(),
               DT::dataTableOutput('es_descr'),
               #
               uiOutput("cond_save_es")
             ))

    removeUI(
      selector = "#savepoly")


  })

  output$es_descr <- renderDT({
      datatable(es_descr, selection = 'multiple', options = list(pageLength = 15))


  })

  observe({
    req(input$es_descr_rows_selected)

    if(length(input$es_descr_rows_selected)!=0){
      n_es_vec<-c("",1:length(input$es_descr_rows_selected))
      output$cond_save_es<-renderUI(
        tagList(
          selectInput("n_es","how many es should each participant map", n_es_vec, selected = ""),
          uiOutput("cond_save_es2")
        )
      )
    }else{
      output$cond_save_es<-renderUI(
        h5("select at least one es")
      )
    }
  })

  observe({
    req(input$n_es)

    if(input$n_es!=""){
      output$cond_save_es2<-renderUI(
        tagList(
          actionButton("save_es", "save selection"),
          uiOutput("cond_save_es3")
        )
      )
    }else{
      output$cond_save_es2<-renderUI(
        h5("select a number of ES to be mapped")
      )
    }
  })

  observeEvent(input$save_es,{
    siteID<-siteID()
    study_area<-study_area()



    study_area$siteSTATUS<-"round1_open"
    study_area$siteNMAPPING<-as.integer(input$n_es)

    selected_es <- es_descr[input$es_descr_rows_selected,  ]
    selected_es$siteID<-rep(siteID,nrow(selected_es))
    #save selected es in tab
    # file <-paste("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/setup_230710/", Sys.Date(), "_es.csv", sep = "")
    # write.csv(selected_es, file, row.names = FALSE)
    # bq_table_upload(es_study, selected_es)
    insert_upload_job("eu-wendy", "integrated_wendy", "es_study", selected_es)

    ## save study area info
    study_area<-as.data.frame(study_area%>%st_drop_geometry())

    insert_upload_job("eu-wendy", "integrated_wendy", "study_site", study_area)

    ### clean ui
    removeUI(selector = "#es_descr")
    removeUI(selector = "#save_es")
    removeUI(selector = "#cond_save_es")
    removeUI(selector = "#n_es")
    removeUI(selector = "#cond_save_es2")

    output$cond_save_es3<-renderUI(
      h4(
        paste0("Your study area has been saved please note the following study id: ",
               study_area$siteID,
               " which is used for the mapping of ecosystem services and your study management")))



  })

 ############# tab 2 check and modify status
  studies<-eventReactive(input$studID_in,{
    studies<-tbl(con,"study_site")
    studies<-studies%>%collect()
  })

  actual_stat<-eventReactive(input$studID_in,{
    req(studies)
    studies<-studies()
    actual_stat<-as.character(studies%>%filter(siteID == input$studID_in)%>%
                                arrange(desc(as.POSIXct(siteCREATETIME)))%>%first()%>%
                                select(siteSTATUS))

  })

  observeEvent(input$studID_in,{
    req(actual_stat)
    studies<-studies()
    actual_stat<-actual_stat()

    if(input$studID_in %in% studies$siteID){
      output$cond_b2<-renderUI({
        tagList(
          h5(paste0("The study ",input$studID_in, " has status: ",actual_stat)),
          actionButton("check_study","check status"),
          uiOutput("cond_b3")
          )
      })
    }else{
      output$cond_b2<-renderUI({
        h5("invalid study id")
      })
    }
  })

  stats<-eventReactive(input$check_study,{
    actual_stat<-actual_stat()
    if(actual_stat == "round1_open"){
      stats<-tbl(con, "es_mappingR1")
      a<-as.character(input$studID_in)
      stats<-stats%>%filter(siteID == a & poss_mapping == "Yes")%>%
        group_by(esID)%>%summarise(n_maps = n_distinct(userID))%>%collect()
    }else if(actual_stat == "round2_open"){
      stats<-tbl(con, "es_mappingR2")
      a<-as.character(input$studID_in)
      stats<-stats%>%filter(siteID == a & map_adjust == "Yes")%>%
        group_by(esID)%>%summarise(n_maps = n_distinct(userID))%>%collect()

    }
  })

  observeEvent(input$check_study,{
    req(stats)
    actual_stat<-actual_stat()
    stats<-stats()
    removeUI(selector = "#studID_in")

    if(actual_stat == "round1_open"){

      output$cond_b3<-renderUI({
        ui=tagList(
          tableOutput('status_r1')%>%withSpinner(),
          uiOutput("cond_b4")
        )
      })
      output$status_r1 <- renderTable({as.data.frame(stats)})
      removeUI(selector = "#check_study")
      if(min(stats$n_maps)<2){
        output$cond_b4<-renderUI({
          "To close round 1, at least 2 maps per ES are needed"
        })
      }else{
        output$cond_b4<-renderUI({
          tagList(
            h5("If you want, you can now close round 1"),
            actionButton("close1","close round 1")
          )

        })
      }


    ## status 2
    }else if(actual_stat == "round1_closed"){
      output$cond_b3<-renderUI({
        ui=tagList(
          h5("Postprocessing of R1 is done, you can open R2"),
          actionButton("open2","open round2")
        )
      })
    }else if(actual_stat == "round2_open"){
      output$cond_b3<-renderUI({
        ui=tagList(
          tableOutput('status_r2')%>%withSpinner(),
          uiOutput("cond_b4")
        )
      })
      output$status_r2 <- renderTable({as.data.frame(stats)})
      removeUI(selector = "#check_study")

      if(min(stats$n_maps)<2){
        output$cond_b4<-renderUI({
          "To close round 2, at least 2 maps per ES are needed"
        })
      }else{
        output$cond_b4<-renderUI({
          tagList(
            h5("If you want, you can now close round 2"),
            actionButton("close2","close round 2")
          )

        })
      }
    }else{
      output$cond_b3<-renderUI({
        ui=tagList(
          h5("Round 2 is closed and postprocessed")
        )
      })
    }

  })

  ###close round1
  observeEvent(input$close1,{
    stats<-stats()
    studies<-studies()
    #create CV map per ES of projects
    img_assetid_ind<-"projects/eu-wendy/assets/es_mapping/es_map_ind"
    img_assetid_all <- "projects/eu-wendy/assets/es_mapping/es_map_all/"

    ## study geom from siteID
    stud_geom_id <- paste0('projects/eu-wendy/assets/study_sites/', input$studID_in)
    stud_geom <- ee$FeatureCollection(stud_geom_id)
    sf_stud_geom<-ee_as_sf(stud_geom)
    coords <- st_coordinates(sf_stud_geom)
    coords<-as.data.frame(coords[,c(1,2)])

    geometry <- ee$Geometry$Rectangle(
      coords = c(min(coords$X), min(coords$Y), max(coords$X), max(coords$Y)),
      proj = "EPSG:4326",
      geodesic = FALSE
    )


    for(n in 1: nrow(stats)){
      esID<-stats$esID[n]

      es_col<-ee$ImageCollection(img_assetid_ind)$select("probability")$filter(
        ee$Filter$eq("esID",esID))$filter(
          ee$Filter$eq("siteID",as.character(input$studID_in)))$filter(ee$Filter$eq("delphi_round",1))


      es_mean <- es_col$reduce(ee$Reducer$mean())$reproject(
        crs= 'EPSG:4326',
        scale= 100
      )

      es_stdDev = es_col$reduce(ee$Reducer$stdDev())$reproject(
        crs= 'EPSG:4326',
        scale= 100
      )
      es_coef_var = es_stdDev$divide(es_mean)

      # img_id<-paste0(img_assetid,"test")
      img_id<-paste0(img_assetid_all, as.character(input$studID_in),"_",esID,"CV_1")
      #
      # #set features of img
      es_coef_var <- es_coef_var$set('esID', esID,
                                   'siteID', as.character(input$studID_in),
                                   'delphi_round', 1,
                                   'stats', "CV")

      task_img <- ee_image_to_asset(
        # image = es_coef_var$select("probability_stdDev"),
        image = es_coef_var,
        assetId = img_id,
        overwrite = T,
        region = geometry
      )

      task_img$start()
    }

    #overwrite status in DB
    stud_new<-studies%>%filter(siteID == input$studID_in)
    stud_new$siteSTATUS <-"round1_closed"
    stud_new$siteCREATETIME<-Sys.time()
    # Execute the update query
    insert_upload_job("eu-wendy", "integrated_wendy", "study_site", stud_new)

  })

  ###open round2
  observeEvent(input$open2,{
    studies<-studies()
    stud_new<-studies%>%filter(siteID == input$studID_in)%>%first()
    stud_new$siteSTATUS <-"round2_open"
    stud_new$siteCREATETIME<-Sys.time()
    # Execute the update query
    insert_upload_job("eu-wendy", "integrated_wendy", "study_site", stud_new)

  })

  ###close round2
  observeEvent(input$close2,{
    studies<-studies()

    img_assetid_ind<-"projects/eu-wendy/assets/es_mapping/es_map_ind"
    img_assetid_all <- "projects/eu-wendy/assets/es_mapping/es_map_all/"

    ## study geom from siteID
    stud_geom_id <- paste0('projects/eu-wendy/assets/study_sites/', input$studID_in)
    stud_geom <- ee$FeatureCollection(stud_geom_id)
    sf_stud_geom<-ee_as_sf(stud_geom)
    coords <- st_coordinates(sf_stud_geom)
    coords<-as.data.frame(coords[,c(1,2)])

    geometry <- ee$Geometry$Rectangle(
      coords = c(min(coords$X), min(coords$Y), max(coords$X), max(coords$Y)),
      proj = "EPSG:4326",
      geodesic = FALSE
    )


    for(n in 1: nrow(stats)){
      esID<-stats$esID[n]

      es_col<-ee$ImageCollection(img_assetid_ind)$select("probability")$filter(
        ee$Filter$eq("esID",esID))$filter(
          ee$Filter$eq("siteID",as.character(input$studID_in)))$filter(ee$Filter$eq("delphi_round",2))


      es_mean <- es_col$reduce(ee$Reducer$mean())$reproject(
        crs= 'EPSG:4326',
        scale= 100
      )

      es_stdDev = es_col$reduce(ee$Reducer$stdDev())$reproject(
        crs= 'EPSG:4326',
        scale= 100
      )
      es_coef_var = es_stdDev$divide(es_mean)
      img_id<-paste0(img_assetid_all, as.character(input$studID_in),"_",esID,"CV_2")
      #
      # #set features of img
      es_coef_var <- es_coef_var$set('esID', esID,
                                     'siteID', as.character(input$studID_in),
                                     'delphi_round', 2,
                                     'stats', "CV")

      task_img <- ee_image_to_asset(
        # image = es_coef_var$select("probability_stdDev"),
        image = es_coef_var,
        assetId = img_id,
        overwrite = T,
        region = geometry
      )

      task_img$start()
    }

    stud_new<-studies%>%filter(siteID == input$studID_in)%>%first()
    stud_new$siteSTATUS <-"round2_closed"
    stud_new$siteCREATETIME<-Sys.time()
    # Execute the update query
    insert_upload_job("eu-wendy", "integrated_wendy", "study_site", stud_new)

  })

}

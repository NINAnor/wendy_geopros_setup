function(input, output, session) {

  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p1A")
  hideTab(inputId = "inTabset", target = "p1B")


  #check user name
  observeEvent(input$check1,{
    if(input$user_name %in% admins$admin_name){
      output$cond0<-renderUI({
        actionButton("login","login")

      })
      removeUI(
        selector = "#check1"
      )
      removeUI(
        selector = "#user_name"
      )
    }else{
      output$cond0<-renderUI({
        "you do not have access to the tool, plese contact admin@consite.com"
      })
    }
  })

  observeEvent(input$login,{
    updateTabsetPanel(session, "inTabset",
                      selected = "p1")
    hideTab(inputId = "inTabset",
            target = "p0")
    showTab(inputId = "inTabset", target = "p1")
  })


  output$cond_b1<-renderUI({
    validate(
      need(input$sitetype != "", 'choose a site type'),
      need(input$site_nat_name != '', 'Provide a site name'),
      need(input$site_descr != '', 'Provide a site description')
    )
    tagList(
      actionButton('sub1', 'confirm', class='btn-primary')

    )

  })

  observeEvent(input$sub1,{
    updateTabsetPanel(session, "inTabset",
                      selected = "p1A")
    hideTab(inputId = "inTabset",
            target = "p1")
    showTab(inputId = "inTabset", target = "p1A")

    if(input$sitetype == "onshore"){
      output$type_dep<-renderUI(
        tagList(
          textOutput("cntr_text"),
          mapedit::selectModUI("map_sel_cntry"),
          uiOutput("cond_cntry")
        )
      )
    }else if(input$sitetype == "offshore"){
      output$type_dep<-renderUI(
        tagList(
          mapedit::editModUI("sel_offshore"),
          htmlOutput("overlay_result2"),
          uiOutput("btn2"),
        )
      )
    }
  })

  output$cntr_text<-renderText("Select your country of interest")
  #country map

      cntry_sel <- callModule(module=selectMod,
                        leafmap=map_cntr,
                        id="map_sel_cntry")

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
            paste0("Your area is ",area, " km2, Save your area")

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

  ## save geom in bq

  siteID<-eventReactive(input$savepoly,{
    nchar<-round(runif(1,8,13),0)
    siteID<-stri_rand_strings(1, nchar, pattern = "[A-Za-z0-9]")
  })


  observeEvent(input$savepoly,{

    if(input$sitetype=="onshore"){

      removeUI(selector = paste0("#sel_onshore","-map"))
      removeUI(
        selector = "#overlay_result")

    }else{
      removeUI(selector = paste0("#sel_offshore","-map"))
      removeUI(
        selector = "#overlay_result2")
    }

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
    show_modal_spinner(
      text = "update data base"
    )
    updateTabsetPanel(session, "inTabset",
                      selected = "p1B")
    hideTab(inputId = "inTabset",
            target = "p1A")
    showTab(inputId = "inTabset", target = "p1B")

    req(siteID)
    siteID<-siteID()



    if(input$sitetype=="onshore"){
      study_area<-rv$onshore_sel()$finished
      sel_country<-sel_country()
      study_area<-study_area%>%select()
      study_area$siteID<-siteID
      study_area$projID<-"pareus"
      study_area$cntrID<-sel_country$ISO3_CODE
    }else{
      study_area<-rv$offshore_sel()$finished
      study_area<-study_area%>%select()
      study_area$cntrID<-"off"
    }

    study_area$siteLANG <- as.character("ENGLISH")
    study_area$siteN_es<-input$n_es

    study_area$siteNAME<-input$site_nat_name
    study_area$siteDESCR<-input$site_descr
    study_area$siteSTATUS<-as.integer(1)
    study_area$siteTYPE <-as.character(input$sitetype)
    study_area$siteAREAkm2<-as.integer(round(as.numeric(st_area(study_area))/1000000,0))
    study_area$siteCREATETIME<-Sys.time()
    study_area$siteCREATOR <-input$user_name
    polygons<-study_area%>%st_drop_geometry()
    polygons$geometry<-st_as_text(study_area$geometry)
    # #
    # # #save it on bq
    poly_table = bq_table(project = project, dataset = dataset, table = 'studSITE')
    bq_table_upload(x = poly_table, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

    remove_modal_spinner()


    ### clean ui

    output$id_note<-renderUI(
      tagList(
        br(),
        # "Your study has been saved. Please note down the following study id: ",
        HTML(paste0("Your study has been saved. Please note down the following study id: <strong>",siteID,"</strong> <br> You need it to invite participants to map ecosystem services and to manage your study"),
             # " You need it to invite participants to map ecosystem services and to manage your study"
      )
      # paste0("Your study area has been saved. Please note the following study id: ",
      #             siteID,
      #             " which is used for the mapping of ecosystem services and your study management")
    )
    )
  })

 ############# tab 2 check and modify status
  studies<-eventReactive(input$checkstud,{
    studies<-tbl(con,"studSITE")
    studies<-studies%>%collect()
  })
  #
  # actual_stat<-eventReactive(input$studID_in,{
  #   req(studies)
  #   studies<-studies()
  #   actual_stat<-as.character(studies%>%filter(siteID == input$studID_in)%>%
  #                               arrange(desc(as.POSIXct(siteCREATETIME)))%>%first()%>%
  #                               select(siteSTATUS))
  #
  # })
  #
  # observeEvent(input$studID_in,{
  #   req(actual_stat)
  #   studies<-studies()
  #   actual_stat<-actual_stat()
  #
  #   if(input$studID_in %in% studies$siteID){
  #     output$cond_b2<-renderUI({
  #       tagList(
  #         h5(paste0("The study ",input$studID_in, " has status: ",actual_stat)),
  #         actionButton("check_study","check status"),
  #         uiOutput("cond_b3")
  #         )
  #     })
  #   }else{
  #     output$cond_b2<-renderUI({
  #       h5("invalid study id")
  #     })
  #   }
  # })
  #
  # stats<-eventReactive(input$check_study,{
  #   actual_stat<-actual_stat()
  #   if(actual_stat == "round1_open"){
  #     stats<-tbl(con, "es_mappingR1")
  #     a<-as.character(input$studID_in)
  #     stats<-stats%>%filter(siteID == a & poss_mapping == "Yes")%>%
  #       group_by(esID)%>%summarise(n_maps = n_distinct(userID))%>%collect()
  #   }else if(actual_stat == "round2_open"){
  #     stats<-tbl(con, "es_mappingR2")
  #     a<-as.character(input$studID_in)
  #     stats<-stats%>%filter(siteID == a & map_adjust == "Yes")%>%
  #       group_by(esID)%>%summarise(n_maps = n_distinct(userID))%>%collect()
  #
  #   }
  # })
  #
  # observeEvent(input$check_study,{
  #   req(stats)
  #   actual_stat<-actual_stat()
  #   stats<-stats()
  #   removeUI(selector = "#studID_in")
  #
  #   if(actual_stat == "round1_open"){
  #
  #     output$cond_b3<-renderUI({
  #       ui=tagList(
  #         tableOutput('status_r1')%>%withSpinner(),
  #         uiOutput("cond_b4")
  #       )
  #     })
  #     output$status_r1 <- renderTable({as.data.frame(stats)})
  #     removeUI(selector = "#check_study")
  #     if(min(stats$n_maps)<2){
  #       output$cond_b4<-renderUI({
  #         "To close round 1, at least 2 maps per ES are needed"
  #       })
  #     }else{
  #       output$cond_b4<-renderUI({
  #         tagList(
  #           h5("If you want, you can now close round 1"),
  #           actionButton("close1","close round 1")
  #         )
  #
  #       })
  #     }
  #
  #
  #   ## status 2
  #   }else if(actual_stat == "round1_closed"){
  #     output$cond_b3<-renderUI({
  #       ui=tagList(
  #         h5("Postprocessing of R1 is done, you can open R2"),
  #         actionButton("open2","open round2")
  #       )
  #     })
  #   }else if(actual_stat == "round2_open"){
  #     output$cond_b3<-renderUI({
  #       ui=tagList(
  #         tableOutput('status_r2')%>%withSpinner(),
  #         uiOutput("cond_b4")
  #       )
  #     })
  #     output$status_r2 <- renderTable({as.data.frame(stats)})
  #     removeUI(selector = "#check_study")
  #
  #     if(min(stats$n_maps)<2){
  #       output$cond_b4<-renderUI({
  #         "To close round 2, at least 2 maps per ES are needed"
  #       })
  #     }else{
  #       output$cond_b4<-renderUI({
  #         tagList(
  #           h5("If you want, you can now close round 2"),
  #           actionButton("close2","close round 2")
  #         )
  #
  #       })
  #     }
  #   }else{
  #     output$cond_b3<-renderUI({
  #       ui=tagList(
  #         h5("Round 2 is closed and postprocessed")
  #       )
  #     })
  #   }
  #
  # })
  #
  # ###close round1
  # observeEvent(input$close1,{
  #   stats<-stats()
  #   studies<-studies()
  #   #create CV map per ES of projects
  #   img_assetid_ind<-"projects/eu-wendy/assets/es_mapping/es_map_ind"
  #   img_assetid_all <- "projects/eu-wendy/assets/es_mapping/es_map_all/"
  #
  #   ## study geom from siteID
  #   stud_geom_id <- paste0('projects/eu-wendy/assets/study_sites/', input$studID_in)
  #   stud_geom <- ee$FeatureCollection(stud_geom_id)
  #   study_area<-ee_as_sf(stud_geom)
  #   coords <- st_coordinates(study_area)
  #   coords<-as.data.frame(coords[,c(1,2)])
  #
  #   geometry <- ee$Geometry$Rectangle(
  #     coords = c(min(coords$X), min(coords$Y), max(coords$X), max(coords$Y)),
  #     proj = "EPSG:4326",
  #     geodesic = FALSE
  #   )
  #
  #
  #   for(n in 1: nrow(stats)){
  #     esID<-stats$esID[n]
  #
  #     es_col<-ee$ImageCollection(img_assetid_ind)$select("probability")$filter(
  #       ee$Filter$eq("esID",esID))$filter(
  #         ee$Filter$eq("siteID",as.character(input$studID_in)))$filter(ee$Filter$eq("delphi_round",1))
  #
  #
  #     es_mean <- es_col$reduce(ee$Reducer$mean())$reproject(
  #       crs= 'EPSG:4326',
  #       scale= 100
  #     )
  #
  #     es_stdDev = es_col$reduce(ee$Reducer$stdDev())$reproject(
  #       crs= 'EPSG:4326',
  #       scale= 100
  #     )
  #     es_coef_var = es_stdDev$divide(es_mean)
  #
  #     # img_id<-paste0(img_assetid,"test")
  #     img_id<-paste0(img_assetid_all, as.character(input$studID_in),"_",esID,"CV_1")
  #     #
  #     # #set features of img
  #     es_coef_var <- es_coef_var$set('esID', esID,
  #                                  'siteID', as.character(input$studID_in),
  #                                  'delphi_round', 1,
  #                                  'stats', "CV")
  #
  #     task_img <- ee_image_to_asset(
  #       # image = es_coef_var$select("probability_stdDev"),
  #       image = es_coef_var,
  #       assetId = img_id,
  #       overwrite = T,
  #       region = geometry
  #     )
  #
  #     task_img$start()
  #   }
  #
  #   #overwrite status in DB
  #   stud_new<-studies%>%filter(siteID == input$studID_in)
  #   stud_new$siteSTATUS <-"round1_closed"
  #   stud_new$siteCREATETIME<-Sys.time()
  #   # Execute the update query
  #   insert_upload_job("eu-wendy", "integrated_wendy", "study_site", stud_new)
  #
  # })
  #
  # ###open round2
  # observeEvent(input$open2,{
  #   studies<-studies()
  #   stud_new<-studies%>%filter(siteID == input$studID_in)%>%first()
  #   stud_new$siteSTATUS <-"round2_open"
  #   stud_new$siteCREATETIME<-Sys.time()
  #   # Execute the update query
  #   insert_upload_job("eu-wendy", "integrated_wendy", "study_site", stud_new)
  #
  # })
  #
  # ###close round2
  # observeEvent(input$close2,{
  #   studies<-studies()
  #
  #   img_assetid_ind<-"projects/eu-wendy/assets/es_mapping/es_map_ind"
  #   img_assetid_all <- "projects/eu-wendy/assets/es_mapping/es_map_all/"
  #
  #   ## study geom from siteID
  #   stud_geom_id <- paste0('projects/eu-wendy/assets/study_sites/', input$studID_in)
  #   stud_geom <- ee$FeatureCollection(stud_geom_id)
  #   study_area<-ee_as_sf(stud_geom)
  #   coords <- st_coordinates(study_area)
  #   coords<-as.data.frame(coords[,c(1,2)])
  #
  #   geometry <- ee$Geometry$Rectangle(
  #     coords = c(min(coords$X), min(coords$Y), max(coords$X), max(coords$Y)),
  #     proj = "EPSG:4326",
  #     geodesic = FALSE
  #   )
  #
  #
  #   for(n in 1: nrow(stats)){
  #     esID<-stats$esID[n]
  #
  #     es_col<-ee$ImageCollection(img_assetid_ind)$select("probability")$filter(
  #       ee$Filter$eq("esID",esID))$filter(
  #         ee$Filter$eq("siteID",as.character(input$studID_in)))$filter(ee$Filter$eq("delphi_round",2))
  #
  #
  #     es_mean <- es_col$reduce(ee$Reducer$mean())$reproject(
  #       crs= 'EPSG:4326',
  #       scale= 100
  #     )
  #
  #     es_stdDev = es_col$reduce(ee$Reducer$stdDev())$reproject(
  #       crs= 'EPSG:4326',
  #       scale= 100
  #     )
  #     es_coef_var = es_stdDev$divide(es_mean)
  #     img_id<-paste0(img_assetid_all, as.character(input$studID_in),"_",esID,"CV_2")
  #     #
  #     # #set features of img
  #     es_coef_var <- es_coef_var$set('esID', esID,
  #                                    'siteID', as.character(input$studID_in),
  #                                    'delphi_round', 2,
  #                                    'stats', "CV")
  #
  #     task_img <- ee_image_to_asset(
  #       # image = es_coef_var$select("probability_stdDev"),
  #       image = es_coef_var,
  #       assetId = img_id,
  #       overwrite = T,
  #       region = geometry
  #     )
  #
  #     task_img$start()
  #   }
  #
  #   stud_new<-studies%>%filter(siteID == input$studID_in)%>%first()
  #   stud_new$siteSTATUS <-"round2_closed"
  #   stud_new$siteCREATETIME<-Sys.time()
  #   # Execute the update query
  #   insert_upload_job("eu-wendy", "integrated_wendy", "study_site", stud_new)
  #
  # })

}

function(input, output, session) {
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")

  observeEvent(input$projtype,{
    if(input$projtype == "onshore"){
      output$type_dep<-renderUI(
        tagList(
          textOutput("cntr_text"),
          mapedit::selectModUI("map_sel_cntry"),
          actionButton("save_countr","save country"),
          br(),
          conditionalPanel(condition = "cntry_sel != NULL",
            uiOutput("cntry_dep")

          )

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
  })

  output$cntr_text<-renderText("Select your country of interest")
  #country map

      cntry_sel <- callModule(module=selectMod,
                        leafmap=map_cntr,
                        id="map_sel_cntry")

      # cntry_sel<-mapedit::selectMap(map_cntr)

      offshore_sel<-callModule(module = editMod,
                             leafmap=map_coast,
                             id="sel_offshore")

  # offshore_sel<-mapedit::editMap(map_coast)

  #reactive values to store mapping
  rv<-reactiveValues(
    onshore_sel = reactive({})
  )


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
                     polygonOptions = F,
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



  })

  ## for onshore:: a helper function to check if poly is inside country
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

        if(area>15000){
          output$overlay_result <- renderText({
            paste0("<font color=\"#FF0000\"> <li>Your area is ",area ," km2, and thus too big, please draw a smaller area of max 15`000 km2<li/></font>")
          })
          removeUI(
            selector = paste0("#savepoly"))

        }else if(area<500){
          output$overlay_result <- renderText({
            paste0("<font color=\"#FF0000\"> <li>Your area is ",area, " km2, and thus too small, please draw a bigger area of min 500 km2<li/></font>")
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

  ## for offshore:: a helper function to check poly area
  # observe({
  #   req(offshore_sel)
  #
  #   rectangles <- offshore_sel()$finished
  #
  #   n_poly<-nrow(as.data.frame(rectangles))
  #
  #   if(n_poly==1){
  #     n_within<-nrow(as.data.frame(st_within(rectangles,coast)))
  #
  #     if(n_within<n_poly){
  #       output$overlay_result2 <- renderText({
  #         paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely inside offshore areas<li/></font>")
  #       })
  #       removeUI(
  #         selector = paste0("#savepoly"))
  #     }else{
  #       area<-round(as.numeric(st_area(rectangles))/1000000,0)
  #
  #       if(area>15000){
  #         output$overlay_result2 <- renderText({
  #           paste0("<font color=\"#FF0000\"> <li>Your area is ",area ," km2, and thus too big, please draw a smaller area of max 15`000 km2<li/></font>")
  #         })
  #         removeUI(
  #           selector = paste0("#savepoly"))
  #
  #       }else if(area<500){
  #         output$overlay_result2 <- renderText({
  #           paste0("<font color=\"#FF0000\"> <li>Your area is ",area, " km2, and thus too small, please draw a bigger area of min 500 km2<li/></font>")
  #         })
  #         removeUI(
  #           selector = paste0("#savepoly"))
  #
  #       }else{
  #         output$btn2<-renderUI(
  #           actionButton("savepoly","save area")
  #         )
  #         output$overlay_result2 <- renderText({
  #           paste0("Your area is ",area, " km2, Save your area now")
  #
  #         })
  #       }
  #
  #     }
  #
  #   }else if(n_poly>1){
  #     output$overlay_result2 <- renderText({
  #       paste("<font color=\"#FF0000\"> <li>Remove areas, just one area allowed<li/></font>")
  #     })
  #     removeUI(
  #       selector = paste0("#savepoly"))
  #
  #   }else if(n_poly==0){
  #     output$overlay_result2 <- renderText({
  #       paste("<font color=\"#FF0000\"><li>Please draw one area<li/></font>")
  #     })
  #     removeUI(
  #       selector = paste0("#savepoly"))
  #
  #   }
  #
  # })


  observeEvent(input$savepoly,{
    study_area<-rv$onshore_sel()$finished
    sel_country<-sel_country()

    req(study_area, cancelOutput = FALSE)
    # study_area<-onshore_sel$finished
    study_area<-study_area%>%select()
    study_area$siteID<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")

    study_area$siteTYPE <-input$projtype
    study_area$siteNAME <-input$name
    study_area$siteDESCR <-input$descr

    study_area$siteCREATOR <-Sys.getenv("USERNAME")

    study_area$cntrID<-sel_country$ISO3_CODE

    study_area$siteSTATUS<-"created_active"

    study_area$siteCREATETIME<-Sys.time()

    ee_study<-study_area
    ee_study$siteCREATETIME<-as.character(ee_study$siteCREATETIME)
    ee_study<-sf_as_ee(ee_study)

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

    # geo <- sf_geojson(study_area, atomise = TRUE)
    # st_write(study_area,"test.geojson")
    # geo1 <- sf_geojson(study_area, atomise = FALSE)
    # geo_js_df <- as.data.frame(geojson_wkt(geo))
    # str(geo)
    #
    # players_table = bq_table(project = "rgee-381312", dataset = "data_base", table = "test_new")
    # bq_table_upload(players_table, geo_js_df)

    ## upload as bq spatial table to WENDY google cloud

  })





}

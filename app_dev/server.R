function(input, output, session) {

  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p1A")
  hideTab(inputId = "inTabset", target = "p1B")


  #check user name
  admins<-eventReactive(input$check1,{
    admins<-tbl(con_admin,"study_admin")
    admins<-admins%>%collect()%>%filter(active == TRUE)
  })

  observeEvent(input$check1,{
    show_modal_spinner(
      text = "check your access"
    )
    req(admins)
    admins<-admins()
    if(input$user_name %in% admins$userName){
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
        h5("You do not have access to create a new study. Please use this form to request a user.")
      })
    }
    remove_modal_spinner()
  })



  sel_inst<-eventReactive(input$login,{
    req(admins)
    admins<-admins()
    sel_inst<-admins%>%filter(userName == input$user_name)%>%select(institution)

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
      need(input$site_nat_name != '', 'Provide a descriptive site title'),
      need(input$siteID != '', 'Provide a site ID (no blank spaces)')
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
          bslib::value_box(
            title="",
            value = "",
            h4(textOutput("cntr_text")),
            mapedit::selectModUI("map_sel_cntry"),
            uiOutput("cond_cntry"),
            br(),
            showcase = bs_icon("globe-europe-africa"),
            theme = value_box_theme(bg = "white", fg = "black")
          ),

        )
      )
    }else if(input$sitetype == "offshore"){
      output$type_dep<-renderUI(
        tagList(
          bslib::value_box(
            title="",
            value = "",
            h4("Draw your study region as a rectangle within the maritime zones"),
            br(),
            h4("Make sure that you define the study region in a way that your focus area for wind energy development is in the center and that you have approx. min of 30km between wind development area and closest study border."),

            mapedit::editModUI("map_sel_offshore"),
            htmlOutput("overlay_result2"),
            uiOutput("btn2"),
            br(),
            showcase = bs_icon("globe-europe-africa"),
            theme = value_box_theme(bg = "white", fg = "black")
          ),
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
          h5("please select a country. It is not possible to select more than one country")
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
                             id="map_sel_offshore")

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
        bslib::value_box(
          title = "",
          value = "",
          h4("Draw your study region as a rectangle within the country borders"),
          br(),
          h4("Make sure that you define the study region in a way that your focus area for wind energy development is in the center and that you have approx. min of 30km between wind development area and closest study border."),
        showcase = bs_icon("exclamation-octagon-fill"),
        theme = value_box_theme(bg = orange, fg = "black")
      ),
        "",
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



  observeEvent(input$savepoly,{

    show_modal_spinner(
      text = "save your study area on our servers"
    )
    updateTabsetPanel(session, "inTabset",
                      selected = "p1B")
    hideTab(inputId = "inTabset",
            target = "p1A")
    showTab(inputId = "inTabset", target = "p1B")

    if(input$sitetype=="onshore"){
      study_area<-rv$onshore_sel()$finished
      sel_country<-sel_country()
      study_area<-study_area%>%select()
      # study_area$siteID<-siteID
      study_area$siteID<-input$siteID
      study_area$projID<-"eu-wendy"
      study_area$cntrID<-sel_country$ISO3_CODE
    }else{
      study_area<-rv$offshore_sel()$finished
      study_area<-study_area%>%select()
      study_area$cntrID<-"off"
    }


    study_area$PART_N_ES<-as.numeric(input$n_es)
    study_area$NAME<-input$site_nat_name
    study_area$STATUS<-as.integer(1)
    study_area$TYPE <-as.character(input$sitetype)
    study_area$AREAkm2<-as.integer(round(as.numeric(st_area(study_area))/1000000,0))
    study_area$CREATETIME<-Sys.time()
    study_area$CREATOR <-input$user_name
    study_area$RESP_INSTITUT
    study_area$LANG<-input$language
    polygons<-study_area%>%st_drop_geometry()
    polygons$geometry<-st_as_text(study_area$geometry)
    # # #save it on bq
    poly_table = bq_table(project = project, dataset = dataset, table = 'study_site')
    bq_table_upload(x = poly_table, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')


    remove_modal_spinner()



  })

  observeEvent(input$save_es,{

#
#     req(siteID)
#     siteID<-siteID()






    ### clean ui

    output$id_note<-renderUI(
      tagList(
        br(),
        # "Your study has been saved. Please note down the following study id: ",
        HTML(paste0("Your study has been saved. Please note down the following study id: <strong>",input$siteID,"</strong> <br> You need it to invite participants to map ecosystem services and to manage your study"),
      )
    )
    )
  })

  mod_manage_study_server("manage_projects")
}

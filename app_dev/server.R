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
          actionButton("save_offshore","save area"),
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
  #   req(sel_country)
  #   sel_country<-sel_country()
  #
  #
  #   rectangles <- offshore_sel()$finished
  #
  #   n_poly<-nrow(as.data.frame(rectangles))
  #
  #   if(n_poly==1){
  #     n_within<-nrow(as.data.frame(st_within(rectangles,coast)))
  #
  #     if(n_within<n_poly){
  #       output$overlay_result <- renderText({
  #         paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely inside offshore areas<li/></font>")
  #       })
  #       removeUI(
  #         selector = paste0("#savepoly"))
  #     }else{
  #       area<-round(as.numeric(st_area(rectangles))/1000000,0)
  #
  #       if(area>15000){
  #         output$overlay_result <- renderText({
  #           paste0("<font color=\"#FF0000\"> <li>Your area is ",area ," km2, and thus too big, please draw a smaller area of max 15`000 km2<li/></font>")
  #         })
  #         removeUI(
  #           selector = paste0("#savepoly"))
  #
  #       }else if(area<500){
  #         output$overlay_result <- renderText({
  #           paste0("<font color=\"#FF0000\"> <li>Your area is ",area, " km2, and thus too small, please draw a bigger area of min 500 km2<li/></font>")
  #         })
  #         removeUI(
  #           selector = paste0("#savepoly"))
  #
  #       }else{
  #         output$btn1<-renderUI(
  #           actionButton("savepoly","save area")
  #         )
  #         output$overlay_result <- renderText({
  #           paste0("Your area is ",area, " km2, Save your area now")
  #
  #         })
  #       }
  #
  #     }
  #
  #   }else if(n_poly>1){
  #     output$overlay_result <- renderText({
  #       paste("<font color=\"#FF0000\"> <li>Remove areas, just one area allowed<li/></font>")
  #     })
  #     removeUI(
  #       selector = paste0("#savepoly"))
  #
  #   }else if(n_poly==0){
  #     output$overlay_result <- renderText({
  #       paste("<font color=\"#FF0000\"><li>Please draw one area<li/></font>")
  #     })
  #     removeUI(
  #       selector = paste0("#savepoly"))
  #
  #   }
  #
  # })







}

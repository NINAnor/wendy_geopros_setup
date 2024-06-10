#' manage_study UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manage_study_ui <- function(id){
  ns <- NS(id)
  tagList(
    h5("Enter the study id you want to modify"),
    br(),
    textInput(ns("site_id"),""),
    actionButton(ns("check_study"),"check status"),
    uiOutput(ns("cond_b1"))

  )
}

#' manage_study Server Functions
#'
#' @noRd
mod_manage_study_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sites<-eventReactive(input$check_study,{
      shinybusy::show_modal_spinner(text = "fetch site status")
      sites<-tbl(con_admin, "studSITE")
      sites<-sites%>%collect()
      shinybusy::remove_modal_spinner()
      sites<-sites
    })

    sitestatus<-eventReactive(input$check_study,{
      req(sites)
      sites<-sites()


      if(input$site_id %in% sites$siteID){
        sitestatus<-as.integer(sites%>%filter(siteID==input$site_id)%>%
                                 arrange(desc(as.POSIXct(siteCREATETIME)))%>%first()%>%
                                 select(siteSTATUS))
      }else{
        sitestatus = 0
      }

      sitestatus<-sitestatus
    })

    ## for site status 1 and 3 we need to know how many maps per es are in the DB
    map_stats<-eventReactive(input$check_study,{
      sitestatus<-sitestatus()
      a<-as.character(input$site_id)
      if(sitestatus == 1){

        shinybusy::show_modal_spinner(text = "fetch map stats")
        map_stats<-tbl(con_admin, "es_mappingR1")
        map_stats<-map_stats%>%filter(siteID == a & poss_mapping == TRUE)%>%
          group_by(esID)%>%summarise(n_maps = n_distinct(userID))%>%collect()
        shinybusy::remove_modal_spinner()

      }else if(sitestatus == 3){
        shinybusy::show_modal_spinner(text = "fetch map stats")
        map_stats<-tbl(con_admin, "es_mappingR2")
        map_stats<-map_stats%>%filter(siteID == a & poss_mapping == TRUE)%>%
          group_by(esID)%>%summarise(n_maps = n_distinct(userID))%>%collect()
        shinybusy::remove_modal_spinner()

      }else{
        map_stats<-NULL
      }

      map_stats<-as.data.frame(map_stats)
    })

    ## based on the amount of maps render other UI

    observeEvent(input$check_study,{
      req(map_stats)
      sitestatus<-sitestatus()
      map_stats<-map_stats()
      removeUI(selector = "#site_id")
      # removeUI(selector = "#check_study")

      if(sitestatus == 1){

        output$cond_b1<-renderUI({
          ui=tagList(
            tableOutput('status_r1'),
            uiOutput(ns("cond_b2"))
          )
        })
        output$status_r1 <- renderTable({as.data.frame(map_stats)})
        removeUI(selector = "#check_study")
        if(min(map_stats$n_maps)<2){
          output$cond_b2<-renderUI({
            "To close round 1, at least 2 maps per ES are needed"
          })
        }else{
          output$cond_b1<-renderUI({
            tagList(
              h5("If you want, you can now close round 1"),
              actionButton(ns("close1"),"close round 1"),
              uiOutput(ns("fin_process1"))
            )

          })
        }


      ## status 2
      }else if(sitestatus == 2){
        output$cond_b1<-renderUI({
          ui=tagList(
            h5("Postprocessing of R1 is done, you can open R2"),
            actionButton(ns("open2"),"open round2")
          )
        })
      }else if(sitestatus == 3){
        output$cond_b1<-renderUI({
          ui=tagList(
            h5("Round 2 is running"),
            tableOutput('status_r2'),
            uiOutput(ns("cond_b2"))
          )
        })
        output$status_r2 <- renderTable({as.data.frame(map_stats)})
        removeUI(selector = "#check_study")

        if(min(map_stats$n_maps)<2){
          output$cond_b1<-renderUI({
            "To close round 2, at least 2 maps per ES are needed"
          })
        }else{
          output$cond_b1<-renderUI({
            tagList(
              h5("If you want, you can now close round 2"),
              actionButton(ns("close2"),"close round 2")
            )

          })
        }
      }else if(sitestatus == 4){
        output$cond_b1<-renderUI({
          ui=tagList(
            h5("Round 2 is closed and postprocessed")
          )
        })
      }else{
        output$cond_b1<-renderUI({
          ui=tagList(
            h5("Invalid ID")
          )
        })

      }

    })

    ### server logic for "closeR1"
    ## the ind maps R1 of the selected siteID have to be merged to cv per es map
    ## status of projects should be set to 2
    observeEvent(input$close1,{
      req(map_stats)
      req(sites)

      map_stats<-map_stats()
      sites<-sites()

      shinybusy::show_modal_spinner(text = "calculate map statistics & save maps")
      objects<-gcs_list_objects("ind_es")
      for(n in 1: nrow(map_stats)){
        esID<-map_stats$esID[n]
        pattern<-paste0(input$site_id,"/",esID,"/")
        filtered_objects <- objects[grep(pattern, objects$name), ]
        tmp_loc<-paste0("indmap_",esID)

        dir.create(file.path(tmp_loc))

        for (i in 1:nrow(filtered_objects)) {
          tmp_name<-paste0("gs://ind_es/",filtered_objects[i,]$name)
          gcs_get_object(tmp_name, bucket = "ind_es", saveToDisk = paste0(tmp_loc,"/",i,".tif"),parseObject = TRUE)
        }

        raster_files <- list.files(tmp_loc, pattern = "\\.tif$", full.names = TRUE)

        # Import all raster files into a list
        rasters <- lapply(raster_files, rast)

        # Combine all rasters into a single SpatRaster object
        raster_stack <- rast(rasters)

        # Calculate the mean of the SpatRaster
        mean_raster <- mean(raster_stack, na.rm = TRUE)
        cv_rast<-app(raster_stack,sd)/mean_raster

        temp_file <- tempfile(fileext = ".tif")
        writeRaster(cv_rast, filename = temp_file)

        file_name <-paste0(input$site_id,"/",esID,"_cv")
        gcs_upload(temp_file, "cv_r1", name = file_name, predefinedAcl = "bucketLevel")
        file.remove(temp_file)
        unlink(tmp_loc,recursive = T)

      }#/map stats
      shinybusy::show_modal_spinner(text = "update data base")

      new_site<-sites%>%filter(siteID==input$site_id & siteSTATUS == 1)%>%
        mutate(siteSTATUS=replace(siteSTATUS, siteSTATUS==1, 3)) %>%
        mutate(siteCREATETIME=Sys.time()) %>%
        as.data.frame()

      site_updated = bq_table(project = "eu-wendy", dataset = dataset, table = 'studSITE')
      bq_table_upload(x = site_updated, values = new_site, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

      shinybusy::remove_modal_spinner()

      removeUI(selector = "#close1")
      output$fin_process1<-renderUI({
        tagList(
          paste0("The site id ",input$site_id," is now open for the second mapping round.")
        )
      })

    })

    ### server logic for "open2"
    ## just update the DB
    observeEvent(input$open2,{
      sites<-sites()

      shinybusy::show_modal_spinner(text = "update data base")
      new_site<-sites%>%filter(siteID==input$site_id & siteSTATUS == 2)%>%
        mutate(siteSTATUS=replace(siteSTATUS, siteSTATUS==2, 3)) %>%
        mutate(siteCREATETIME=Sys.time()) %>%
        as.data.frame()

      site_updated = bq_table(project = "eu-wendy", dataset = dataset, table = 'studSITE')
      bq_table_upload(x = site_updated, values = new_site, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

      shinybusy::remove_modal_spinner()


    })

    ### server logic for "close2"
    ## calc cv maps R2 like R1
    ## with CV maps calc IAR and pdf for all es
    ## save pdf ES on gcs to be included into consite
    # observeEvent(input$close2,{
    #   map_stats<-map_stats()
    #   sites<-sites()
    #
    #   shinybusy::show_modal_spinner(text = "calculate map statistics & save maps")
    #   objects<-gcs_list_objects("ind_es_r2")
    #   for(n in 1: nrow(map_stats)){
    #     esID<-map_stats$esID[n]
    #     pattern<-paste0(input$site_id,"/",esID,"/")
    #     filtered_objects <- objects[grep(pattern, objects$name), ]
    #     tmp_loc<-paste0("indmap2_",esID)
    #
    #     dir.create(file.path(tmp_loc))
    #
    #     for (i in 1:nrow(filtered_objects)) {
    #       tmp_name<-paste0("gs://ind_es/",filtered_objects[i,]$name)
    #       gcs_get_object(tmp_name, bucket = "ind_es_r2", saveToDisk = paste0(tmp_loc,"/",i,".tif"),parseObject = TRUE)
    #     }
    #
    #     raster_files <- list.files(tmp_loc, pattern = "\\.tif$", full.names = TRUE)
    #
    #     # Import all raster files into a list
    #     rasters <- lapply(raster_files, rast)
    #
    #     # Combine all rasters into a single SpatRaster object
    #     raster_stack <- rast(rasters)
    #
    #     # Calculate the mean of the SpatRaster
    #     mean_raster <- mean(raster_stack, na.rm = TRUE)
    #     cv_rast<-app(raster_stack,sd)/mean_raster
    #
    #     temp_file <- tempfile(fileext = ".tif")
    #     writeRaster(cv_rast, filename = temp_file)
    #     file_name <-paste0(input$site_id,"/",esID,"_cv")
    #     gcs_upload(temp_file, "cv_r2", name = file_name, predefinedAcl = "bucketLevel")
    #     file.remove(temp_file)
    #     unlink(tmp_loc,recursive = T)
    #
    #     ## pdf calculation function
    #
    #
    #
    #
    #
    #   }#/map stats
    #   shinybusy::show_modal_spinner(text = "update data base")
    #
    #   new_site<-sites%>%filter(siteID==input$site_id & siteSTATUS == 3)%>%
    #     mutate(siteSTATUS=replace(siteSTATUS, siteSTATUS==3, 4)) %>%
    #     mutate(siteCREATETIME=Sys.time()) %>%
    #     as.data.frame()
    #
    #   site_updated = bq_table(project = "eu-wendy", dataset = dataset, table = 'studSITE')
    #   bq_table_upload(x = site_updated, values = new_site, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
    #
    #   shinybusy::remove_modal_spinner()
    # })


  })



}

## To be copied in the UI
# mod_manage_study_ui("manage_study_1")

## To be copied in the server
# mod_manage_study_server("manage_study_1")

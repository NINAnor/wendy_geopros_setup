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

    sitestatus<-eventReactive(input$check_study,{
      shinybusy::show_modal_spinner(text = "fetch site status")
      projects<-tbl(con_admin, "studSITE")
      projects<-projects%>%collect()
      shinybusy::remove_modal_spinner()

      if(input$site_id %in% projects$siteID){
        sitestatus<-as.integer(projects%>%filter(siteID==input$site_id)%>%
                                 arrange(desc(as.POSIXct(siteCREATETIME)))%>%first()%>%
                                 select(siteSTATUS))
      }else{
        sitestatus = NULL
      }
      sitestatus<-sitestatus
    })

    ## for site status 1 and 3 we need to know how many maps per es are in the DB
    map_stats<-eventReactive(input$check_study,{
      sitestatus<-sitestatus()
      a<-as.character(input$site_id)
      if(sitestatus == 1 ){

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
              actionButton(ns("close1"),"close round 1")
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
      }else{
        output$cond_b1<-renderUI({
          ui=tagList(
            h5("Round 2 is closed and postprocessed")
          )
        })
      }

    })

    ### server logic for "closeR1"
    ## the ind maps R1 of the gee projectID have to be merged per ES and per studyID -- CV map
    ## status of projects should be set to 2
    observeEvent(input$close1,{
      map_stats<-map_stats()
      # studies<-studies()
      #create CV map per ES of projects
      # img_assetid_ind<-"projects/eu-wendy/assets/es_mapping/es_map_ind"
      # img_assetid_all <- "projects/eu-wendy/assets/es_mapping/es_map_all/"


      #instead of loading

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

        # raster_files <- list.files(tmp_loc, pattern = "\\.tif$", full.names = TRUE)
        #
        # rasters <- lapply(raster_files, terra::rast)
        #
        # raster_stack <- terra::rast(raster_files)
        #
        # mean_raster <- mean(raster_stack, na.rm = TRUE)


      }

      #overwrite status in DB
      # stud_new<-studies%>%filter(siteID == input$studID_in)
      # stud_new$siteSTATUS <-as.integer(2)
      # stud_new$siteCREATETIME<-Sys.time()
      # # Execute the update query
      # insert_upload_job("eu-wendy", "integrated_wendy", "study_site", stud_new)

    })



  })





  ### server logic for "openR2"
  ## the status of the DB should be changed to 3

  ### server logic for "closeR2"
  ## the ind maps R2 of the gee projectID have to be merged per ES and per studyID -- CV map
  ## status of projects should be set to 4

}

## To be copied in the UI
# mod_manage_study_ui("manage_study_1")

## To be copied in the server
# mod_manage_study_server("manage_study_1")

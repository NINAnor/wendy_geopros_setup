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
      sites<-tbl(con_admin, "study_site")
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
      print(sitestatus)
      sitestatus<-sitestatus
    })

    ## for site status 1 and 3 we need to know how many maps per es are in the DB
    map_stats<-eventReactive(input$check_study,{
      sitestatus<-sitestatus()
      a<-as.character(input$site_id)
      if(sitestatus == 1 | sitestatus == 3){

        shinybusy::show_modal_spinner(text = "fetch map stats")
        map_stats<-tbl(con_admin, "es_mappingR1")
        map_stats<-map_stats%>%filter(siteID == a & poss_mapping == TRUE)%>%
          group_by(esID)%>%summarise(n_maps = n_distinct(userID))%>%collect()
        shinybusy::remove_modal_spinner()

      }else{
        map_stats<-NULL
      }
      #print(map_stats)
      map_stats<-as.data.frame(map_stats)
    })

    ## based on the amount of maps render other UI

    observeEvent(input$check_study,{
      req(map_stats)
      sitestatus<-sitestatus()
      map_stats<-map_stats()
      # removeUI(selector = "#site_id")
      # removeUI(selector = "#check_study")
      output$cond_b1<-renderUI({
        ui=tagList(
          tableOutput(ns('status_r1')),
          uiOutput(ns("cond_b2"))
        )
      })

      if(sitestatus == 1){

        output$status_r1 <- renderTable({as.data.frame(map_stats)})
        print(min(map_stats$n_maps))
        print(nrow(map_stats))


          if(min(map_stats$n_maps)<2){
            if(nrow(map_stats != 10)){
              outputcond_b2<-renderUI({h4("To close session I, some ecosystem services are missing and need to be mapped.")})
            }else{
              outputcond_b2<-renderUI({h4("To close session I there need to be more results for some ecosystem services")})
            }

          }else if(min(map_stats$n_maps)>2 & nrow(map_stats == 10)){
            outputcond_b2<-renderUI({tagList(
              h5("If you want, you can now close session I"),
              actionButton(ns("close1"),"close session I"),
              uiOutput(ns("fin_process1"))
            )})
          }else if(min(map_stats$n_maps)>2 & nrow(map_stats != 10)){
            outputcond_b2<-renderUI({h4("To close session I there need to be more results for some ecosystem services")})
          }





      ## status 2
      }else if(sitestatus == 3){
        output$cond_b1<-renderUI({
          ui=tagList(
            h5("Session II is running"),
            tableOutput(ns('status_r2')),
            uiOutput(ns("cond_b2"))
          )
        })
        output$status_r2 <- renderTable({as.data.frame(map_stats)})
        removeUI(selector = "#check_study")

        if(min(map_stats$n_maps)<2 & nrow(map_stats != 10)){
          output$cond_b1<-renderUI({
            "To close session II, at least 2 maps for each of the 10 ecosystem services are needed"
          })
        }else{
          output$cond_b1<-renderUI({
            tagList(
              h5("If you want, you can now close session II"),
              actionButton(ns("close2"),"close session II")
            )

          })
        }
      }else if(sitestatus == 4){
        output$cond_b1<-renderUI({
          ui=tagList(
            h5("Session II is closed and postprocessed")
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
    ## AHP calculations
    ## status of projects should be set directly to 3
    observeEvent(input$close1,{
      req(map_stats)
      req(sites)

      map_stats<-map_stats()
      sites<-sites()

      shinybusy::show_modal_spinner(text = "Close mapping session 1 & update data base")

      new_site<-sites%>%filter(siteID==input$site_id & siteSTATUS == 1)%>%
        mutate(siteSTATUS=replace(siteSTATUS, siteSTATUS==1, 3)) %>%
        mutate(siteCREATETIME=Sys.time()) %>%
        as.data.frame()

      ### AHP calculation
      es_pair <- tbl(con_admin, "es_pair")
      es_pair <- es_pair%>%select(es_pair, ES_left,ES_right,selection_text,selection_val,userID,siteID,ahp_section) %>%filter(siteID == input$site_id)%>% collect()
      perform_ahp_update_db(es_pair = es_pair, con_admin = con_admin, studyID = input$site_id)

      ### rated impacts for dist
      es_dist <-tbl(con_admin, "es_impact")
      es_dist<-es_dist%>%filter(siteID == input$site_id)%>% collect()
      influence_rating_summary(es_dist = es_dist, con_admin = con_admin, studyID = input$site_id)


      site_updated = bq_table(project = "eu-wendy", dataset = dataset, table = 'study_site')
      bq_table_upload(x = site_updated, values = new_site, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

      shinybusy::remove_modal_spinner()

      removeUI(selector = "#close1")
      output$fin_process1<-renderUI({
        tagList(
          paste0("The site id ",input$site_id," is now open for the second mapping round.")
        )
      })

      ############# AHP calculations


    })

    ### server logic for "open2"
    ## just update the DB
    observeEvent(input$close2,{
      sites<-sites()

      shinybusy::show_modal_spinner(text = "update data base - postprocess session II")
      new_site<-sites%>%filter(siteID==input$site_id & siteSTATUS == 3)%>%
        mutate(siteSTATUS=replace(siteSTATUS, siteSTATUS==2, 4)) %>%
        mutate(siteCREATETIME=Sys.time()) %>%
        as.data.frame()

      site_updated = bq_table(project = "eu-wendy", dataset = dataset, table = 'studSITE')
      bq_table_upload(x = site_updated, values = new_site, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

      ## insert a helper raster on gcs bucket to trigger IAR, z, pdf calc fkts

      r <- rast(ncol=10, nrow=10)

      # Set random integer values between a specified range (e.g., 1 to 100)
      values(r) <- sample(1:100, ncell(r), replace=TRUE)
      tmp_name<-paste0(input$site_id,".tif")
      writeRaster(r, filename = tmp_name)

      file_name <-paste0("99_help_rast/",input$site_id)
      gcs_upload(tmp_name, bucket_name, name = file_name, predefinedAcl = "bucketLevel")
      file.remove(tmp_name)
      unlink(tmp_loc,recursive = T)


      shinybusy::remove_modal_spinner()

      ## as soon as site status == 4
      #google cloud function:
      # import CV R2 for the stud_ID (easy)
      # Calc IAR for all the ES per stud_ID (easy)
      # calc z value per ES-IAR (medium)


    })


  })



}

## To be copied in the UI
# mod_manage_study_ui("manage_study_1")

## To be copied in the server
# mod_manage_study_server("manage_study_1")

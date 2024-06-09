#' explore_studies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explore_studies_ui <- function(id){
  ns <- NS(id)
  tagList(
    h5("Insert your study  ID to inspect and modify the study status"),
    br(),
    textInput(ns("site_id"),""),
    br(),
    actionButton(ns("sub1"),"explore project")
  )
}

#' explore_studies Server Functions
#'
#' @noRd
mod_explore_studies_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## as soon as study ID is provided, there will be KPIs regarding the actual state of the study and the number of participants per ES
    observeEvent(input$sub1,{
      # study_area<-study_area()
        insertUI(selector = paste0("#",ns("sub1")),
                 where = "afterEnd",
                 ui=tagList(
                   textOutput(ns("status1")),

                 )

        )
      shinybusy::show_modal_spinner(text = "fetch data")
      projects<-tbl(con_admin, "studSITE")
      projects<-projects%>%collect()
      shinybusy::remove_modal_spinner()
      if(input$site_id %in% projects$siteID){
        status<-projects%>%filter(siteID==input$site_id)%>%select(siteSTATUS)
        # study_geom<-readRDS("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/GEOPROSPECTIVE/admin_app/study_geom/study_geom.rds")
        # n_study_areas<-nrow(study_geom%>%filter(projID==input$proj_id)%>%distinct(studyID))


        output$status1<-renderText(paste0("The site has status  ", as.numeric(status)))


        ## here N of participants per ES can be visualized

      }else{
        output$status1<-renderText(paste0("This project does not exist"))
      }

    })


  })
}

## To be copied in the UI
# mod_explore_studies_ui("explore_studies_1")

## To be copied in the server
# mod_explore_studies_server("explore_studies_1")

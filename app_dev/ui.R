fluidPage(
  titlePanel(title =  div(img(src="wendy.PNG", width ='120'), 'geoPROSPECTIVE admin'), windowTitle = "geoPROSPECTIVE admin" ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Create a geoPROSPECTIVE project", value = "p0",
                       br(),
                       textInput("user_name","Please login with your geoPROSPECTIVE user name to create a new study area"),
                       actionButton("check1","check access"),
                       uiOutput("cond0")),
              tabPanel(title = "Create a geoPROSPECTIVE project", value = "p1",
                       br(),
                       h5("Here you can create a new study area to map nature benefits / ecosystem services."),
                       br(),
                       textInput("siteID","Provide a short name of the study area no blank spaces"),
                       br(),
                       textInput("site_nat_name","A longer readable name of the study area"),
                       br(),
                       selectInput("sitetype","What kind of study area is it?",choices = c("","onshore","offshore"), selected = ""),
                       uiOutput("cond_b1")
              ),
              tabPanel(title = "Create a geoPROSPECTIVE project", value = "p1A",
                       uiOutput("type_dep")),
              tabPanel(title = "Create a geoPROSPECTIVE project", value = "p1B",
                       uiOutput("id_note")),
              tabPanel(title = "Create a geoPROSPECTIVE project", value = "p2",
                       mod_manage_study_ui("manage_projects")
                       )
                      )

)#/page

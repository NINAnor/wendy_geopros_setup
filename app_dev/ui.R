fluidPage(
  titlePanel(title =  div(img(src="wendy.PNG", width ='120'), 'conSITE admin'), windowTitle = "conSITE admin" ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Create a conSITE project", value = "p0",
                       br(),
                       textInput("user_name","Please login with your consite user name to create a new study area"),
                       actionButton("check1","check access"),
                       uiOutput("cond0")),
              tabPanel(title = "Create a conSITE project", value = "p1",
                       br(),
                       h5("Here you can create a new study area to map ecosystem services."),
                       br(),
                       textInput("site_nat_name","Provide a short name of the study area no blank spaces"),
                       br(),
                       textInput("site_descr","A short description of the study area"),
                       br(),
                       # numericInput("stud_proj","If available, provide the NINA internal project number the study site is linked to",0),
                       br(),
                       selectInput("sitetype","What kind of study area is it?",choices = c("","onshore","offshore"), selected = ""),
                       uiOutput("cond_b1")
              ),
              tabPanel(title = "Create a conSITE project", value = "p1A",
                       uiOutput("type_dep")),
              tabPanel(title = "Create a conSITE project", value = "p1B",
                       uiOutput("id_note")),
              tabPanel(title = "Explore & manage studies", value = "p2",
                       h5("Insert your study  ID to inspect and modify the study status"),
                       br(),
                       textInput("studID_in",""),
                       actionButton("checkstud","check status"),
                       uiOutput("cond_b2"),
                       uiOutput("status_ui")
                       )
                      )

)#/page

fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel(title =  div(img(src="logo.PNG", width ='120'), 'Admin portal of WENDY'), windowTitle = "Admin Geopros" ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Create a new project", value = "p0",
                       br(),
                       h5("Here you can create a new project to map biodiversity and ecosystem services."),
                       br(),
                       textInput("proj_nat_name","Provide a short name of the project no blank spaces"),
                       br(),
                       textInput("proj_descr","A short description of the project"),
                       br(),
                       selectInput("projtype","What kind of project is it?",choices = c("","onshore","offshore"), selected = ""),
                       uiOutput("cond_b1")


              ),
              tabPanel(title = "Explore & manage studies", value = "p2",
                       h5("Insert your study  ID to inspect and modify the study status"),
                       br(),
                       textInput("stud_ID_in",""),
                       ),
              tabPanel(title = "Consite", value = "p4",
                       h5("You can now proceed within the consite tool to balance natural values and energy production")
              )

                      )

)#/page

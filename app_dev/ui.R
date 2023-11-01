fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel(title =  div(img(src="logo.PNG", width ='120'), 'Admin portal of geoprospective'), windowTitle = "Admin Geopros" ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Create new project", value = "p0",
                       br(),
                       h5("Here you can create a new project to map biodiversity and ecosystem services."),
                       br(),
                       textInput("proj_nat_name","Provide a short name of the project no blank spaces"),
                       br(),
                       textInput("proj_descr","A short description of the project"),
                       br(),
                       h5("Define the study area"),
                       "Select your country of interest",
                       mapedit::selectModUI("map_sel_cntry"),
                       br(),
                       actionButton("create_pr","create project"),
                       br(),
                       textOutput("projidtext")

              ),
              tabPanel(title = "Choose ecosystem services", value = "p1",
                       # mod_define_study_area_ui("study_area")
                       ),
              tabPanel(title = "Explore running studies", value = "p2",
                       # mod_explore_studies_ui("explore_projects")
                       ),
              tabPanel(title = "Manage studies", value = "p3",
                       # mod_manage_study_ui("manage_proj")
                       ),
              tabPanel(title = "Consite", value = "p4",
                       h5("You can now proceed within the consite tool to balance natural values and energy production")
              )

                      )

)#/page

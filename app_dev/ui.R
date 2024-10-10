fluidPage(
  titlePanel(title =  div(img(src="wendy.PNG", width ='120'), 'Mapping nature values - Admin tool'), windowTitle = "PGIS-ADMIN"),
  tags$head(
    # Custom CSS to ensure dropdown shows fully
    tags$style(HTML("
        .box { overflow: visible; }
        .selectize-dropdown-content { overflow-y: auto; max-height: 200px; }
      "))
  ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Create a new study area", value = "p0",
                       br(),
                       bslib::value_box(
                         title="",
                         value = "",
                         h4("This tool enables you to setup a study for participatory mapping of ecosystem services. Thus, the user of the tool is a study administrator. The role and tasks for the administrator are described within this handbook. "),
                         br(),
                         h4("As soon as participants have compleated both sessions of mapping, the produced impact maps will be available in the consite tool."),
                         showcase = bs_icon("book"),
                         theme = value_box_theme(bg = blue, fg = "black")
                       ),
                       bslib::value_box(
                         title = "",
                         value = "",
                         h4("If you have a active study administrator account, please provide your user name below. If you want to open such a profile, fill out this form."),
                         textInput("user_name",""),
                         actionButton("check1","check access"),
                         uiOutput("cond0")),
                         showcase = bs_icon("exclamation-octagon-fill"),
                         theme = value_box_theme(bg = orange, fg = "black")
                       ),

              tabPanel(title = "Create a new study area", value = "p1",
                       br(),
                       bslib::value_box(
                         title="",
                         value = "",
                         h4("create a unique, short site id that you easily remember. You will use the site id to manage the study and to find the results within the consite tool"),
                         br(),
                         textInput("siteID","site id (no blank spaces)"),
                         br(),
                           showcase = bs_icon("1-circle"),
                         theme = value_box_theme(bg = orange, fg = "black")
                       ),
                       br(),
                       bslib::value_box(
                         title="",
                         value = "",
                         h4("Next, define a natural name of your study area. This may include blank spaces"),
                         br(),
                         textInput("site_nat_name",""),
                         showcase = bs_icon("2-circle"),
                         theme = value_box_theme(bg = "white", fg = "black")
                       ),

                       br(),
                       bslib::value_box(
                         title="",
                         value = "",
                         h4("Planned number of participants"),
                         br(),
                         h5("The study consists of a total of ten different ecosystem services to map with participants. It might take approx. 10 min for a participant to map one ecosystem service, maybe much less."),
                         h5("However, to optimize your study, please indicate how many ecosystem services each participant should map as a maximum. In other words how much time do you want to spend participants with the study?"),
                         h5("The less time per participant, the more participants you need."),
                         selectInput("n_es","",choices = c("1","2","3","4","5","6","7","8","9","10"), selected = "",selectize = FALSE),
                         showcase = bs_icon("3-circle"),
                         theme = value_box_theme(bg = green, fg = "black")
                       ),
                       br(),
                       bslib::value_box(
                         title="",
                         value = "",
                         h4("Choose the language of the study."),
                         h5("If the language is not listed, please contact the tool administrator."),
                         br(),
                         selectInput("language","",choices = list("Greek" = "gk",
                                                                  "Norwegian" = "no",
                                                                  "Italian" = "it",
                                                                  "Spanish"="es",
                                                                  "English" = "en",
                                                                  "Slovak" = "svk",
                                                                  "French" = "fr"),selected = "",selectize = FALSE),
                         showcase = bs_icon("4-circle"),
                         theme = value_box_theme(bg = "white", fg = "black")
                       ),
                       br(),
                       bslib::value_box(
                         title="",
                         value = "",
                         h4("Are you going to create a onshore or offshore mapping study?"),
                         br(),
                         selectInput("sitetype","",choices = c("","onshore","offshore"), selected = "",selectize = FALSE),
                         showcase = bs_icon("5-circle"),
                         theme = value_box_theme(bg = blue, fg = "black")
                       ),
                       br(),
                       uiOutput("cond_b1")



              ),
              tabPanel(title = "Create a new study area", value = "p1A",
                       uiOutput("type_dep")),
              tabPanel(title = "Create a new study area", value = "p1B",
                       uiOutput("id_note")),
              tabPanel(title = "Assess mapping progress", value = "p2",
                       mod_manage_study_ui("manage_projects")
                       )
                      )

)#/page

#This will be a shiny version to run DIGSS
#Some of the functions will be tinkered to improve the display in webpages

library(shiny)
library(ggplot2)
library(viridis)
#load DIGSS functions
source("SurveySimShiny.R")
source("SurveyLoopsShiny.R")
source("PlotSurveySummShiny.R")

source("AreaEstimator.R")
source("CloudGenerator.R")
source("FieldMapShiny.R")

source("Str2Vector.R")

ui <- fluidPage(
  #CSS style config
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  #enable shinyJs to control fields as enabled or not
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("DIGSS - Determination of Intervals using Georeferenced Survey Simulation"),
  
  # Sidebar layout setup 
  sidebarLayout(
    sidebarPanel(
      tags$table(border=0, style = "width:100%",
                 tags$tbody(
                   tags$tr(
                     tags$td(tags$h3("Input parameters:")),
                     tags$td(textInput("simm_label","Simulation label:",value="Simulation 1"))
                   )
                 )
      ),
      
      tags$div(style ="display: inline-block", 
               HTML("<h4>Survey Area Properties (km):</h4>")),
      tags$div(class="tooltip2",
               "?",
               tags$span(class="tooltiptext",
                         HTML("<strong>Survey area: </strong> <br> 
                                    The survey area is defined as a rectangle. 
                                    Input values for width and length in kilometers."))
      ),
      
      tags$table(border=0, style="width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(numericInput("area_width","Area width (km)",value="1.0",width="200px")),
                              tags$td(numericInput("area_length","Area length (km)",value="1.0",width="200px"))
                            )
                 )
      ),
      #Grid properties
      
      tags$div(HTML("<h4>Grid Properties:</h4>")),
      
      tags$table(border=0, style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(
                                tags$div(style ="display: inline-block; width = 50%", 
                                         selectInput("grid_type","Grid type:",c("square","rectangle","staggered","hexagonal","arbitrary"),width="150")
                                )),
                              tags$td(style = "padding-left:3px;padding-right:7px",
                                      tags$div(class="tooltip2",
                                               "?",
                                               tags$span(class="tooltiptext",
                                                         HTML("<strong>Grid type: </strong> <br> 
                                    Type of survey grid to be superimposed on the survey area."))
                                    
                                      )
                              ),
                              tags$td(tags$div(style ="display: inline-block", 
                                               numericInput("col_width","Column width (m)",200,min=1,width="100%")
                              )),
                              tags$td(style = "padding-left:3px;padding-right:7px",
                                      tags$div(class="tooltip2",
                                               "?",
                                               tags$span(class="tooltiptext",
                                                         HTML("<strong>Column width: </strong> <br> 
                                                                The space between survey columns in the grid IN METERS"))
                                               
                                      )
                              ),
                              tags$td(tags$div(style ="display: inline-block", 
                                               numericInput("survey_radius","Survey radius (m)",0.5,min=0, width="100%")
                              )),
                              tags$td(style = "padding-left:3px",
                                      tags$div(class="tooltip2",
                                               "?",
                                               tags$span(class="tooltiptext",
                                                         HTML("<strong>Survey radius: </strong> <br> 
                                                                The radius IN METERS of the survey pit (assumed to be a circle)."))
                                               
                                      )
                              )
                            )
                 )
      ),
      
      tags$div(HTML("<h4>Site properties:</h4>")),
      
      tags$table(border=0, style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(tags$div(style ="display: inline-block", 
                                               radioButtons("dens_choice","Site Density:",c("single value", "Range"),width="200px",inline = TRUE)
                              ),
                              
                              tags$div(class="tooltip2",
                                       "?",
                                       tags$span(class="tooltiptext",
                                                 HTML("<strong>Site Density: </strong> <br> 
                                    Number of sites/km<sup>2</sup>. <br> 
                                    Can be either one value or a range (min and max) to create a variable of densities."))
                                    
                              )
                              ),
                              tags$td(tags$div(style ="display: inline-block", 
                                               numericInput("overlap","Site overlap",0.5,min=0,max=1,step=0.1)
                              ),
                              
                              tags$div(class="tooltip2",
                                       "?",
                                       tags$span(class="tooltiptext",
                                                 HTML("<strong>Site overlap: </strong> <br> 
                                    maximum overlap of site area, ranging from <br> 
                                    0 = no overlap allowed to <br>
                                    1 = complete overlap possible"))
                              )
                              ),
                              
                            )
                 )
      ),
      
      tags$table(border=0, style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(numericInput("site_dens_val","Density",value=5,min=0)),
                              tags$td(numericInput("site_dens_min","Density - min",value=0,min=0)),
                              tags$td(numericInput("site_dens_max","Density - max",value=0,min=0))
                            )
                 )
      ),
      tags$div(style ="display: inline-block", 
               radioButtons("area_choice","Site Area:",c("single value", "Variable"),width="200px",inline = TRUE)
      ),
      
      tags$div(class="tooltip2",
               "?",
               tags$span(class="tooltiptext",
                         HTML("<strong>Site Area: </strong> <br> 
                                    Can be one of two options: <br>
                                    1. one value indicating the area of all sites, in m<sup>2</sup>; <br>
                                    2. range defined by 4 values: min, max, mean (or median), and standard deviation in m<sup>2</sup>."))
               
      ),
      tags$table(border=0,style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(numericInput("site_area_val","Area",value=5000),min=0),
                              tags$td(numericInput("site_area_mean","Area - mean",value=0),min=0),
                              tags$td(numericInput("site_area_sd","Area - st dev",value=0),min=0),
                              tags$td(numericInput("site_area_min","Area - min",value=0),min=0),
                              tags$td(numericInput("site_area_max","Area - max",value=0),min=0)
                            )
                 )
      ),
      
      tags$table(border=0, style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(tags$div(style ="display: inline-block", 
                                               radioButtons("obj_dens_choice","Artifact Density:",c("single value", "Range"),width="200px",inline = TRUE)
                              ),
                              
                              tags$div(class="tooltip2",
                                       "?",
                                       tags$span(class="tooltiptext",
                                                 HTML("<strong>Artifact Density: </strong> <br> 
                                                        artifacts per m<sup>2</sup>.<br> 
                                                        Can be a single value (uniform for all sites) or a range of values defined as min and max"))
                              )
                              ),
                              tags$td(tags$div(style ="display: inline-block", 
                                               selectInput("obj_dist","Artifact distribution:",c("uniform","linear","spherical","sinusoidal"),width="150")
                              ),
                              
                              tags$div(class="tooltip2",
                                       "?",
                                       tags$span(class="tooltiptext",
                                                 HTML("<strong>Artifact distribution: </strong> <br> 
                                                        type of cloud distribution for artifacts inside sites."))
                              )
                              ),
                              
                            )
                 )
      ),
      tags$table(border=0, style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(numericInput("obj_dens_val","Density",value=1,min=0)),
                              tags$td(numericInput("obj_dens_min","Density - min",value=0,min=0)),
                              tags$td(numericInput("obj_dens_max","Density - max",value=0,min=0))
                            )
                 )
      ),
      
      #simulation + run
      tags$table(border=0, style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(tags$div(style ="display: inline-block", 
                                               numericInput("sims","Simulations",value=10,min=0)
                              )
                              ,
                              
                              tags$div(class="tooltip2",
                                       "?",
                                       tags$span(class="tooltiptext",
                                                 HTML("<strong>Simulations: </strong> <br> 
                                                                Number of random maps to be created and contrasted with the grids."))
                                       
                                       
                              )
                              ),
                              
                              tags$td(tags$div(style ="display: inline-block", 
                                               checkboxInput("plot_artifact","Plot artifacts")
                              )
                              ,
                              
                              tags$div(class="tooltip2",
                                       "?",
                                       tags$span(class="tooltiptext",
                                                 HTML("<strong>Artifact plotting: </strong> <br> 
                                                                Although pretty, IT IS SUPER RESOURCE INTENSIVE!! Use only when artifact density is not too high."))
                                       
                                       
                              )
                              ),
                              
                              tags$td(actionButton("run","Run DIGSS"))
                            ),
                            tags$tr(
                              tags$td(),
                              tags$td(tags$div(style ="display: inline-block", 
                                               selectInput("sims_to_plot","Select simulations:",c(""),multiple = TRUE)
                              )
                              ,
                              
                              tags$div(class="tooltip2",
                                       "?",
                                       tags$span(class="tooltiptext",
                                                 HTML("<strong>Simulations to plot: </strong> <br> 
                                                                Select one or more simulations to compare through plots"))
                                       
                                       
                              )),
                              tags$td(selectInput("summ_plot","Plot density for:",c("Sites found"="sites.found",
                                                                                    "Survey hits"="survey.hits", 
                                                                                    "Success Rate"="success.rate.index", 
                                                                                    "Sites found on artif."="sites.foundARTI",
                                                                                    "Survey with artifacts"="survey.hitsARTI"))
                              )
                            )
                 )
      ),
      
      tags$h4("Simulation Loops"),
      
      tags$table(border=0, style = "width:100%",
                 
                 tags$tbody(align = "center",
                            tags$tr(
                              tags$td(tags$div(style ="display: inline-block", 
                                               selectInput("loop_variable","Chose parameter to loop:",
                                                           c("column width", "grid type","simulations","survey area","site density - constant","site density - range", "site area - constant","site area - range","site overlap","artifact density - const.","artifact density - range", "artifact distribution","survey radius"),
                                                           width="200px")
                              ),
                              
                              tags$div(class="tooltip2", 
                                       "?",
                                       tags$span(class="tooltiptext",style = "width:450px;top:-300px",
                                                 HTML("<strong>Variable to be looped: </strong> <br> 
                                                        Can be any of the variables that exist in SurveyParameters: <br>
                                                        VALUES MUST BE SEPARATED BY COMMAS IN THEIR RESPECTIVE FIELDS OR CAN BE PASSED AS RANGES 
                                                        WITH COLONS FOR INTEGERS (e.g., 1:15) <br>

                                                        <i>column width</i>: vector of numbers with distances between STP rows <br> 

                                                        <i>grid type</i>: vector of strings with names of grid types <br> 

                                                        <i>simulations</i>: vector of numbers with numbers of simulations <br> 

                                                        <i>survey area</i>:  2 vectors, one for x and one for y of area (vectors MUST be same length) <br> 

                                                        <i>site density - constant</i>: vector with numbers of site density <br> 
                                                        
                                                        <i>site density - range</i>: 2 vectors, one with minimum site densities, one with maximum (vectors MUST be same length) <br> 

                                                        <i>site area - constant</i>: vector with numbers of site areas <br> 
                                                        
                                                        <i>site area - range</i>: list with 4 vectors, one with minimums, one with maximums, one with means, one with st.devs (vectors MUST be same length)<br> 

                                                        <i>overlap</i>: vector of numbers with varying overlaps <br>

                                                        <i>Artifact density - const.</i>: vector with varying artifact density <br>
                                                        
                                                        <i>Artifact density - range</i>: 2 vectors, one with minimum artifact densities, one with maximum (vectors MUST be same length)<br>

                                                        <i>Artifact distribution</i>: vector of names of artifact distribution<br>

                                                        <i>Survey radius</i>: vector with varying survey radii"))
                              )
                              ),
                              tags$td(actionButton("loop_run", "Run loops"))
                            ),
                            tags$tr(
                              tags$td(textInput("loopv1","")),
                              tags$td(textInput("loopv2",""))
                            ),
                            tags$tr(
                              tags$td(textInput("loopv3","")),
                              tags$td(textInput("loopv4",""))
                            ),
                            tags$tr(
                              tags$td(),
                              tags$td(selectInput("loop_plot","Plot results for:",c("Sites found"="SitesFound",
                                                                                    "Sites found on artif."="SitesFoundOnArtifacts",
                                                                                    "Artifacts per survey"="ArtifactsPerSurvey",
                                                                                    "Surveys per simulation"="SurveysPerSim", 
                                                                                    "Success rate"="SuccessRateIndex"))
                              )
                              
                            )
                 )
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs", id = "result_tabs",
                  tabPanel("Welcome",
                           htmlOutput("welcome")),
                  tabPanel("Sim Results",
                           actionButton("dwld_table","Download Results"),
                           tags$table(border=0, style = "width:100%",
                                      
                                      tags$tbody(align = "left",
                                                 tags$tr(
                                                   tags$td(style = ("width: 50%"), plotOutput("sitemap",inline=TRUE)),
                                                   tags$td(tags$h4(tags$b(textOutput("table_title"))),
                                                           tableOutput("survey_pars"))
                                                 )
                                      )
                           ),
                           h2(textOutput("surveys_txt")),
                           tableOutput("summary"),
                           plotOutput("summary_plot")
                  ),
                  tabPanel("Loop Results", 
                           actionButton("dwld_table_loop","Download Results"),
                           h2(textOutput("loop_txt")),
                           tableOutput("loop_summary"),
                           plotOutput("loop_result_plot"),
                           textOutput("txt")
                  )
      )
    )
  )
)
# Define server: session is required to update text labels
server <- function(input, output, session) {
  #lock dowload options unless results exist
  shinyjs::disable("dwld_table")
  shinyjs::disable("dwld_table_loop")
 
  #lock plot options unless results exist
  shinyjs::disable("summ_plot")
  shinyjs::disable("sims_to_plot")
  shinyjs::disable("loop_plot")
  
  #create global object for results
  results<-list()
  loop_results<-NULL
  
  sims_done<-0
  results_labels<-""
  results_exist<-FALSE
  res_to_plot<-NULL
  
  #these are for grid_ratio updates in the loop function
  modals2do<-NULL
  modals_left<-0
  grid_ratios<-NULL
  SurveyParGE<-NULL
  loop_varGE<-NULL
  loop_parsGE<-NULL
  x_lab<-NULL
  
  #welcome text
  output$welcome <- renderUI(includeHTML("welcome.html"))
  
  #radio_buttons
  observeEvent(input$dens_choice, {
    if(input$dens_choice == "single value"){
      shinyjs::enable("site_dens_val")
      shinyjs::disable("site_dens_min")
      shinyjs::disable("site_dens_max")
    } else {
      shinyjs::disable("site_dens_val")
      shinyjs::enable("site_dens_min")
      shinyjs::enable("site_dens_max")
    }
  })
  
  observeEvent(input$area_choice, {
    if(input$area_choice == "single value"){
      shinyjs::enable("site_area_val")
      shinyjs::disable("site_area_mean")
      shinyjs::disable("site_area_sd")
      shinyjs::disable("site_area_min")
      shinyjs::disable("site_area_max")
    } else {
      shinyjs::disable("site_area_val")
      shinyjs::enable("site_area_mean")
      shinyjs::enable("site_area_sd")
      shinyjs::enable("site_area_min")
      shinyjs::enable("site_area_max")
    }
  })
  
  observeEvent(input$obj_dens_choice, {
    if(input$obj_dens_choice == "single value"){
      shinyjs::enable("obj_dens_val")
      shinyjs::disable("obj_dens_min")
      shinyjs::disable("obj_dens_max")
    } else {
      shinyjs::disable("obj_dens_val")
      shinyjs::enable("obj_dens_min")
      shinyjs::enable("obj_dens_max")
    }
  })
  
  #update loop fields
  observeEvent(input$loop_variable,{
    x<-input$loop_variable
    
    if(x=="column width"){
      updateTextInput(session,"loopv1",label = "Vector of widths:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="grid type"){
      updateTextInput(session,"loopv1",label = "List of grid type names:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="simulations"){
      updateTextInput(session,"loopv1",label = "Vector of simulation numbers:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="survey area"){
      updateTextInput(session,"loopv1",label = "Vector of widths (km):")
      updateTextInput(session,"loopv2",label = "Vector of heights (km):")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::enable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="site density - constant"){
      updateTextInput(session,"loopv1",label = "Vector of site densities:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="site density - range"){
      updateTextInput(session,"loopv1",label = "Vector of minimum site densities:")
      updateTextInput(session,"loopv2",label = "Vector of maximum site densities:")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::enable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="site area - constant"){
      updateTextInput(session,"loopv1",label = "Vector of site areas:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="site area - range"){
      updateTextInput(session,"loopv1",label = "Vector of site area MEANS:")
      updateTextInput(session,"loopv2",label = "Vector of site area ST. DEVS.:")
      updateTextInput(session,"loopv3",label = "Vector of MINIMUM site area:")
      updateTextInput(session,"loopv4",label = "Vector of MAXIMUM site area:")
      shinyjs::enable("loopv1")
      shinyjs::enable("loopv2")
      shinyjs::enable("loopv3")
      shinyjs::enable("loopv4")
      
    }else if(x=="site overlap"){
      updateTextInput(session,"loopv1",label = "Vector of site overlap values:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="artifact density - const."){
      updateTextInput(session,"loopv1",label = "Vector of artifact densities:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="artifact density - range"){
      updateTextInput(session,"loopv1",label = "Vector of minimum artifact densities:")
      updateTextInput(session,"loopv2",label = "Vector of maximum artifact densities:")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::enable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="artifact distribution"){
      updateTextInput(session,"loopv1",label = "List of artifact distributions:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }else if(x=="survey radius"){
      updateTextInput(session,"loopv1",label = "Vector of surface radii:")
      updateTextInput(session,"loopv2",label = "")
      updateTextInput(session,"loopv3",label = "")
      updateTextInput(session,"loopv4",label = "")
      shinyjs::enable("loopv1")
      shinyjs::disable("loopv2")
      shinyjs::disable("loopv3")
      shinyjs::disable("loopv4")
      
    }
  })
  
  #popup menu for non-symmetric grids
  
  observeEvent(input$grid_type,{
    if(input$grid_type=="rectangle" | input$grid_type=="arbitrary"){
      showModal(modalDialog(
        numericInput("grid_ratio","Input the length to width ratio of survey grid",value=1),
        size="s",
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK"))
      )
      
      )
    }
  })
  
  observeEvent(input$ok,{
    removeModal()
  })
  
  #run DIGSS
  observeEvent(input$run,{
    
    SurveyParameters<-load_parameters()
    
    #update values for list of results
    sims_done<<-sims_done+1
    results_labels[sims_done]<<-input$simm_label
    updateTextInput(session,"simm_label",value=paste("Simulation",(sims_done+1)))
    updateSelectInput(session, "sims_to_plot",selected = input$simm_label, choices=results_labels)

    #run survey sim
    survey_results<-surveySimShiny(SurveyParameters, artifact.analysis=TRUE, plot=TRUE, 
                                   plot.artifacts=ifelse(input$plot_artifact==TRUE,TRUE,FALSE),areaprecision=1000, grid_ratio=input$grid_ratio)
    
    results[[sims_done]]<<-survey_results
    results_exist<<-TRUE
    
    
    #update main panel
    
    output$sitemap<-renderPlot(replayPlot(survey_results$plot),width=600,height=600)
    
    output$surveys_txt<-renderText(paste("Surveys per simulation:",survey_results$Summary[1,2]))
    
    output$summary<-renderTable(survey_results$Summary[2:5,])
    
    output$summary_plot<-renderPlot(plotSurveySummShiny(list(survey_results),input$summ_plot,labels = results_labels[sims_done]))
    
    #create summary table
    
    summ_table<-data.frame("Parameters"=NA,"Values"=NA)
    summ_table[1,]<-c("Survey area:",paste(survey_results[[4]]$Area[1],"x",survey_results[[4]]$Area[2],"km"))
    summ_table[2,]<-c("Grid type:",input$grid_type)
    summ_table[3,]<-c("Column width:",survey_results[[4]]$col.width)
    summ_table[4,]<-c("Survey radius:",survey_results[[4]]$survey.radius)
    if(input$dens_choice == "single value"){
      summ_table[5,]<-c("Site density:",survey_results[[4]]$site.density)
    }else{
      summ_table[5,]<-c("Site density - min:",survey_results[[4]]$site.density[1])
      summ_table[6,]<-c("Site density - max:",survey_results[[4]]$site.density[2])
      
    }
    summ_table[nrow(summ_table)+1,]<-c("Site overlap:",survey_results[[4]]$overlap)
    if(input$area_choice == "single value"){
      summ_table[nrow(summ_table)+1,]<-c("Site area:",survey_results[[4]]$site.area)
    }
    else
    {
      summ_table[nrow(summ_table)+1,]<-c("Site area - mean:",survey_results[[4]]$site.area[3])
      summ_table[nrow(summ_table)+1,]<-c("Site area - st. dev:",survey_results[[4]]$site.area[4])    
      summ_table[nrow(summ_table)+1,]<-c("Site area - min:",survey_results[[4]]$site.area[1])    
      summ_table[nrow(summ_table)+1,]<-c("Site area - max:",survey_results[[4]]$site.area[2])    
    }
    summ_table[nrow(summ_table)+1,]<-c("Artifact distribution:",input$obj_dist)
    if(input$obj_dens_choice == "single value"){
      summ_table[nrow(summ_table)+1,]<-c("Artifact density:",survey_results[[4]]$obj.density)
      
    }
    else
    {
      summ_table[nrow(summ_table)+1,]<-c("Artifact density - min:",survey_results[[4]]$obj.density[1])
      summ_table[nrow(summ_table)+1,]<-c("Artifact density - max:",survey_results[[4]]$obj.density[2])
    }
    
    summ_table[nrow(summ_table)+1,]<-c("Simulations run:",survey_results[[4]]$simulations)
    
    
    output$table_title<-renderText(results_labels[sims_done])
    output$survey_pars<-renderTable(summ_table)
    
    #enable plot button
    shinyjs::enable("summ_plot")
    shinyjs::enable("sims_to_plot")
    
    #enable download buttons
    shinyjs::enable("dwld_table")
   
    #select the right navbar
    updateTabsetPanel(session, "result_tabs", selected = "Sim Results")
    
  })
  
  observeEvent(input$summ_plot,{
    if(results_exist==TRUE){
      output$summary_plot<-renderPlot(plotSurveySummShiny(results[res_to_plot],input$summ_plot,labels = results_labels[res_to_plot]))
    }
  })
  
  observeEvent(input$sims_to_plot,{
    if(results_exist==TRUE){
      results_to_plot=1:length(results_labels)
      for(a in 1:length(results_labels)){
        if(length(which(input$sims_to_plot==results_labels[a]))==0){
          results_to_plot<-results_to_plot[-which(results_to_plot==a)]
        }
      }
      
      res_to_plot<<-results_to_plot
      
      output$summary_plot<-renderPlot(plotSurveySummShiny(results[res_to_plot],input$summ_plot,labels = results_labels[res_to_plot]))
      
    }
  })
  
  #run loops
  observeEvent(input$loop_run,{
    
    #enable plot button
    shinyjs::enable("loop_plot")
    
    #enable download buttons
    shinyjs::enable("dwld_table_loop")
    
    SurveyParameters<-load_parameters()
     
    if(input$loop_variable=="column width"){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Survey column width in each iteration"
      loop_var<-"col.width"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="grid type"){
      loop_pars<-string2vector(input$loopv1,FALSE)
      x_lab<<-"Grid type in each iteration"
      loop_var<-"grid.type"
      
      GRID<-c("square","rectangle","staggered","hexagonal","arbitrary.staggered")
      
      grid_ratios<<-rep(NA,length(loop_pars))
      
      for(a in 1:length(loop_pars)){
        
        grid.type<-pmatch(loop_pars[a],GRID)
        if(grid.type==2 |grid.type==5){
          modals2do<<-c(modals2do,a)
          modals_left<<-modals_left+1
        }
      }
      
      grid_ratio_loop(SurveyParameters,loop_var,loop_pars)
      
    }else if(input$loop_variable=="simulations"){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Number of simulation in each iteration"
      loop_var<-"simulations"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="survey area"){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Survey area  in each iteration"
      loop_var<-"area"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="site density - constant"){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Site density in each iteration"
      loop_var<-"site.density"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="site density - range"){
      loop_pars<-list()
      loop_pars[[1]]<-string2vector(input$loopv1)
      loop_pars[[2]]<-string2vector(input$loopv2)
      x_lab<<-"Site density in each iteration"
      loop_var<-"site.density"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="site area - constant"){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Site area in each iteration"
      loop_var<-"site.area"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="site area - range"){
      loop_pars<-list()
      loop_pars[[1]]<-string2vector(input$loopv1)
      loop_pars[[2]]<-string2vector(input$loopv2)
      loop_pars[[3]]<-string2vector(input$loopv3)
      loop_pars[[4]]<-string2vector(input$loopv4)
      x_lab<<-"Site area in each iteration"
      loop_var<-"site.area"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="site overlap"){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Site overlap in each iteration"
      loop_var<- "overlap"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="artifact density - const."){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Artifact density in each iteration"
      loop_var<-"obj.density"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="artifact density - range"){
      loop_pars<-list()
      loop_pars[[1]]<-string2vector(input$loopv1)
      loop_pars[[2]]<-string2vector(input$loopv2)
      x_lab<<-"Artifact density in each iteration"
      loop_var<-"obj.density"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="artifact distribution"){
      loop_pars<-string2vector(input$loopv1,FALSE)
      x_lab<<-"Artifact distribution in each iteration"
      loop_var<-"obj.distribution"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL)
      
    }else if(input$loop_variable=="survey radius"){
      loop_pars<-string2vector(input$loopv1)
      x_lab<<-"Survey radius in each iteration"
      loop_var<-"survey.radius"
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL) 
    }
    
    #select the right navbar
    updateTabsetPanel(session, "result_tabs", selected = "Loop Results")
    
  })
  
  #The function is external to the observe event so that I can call modals for grid_ratio
  RunLoop<-function(SurveyParameters,loop_var,loop_pars,grid_ratio=NULL){
    loop_results<-surveyLoops(SurveyParameters,loop_var,loop_pars,grid_ratio=grid_ratio)
    
    loop_results<<-loop_results
    
    #plot
    
    tmp<-loop_results$SitesFound
    
    line_palette<-viridis(5)
    fill_palette<-viridis(5,alpha = 0.5)
    
    df95<-data.frame(x=c(1:nrow(tmp),nrow(tmp):1),
                     range=c(tmp$`Quant 2.5%`,tmp$`Quant 97.5%`[nrow(tmp):1]))
    
    p<-ggplot(data=tmp,aes(x=reorder(`Loop Value`,1:nrow(tmp)),y=Mean,group=1), color=col_palette[1])+
      geom_line(size=1)+
      geom_point(size=4)+
      geom_errorbar(aes(ymin= Mean-`St Dev`, ymax= Mean+`St Dev`), width=.1)+
      geom_polygon(data=df95,aes(x=x,y=range),fill=fill_palette[1])+
      xlab(x_lab)+
      theme_minimal()
    
    output$loop_txt<-renderText("Sites found in each iteration:")
    output$loop_summary<-renderTable(loop_results$SitesFound)
    output$loop_result_plot<-renderPlot(p)
  }
  
  #This will run the modals for grid_ratios
  
  grid_ratio_loop<-function(SurveyParameters,loop_var,loop_pars){
    #saving as global values, to be called from the modal button. 
    SurveyParGE<<-SurveyParameters
    loop_varGE<<-loop_var
    loop_parsGE<<-loop_pars
    
    if(length(modals2do)==0 | modals_left==0){
      
      RunLoop(SurveyParameters,loop_var,loop_pars,grid_ratio=grid_ratios)
      
    }else{
      index<-modals2do[length(modals2do)-modals_left+1]
      showModal(modalDialog(
        numericInput("grid_ratio_loop",paste0("Input the length to width ratio for iteration",index),value=1),
        size="s",
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_loop", "OK"))
      )
      )
    }
  }
  
  #control and updates the grid_ratios
  observeEvent(input$ok_loop,{
    index<-length(modals2do)-modals_left+1
    grid_ratios[modals2do[index]]<<-input$grid_ratio_loop
    
    removeModal()
    
    modals_left<<-modals_left-1
    
    grid_ratio_loop(SurveyParGE,loop_varGE,loop_parsGE)
  })
  
  #Control the plot to show in results 
  observeEvent(input$loop_plot,{
    if(is.null(loop_results)==FALSE){
      line_palette<-viridis(5)
      fill_palette<-viridis(5,alpha = 0.5)
      
      if(input$loop_plot=="SitesFound"){
        output$loop_txt<-renderText("Sites found in each iteration:")
        output$loop_summary<-renderTable(loop_results$SitesFound)
        
        #plot
        tmp<-loop_results$SitesFound
        
        df95<-data.frame(x=c(1:nrow(tmp),nrow(tmp):1),
                         range=c(tmp$`Quant 2.5%`,tmp$`Quant 97.5%`[nrow(tmp):1]))
        
        p<-ggplot(data=tmp,aes(x=reorder(`Loop Value`,1:nrow(tmp)),y=Mean,group=1), color=col_palette[1])+
          geom_line(size=1)+
          geom_point(size=4)+
          geom_errorbar(aes(ymin= Mean-`St Dev`, ymax= Mean+`St Dev`), width=.1)+
          geom_polygon(data=df95,aes(x=x,y=range),fill=fill_palette[1])+
          xlab(x_lab)+
          theme_minimal()
      }
      else if(input$loop_plot=="SitesFoundOnArtifacts"){
        output$loop_txt<-renderText("Sites found based on artifacts in each iteration:")
        output$loop_summary<-renderTable(loop_results$SitesFoundOnArtifacts)
        
        #plot
        tmp<-loop_results$SitesFoundOnArtifacts
        
        df95<-data.frame(x=c(1:nrow(tmp),nrow(tmp):1),
                         range=c(tmp$`Quant 2.5%`,tmp$`Quant 97.5%`[nrow(tmp):1]))
        
        p<-ggplot(data=tmp,aes(x=reorder(`Loop Value`,1:nrow(tmp)),y=Mean,group=1), color=col_palette[2])+
          geom_line(size=1)+
          geom_point(size=4)+
          geom_errorbar(aes(ymin= Mean-`St Dev`, ymax= Mean+`St Dev`), width=.1)+
          geom_polygon(data=df95,aes(x=x,y=range),fill=fill_palette[2])+
          xlab(x_lab)+
          theme_minimal()
      }
      else if(input$loop_plot=="ArtifactsPerSurvey"){
        output$loop_txt<-renderText("Artifacts per survey in each iteration:")
        output$loop_summary<-renderTable(loop_results$ArtifactsPerSurvey)
        
        #plot
        tmp<-loop_results$ArtifactsPerSurvey
        
        df95<-data.frame(x=c(1:nrow(tmp),nrow(tmp):1),
                         range=c(tmp$`Quant 2.5%`,tmp$`Quant 97.5%`[nrow(tmp):1]))
        
        p<-ggplot(data=tmp,aes(x=reorder(`Loop Value`,1:nrow(tmp)),y=Mean,group=1), color=col_palette[3])+
          geom_line(size=1)+
          geom_point(size=4)+
          geom_errorbar(aes(ymin= Mean-`St Dev`, ymax= Mean+`St Dev`), width=.1)+
          geom_polygon(data=df95,aes(x=x,y=range),fill=fill_palette[3])+
          xlab(x_lab)+
          theme_minimal()
      }
      else if(input$loop_plot=="SuccessRateIndex"){
        output$loop_txt<-renderText("Success Rate Index in each iteration:")
        output$loop_summary<-renderTable(loop_results$SuccessRateIndex)
        
        #plot
        tmp<-loop_results$SuccessRateIndex
        
        df95<-data.frame(x=c(1:nrow(tmp),nrow(tmp):1),
                         range=c(tmp$`Quant 2.5%`,tmp$`Quant 97.5%`[nrow(tmp):1]))
        
        p<-ggplot(data=tmp,aes(x=reorder(`Loop Value`,1:nrow(tmp)),y=Mean,group=1), color=col_palette[4])+
          geom_line(size=1)+
          geom_point(size=4)+
          geom_errorbar(aes(ymin= Mean-`St Dev`, ymax= Mean+`St Dev`), width=.1)+
          geom_polygon(data=df95,aes(x=x,y=range),fill=fill_palette[4])+
          xlab(x_lab)+
          theme_minimal()
      }
      else if(input$loop_plot=="SurveysPerSim"){
        output$loop_txt<-renderText("Surveys in each iteration:")
        output$loop_summary<-renderTable(loop_results$SurveysPerSim)
        
        tmp<-loop_results$SurveysPerSim
        
        
        p<-ggplot(data=tmp,aes(x=reorder(`Loop Value`,1:nrow(tmp)),y=N,group=1), color=col_palette[5])+
          geom_line(size=1)+
          geom_point(size=4)+
          xlab(x_lab)+
          theme_minimal()
      }
      
      output$loop_result_plot<-renderPlot(p)
    }
  })
  
  #Download results
  observeEvent(input$dwld_table,{
    showModal(modalDialog(
      selectInput("sim_to_save","Select simulation to download:",selected = input$simm_label, choices=results_labels),
      selectInput("table_to_save","Select data to download:",selected = "Summary", choices=c("Summary","By Site","By Artifact")),
      size="m",
        
      footer = tagList(
        downloadButton("dwld", "Download"),
        modalButton("DONE")
      )
    )
    )
  })
  
  observeEvent(input$sim_to_save,{
    result_to_save<-which(input$sim_to_save==results_labels)
    tbl_to_save<-which(input$table_to_save==c("Summary","By Site","By Artifact"))
   
    output$dwld <- downloadHandler(
      filename = function() {
        paste0(input$sim_to_save,"_",input$table_to_save, ".csv")
      },
      content = function(file) {
        write.csv(results[[result_to_save]][tbl_to_save], file)
      }
    )
  })
  
  observeEvent(input$table_to_save,{
    result_to_save<-which(input$sim_to_save==results_labels)
    tbl_to_save<-which(input$table_to_save==c("Summary","By Site","By Artifact"))
   
     output$dwld <- downloadHandler(
      filename = function() {
        paste0(input$sim_to_save,"_",input$table_to_save, ".csv")
      },
      content = function(file) {
        write.csv(results[[result_to_save]][tbl_to_save], file)
      }
    )
  })
 
  #Download loop results
  observeEvent(input$dwld_table_loop,{
    showModal(modalDialog(
      selectInput("table_to_save_loop","Select data to download:",selected = "Sites found", choices=c("Total surveys","Sites found","Sites found - artifacts","Artifacts found", "Success Rate Index")),
      size="m",
      
      footer = tagList(
        downloadButton("dwld_loop", "Download"),
        modalButton("DONE")
      )
    )
    )
  })
  
  observeEvent(input$table_to_save_loop,{
    tbl_to_save<-which(input$table_to_save_loop==c("Total surveys","Sites found","Sites found - artifacts","Artifacts found", "Success Rate Index"))
    
    output$dwld_loop <- downloadHandler(
      filename = function() {
        paste0("Loop_",input$table_to_save_loop, ".csv")
      },
      content = function(file) {
        write.csv(loop_results[tbl_to_save], file)
      }
    )
  })
  
  #function to load parameters
  load_parameters<-function(){
    #create the data
    if(input$dens_choice == "single value"){
      tmp_dens<-input$site_dens_val
    }else{
      tmp_dens<-c(input$site_dens_min,input$site_dens_max)
    }
    
    if(input$area_choice == "single value"){
      tmp_site_area = input$site_area_val
    }else{
      tmp_site_area = c(input$site_area_min,input$site_area_max, input$site_area_mean,input$site_area_sd)
    }
    
    if(input$obj_dens_choice == "single value"){
      tmp_obj_dens = input$obj_dens_val
    }else{
      tmp_obj_dens = c(input$obj_dens_min,input$obj_dens_max)
    }
    
    SurveyParameters<-list(
      col.width = input$col_width,
      grid.type = input$grid_type,
      simulations = input$sims,
      area = c(input$area_width, input$area_length),
      site.density=tmp_dens,
      site.area=tmp_site_area,
      overlap=input$overlap,
      obj.density=tmp_obj_dens,
      obj.distribution=input$obj_dist,
      survey.radius=input$survey_radius
    )
    
    class(SurveyParameters)="SurveySim"
    
    return(SurveyParameters)
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

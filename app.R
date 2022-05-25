#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load libraries----
library(doolkit)
library(rgl)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
options(rgl.useNULL = TRUE)


# Load functions----
colrange <- function(x) {
  if (x == 1) result <- c("white", "black")
  if (x == 2) result <- colorspace::desaturate(c("royalblue", "white", "red"), amount = 0.3)
  if (x == 3) result <- colorspace::desaturate(c("royalblue", "lightskyblue", rep("olivedrab3", 3), "yellow1", "orange", "red"), amount = 0.3)
  if (x == 4) result <- colorspace::desaturate(c("lightgreen","goldenrod1","yellow1","white","white","lightskyblue","dodgerblue4","royalblue"), amount = 0.3)
  if (x == 5) result <- colorspace::desaturate(c("firebrick4","red","orangered","orange","yellow1","olivedrab3","lightseagreen","royalblue","royalblue4","royalblue","lightseagreen","olivedrab3","yellow1","orange","orangered","red","firebrick4"), amount = 0.3)
  if (x == 6) result <- colorspace::desaturate(c("blue","green","yellow","orange","red"), amount = 0.3)
  if (x == 7) result <- colorspace::desaturate(c("dodgerblue4","lightskyblue","sienna4","yellow1","red3","plum1","darkgreen","olivedrab3"), amount = 0.1)
  if (x == 8) result <- colorspace::desaturate(c("royalblue4","royalblue","lightseagreen","olivedrab3","yellow1","orange","orangered","red","firebrick4"), amount = 0.3)
  return(result)
}

compute.polygonal <- function(mesh, x) {
  if (x == 1) result <- Rvcg::vcgArea(mesh, perface = TRUE)$pertriangle
  if (x == 2) result <- doolkit::elev(mesh, origin = FALSE)
  if (x == 3) result <- doolkit::inclin(mesh)
  if (x == 4) result <- doolkit::orient(mesh)
  if (x == 5) result <- doolkit::slope(mesh)
  if (x == 6) result <- doolkit::angularity(mesh, ratio = FALSE)
  if (x == 7) result <- doolkit::angularity(mesh, ratio = TRUE)
  if (x == 8) result <- Rvcg::vcgCurve(mesh)$meanitmax
  if (x == 9) result <- Rvcg::vcgCurve(mesh)$gaussitmax
  if (x == 10) result <- Rvcg::vcgCurve(mesh)$K1
  if (x == 11) result <- Rvcg::vcgCurve(mesh)$K2
  if (x == 12) result <- doolkit::arc(mesh, range = c(-20, 20))
  if (x == 13) result <- doolkit::dne(mesh)
  return(result)
}

dta.legend <- function(x) {
  if (x == 1) result <- "3D Area"
  if (x == 2) result <- "Elevation"
  if (x == 3) result <- "Inclination"
  if (x == 4) result <- "Orientation"
  if (x == 5) result <- "Slope"
  if (x == 6) result <- "Angularity"
  if (x == 7) result <- "Angularity (as ratio)"
  if (x == 8) result <- "Mean curvature"
  if (x == 9) result <- "Gauss curvature"
  if (x == 10) result <- "Principal curvature K1"
  if (x == 11) result <- "Principal curvature K2"
  if (x == 12) result <- "Area-Relative Curvature"
  if (x == 13) result <- "Dirichlet Normal Energy"
  return(result)
}

minrange <- function(x) {
  if (x == 1) result <- NULL
  if (x == 2) result <- NULL
  if (x == 3) result <- NULL
  if (x == 4) result <- NULL
  if (x == 5) result <- 0
  if (x == 6) result <- NULL
  if (x == 7) result <- NULL
  if (x == 8) result <- NULL
  if (x == 9) result <- NULL
  if (x == 10) result <- NULL
  if (x == 11) result <- NULL
  if (x == 12) result <- -20
  if (x == 13) result <- NULL
  return(result)
}

maxrange <- function(x) {
  if (x == 1) result <- NULL
  if (x == 2) result <- NULL
  if (x == 3) result <- NULL
  if (x == 4) result <- NULL
  if (x == 5) result <- 90
  if (x == 6) result <- NULL
  if (x == 7) result <- NULL
  if (x == 8) result <- NULL
  if (x == 9) result <- NULL
  if (x == 10) result <- NULL
  if (x == 11) result <- NULL
  if (x == 12) result <- 20
  if (x == 13) result <- NULL
  return(result)
}

legtype <- function(x) {
  if (x == 1) result = "stack"
  if (x == 2) result = "pie"
  if (x == 3) result = "log"
  return(result)
}

nametag <- function(name, display) {
  result <- paste("")
  if (display) {
    name <- basename(name)
    result <- paste(name)
  }
  return(result)
}





# Define UI for application----
ui <- dashboardPage(
  # CSS style----
  includeCSS("doolkit.css"),

  #dashboardHeader----
  header = dashboardHeader(title = img(src = "doolkit.png", height = 42, width = 42, " doolkit"),
                  #titleWidth = 400,
                  dropdownMenu(
                    type = "notifications",
                    headerText = strong("HELP"),
                    icon = icon("question"),
                    badgeStatus = NULL,
                    notificationItem(
                      text = "",
                      icon = icon("spinner")),
                    notificationItem(
                      text = "",
                      icon = icon("mountain")),
                    notificationItem(
                      text = "",
                      icon = icon("crop")),
                    notificationItem(
                      text = "",
                      icon = icon("chart-area")),
                    notificationItem(
                      text = "",
                      icon = icon("table"))),
                  # About
                  tags$li(
                    class = "dropdown",
                    a(strong("About..."),
                      height = 40,
                      href = "https://www.rdocumentation.org/packages/doolkit/versions/1.42.1",
                      title = "",
                      target = "_blank")
                  )
                  #close dashboardHeader
  ),

  #dashboardSidebar----
  sidebar = dashboardSidebar(
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    
      sidebarMenu(
      # import surfaces----
      menuItem("Import",
               icon = icon("spinner"),
               # input: import surfaces
               fileInput(inputId = "import_surface",
                         label = "Import surface",
                         multiple = FALSE,
                         accept = c("text/plain",
                                    ".stl",
                                    ".ply"))),

      # mapping----
      menuItem("Map",
               icon = icon("mountain"),
               # input: select variable
               selectInput(inputId = "map_var_select",
                           label = "Select variable to map",
                           selected = 2,
                           choices = list("3D area" = 1,
                                          "Elevation" = 2,
                                          "Inclination" = 3,
                                          "Orientation" = 4,
                                          "Slope" = 5,
                                          "Angularity (in degree)" = 6,
                                          "Angularity (as ratio)" = 7,
                                          "Curvature (mean)" = 8,
                                          "Curvature (Gaussian)" = 9,
                                          "Curvature (K1)" = 10,
                                          "Curvature (K2)" = 11,
                                          "Curvature (ARC)" = 12,
                                          "Curvature (DNE)" = 13)),

               # input: color...
               selectInput(inputId = "col_range_select",
                           label = "Color range",
                           selected = 4,
                           choices = list("angularity" = 1,
                                          "arc" = 2,
                                          "dne" = 3,
                                          "elev" = 4,
                                          "inclin" = 5,
                                          "oedist" = 6,
                                          "orient" = 7,
                                          "slope" = 8)),
               # ...levels
               sliderInput(inputId = "col_levels_select",
                           label = "Color levels",
                           min = 2,
                           max = 256,
                           value = 256),

               # input: legend...
                
               fluidRow(
                 # ...type
                 column(width = 8,
                        selectInput(inputId = "leg_type_select",
                                    label = "Legend",
                                    selected = 1,
                                    choices = list("stack" = 1,
                                                   "pie" = 2,
                                                   "log" = 3))),
                 
                 # ...options
                 column(width = 5,
                        checkboxInput(inputId = "leg_options_select",
                                      label = "Display?",
                                      value = TRUE),
                        checkboxInput(inputId = "scale_options_select",
                                      label = "Scalebar?",
                                      value = FALSE)),
                        
                        
                 column(width = 5,
                        checkboxInput(inputId = "name_options_select",
                                      label = "Filename?",
                                      value = FALSE)))),



      # Cropping----
      menuItem("Crop",
               icon = icon("crop"),
               # input: select variable
               selectInput(inputId = "crop_var_select",
                           label = "Select a filter variable",
                           selected = 2,
                           choices = list("3D area" = 1,
                                          "Elevation" = 2,
                                          "Inclination" = 3,
                                          "Orientation" = 4,
                                          "Slope" = 5,
                                          "Angularity (in degree)" = 6,
                                          "Angularity (as ratio)" = 7,
                                          "Curvature (mean)" = 8,
                                          "Curvature (Gaussian)" = 9,
                                          "Curvature (K1)" = 10,
                                          "Curvature (K2)" = 11,
                                          "Curvature (ARC)" = 12,
                                          "Curvature (DNE)" = 13)),

               #input: select network...
               # ...filter range
               sliderInput(inputId = "net_range_select",
                           label = "Filter range",
                           min = 0,
                           max = 100,
                           value = c(0, 100)),
               # ...size
               sliderInput(inputId = "net_size_select",
                           label = "Minimal network size",
                           min = 1,
                           max = 100,
                           value = median(seq(1, 100)))),

      # graphs----
      menuItem("Graph",
               icon = icon("chart-area"),
               
               # input: select graph type
               selectInput(inputId = "graph_style",
                           label = "Graph style",
                           selected = 2,
                         choices = list("Histogram" = 1,
                                          "Profile" = 2))
      ),

      # table----
      menuItem("Table",
               icon = icon("table"),
               
               fluidRow(
                 column(4,
                        checkboxGroupInput(inputId = "table_select_relief", 
                                         label = "Relief", 
                                         choices = list("3D_area",
                                                        "Inclination",
                                                        "Slope", 
                                                        "RFI", 
                                                        "LRFI",
                                                        "Gamma"),
                                         selected = c("Slope", "RFI"))),
                 
                 column(4,
                        checkboxGroupInput(inputId = "table_select_sharpness", 
                                    label = "Sharpness", 
                                    choices = list("Angularity", 
                                                   "_ratio", 
                                                   "DNE",
                                                   "ARC",
                                                   "_positive",
                                                   "_negative"),
                                    selected = "DNE"))),
               
               
               fluidRow(
                 column(4,
                        checkboxGroupInput(inputId = "table_select_shape", 
                                           label = "Shape", 
                                           choices = list("Form_factor",
                                                          "Elongation",
                                                          "Lemniscate"),
                                           selected = NULL)),
                 
                 column(4,
                        checkboxGroupInput(inputId = "table_select_complexity", 
                                           label = "Complexity", 
                                           choices = list("OPCR", 
                                                          "_4bins", 
                                                          "_2bins"),
                                           selected = "OPCR"))),
               
               # Patch size for complexity
               sliderInput(inputId = "patch_size_select",
                           label = "Orientation patch size",
                           min = 3,
                           max = 100,
                           value = 3),
               
               
               fluidRow(
                 column(4,
                      checkboxGroupInput(inputId = "table_select_profile_slope", 
                                         label = "Cumulative\nprofile\nslope", 
                                         choices = list("_inclination",
                                                        "_slope",
                                                        "_angularity",
                                                        "_ARC",
                                                        "_orientation"),
                                         selected = NULL)),
                 column(4,
                        checkboxGroupInput(inputId = "table_select_profile_auc", 
                                           label = "Cumulative\nprofile\nAUC", 
                                           choices = list("_inclination",
                                                          "_slope",
                                                          "_angularity",
                                                          "_ARC",
                                                          "_orientation"),
                                           selected = NULL)))
               
      )

      #close sidebarMenu
    )
    #close dashboardSidebar
  ),

  #dashboardBody----
  body = dashboardBody(
    
    # compatibility with css
    tags$head(tags$script(src = "doolkit.css")),

    # rgl widget
    fluidRow(
      # graphics
      column(
        width = 12,
        uiOutput("dkdisplay"))),
    
    # datatable
    fluidRow(
      column(12,
             tableOutput('table')
      )
    )
    
    #close dashboardBody
  )
)


# Define server logic----
server <- function(input, output) {

  # Display multitab rgl/graphics-----
  output$dkdisplay <- renderUI({
    div(style = "position: relative",
        tabBox(id = "dkdisplay",
               height = 600, 
               width = 12,
               # 3d dkmap
               tabPanel(title = "3D map",
                        # save
                        #div(
                        #  style = "position: relative; left: 0.5em; bottom: 0.5em;",
                        #  actionButton(inputId = "map_save_html", 
                        #               label = "Save as html",
                        #               icon = icon("download", class = "opt"))),
                        # widget
                        rglwidgetOutput(outputId = "dkmap",
                                        width = "512px",
                                        height = "512px")),
               # Histogram / profile
               tabPanel(title = "Distribution",
                        # save
                        div(
                          style = "position: relative; left: 0.5em; bottom: 0.5em;",
                          dropdown( 
                            downloadButton(outputId = "down_plot_select", label = "Save plot"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE)),          
                        # plot
                        plotOutput(outputId = "dkplot", 
                                   height = 300))
               #end tabBox 
        )
    )
  })
  
  #Display rgl window----
  #save <- options(rgl.inShiny = TRUE)
  #on.exit(options(save))
  output$dkmap <- renderRglwidget(
    expr =
      {
        #Wait for fileInput
        req(input$import_surface)
        #Build mesh
        mesh <- Rvcg::vcgImport(input$import_surface$datapath,
                                updateNormals = TRUE,
                                silent = TRUE)
        #Build y
        y <- compute.polygonal(mesh = mesh,
                               x = input$map_var_select)
        #Color
        col.range <- colrange(input$col_range_select)
        #Range
        min.range <- minrange(input$map_var_select)
        max.range <- maxrange(input$map_var_select)
        #Legend
        leg.type <- legtype(input$leg_type_select)
        leg.label <- dta.legend(input$map_var_select)
        #Nametag
        name.tag <- nametag(name = input$import_surface$name,
                            display = input$name_options_select)
        #Alpha
        ybis <- compute.polygonal(mesh = mesh,
                                  x = input$crop_var_select)
        polynetwork <- doolkit::poly.network(mesh = mesh,
                                             y = ybis,
                                             lwr.limit = quantile(ybis, input$net_range_select[1]/100),
                                             upr.limit = quantile(ybis, input$net_range_select[2]/100),
                                             min.size = input$net_size_select)
        
        alpha <- rep(0.1, Rvcg::nfaces(mesh))
        alpha[polynetwork@faces] <- 0.99
        #Close any existing rgl window
        try(close3d())
        #Build dkmap...
        dkmap(mesh = mesh,
              y = y,
              col = col.range,
              col.levels = input$col_levels_select,
              legend.lab = leg.label,
              legend.type = leg.type,
              legend = input$leg_options_select,
              scalebar = input$scale_options_select,
              alpha = alpha,
              lit = FALSE,
              bg = "grey",
              main = name.tag,
              cex.main = 2,
              orient = "occlusal",
              min.range = min.range,
              max.range = max.range
        )
        #...and display it
        rglwidget()
      })
  
  # Snapshot rgl----
  observeEvent(input$map_save_html, {
    filename <- tcltk::tclvalue(tcltk::tkgetSaveFile(title = "Save map as...",
                                                     initialfile = "Untitled"))
    htmlwidgets::saveWidget(rglwidget(width = 512,
                                      height = 512),
                            paste(filename,
                                  ".html",
                                  sep = ""))
  })
  
  # Display graphics----
  plot_select <- reactive({
    # Wait for fileInput
    req(input$import_surface)
    # Import mesh
    mesh <- Rvcg::vcgImport(input$import_surface$datapath,
                            updateNormals = TRUE,
                            silent = TRUE)
    # Compute topographic variable
    y <- compute.polygonal(mesh = mesh,
                           x = input$map_var_select)
    # ...histogram
    if (input$graph_style == 1) {
      dkdata <- data.frame(y = y)
      plot <- ggplot2::ggplot(dkdata, ggplot2::aes(x = y)) + 
        ggplot2::geom_histogram(color = "white", fill = "hotpink") +
        ggplot2::labs(dta.legend(input$map_var_select))
      plotname <- "Histogram"
    } else { 
    # ...profile
        if (input$graph_style == 2) {
        plot <- dkprofile(y, col = "hotpink")$profile
        plotname <- "Cumulative profile"
      }
    }
    # add title
    plot + ggplot2::ggtitle(label = paste(plotname, 
                                          dta.legend(input$map_var_select), 
                                          sep = ", ")) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
  })
  # Output
  output$dkplot <- renderPlot(plot_select())
  
  # Download graphics----
  download_box <- function(exportname, plot) {
    downloadHandler(
      filename = function() {
        paste(exportname, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = plot, device = "png", width = 8)
      }
    )
  }
  # Output
  output$down_plot_select <- download_box("Untitled", plot_select())
  
  # Display table----
  output$table <- renderTable({
    # Wait for fileInput
    req(input$import_surface)
    #Build mesh
    mesh <- Rvcg::vcgImport(input$import_surface$datapath,
                            updateNormals = TRUE,
                            silent = TRUE)
    # Build primitive table
    mytable <- matrix(c("Vertices", 
                      "Faces",
                      Rvcg::nverts(mesh),
                      Rvcg::nfaces(mesh)),
                      ncol = 2)
    # Add lines to the table
    if ("3D_area" %in% input$table_select_relief) {
      mytable <- rbind(mytable, 
                       c("Surface_area",
                         round(Rvcg::vcgArea(mesh), 3)))
    }
    if ("Inclination" %in% input$table_select_relief) {
      mytable <- rbind(mytable, 
                       c("Average_inclination",
                         round(mean(doolkit::inclin(mesh)), 3)))
    }
    if ("Slope" %in% input$table_select_relief) {
      mytable <- rbind(mytable, 
                       c("Average_slope",
                         round(mean(doolkit::slope(mesh)), 3)))
    }
    if ("RFI" %in% input$table_select_relief) {
      mytable <- rbind(mytable, 
                       c("RFI",
                         round(doolkit::rfi(mesh, method = "Ungar"), 3)))
    }
    if ("LRFI" %in% input$table_select_relief) {
      mytable <- rbind(mytable, 
                       c("LRFI",
                         round(doolkit::rfi(mesh, method = "Boyer"), 3)))
    }
    if ("Gamma" %in% input$table_select_relief) {
      mytable <- rbind(mytable, 
                       c("Gamma",
                         round(doolkit::rfi(mesh, method = "Guy"), 3)))
    }
    if ("Angularity" %in% input$table_select_sharpness) {
      mytable <- rbind(mytable, 
                       c("Angularity",
                         round(mean(doolkit::angularity(mesh)), 3)))
    }
    if ("_ratio" %in% input$table_select_sharpness) {
      mytable <- rbind(mytable, 
                       c("Angularity_ratio",
                         round(mean(doolkit::angularity(mesh, ratio = TRUE)), 3)))
    }
    if ("DNE" %in% input$table_select_sharpness) {
      mytable <- rbind(mytable, 
                       c("DNE",
                         round(doolkit::dne(mesh, total = TRUE), 3)))
    }
    if ("ARC" %in% input$table_select_sharpness) {
      mytable <- rbind(mytable, 
                       c("ARC",
                         round(mean(doolkit::arc(mesh)), 3)))
    }
    if ("_positive" %in% input$table_select_sharpness) {
      mytable <- rbind(mytable, 
                       c("Positive_ARC",
                         round(mean(doolkit::arc(mesh)[which(doolkit::arc(mesh) >= 0)]), 3)))
    }
    if ("_negative" %in% input$table_select_sharpness) {
      mytable <- rbind(mytable, 
                       c("Negative_ARC",
                         round(mean(doolkit::arc(mesh)[which(doolkit::arc(mesh) < 0)]), 3)))
    }
    if ("Form_factor" %in% input$table_select_shape) {
      mytable <- rbind(mytable, 
                       c("Form_factor",
                         round(doolkit::shape.index(mesh)$FormFactor, 3)))
    }
    if ("Elongation" %in% input$table_select_shape) {
      mytable <- rbind(mytable, 
                       c("Elongation",
                         round(doolkit::shape.index(mesh)$Elongation, 3)))
    }
    if ("Lemniscate" %in% input$table_select_shape) {
      mytable <- rbind(mytable, 
                       c("Lemniscate_ratio",
                         round(doolkit::shape.index(mesh)$K, 3)))
    }
    if ("OPCR" %in% input$table_select_complexity) {
      mytable <- rbind(mytable, 
                       c("OPCR",
                         round(doolkit::opcr(mesh,
                                             min.size = input$patch_size_select,
                                             bins = 8)$opcr, 2)))
    }
    if ("_4bins" %in% input$table_select_complexity) {
      mytable <- rbind(mytable, 
                       c("OPCR_4bins",
                         round(doolkit::opcr(mesh,
                                             min.size = input$patch_size_select,
                                             bins = 4)$opcr, 2)))
    }
    if ("_2bins" %in% input$table_select_complexity) {
      mytable <- rbind(mytable, 
                       c("OPCR_2bins",
                         round(doolkit::opcr(mesh,
                                             min.size = input$patch_size_select,
                                             bins = 2)$opcr, 2)))
    }
    if ("_inclination" %in% input$table_select_profile_slope) {
      mytable <- rbind(mytable, 
                       c("Inclination_profile_slope",
                         round(doolkit::dkprofile(doolkit::inclin(mesh))$slope, 2)))
    }
    if ("_slope" %in% input$table_select_profile_slope) {
      mytable <- rbind(mytable, 
                       c("Slope_profile_slope",
                         round(doolkit::dkprofile(doolkit::slope(mesh))$slope, 2)))
    }
    if ("_angularity" %in% input$table_select_profile_slope) {
      mytable <- rbind(mytable, 
                       c("Angularity_profile_slope",
                         round(doolkit::dkprofile(doolkit::angularity(mesh))$slope, 2)))
    }
    if ("_ARC" %in% input$table_select_profile_slope) {
      mytable <- rbind(mytable, 
                       c("ARC_profile_slope",
                         round(doolkit::dkprofile(doolkit::arc(mesh))$slope, 2)))
    }
    if ("_orientation" %in% input$table_select_profile_slope) {
      mytable <- rbind(mytable, 
                       c("Orientation_profile_slope",
                         round(doolkit::dkprofile(doolkit::orient(mesh))$slope, 2)))
    }
    if ("_inclination" %in% input$table_select_profile_auc) {
      mytable <- rbind(mytable, 
                       c("Inclination_profile_AUC",
                         round(doolkit::dkprofile(doolkit::inclin(mesh))$auc, 2)))
    }
    if ("_slope" %in% input$table_select_profile_auc) {
      mytable <- rbind(mytable, 
                       c("Slope_profile_AUC",
                         round(doolkit::dkprofile(doolkit::slope(mesh))$auc, 2)))
    }
    if ("_angularity" %in% input$table_select_profile_auc) {
      mytable <- rbind(mytable, 
                       c("Angularity_profile_AUC",
                         round(doolkit::dkprofile(doolkit::angularity(mesh))$auc, 2)))
    }
    if ("_ARC" %in% input$table_select_profile_auc) {
      mytable <- rbind(mytable, 
                       c("ARC_profile_AUC",
                         round(doolkit::dkprofile(doolkit::arc(mesh))$auc, 2)))
    }
    if ("_orientation" %in% input$table_select_profile_auc) {
      mytable <- rbind(mytable, 
                       c("Orientation_profile_AUC",
                         round(doolkit::dkprofile(doolkit::orient(mesh))$auc, 2)))
    }
    
    
    
    # Change column names
    colnames(mytable) <- c("File", tail(input$import_surface$name, 1))
    mytable
    
      })
  
  
  
}
  

  # Run the application
  shinyApp(ui = ui, server = server)

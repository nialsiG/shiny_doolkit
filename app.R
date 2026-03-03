#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load libraries----
RGL_USE_NULL <- TRUE
# options(rgl.useNULL = RGL_USE_NULL)
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
library(doolkit)
library(rgl)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyjs)


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
  # CSS style
  includeCSS("doolkit.css"),
  # Header----
  header = dashboardHeader(
    title = img(src = "doolkit.png", height = 42, width = 42, " doolkit"),
    # ...dropdown
    dropdownMenu(
      type = "notifications",
      headerText = strong("HELP"),
      icon = icon("question"),
      badgeStatus = NULL,
      notificationItem(text = "", icon = icon("spinner")),
      notificationItem(text = "", icon = icon("mountain")),
      notificationItem(text = "", icon = icon("crop")),
      notificationItem(text = "", icon = icon("chart-area")),
      notificationItem(text = "", icon = icon("table"))),
    # ...about
    tags$li(
      class = "dropdown",
      a(strong("About..."),
        href = "https://www.rdocumentation.org/packages/doolkit/versions/1.42.2",
        height = 40,
        title = "",
        target = "_blank"))),

  # Sidebar----
  sidebar = dashboardSidebar(
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    sidebarMenu(
      # Single surface----
      menuItem(
        "Single surface",
        icon = icon("tooth"),
        # ...import----
        menuItem(
          "File",
          icon = icon("file-import"),
          # input: import surfaces
          fileInput(
            inputId = "import_surface",
            label = "Import surface",
            multiple = FALSE,
            accept = c("text/plain", ".stl", ".ply")
          )
        ),
        # ...crop----
        menuItem(
          "Crop",
          icon = icon("crop"),
          # input: select variable
          selectInput(
            inputId = "crop_var_select",
            label = "Select a filter variable",
            selected = 2,
            choices = list(
              "3D area" = 1,
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
          sliderInput(
            inputId = "net_range_select",
            label = "Filter range",
            min = 0,
            max = 100,
            value = c(0, 100)),
          sliderInput(
            inputId = "net_size_select",
            label = "Minimal network size",
            min = 1,
            max = 100,
            value = median(seq(1, 100))
          )
        ),
        # ...tilt----
        menuItem(
          "Tilt",
          icon = icon("lines-leaning")
        ),
        # ...map----
        menuItem(
          "Map",
          icon = icon("mountain"),
          # input: select variable
          selectInput(
            inputId = "map_var_select",
            label = "Select variable to map",
            selected = 2,
            choices = list(
              "3D area" = 1,
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
          selectInput(
            inputId = "col_range_select",
            label = "Color range",
            selected = 4,
            choices = list(
              "angularity" = 1,
              "arc" = 2,
              "dne" = 3,
              "elev" = 4,
              "inclin" = 5,
              "oedist" = 6,
              "orient" = 7,
              "slope" = 8)),
          # input: levels
          sliderInput(
            inputId = "col_levels_select",
            label = "Color levels",
            min = 2,
            max = 256,
            value = 256),
          # input: legend...
          fluidRow(
            # ...legend type
            column(
              width = 8,
              selectInput(
                inputId = "leg_type_select",
                label = "Legend",
                selected = 1,
                choices = list(
                  "stack" = 1,
                  "pie" = 2,
                  "log" = 3))),
            # ...options
            column(
              width = 5,
              checkboxInput(
                inputId = "leg_options_select",
                label = "Display?",
                value = TRUE),
              checkboxInput(
                inputId = "scale_options_select",
                label = "Scalebar?",
                value = FALSE)),
            # ...filename
            column(
              width = 5,
              checkboxInput(
                inputId = "name_options_select",
                label = "Filename?",
                value = FALSE)
            )
          )
        ),
        # ...distribution----
        menuItem(
          "Distribution",
          icon = icon("chart-column"),
          # input: select variable to graph
          selectInput(
            inputId = "chart_var_select",
            label = "Select variable to map",
            selected = 2,
            choices = list(
              "3D area" = 1,
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
          # input: select graph type
          selectInput(
            inputId = "chart_style",
            label = "Chart style",
            selected = 1,
            choices = list(
              "Histogram" = 1,
              "Cumulative profile" = 2)
          )
        ),
        # ...batch analysis----
        menuItem(
          "Batch analysis",
          icon = icon("object-group"),
          menuItem(
            "Options...",
            icon = icon("gears"),
            # ...patch size for complexity
            sliderInput(
              inputId = "patch_size_select",
              label = "Orientation patch size",
              min = 3,
              max = 100,
              value = 3
            )
            # ...
          ),
          fluidRow(
            column(
              4,
              checkboxGroupInput(
                inputId = "single_table_select_relief",
                label = "Relief",
                choices = list(
                  "3D_area",
                  "Inclination",
                  "Slope",
                  "RFI",
                  "LRFI",
                  "Gamma"),
                selected = c("Slope", "RFI"))
            ),
            column(
              4,
              checkboxGroupInput(
                inputId = "single_table_select_sharpness",
                label = "Sharpness",
                choices = list(
                  "Angularity",
                  "_ratio",
                  "DNE",
                  "ARC",
                  "_positive",
                  "_negative"),
                selected = "DNE")
            )
          ),

          fluidRow(
            column(
              4,
              checkboxGroupInput(
                inputId = "table_select_shape",
                label = "Shape",
                choices = list(
                  "Form_factor",
                  "Elongation",
                  "Lemniscate"),
                selected = NULL)
            ),
            column(
              4,
              checkboxGroupInput(
                inputId = "table_select_complexity",
                label = "Complexity",
                choices = list(
                  "OPCR",
                  "_4bins",
                  "_2bins"),
                selected = "OPCR")
            )
          )
        )
      ),

      # Surface to surface----
      menuItem(
        "Surface-to-surface",
        icon = icon("layer-group"),
        # ...import
        menuItem(
          "File",
          icon = icon("file-import"),
          # input: import surfaces
          fileInput(
            inputId = "import_oes_surface",
            label = "Select outer surface file",
            multiple = FALSE,
            accept = c("text/plain", ".stl", ".ply")
          ),
          fileInput(
            inputId = "import_edj_surface",
            label = "Select inner surface file",
            multiple = FALSE,
            accept = c("text/plain", ".stl", ".ply")
          )
        ),
        # ...distance
        menuItem(
          "Distance",
          icon = icon("arrows-left-right-to-line")
        )
      ),

      # Multi-surface----
      menuItem(
        "Multi-surface",
        icon = icon("cubes"),
        # ...import
        menuItem(
          "Files",
          icon = icon("file-import"),
          # input: import surfaces
          fileInput(
            inputId = "import_multi_surfaces",
            label = "Select files",
            multiple = TRUE,
            accept = c("text/plain", ".stl", ".ply")
          )
        ),
        # ...
        menuItem(
          "Batch analysis",
          icon = icon("object-group"),
          menuItem(
            "Options...",
            icon = icon("gears"),
            # ...patch size for complexity
            sliderInput(
              inputId = "multi_patch_size_select",
              label = "Orientation patch size",
              min = 3,
              max = 100,
              value = 3
            )
            # ...
          ),
          fluidRow(
            column(
              4,
              checkboxGroupInput(
                inputId = "multi_table_select_relief",
                label = "Relief",
                choices = list(
                  "3D_area",
                  "Inclination",
                  "Slope",
                  "RFI",
                  "LRFI",
                  "Gamma"),
                selected = c("Slope"))
            ),
            column(
              4,
              checkboxGroupInput(
                inputId = "multi_table_select_sharpness",
                label = "Sharpness",
                choices = list(
                  "Angularity",
                  "_ratio",
                  "DNE",
                  "ARC",
                  "_positive",
                  "_negative"),
                selected = "DNE")
            )
          ),

          fluidRow(
            column(
              4,
              checkboxGroupInput(
                inputId = "multi_table_select_shape",
                label = "Shape",
                choices = list(
                  "Form_factor",
                  "Elongation",
                  "Lemniscate"),
                selected = NULL)
            ),
            column(
              4,
              checkboxGroupInput(
                inputId = "multi_table_select_complexity",
                label = "Complexity",
                choices = list(
                  "OPCR",
                  "_4bins",
                  "_2bins"),
                selected = "OPCR")
            )
          ),
          actionButton("batch_multi_event", "Start batch analysis")
        )
      )
      # close sidebar
    )
  ),

  # Body----
  body = dashboardBody(
    # compatibility with css
    tags$head(tags$script(src = "doolkit.css")),

    # rgl widget
    fluidRow(
      # graphics
      column(
        width = 12,
        uiOutput("dkdisplay"))),

    # # datatable
    # fluidRow(
    #   column(
    #     12,
    #     tableOutput("table"))),

    textOutput("time")

    # Close Body
  )
  # Close ui
)



# Define server logic required to draw a histogram----
server <- function(input, output) {
  # Tabs----
  output$dkdisplay <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "dkdisplay",
        height = 600,
        width = 12,
        # ...3d dkmap
        tabPanel(
          title = "Map",
          # save
          div(
            style = "position: relative; left: 0.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_map_select", label = "Save as html"),
              size = "xs",
              icon = icon("download", class = "opt"),
              up = TRUE)
          ),
          rglwidgetOutput(outputId = "dkmap", width = "512px", height = "512px")
        ),
        # ...Charts
        tabPanel(
          title = "Charts",
          # save
          div(
            style = "position: relative; left: 0.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_plot_select", label = "Save plot"),
              size = "xs",
              icon = icon("download", class = "opt"),
              up = TRUE)
          ),
          plotOutput(outputId = "dkplot", width = "512px", height = "512px")
        ),
        # ...Dataframe
        tabPanel(
          title = "Dataframe",
          # save
          div(
            style = "position: relative; left: 0.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_dataframe_select", label = "Save dataframe"),
              size = "xs",
              icon = icon("download", class = "opt"),
              up = TRUE)
          ),
          DTOutput(outputId = "body_dataframe")
        )

        #end tabBox
      )
    )
  })

  # Reactive values----
  batchData <- reactiveValues(data = NULL)

  # Display rgl map----
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

  # Display charts----
  output$dkplot <- renderPlot({
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
    if (input$chart_style == 1) {
      dkdata <- data.frame(y = y)
      plot <- ggplot2::ggplot(dkdata, ggplot2::aes(x = y)) +
        ggplot2::geom_histogram(color = "white", fill = "hotpink") +
        ggplot2::labs(dta.legend(input$map_var_select))
      plotname <- "Histogram"
    } else {
      # ...profile
      if (input$chart_style == 2) {
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

  # Display dataframe----
  output$body_dataframe <- renderDT({
    Mydf <- batchData$data
    Mydf <- unclass(Mydf)
    Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    DT::datatable(Mydf,
                  rownames = FALSE,
                  extensions = c("FixedColumns"),
                  selection = list(target = "column"),
                  options = list(
                    fixedColumns = list(leftColumns = 1),
                    scrollX = TRUE
                  )
    )
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


  # Download graphics----
  download_box <- function(exportname, plot) {
    downloadHandler(
      filename = function() {
        paste(exportname, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot, device = "png", width = 8)
      }
    )
  }
  # Output
  output$down_plot_select <- download_box("Untitled", plot_select())

  # Multiple batch analysis----
  output$time <- renderText({
    format(Sys.time(), "%a %b %d %X %Y")
  }) |>
    bindEvent(input$batch_multi_event)




  # Buttons----
  # ...multibatch
  observeEvent(input$batch_multi_event, {
    multi_opcr_patchsize <- input$multi_patch_size_select
    mesh_files <- input$import_multi_surfaces$datapath
    batchData$data <- get_batch_multi_dataframe(mesh_files, multi_opcr_patchsize)
  })

  # Methods----
  # ...multi batch
  get_batch_multi_dataframe <- function(mesh_files, multi_opcr_patchsize) {
    # Prepare function list
    my_funs <- c(input$multi_table_select_relief, input$multi_table_select_sharpness, input$multi_table_select_shape, input$multi_table_select_complexity)
    fun_list <- list()
    for (fun in my_funs) {
      if (fun == "3D_area") fun_list <- rlist::list.append(fun_list, "3d_area" = function(mesh) return(Rvcg::vcgArea(mesh)))
      if (fun == "Inclination") fun_list <- rlist::list.append(fun_list, "Inclination" = function(mesh) return(mean(doolkit::inclin(mesh))))
      if (fun == "Slope") fun_list <- rlist::list.append(fun_list, "Slope" = function(mesh) return(mean(doolkit::slope(mesh))))

      if (fun == "RFI") fun_list <- rlist::list.append(fun_list, "RFI" = function(mesh) return(doolkit::rfi(mesh, method = "Ungar")))
      if (fun == "LRFI") fun_list <- rlist::list.append(fun_list, "LRFI" = function(mesh) return(doolkit::rfi(mesh, method = "Boyer")))
      if (fun == "Gamma") fun_list <- rlist::list.append(fun_list, "Gamma" = function(mesh) return(doolkit::rfi(mesh, method = "Guy")))

      if (fun == "Angularity") fun_list <- rlist::list.append(fun_list, "Angularity" = function(mesh) return(mean(doolkit::angularity(mesh))))
      if (fun == "_ratio") fun_list <- rlist::list.append(fun_list, "Angularity_ratio" = function(mesh) return(mean(doolkit::angularity(mesh, ratio = TRUE))))
      if (fun == "DNE") fun_list <- rlist::list.append(fun_list, "DNE" = function(mesh) return(doolkit::dne(mesh, total = TRUE)))
      if (fun == "ARC") fun_list <- rlist::list.append(fun_list, "ARC" = function(mesh) return(mean(doolkit::arc(mesh))))
      if (fun == "_positive") fun_list <- rlist::list.append(fun_list, "Arc_positive" = function(mesh) {
        curvature <- doolkit::arc(mesh)
        return(mean(curvature[curvature >= 0]))
      })
      if (fun == "_negative") fun_list <- rlist::list.append(fun_list, "Arc_negative" = function(mesh) {
        curvature <- doolkit::arc(mesh)
        return(mean(curvature[curvature < 0]))
      })
      if (fun == "Form_factor") fun_list <- rlist::list.append(fun_list, "Form_factor" = function(mesh) return(doolkit::shape.index(mesh)$FormFactor))
      if (fun == "Elongation") fun_list <- rlist::list.append(fun_list, "Elongation" = function(mesh) return(doolkit::shape.index(mesh)$Elongation))
      if (fun == "Lemniscate") fun_list <- rlist::list.append(fun_list, "Lemniscate_ratio" = function(mesh) return(doolkit::shape.index(mesh)$K))
      if (fun == "OPCR") fun_list <- rlist::list.append(fun_list, "OPCR" = function(mesh) return(doolkit::opcr(mesh, bins = 8, min.size = multi_opcr_patchsize)$opcr))
      if (fun == "_4bins") fun_list <- rlist::list.append(fun_list, "OPCR_4bins" = function(mesh) return(doolkit::opcr(mesh, bins = 4, min.size = multi_opcr_patchsize)$opcr))
      if (fun == "_2bins") fun_list <- rlist::list.append(fun_list, "OPCR_2bins" = function(mesh) return(doolkit::opcr(mesh, bins = 2, min.size = multi_opcr_patchsize)$opcr))
    }

    # fun_list[sapply(fun_list, is.null)] <- NULL
    #TODO manage empty lists

    result <- doolkit::batch.multi(mesh_files, fun_list)
    return(result)
  }
}


# Run the application
shinyApp(ui = ui, server = server)

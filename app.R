#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load libraries----
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
library(doolkit)
library(rgl)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(svglite)
library(colourpicker)

predefined_palettes <- list(
  "area2D" = c("white", "black"),
  "arc" = colorspace::desaturate(c("royalblue", "white", "red"), amount = 0.3),
  "dne" = colorspace::desaturate(c("royalblue", "lightskyblue", rep("olivedrab3", 3), "yellow1", "orange", "red"), amount = 0.3),
  "elev" = colorspace::desaturate(c("lightgreen","goldenrod1","yellow1","white","white","lightskyblue","dodgerblue4","royalblue"), amount = 0.3),
  "inclin" = colorspace::desaturate(c("firebrick4","red","orangered","orange","yellow1","olivedrab3","lightseagreen","royalblue","royalblue4","royalblue","lightseagreen","olivedrab3","yellow1","orange","orangered","red","firebrick4"), amount = 0.3),
  "oedist" = colorspace::desaturate(c("blue","green","yellow","orange","red"), amount = 0.3),
  "orient" = colorspace::desaturate(c("dodgerblue4","lightskyblue","sienna4","yellow1","red3","plum1","darkgreen","olivedrab3"), amount = 0.1),
  "slope" = colorspace::desaturate(c("royalblue4","royalblue","lightseagreen","olivedrab3","yellow1","orange","orangered","red","firebrick4"), amount = 0.3)
)


# UI----
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
        # Input to let the user select the max upload size in MB
        numericInput("max_upload_size", "Set Max Upload Size (MB):", value = 5, min = 1, max = 1000),
        actionButton("update_limit", "Apply New Limit", class = "btn-primary"),
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
              "Curvature (ARC)" = 10,
              "Curvature (DNE)" = 11)),
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
        # ...batch analysis----
        menuItem(
          "Batch analysis",
          icon = icon("object-group"),
          menuItem(
            "Options...",
            icon = icon("gears")
            #TODO add options here
          ),
          # ......variables----
          fluidRow(
            column(
              4,
              checkboxGroupInput(
                inputId = "single_table_select_relief",
                label = "Relief",
                choices = list(
                  "Elevation",
                  "Inclination",
                  "Orientation",
                  "Slope"),
                selected = c("Slope", "Orientation")),
              checkboxGroupInput(
                inputId = "single_table_select_topology",
                label = "Topology",
                choices = list(
                  "3D area"),
                selected = NULL)
            ),
            column(
              4,
              checkboxGroupInput(
                inputId = "single_table_select_sharpness",
                label = "Sharpness",
                choices = list(
                  "Angularity (in degree)",
                  "Angularity (as ratio)",
                  "Curvature (mean)",
                  "Curvature (Gaussian)",
                  "Curvature (ARC)",
                  "Curvature (DNE)"),
                selected = "Curvature (ARC)")
            )
          ),

          # ......start button----
          actionButton("batch_single_event", "Start batch analysis")
        )
      ),

      # Map----
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
            "Curvature (ARC)" = 10,
            "Curvature (DNE)" = 11,
            "Distance" = 12)
        ),

        # input: color
        menuItem(
          "Color",
          icon = icon("palette"),
          selectInput("palette_choice", "Select a color palette:",
                      choices = c("Custom", names(predefined_palettes))),
          sliderInput("color_count", "Number of palette colors:",
                      min = 2, max = 10, value = 3),
          uiOutput("color_pickers_ui")
        ),

        # input: legend
        menuItem(
          "Legend",
          icon = icon("chart-bar"),
            # ...color levels
            sliderInput(
              inputId = "col_levels_select",
              label = "Legend color levels",
              min = 2,
              max = 256,
              value = 256),

          fluidRow(
            # ...legend type
            column(
              width = 8,
              selectInput(
                inputId = "leg_type_select",
                label = "Legend type",
                selected = 1,
                choices = list(
                  "stack" = 1,
                  "pie" = 2,
                  "log" = 3)),
            # ...options
            checkboxInput(
              inputId = "leg_options_select",
              label = "Show legend",
              value = TRUE),
            checkboxInput(
              inputId = "scale_options_select",
              label = "Show scalebar",
              value = FALSE),
            checkboxInput(
              inputId = "name_options_select",
              label = "Show filename",
              value = FALSE)
          ))
        )
      ),

      # Distribution----
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
            "Curvature (ARC)" = 10,
            "Curvature (DNE)" = 11)),
        # input: select graph type
        selectInput(
          inputId = "chart_style",
          label = "Chart style",
          selected = 1,
          choices = list(
            "Histogram" = 1,
            "Cumulative profile" = 2)
        ),

        # input: options
        menuItem(
          "Options",
          icon = icon("gears"),
          checkboxInput(
            inputId = "chart_options_percentage",
            label = "Draw profile using percentage",
            value = TRUE),
          checkboxInput(
            inputId = "chart_options_show_auc",
            label = "Show profile AUC",
            value = FALSE),
          checkboxInput(
            inputId = "chart_options_show_slope",
            label = "Show profile slope",
            value = FALSE)
        )
      ),

      # Surface to surface----
      menuItem(
        "Surface-to-surface",
        icon = icon("layer-group"),
        # ...import----
        menuItem(
          "File",
          icon = icon("file-import"),
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

        # ...pairing----
        menuItem(
          "Pairing",
          icon = icon("arrows-left-right-to-line"),
          # input: select pairing method
          selectInput(
            inputId = "pairing_method_select",
            label = "Select mesh-to-mesh triangle pairing method",
            selected = 2,
            choices = list(
              "Nearest triangle" = 1,
              "Along normals" = 2,
              "Along Z-axis" = 3
            )
          )
        ),
        # ...face-scale analysis----
        menuItem(
          "Face-scale analysis",
          icon = icon("object-group"),
          menuItem(
            "Options...",
            icon = icon("gears")
            #TODO add options here
          ),
          # ......variables----
          fluidRow(
            column(
              4,
              checkboxGroupInput(
                inputId = "double_table_select",
                label = "Select variables",
                choices = list(
                  "Paired triangle indices",
                  "Distance",
                  "Elevation delta",
                  "Inclination delta",
                  "Slope delta",
                  "Angularity delta (in degree)",
                  "Angularity delta (as ratio)",
                  "Curvature delta (mean)",
                  "Curvature delta (Gaussian)",
                  "Curvature delta (ARC)",
                  "Curvature delta (DNE)"),
                selected = c("Paired triangle indices", "Distance"))
            )
          ),
          # ......start button----
          actionButton("face_batch_double_event", "Start batch analysis")
        ),

          # ...mesh-scale analysis----
          menuItem(
            "Mesh-scale analysis",
            icon = icon("object-group"),
            menuItem(
              "Options...",
              icon = icon("gears")
              #TODO add options here
            ),
            # ......variables----
            fluidRow(
              column(
                4,
                checkboxGroupInput(
                  inputId = "double_table_select_relief",
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
                  inputId = "double_table_select_sharpness",
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
                  inputId = "double_table_select_distance",
                  label = "Shape",
                  choices = list(
                    "Distance",
                    "Elongation",
                    "Lemniscate"),
                  selected = NULL)
              ),
              column(
                4,
                checkboxGroupInput(
                  inputId = "double_table_select_complexity",
                  label = "Complexity",
                  choices = list(
                    "OPCR",
                    "_4bins",
                    "_2bins"),
                  selected = "OPCR")
              )
            ),
            # ......start button----
          actionButton("mesh_batch_double_event", "Start batch analysis")
        )
      ),

      # Multi-surface----
      menuItem(
        "Multi-surface",
        icon = icon("cubes"),
        # ...import----
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
        # ...batch analysis----
        menuItem(
          "Batch analysis",
          icon = icon("object-group"),
          # ......options----
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
            #TODO add options here
          ),
          # ......variables----
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
          # ......start button----
          actionButton("batch_multi_event", "Start batch analysis")
        )
      )
    )
  ),


  # Body----
  body = dashboardBody(
    # compatibility with css
    tags$head(tags$script(src = "doolkit.css")),
    # dkdisplay
    tabBox(
      # ...3d dkmap
      tabPanel(
        title = "Maps",
        div(
          style = "position: relative; left: 0.5em; bottom: 0.5em;",
          dropdown(
            downloadButton(outputId = "download_map_png", label = "Save plot as .png"),
            downloadButton(outputId = "download_map_html", label = "Save plot as .html"),
            size = "xs",
            icon = icon("download", class = "opt"),
            up = TRUE)
        ),
        rglwidgetOutput(outputId = "dkmap", width = "512px", height = "512px")
      ),
      # ...Charts
      tabPanel(
        title = "Charts",
        div(
          style = "position: relative; left: 0.5em; bottom: 0.5em;",
          dropdown(
            downloadButton(outputId = "download_chart_jpg", label = "Save plot as .jpg"),
            downloadButton(outputId = "download_chart_png", label = "Save plot as .png"),
            downloadButton(outputId = "download_chart_svg", label = "Save plot as .svg"),
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
        fluidRow(
          div(
            style = "position: relative; left: 0.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "download_dataframe_txt", label = "Save as .txt"),
              downloadButton(outputId = "download_dataframe_csv", label = "Save as .csv"),
              downloadButton(outputId = "download_dataframe_rds", label = "Save as .RDS"),
              downloadButton(outputId = "download_dataframe_xlsx", label = "Save as .xlsx"),
              size = "xs",
              icon = icon("download", class = "opt"),
              up = TRUE)
          )
        ),
        fluidRow(
          DTOutput(outputId = "body_dataframe")
        )
      )
    ),
  )
)



# ----

# Server----
server <- function(input, output, session) {
  # Reactive values----
  batchData <- reactiveValues(data = data.frame())
  loadedItems <- reactiveValues(mesh = NULL)

  # RGL
  options(rgl.useNULL = TRUE)


  # Change max upload size
  # ...initialize at 5MB
  options(shiny.maxRequestSize = 5 * 1024^2)
  # ...on value changed
  observeEvent(input$update_limit, {
    req(input$max_upload_size)
    new_size_bytes <- input$max_upload_size * 1024^2
    options(shiny.maxRequestSize = new_size_bytes)
    showNotification(
      paste("Upload limit updated to", input$max_upload_size, "MB"),
      type = "message"
    )
  })

  # Make rgl map----
  make_map <- reactive({
    #Wait for fileInput
    req(input$import_surface)
    #Build mesh
    loadedItems$mesh <- Rvcg::vcgImport(input$import_surface$datapath, updateNormals = TRUE, silent = TRUE)
    #Build y
    y <- compute.polygonal(mesh = loadedItems$mesh,
                           x = input$map_var_select)
    #Color
    col.range <- stored_colors()
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
    ybis <- compute.polygonal(mesh = loadedItems$mesh,
                              x = input$crop_var_select)
    polynetwork <- doolkit::poly.network(mesh = loadedItems$mesh,
                                         y = ybis,
                                         lwr.limit = quantile(ybis, input$net_range_select[1]/100),
                                         upr.limit = quantile(ybis, input$net_range_select[2]/100),
                                         min.size = input$net_size_select)

    alpha <- rep(0.1, Rvcg::nfaces(loadedItems$mesh))
    alpha[polynetwork@faces] <- 0.99
    #Close any existing rgl window
    try(close3d())
    #Build dkmap...
    dkmap(mesh = loadedItems$mesh,
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

  # Display rgl map----
  #save <- options(rgl.inShiny = TRUE)
  #on.exit(options(save))
  output$dkmap <- renderRglwidget({
    make_map()
  })

  # Make ggplot chart----
  make_plot <- reactive({
    # Wait for fileInput
    req(input$import_surface)
    # Import mesh
    loadedItems$mesh <- Rvcg::vcgImport(input$import_surface$datapath,
                                        updateNormals = TRUE,
                                        silent = TRUE)
    # Compute topographic variable
    y <- compute.polygonal(mesh = loadedItems$mesh,
                           x = input$chart_var_select)
    # ...histogram
    if (input$chart_style == 1) {
      dkdata <- data.frame(y = y)
      plot <- ggplot2::ggplot(dkdata, ggplot2::aes(x = y)) +
        ggplot2::geom_histogram(color = "white", fill = "hotpink") +
        ggplot2::xlab(dta.legend(input$chart_var_select))
      plotname <- "Histogram"
    } else {
      # ...profile
      if (input$chart_style == 2) {
        profile <- dkprofile(y, col = "hotpink", as.percentage = input$chart_options_percentage)
        plot <- profile$profile
        plotname <- "Cumulative profile"

      }
    }
    # add title
    plot_complete_title <- paste(plotname,
                   dta.legend(input$chart_var_select),
                   sep = ", ")
    if (input$chart_options_show_slope && input$chart_style == 2) plot_complete_title <- paste(plot_complete_title, "\nslope: ", profile$slope)
    if (input$chart_options_show_auc && input$chart_style == 2) plot_complete_title <- paste(plot_complete_title, "\nAUC: ", profile$auc)
    plot + ggplot2::ggtitle(label = plot_complete_title) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
  })
  # Display ggplot chart----
  output$dkplot <- renderPlot({
    make_plot()
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



  # Buttons----
  # ...single batch----
  observeEvent(input$batch_single_event, {
    loadedItems$mesh <- Rvcg::vcgImport(input$import_surface$datapath, updateNormals = TRUE, silent = TRUE)
    batchData$data <- get_batch_single_dataframe(loadedItems$mesh)
  })

  # ...double batch

  # ...multi batch----
  observeEvent(input$batch_multi_event, {
    multi_opcr_patchsize <- input$multi_patch_size_select
    mesh_files <- input$import_multi_surfaces$datapath
    batchData$data <- get_batch_multi_dataframe(mesh_files, input$import_multi_surfaces$name, multi_opcr_patchsize)
  })

  # ...download dataframe----
  # ......as .txt
  output$download_dataframe_txt <- downloadHandler(
    filename = function() {
      paste('placeholder.txt', sep='')
    },
    content = function(file) {
      write.table(batchData$data, file = file, sep = "\t", row.names = F, append = F, quote = F)
    },
    contentType = "text/txt"
  )
  # ......as .csv
  output$download_dataframe_csv <- downloadHandler(
    filename = function() {
      paste('placeholder.csv', sep='')
    },
    content = function(file) {
      write.table(batchData$data, file = file, sep = "\t", row.names = F, append = F, quote = F)
    },
    contentType = "text/csv"
  )
  # ......as .RDS
  output$download_dataframe_rds <- downloadHandler(
    filename = function() {
      paste('placeholder.RDS', sep='')
    },
    content = function(file) {
      saveRDS(batchData$data, file = file)
    },
    contentType = "application/octet-stream"
  )
  # ......as .xslx
  output$download_dataframe_xlsx <- downloadHandler(
    filename = function() {
      paste('placeholder.xlsx', sep='')
    },
    content = function(file) {
      openxlsx::write.xlsx(batchData$data, file = file, asTable = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # ...download chart----
  # ......as .jpg
  output$download_chart_jpg <- downloadHandler(
    filename = function() {
      paste("Untitled_chart_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_plot(), device = "jpg", width = 8, height = 6)
    }
  )
  # ......as .png
  output$download_chart_png <- downloadHandler(
    filename = function() {
      paste("Untitled_chart_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_plot(), device = "png", width = 8, height = 6)
    }
  )
  # ......as .svg
  output$download_chart_svg <- downloadHandler(
    filename = function() {
      paste("Untitled_chart_", Sys.Date(), ".svg", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_plot(), device = "svg", width = 8, height = 6)
    }
  )

  # ...download map----
  # ......as png
  output$download_map_png <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      rgl::snapshot3d(file, fmt = "png")
    }
  )
  # ......as html
  output$download_map_html <- downloadHandler(
    filename = "interactive_plot.html",
    content = function(file) {
      widget_to_save <- make_map()
      htmlwidgets::saveWidget(widget_to_save, file, selfcontained = TRUE)
    }
  )

  # COLORS!
  # Update slider when a predefined palette is chosen
  observeEvent(input$palette_choice, {
    if (input$palette_choice != "Custom") {
      palette <- predefined_palettes[[input$palette_choice]]
      updateSliderInput(inputId = "color_count", value = length(palette))
    }
  })

  # Dynamically generate the color picker inputs
  output$color_pickers_ui <- renderUI({
    count <- input$color_count

    # Check if we should use colors from a template or default to black
    current_palette <- if (input$palette_choice != "Custom") {
      predefined_palettes[[input$palette_choice]]
    } else {
      grDevices::colorRampPalette(c("black","white"))(count)
    }

    # Create a list of colourInput widgets
    lapply(1:count, function(i) {
      # Use template color if available, otherwise default to black
      init_color <- if(i <= length(current_palette)) current_palette[i] #else "#000000"

      colourInput(inputId = paste0("col_", i),
                  label = paste("Color", i),
                  value = init_color)
    })
  })

  # Reactive expression to store and retrieve the color vector
  stored_colors <- reactive({
    count <- input$color_count
    default_palette <- grDevices::colorRampPalette(c("black","white"))(count)
    # Collect values from the dynamically created inputs
    vapply(1:count, function(i) {
      input[[paste0("col_", i)]] %||% default_palette[i]
    }, character(1))
  })


  # Methods----
  # ...3D map----
  # get per triangle values
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
    if (x == 10) result <- doolkit::arc(mesh, range = c(-20, 20))
    if (x == 11) result <- doolkit::dne(mesh)
    return(result)
  }
  # get variable name on legend
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
    if (x == 10) result <- "Area-Relative Curvature"
    if (x == 11) result <- "Dirichlet Normal Energy"
    return(result)
  }
  # get min range according to selected variable
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
    if (x == 10) result <- -20
    if (x == 11) result <- NULL
    return(result)
  }
  # get max range according to selected variable
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
    if (x == 10) result <- 20
    if (x == 11) result <- NULL
    return(result)
  }
  # get selected legend type
  legtype <- function(x) {
    if (x == 1) result = "stack"
    if (x == 2) result = "pie"
    if (x == 3) result = "log"
    return(result)
  }
  # get nametag
  nametag <- function(name, display) {
    result <- paste("")
    if (display) {
      name <- basename(name)
      result <- paste(name)
    }
    return(result)
  }

  # ...single batch----
  get_batch_single_dataframe <- function(mesh_file) {
    # Prepare function list
    selected_functions <- c(input$single_table_select_topology, input$single_table_select_relief, input$single_table_select_sharpness)
    fun_list <- list()
    for (fun in selected_functions) {

      if (fun == "3D area") fun_list <- rlist::list.append(fun_list, "3D area" = function(mesh) return(Rvcg::vcgArea(mesh, perface = TRUE)$pertriangle))
      if (fun == "Elevation") fun_list <- rlist::list.append(fun_list, "Elevation" = function(mesh) return(doolkit::elev(mesh)))
      if (fun == "Inclination") fun_list <- rlist::list.append(fun_list, "Inclination" = function(mesh) return(doolkit::inclin(mesh)))
      if (fun == "Orientation") fun_list <- rlist::list.append(fun_list, "Orientation" = function(mesh) return(doolkit::orient(mesh)))
      if (fun == "Slope") fun_list <- rlist::list.append(fun_list, "Slope" = function(mesh) return(doolkit::slope(mesh)))
      if (fun == "Angularity (in degree)") fun_list <- rlist::list.append(fun_list, "Angularity_in_degree)" = function(mesh) return(doolkit::angularity(mesh, ratio = FALSE)))
      if (fun == "Angularity (as ratio)") fun_list <- rlist::list.append(fun_list, "Angularity_as_ratio)" = function(mesh) return(doolkit::angularity(mesh, ratio = TRUE)))
      if (fun == "Curvature (mean)") fun_list <- rlist::list.append(fun_list, "Mean_curvature" = function(mesh) return(Rvcg::vcgCurve(mesh)$meanitmax))
      if (fun == "Curvature (Gaussian)") fun_list <- rlist::list.append(fun_list, "Gaussian_curvature" = function(mesh) return(Rvcg::vcgCurve(mesh)$gaussitmax))
      if (fun == "Curvature (ARC)") fun_list <- rlist::list.append(fun_list, "ARC" = function(mesh) return(doolkit::arc(mesh, range = c(-20, 20))))
      if (fun == "Curvature (DNE)") fun_list <- rlist::list.append(fun_list, "DNE" = function(mesh) return(doolkit::dne(mesh)))
    }

      #TODO manage empty lists

      result <- doolkit::batch.single(mesh_file, fun_list)

      print(c("functions = ", selected_functions))
      print(c("colnames = ", colnames(result)))

      return(result)
  }

  # ...multi batch----
  get_batch_multi_dataframe <- function(mesh_files, filenames, multi_opcr_patchsize) {
    # Prepare function list
    selected_functions <- c(input$multi_table_select_relief, input$multi_table_select_sharpness, input$multi_table_select_shape, input$multi_table_select_complexity)
    fun_list <- list()
    for (fun in selected_functions) {
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

    #TODO manage empty lists

    result <- doolkit::batch.multi(files = mesh_files, functions = fun_list, filenames = filenames)
    return(result)
  }

  session$onSessionEnded(function() {
    options(shiny.maxRequestSize = 5 * 1024^2)
    options(rgl.useNULL = FALSE)
    message("Session ended. Resetting shiny.maxRequestSize to 5MB.")
  })
}


# Run the application----
shinyApp(ui = ui, server = server)

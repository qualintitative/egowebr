#' Visualize an Egor Object with R Shiny
#' 
#' A custom version of egor::egor_vis_app with minor modifications
#' 
#' @param object An egor-class object.
#' @param shiny_opts A list of arguments to be passed to shiny::shinyApp's options argument.
#' @export
visualize_egor <- function (object = NULL, shiny_opts = list(launch.browser = TRUE)) 
{
  require(dplyr)
  egor:::require_shiny(paste(sQuote("egor"), "network visualization app"))
  egor:::require_igraph(paste(sQuote("egor"), "network visualization app"))
  IDVARS <- list(ego = ".egoID", alter = ".altID", 
                 source = ".srcID", target = ".tgtID")
  egors <- ls(envir = .GlobalEnv)[sapply(mget(ls(envir = .GlobalEnv), 
                                              envir = .GlobalEnv), function(x) class(x)[1] == "egor")]
  col_pal_names <- c("Heat Colors", "Yellow-Green", 
                     "Red-Yellow", "Blue-Red", "Black-White", 
                     "Greys", "Rainbow", "Topo Colors")
  shiny_opts <- c(shiny_opts, width = "20")
  object_enex <- as.character(enexpr(object))
  shiny::shinyApp(ui = 
      shiny::fluidPage(title = "egor's Network Visualization App", 
          shiny::fluidRow(width = 12, 
              shiny::plotOutput("Plot", width = "100%", height = 780)
              ), 
          shiny::fluidRow(width = 12,
              shiny::column(3, 
                  shiny::selectInput("egor", "Select `egor` object", egors, selected = object_enex),
                  shiny::uiOutput("choose_nnumber"),
                  shiny::numericInput("x_dim", "Columns", 3, min = 1), 
                  shiny::numericInput("y_dim", "Rows", 2, min = 1), 
                  shiny::tags$div(title = 
                      "When including ego make sure that corresponding ego and alter variables have equal names in the `egor` object.",
                      shiny::tags$div(shiny::checkboxInput("include_ego", "Include Ego", FALSE),  style = "float: left;"), 
                      shiny::tags$div(shiny::icon("question-circle"), style = "float: left; margin-top: 12px; margin-left: -200px;")
                      )
                  ), 
              shiny::column(3, 
                  shiny::sliderInput("zoom_factor_v", label = "Vertex Size:", min = 0, max = 20, value = 1, step = 0.1), 
                  shiny::sliderInput("zoom_factor_e", label = "Edge Width:", min = 1, max = 10, value = 3, step = 0.1), 
                  shiny::sliderInput("font_size", label = "Font Size:", min = 0.5, max = 5, value = 1, step = 0.1)
                  ), 
              shiny::column(6, 
                  shiny::tabsetPanel(
                      shiny::tabPanel("Vertices", 
                          shiny::column(6, 
                              shiny::uiOutput("choose_v.size"), 
                              shiny::uiOutput("choose_v.color"), 
                              shiny::selectInput("v.color_pal", "Color Palette:", choices = col_pal_names)
                              ), 
                          shiny::column(6, 
                              shiny::uiOutput("choose_v.label"), 
                              shiny::textInput("l.label", "Legend Label:")
                              )
                          ),
                      shiny::tabPanel("Edges", 
                          shiny::column(6, 
                              shiny::uiOutput("choose_e.width"),
                              shiny::uiOutput("choose_e.color")),
                              shiny::column(6, 
                              shiny::selectInput("e.color_pal", "Color Palette:", choices = col_pal_names)
                              )
                          ), 
                      shiny::tabPanel("Plot Options", 
                          shiny::column(6,
                              shiny::helpText("Select ego attributes and results do display on plot."), 
                              shiny::uiOutput("choose_disp.results")
                              ), 
                          shiny::column(6, 
                              shiny::helpText("Select a venn and pie variable, to switch plot type to", 
                                  shiny::br(), 
                                  "'ego-socio-gram'."), 
                              shiny::uiOutput("choose_venn_var"), 
                              shiny::uiOutput("choose_pie_var"), 
                              shiny::checkboxInput("ascending_inwards", "Venn Circles Ascending Inwards", value = TRUE)
                              ), 
                          ), 
                      shiny::tabPanel("Sort & Filter", 
                          shiny::column(6, 
                              shiny::uiOutput("choose_sort_by"), 
                              shiny::uiOutput("choose_filter_var"),
                              shiny::textInput("filter", "Filter Value:")
                              ), 
                          shiny::column(6, 
                              shiny::uiOutput("choose_box_color"), 
                              shiny::selectInput("box_col_pal", "Color Palette:", choices = col_pal_names)
                              )
                          ), 
                      shiny::tabPanel("Export", 
                          shiny::column(3, 
                              shiny::br(shiny::downloadButton("save_plot", label = "Save this Plot")), 
                              shiny::br(shiny::downloadButton("save_all_plots", label = "Save all Plots"))
                              )
                          )
                      )
                  )
              ), 
          shiny::fluidRow(
              shiny::column(12, 
                  shiny::inputPanel(
                      shiny::tags$div(
                          style = "height: 250px; width: 100%;", 
                          shiny::tags$p("egor's Network Visualization App")
                          )
                      )
                  )
              )
          ), 
      server = function(input, output) {
          values <- shiny::reactiveValues(default = 0)
          values$v.size     <- "-Select Entry-"
          values$v.color    <- "-Select Entry-"
          values$v.label    <- "-Select Entry-"
          values$e.width    <- "-Select Entry-"
          values$e.color    <- "-Select Entry-"
          values$box_color  <- "-Select Entry-"
          values$sort_by    <- "-Select Entry-"
          values$filter_var <- "-Select Entry-"
          values$filter     <- ""
          values$venn_var   <- "-Select Entry-"
          values$pie_var    <- "-Select Entry-"
          values$nnumber    <- 1
          
          values$disp.results <- c()
          obj <- shiny::reactive({
              get(input$egor, envir = .GlobalEnv)
              })
          result_names <- shiny::reactive({
              rn <- names(obj()$ego)
              })
          
          r.atts <- shiny::reactive(egor:::make_select_vector(result_names(), e = TRUE))
          v.atts <- shiny::reactive(egor:::make_select_vector(names(obj()$alter)))
          e.atts <- shiny::reactive(egor:::make_select_vector(names(obj()$aatie), e = TRUE))
          
          v.atts.no.e <- shiny::reactive(egor:::make_select_vector(c(names(
            obj()$alter[!names(obj()$alter) %in% result_names()]), ".egoID", ".altID")))
          
          output$choose_nnumber <- shiny::renderUI({
              shiny::numericInput("nnumber", "Network No.:", 
                  step = input$x_dim * input$y_dim, 
                  min = 1, 
                  max = nrow(egor:::apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter)$ego), 
                  value = 1
                  )
              })
          output$choose_filter_var <- shiny::renderUI({
              shiny::selectInput("filter_var", "Filter by:", choices = (r.atts()))
              })
          
          shiny::outputOptions(output, "choose_filter_var", suspendWhenHidden = FALSE)
          output$choose_sort_by <- shiny::renderUI({
              shiny::selectInput("sort_by", "Sort by:", choices = (r.atts()))
              })
          shiny::outputOptions(output, "choose_sort_by", suspendWhenHidden = FALSE)
          output$choose_box_color <- shiny::renderUI({
              shiny::selectInput("box_color", "Highlight:", choices = r.atts())
              })
          shiny::outputOptions(output, "choose_box_color", suspendWhenHidden = FALSE)
          output$choose_v.size <- shiny::renderUI({
              shiny::selectInput("v.size", "Vertex Size:", choices = v.atts.no.e())
              })
          shiny::outputOptions(output, "choose_v.size", suspendWhenHidden = FALSE)
          output$choose_v.color <- shiny::renderUI({
              shiny::selectInput("v.color", "Vertex Color:", choices = v.atts.no.e())
              })
          shiny::outputOptions(output, "choose_v.color", suspendWhenHidden = FALSE)
          output$choose_v.label <- shiny::renderUI({
              shiny::selectInput("v.label", "Vertex Labels:", choices = v.atts.no.e())
              })
          shiny::outputOptions(output, "choose_v.label", suspendWhenHidden = FALSE)
          output$choose_e.width <- shiny::renderUI({
              shiny::selectInput("e.width", "Edge Width:", choices = e.atts())
              })
          shiny::outputOptions(output, "choose_e.width", suspendWhenHidden = FALSE)
          output$choose_e.color <- shiny::renderUI({
              shiny::selectInput("e.color", "Edge Color:", choices = e.atts())
              })
          shiny::outputOptions(output, "choose_e.color", suspendWhenHidden = FALSE)
          output$choose_venn_var <- shiny::renderUI({
              shiny::selectInput("venn_var", "Venn Variable:", choices = v.atts.no.e())
              })
          shiny::outputOptions(output, "choose_venn_var", suspendWhenHidden = FALSE)
          output$choose_pie_var <- shiny::renderUI({
              shiny::selectInput("pie_var", "Pie Variable:", choices = v.atts.no.e())
              })
          shiny::outputOptions(output, "choose_pie_var", suspendWhenHidden = FALSE)
          output$choose_disp.results <- shiny::renderUI({
              shiny::selectInput("disp.results", "Results:", choices = result_names(), multiple = TRUE)
              })
          
          shiny::outputOptions(output, "choose_disp.results", suspendWhenHidden = FALSE)
          shiny::observeEvent(input$box_color, {
              values$box_color <- input$box_color
              })
          shiny::observeEvent(input$v.size, {
              values$v.size <- input$v.size
              })
          shiny::observeEvent(input$filter, {
              values$filter <- input$filter
              })
          shiny::observeEvent(input$filter_var, {
              values$filter_var <- input$filter_var
              })
          shiny::observeEvent(input$sort_by, {
              values$sort_by <- input$sort_by
              })
          shiny::observeEvent(input$v.color, {
              values$v.color <- input$v.color
              })
          shiny::observeEvent(input$v.label, {
              values$v.label <- input$v.label
              })
          shiny::observeEvent(input$e.width, {
              values$e.width <- input$e.width
              })
          shiny::observeEvent(input$e.color, {
              values$e.color <- input$e.color
              })
          shiny::observeEvent(input$nnumber, {
              values$nnumber <- input$nnumber
              })
          shiny::observeEvent(input$disp.results, {
              values$disp.results <- input$disp.results
              }, ignoreNULL = FALSE)
          shiny::observeEvent(input$venn_var, {
              values$venn_var <- input$venn_var
              })
          shiny::observeEvent(input$pie_var, {
              values$pie_var <- input$pie_var
              })
          
          output$Plot <- 
              shiny::renderPlot({
                  plot(egor:::apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter), 
                      values$nnumber, 
                      input$x_dim, 
                      input$y_dim, 
                      venn_var = values$venn_var, 
                      pie_var = values$pie_var, 
                      vertex_size_var = 
                          if (values$v.size != "-Select Entry-") 
                              values$v.size
                          else NULL, 
                      vertex_color_var = 
                          if (values$v.color != "-Select Entry-") 
                              values$v.color
                          else NULL, 
                      vertex_color_palette = input$v.color_pal, 
                      vertex_color_legend_label = input$l.label, 
                      vertex_label_var = 
                          if (values$v.label != "-Select Entry-") 
                              input$v.label
                          else NULL, 
                      edge_width_var = 
                          if (values$e.width != "-Select Entry-") 
                              values$e.width
                          else NULL, 
                      edge_color_var = 
                          if (values$e.color != "-Select Entry-") 
                              values$e.color
                          else NULL, 
                      edge_color_palette = input$e.color_pal, 
                      highlight_box_col_var = 
                          if (values$box_color != "-Select Entry-") 
                              values$box_color
                          else NULL, 
                      highlight_box_col_palette = input$box_col_pal, 
                      res_disp_vars = values$disp.results, 
                      vertex_zoom = input$zoom_factor_v, 
                      edge_zoom = input$zoom_factor_e, 
                      font_size = input$font_size, 
                      include_ego = input$include_ego, 
                      type = 
                          if (values$venn_var != "-Select Entry-" & values$pie_var != "-Select Entry-") 
                              "egogram"
                          else "egograph", 
                      ascending_inwards = input$ascending_inwards
                      )
                  })
                  
              output$save_all_plots <- 
                  shiny::downloadHandler(
                      filename = function() {
                          paste(input$egor, "_plots_export", ".pdf", sep = "")
                          }, 
                      content = function(file) {
                          pdf(file, width = 16, onefile = TRUE)
                          for (
                              i in seq(
                                  1, 
                                  nrow(egor:::apply_sort_filter_to_obj(
                                      obj(), values$sort_by, values$filter_var, values$filter)$ego), 
                                  by = input$x_dim * input$y_dim
                                  )
                              ) {
                              plot(egor:::apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter), 
                                  i, 
                                  input$x_dim, 
                                  input$y_dim, 
                                  venn_var = values$venn_var, 
                                  pie_var = values$pie_var, 
                                  vertex_size_var = 
                                      if (values$v.size != "-Select Entry-") 
                                          values$v.size
                                      else NULL, 
                                  vertex_color_var = 
                                      if (values$v.color != "-Select Entry-") 
                                          values$v.color
                                      else NULL, 
                                  vertex_color_palette = input$v.color_pal, 
                                  vertex_color_legend_label = input$l.label, 
                                  vertex_label_var = 
                                      if (values$v.label != "-Select Entry-") 
                                          input$v.label
                                      else NULL, 
                                  edge_width_var = 
                                      if (values$e.width != "-Select Entry-") 
                                          values$e.width
                                      else NULL, 
                                  edge_color_var = 
                                      if (values$e.color != "-Select Entry-") 
                                          values$e.color
                                      else NULL, 
                                  edge_color_palette = input$e.color_pal, 
                                  highlight_box_col_var = 
                                      if (values$box_color != "-Select Entry-") 
                                          values$box_color
                                      else NULL, 
                                  highlight_box_col_palette = input$box_col_pal, 
                                  res_disp_vars = values$disp.results, 
                                  vertex_zoom = input$zoom_factor_v, 
                                  edge_zoom = input$zoom_factor_e, 
                                  font_size = input$font_size, 
                                  include_ego = input$include_ego, 
                                  type = 
                                      if (values$venn_var != "-Select Entry-" & values$pie_var != "-Select Entry-") 
                                          "egogram"
                                      else "egograph"
                                  )
                              }
                          dev.off()
                      }
                  )
              
              output$save_plot <- shiny::downloadHandler(
                  filename = function() {
                      paste(input$egor, "_", values$nnumber, ".pdf", sep = "")
                      }, 
                  content = function(file) {
                      pdf(file, width = 16)
                      plot(egor:::apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter), 
                          values$nnumber, 
                          input$x_dim, input$y_dim, venn_var = values$venn_var, 
                          pie_var = values$pie_var, 
                          vertex_size_var = 
                              if (values$v.size != "-Select Entry-") 
                                  values$v.size
                              else NULL, 
                          vertex_color_var = 
                              if (values$v.color != "-Select Entry-") 
                                  values$v.color
                              else NULL, 
                          vertex_color_palette = input$v.color_pal, 
                          vertex_color_legend_label = input$l.label, 
                          vertex_label_var = 
                              if (values$v.label != "-Select Entry-") 
                                  input$v.label
                              else NULL, 
                          edge_width_var = 
                              if (values$e.width != "-Select Entry-")
                                  values$e.width
                              else NULL, 
                          edge_color_var = 
                              if (values$e.color != "-Select Entry-") 
                                  values$e.color
                              else NULL, 
                          edge_color_palette = input$e.color_pal, 
                          highlight_box_col_var = 
                              if (values$box_color != "-Select Entry-") 
                                  values$box_color
                              else NULL, 
                          highlight_box_col_palette = input$box_col_pal, 
                          res_disp_vars = values$disp.results, 
                          vertex_zoom = input$zoom_factor_v, 
                          edge_zoom = input$zoom_factor_e, 
                          font_size = input$font_size, 
                          type = 
                              if (values$venn_var != "-Select Entry-" & values$pie_var != "-Select Entry-") 
                                  "egogram"
                              else "egograph"
                              )
                          dev.off()
                      }
                  )
              }, 
          options = shiny_opts
          )  
      }
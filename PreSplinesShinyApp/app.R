library(shiny)
library(markdown)
library(dplyr)

set.seed(123)
dens1 = function(x) dbeta(x, 2, 2)
dens2 = function(x) dbeta(x, 2, 5)
dens3 = function(x) dbeta(x, 5, 2)

rand_x_1 = rbeta(100000, 2, 2)
rand_x_2 = rbeta(100000, 2, 5)
rand_x_3 = rbeta(100000, 5, 2)

source("read_function_objects.R")
for (i in 1:4) {
  if (get(paste0("range_truth", i)) == 0) {
    assign(paste0("range_truth", i), 1)
  }
  if (get(paste0("range_truth", i, "_deriv1")) == 0) {
    assign(paste0("range_truth", i, "_deriv1"), 1)
  }
  if (get(paste0("range_truth", i, "_deriv2")) == 0) {
    assign(paste0("range_truth", i, "_deriv2"), 1)
  }
}
source("performance_measures_helpers.R")

ui <- fluidPage(
  
  titlePanel("Illustration of performance measures for estimated non-linear associations"),
  
  tabsetPanel(
    type = "tabs",
    tabPanel("Illustration", 
             HTML("<br>Select a performance measure by choosing a specific option for each aspect of the categorization.<br> 
  The resulting performance measure is then described in words on the right hand side.<br> 
  For four different examples (each consisting of five estimates), the values of the chosen performance measure are shown together with the ranking of the estimates.<br><br>"),
             
             sidebarLayout(
               
               sidebarPanel(style='padding:5px;',
                            wellPanel(style = "background: #FCFCFC",
                                      h3("Select the performance measure", style = 'margin-top: 2px;'),
                                      fluidRow(
                                        
                                        column(3,
                                               
                                               radioButtons("localization", "Localization:",
                                                            choices = c("Range", "Point")),
                                               
                                               radioButtons("tier", "Functional characteristic:",
                                                            choices = c("f(x)", "f'(x)", "f''(x)")),
                                               
                                               radioButtons("loss", "Loss:",
                                                            choices = c("Difference", "Absolute", "Squared", "Epsilon-level accuracy")),
                                               
                                               conditionalPanel(
                                                 condition = "input.loss == 'Epsilon-level accuracy' & input.dimension=='Y'",
                                                 numericInput("epsY", "epsilon", value = 0.05, min = 0, max = 1, step = 0.01)
                                               ),
                                               conditionalPanel(
                                                 condition = "input.loss == 'Epsilon-level accuracy' & input.dimension=='X'",
                                                 numericInput("epsX", "epsilon", value = 0.05, min = 0, max = 1, step = 0.01)
                                               )
                                        ),
                                        column(4,
                                               
                                               conditionalPanel(
                                                 condition = "input.localization == 'Range'",
                                                 radioButtons("dimension", "Axis of aggregation:",
                                                              choices = c("Y", "X"))),
                                               
                                               conditionalPanel(
                                                 condition = "input.localization == 'Point'",
                                                 numericInput("Xloc", "x", value = 0.5, min = 0, max = 1, step = 0.01)
                                               ),
                                               
                                               conditionalPanel(
                                                 condition = "input.localization == 'Range' & input.dimension=='Y'",
                                                 radioButtons("weights", "Type of aggregation:",
                                                              choiceNames = list(withMathJax("Integration over \\(dx\\)"), withMathJax("Expectation over \\(dF_X\\)"), withMathJax("Quantile with respect to \\(F_X\\)"), "Maximum", "Minimum"),
                                                              choiceValues = list("Expectation", "Expectation over dF_X(x)", "Quantile with respect to F_X", "Maximum", "Minimum"))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.localization == 'Range' & input.dimension=='X'",
                                                 radioButtons("Xweights", "Type of aggregation:",
                                                              choices = c("Number of roots", "Location of maximum", "Location of minimum"))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.localization == 'Range' & input.dimension=='Y' & input.weights == 'Quantile with respect to F_X'",
                                                 numericInput("probs", "Quantile q", value = 0.5, min = 0, max = 1, step = 0.05)
                                               ),

                                               conditionalPanel(
                                                 condition = "input.localization == 'Range'",
                                                 radioButtons("range", "Scope of aggregration:",
                                                              choiceNames = list(withMathJax("whole range \\([0,1]\\)"), withMathJax("subrange \\([F_X^{-1}(0.05), F_X^{-1}(0.95)]\\)")),
                                                              choiceValues = list("whole", "5_95"))
                                               ),
                                               conditionalPanel(
                                                 #condition = "input.localization == 'Range'",
                                                 condition = "input.localization == 'Range' & (input.weights == 'Expectation over dF_X(x)' | input.weights == 'Quantile with respect to F_X' |
                                                 input.range == '5_95')",
                                                 radioButtons("distr", withMathJax("\\(F_X\\) (distribution of \\(X\\)):"),
                                                              choices = c("beta(2,2)", "beta(2,5)", "beta(5,2)"))
                                               )
                                               
                                        )))),
               #actionButton("do", "Update performance measure",
               #             style = 'padding:10px; font-size:100%; width: 100%')
               
               
               mainPanel(fluidRow(column(8,
                                         htmlOutput("selected_values"),
                                         #uiOutput("formula"),
                                         HTML("<br>"),
                                         tabsetPanel(id = "which_truth", 
                                                     tabPanel("Example 1", 
                                                              fluidRow(
                                                                column(6, align="center", strong("Estimate")),
                                                                column(2, align="center", strong("Performance value")),
                                                                column(2, align="center", strong("Rank"))),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 1/truth1_estim1.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth1_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth1_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth1_estim1"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 1/truth1_estim2.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth1_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth1_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth1_estim2"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 1/truth1_estim3.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth1_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth1_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth1_estim3"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 1/truth1_estim4.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth1_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth1_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth1_estim4"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 1/truth1_estim5.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth1_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth1_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth1_estim5"))
                                                              )), 
                                                     tabPanel("Example 2", 
                                                              fluidRow(
                                                                column(6, align="center", strong("Estimate")),
                                                                column(2, align="center", strong("Performance value")),
                                                                column(2, align="center", strong("Rank"))),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 2/truth2_estim1.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth2_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth2_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth2_estim1"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 2/truth2_estim2.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth2_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth2_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth2_estim2"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 2/truth2_estim3.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth2_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth2_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth2_estim3"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 2/truth2_estim4.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth2_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth2_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth2_estim4"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 2/truth2_estim5.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth2_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth2_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth2_estim5"))
                                                              )),
                                                     tabPanel("Example 3", 
                                                              fluidRow(
                                                                column(6, align="center", strong("Estimate")),
                                                                column(2, align="center", strong("Performance value")),
                                                                column(2, align="center", strong("Rank"))),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 3/truth3_estim1.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth3_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth3_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth3_estim1"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 3/truth3_estim2.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth3_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth3_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth3_estim2"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 3/truth3_estim3.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth3_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth3_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth3_estim3"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 3/truth3_estim4.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth3_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth3_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth3_estim4"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 3/truth3_estim5.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth3_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth3_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth3_estim5"))
                                                              )),
                                                     tabPanel("Example 4", 
                                                              fluidRow(
                                                                column(6, align="center", strong("Estimate")),
                                                                column(2, align="center", strong("Performance value")),
                                                                column(2, align="center", strong("Rank"))),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 4/truth4_estim1.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth4_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth4_estim1")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth4_estim1"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 4/truth4_estim2.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth4_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth4_estim2")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth4_estim2"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 4/truth4_estim3.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth4_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth4_estim3")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth4_estim3"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 4/truth4_estim4.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth4_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth4_estim4")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth4_estim4"))
                                                              ),
                                                              fluidRow(
                                                                #column(6, align="center", img(src = "truth 4/truth4_estim5.png", width = 300)),
                                                                column(6, align="center", uiOutput("plot_truth4_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("v_truth4_estim5")),
                                                                column(2, align="center", HTML("<br><br><br>"), textOutput("r_truth4_estim5"))
                                                              ))
                                         )
               )
               )))),
    tabPanel("Explanatory comments",
             includeMarkdown("www/comments.md")),
    tabPanel("About",
             includeHTML("www/about.html"))
    
  ))

server <- function(input, output) {
  
  # description of performance measure ##########################################
  
  output$selected_values <- renderText({
    ifelse(input$localization == "Range", ifelse(input$dimension == "X", 
                                                  paste0("<h4><b>This global performance measure considers a ", input$loss, " loss on the ",
                                                         input$Xweights, " of ", input$tier, ":</b></h4>"),
                                                  paste0("<h4><b>This global performance measure considers the ", input$weights, 
                                                         ifelse(input$weights == "Expectation over dF_X(x)", paste0(" (with respect to the distribution ", input$distr, ")"), ""), 
                                                         " of a ", input$loss, " loss on ",  input$tier, ":</b></h4>")),
           paste0("<h4><b>This local performance measure considers a ", input$loss, " loss ", "on ", 
                  input$tier, " at x = ", input$Xloc, ":</b></h4>"))
    
  }) #%>% bindEvent(input$do)
  
  # plots of ground truths and estimates ##########################################
  
  plot_path <- reactive({
    
    path_to_image = vector(mode='list', length=4)
    for (i in 1:4) path_to_image[[i]] = character()
    if (input$range == "whole") {
      for (i in 1:4) {
        for (j in 1:5) {
          path_to_image[[i]][j] = paste0("truth ", i, "/truth", i, "_estim", j, ".png")
        }
      }
    } else if (input$range == "5_95" & input$distr =="beta(2,2)") {
      for (i in 1:4) {
        for (j in 1:5) {
          path_to_image[[i]][j] = paste0("truth ", i, "/truth", i, "_estim", j, "_quantiles_2_2.png")
        }
      }
    } else if (input$range == "5_95" & input$distr =="beta(2,5)") {
      for (i in 1:4) {
        for (j in 1:5) {
          path_to_image[[i]][j] = paste0("truth ", i, "/truth", i, "_estim", j, "_quantiles_2_5.png")
        }
      }
   } else if (input$range == "5_95" & input$distr =="beta(5,2)") {
     for (i in 1:4) {
       for (j in 1:5) {
         path_to_image[[i]][j] = paste0("truth ", i, "/truth", i, "_estim", j, "_quantiles_5_2.png")
       }
     }
  }
    return(path_to_image)
  })
  
  output$plot_truth1_estim1 <- renderUI({
    img(width = 300, src = plot_path()[[1]][1])
  })
  output$plot_truth1_estim2 <- renderUI({
    img(width = 300, src = plot_path()[[1]][2])
  })
  output$plot_truth1_estim3 <- renderUI({
    img(width = 300, src = plot_path()[[1]][3])
  })
  output$plot_truth1_estim4 <- renderUI({
    img(width = 300, src = plot_path()[[1]][4])
  })
  output$plot_truth1_estim5 <- renderUI({
    img(width = 300, src = plot_path()[[1]][5])
  })
  
  output$plot_truth2_estim1 <- renderUI({
    img(width = 300, src = plot_path()[[2]][1])
  })
  output$plot_truth2_estim2 <- renderUI({
    img(width = 300, src = plot_path()[[2]][2])
  })
  output$plot_truth2_estim3 <- renderUI({
    img(width = 300, src = plot_path()[[2]][3])
  })
  output$plot_truth2_estim4 <- renderUI({
    img(width = 300, src = plot_path()[[2]][4])
  })
  output$plot_truth2_estim5 <- renderUI({
    img(width = 300, src = plot_path()[[2]][5])
  })
  
  output$plot_truth3_estim1 <- renderUI({
    img(width = 300, src = plot_path()[[3]][1])
  })
  output$plot_truth3_estim2 <- renderUI({
    img(width = 300, src = plot_path()[[3]][2])
  })
  output$plot_truth3_estim3 <- renderUI({
    img(width = 300, src = plot_path()[[3]][3])
  })
  output$plot_truth3_estim4 <- renderUI({
    img(width = 300, src = plot_path()[[3]][4])
  })
  output$plot_truth3_estim5 <- renderUI({
    img(width = 300, src = plot_path()[[3]][5])
  })
  
  output$plot_truth4_estim1 <- renderUI({
    img(width = 300, src = plot_path()[[4]][1])
  })
  output$plot_truth4_estim2 <- renderUI({
    img(width = 300, src = plot_path()[[4]][2])
  })
  output$plot_truth4_estim3 <- renderUI({
    img(width = 300, src = plot_path()[[4]][3])
  })
  output$plot_truth4_estim4 <- renderUI({
    img(width = 300, src = plot_path()[[4]][4])
  })
  output$plot_truth4_estim5 <- renderUI({
    img(width = 300, src = plot_path()[[4]][5])
  })
  
  # values of performance measures ################################################
  
  perf_meas <- reactive({
    
    # calculate performance values 
    v = vector(mode='list', length = 4)
    
    q05 = 0; q95 = 1
    
    if (input$distr == "beta(2,2)") {
      q05 = qbeta(0.05, 2, 2); q95 = qbeta(0.95, 2, 2)
    } else if (input$distr == "beta(2,5)") {
      q05 = qbeta(0.05, 2, 5); q95 = qbeta(0.95, 2, 5)
    } else if (input$distr == "beta(5,2)") {
      q05 = qbeta(0.05, 5, 2); q95 = qbeta(0.95, 5, 2)
    }
    
    
    rand_x_1_5_95 = rand_x_1[rand_x_1 > q05 & rand_x_1 < q95]
    rand_x_2_5_95 = rand_x_2[rand_x_2 > q05 & rand_x_2 < q95]
    rand_x_3_5_95 = rand_x_3[rand_x_3 > q05 & rand_x_3 < q95]
    
    
    ###########################################################################################################################
    # 1. Measures with localization = "range", scope of aggregation = "global" ################################################
    ###########################################################################################################################
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
      #form_latex = '\\(\\int_{\\mathcal{X}} (\\hat f(x) - f(x)) \\mathrm{d} x\\)'
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
      #form_latex = '\\(\\int_{\\mathcal{X}} |\\hat f(x) - f(x)| \\mathrm{d} x\\)'
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
      #form_latex = '\\(\\int_{\\mathcal{X}} (\\hat f(x) - f(x))^2 \\mathrm{d} x\\)'
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
      #form_latex = '\\(\\int_{\\mathcal{X}} 1_{\\{|\\hat f(x) - f(x)| \\leq \\epsilon\\} } \\mathrm{d} x\\)'
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      v[[2]][4] = Inf
      
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      v[[2]][4] = -Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      v[[2]][4] = -Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      v[[2]][4] = -Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      v[[2]][4] = Inf
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }    
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_01, get(paste0("roots_truth", i))$roots_01, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_01, get(paste0("roots_truth", i))$roots_01, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_01, get(paste0("roots_truth", i))$roots_01, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_01, get(paste0("roots_truth", i))$roots_01, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = 0, upper = 1, epsilon = input$epsX, loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_01, get(paste0("roots_truth", i, "_deriv1"))$roots_01, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_01, get(paste0("roots_truth", i, "_deriv1"))$roots_01, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_01, get(paste0("roots_truth", i, "_deriv1"))$roots_01, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_01, get(paste0("roots_truth", i, "_deriv1"))$roots_01, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = 0, upper = 1, epsilon = input$epsX, loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_01, get(paste0("roots_truth", i, "_deriv2"))$roots_01, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_01, get(paste0("roots_truth", i, "_deriv2"))$roots_01, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_01, get(paste0("roots_truth", i, "_deriv2"))$roots_01, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_01, get(paste0("roots_truth", i, "_deriv2"))$roots_01, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "difference", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "absolute", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "squared", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "epsilon", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Difference" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "difference", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Absolute" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "absolute", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Squared" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "squared", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: global, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Epsilon-level accuracy" & input$range == "whole") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = 0, upper = 1, epsilon = input$epsX, loss = "epsilon", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = TRUE
    }
    
    ###########################################################################################################################
    # 2. Measures with localization = "range", scope of aggregation = between 5% and 95% quantile #############################
    ###########################################################################################################################
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: expectation, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Maximum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f, type of aggregation: minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f(x)" & input$weights == "Minimum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: expectation, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_2_5_95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Maximum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f', type of aggregation: minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f'(x)" & input$weights == "Minimum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: expectation, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens1, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens2, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # expectation over dF_X(x) with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Expectation over dF_X(x)" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = expectation_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), dens = dens3, lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_1_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(2,5), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_2_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: 
    # Quantile with respect to F_X with dens = beta(5,2), loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Quantile with respect to F_X" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = quantile_dFx(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), rand_x = rand_x_3_5_95, probs = input$probs, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }    
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Maximum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon", maximum = TRUE)
          v[[i]][j] = NA 
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: Y, functional characteristic: f'', type of aggregation: minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "Y" &
        input$tier == "f''(x)" & input$weights == "Minimum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = maxmin(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon", maximum = FALSE)
          v[[i]][j] = NA 
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i))$roots_q05q95_beta_2_2, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i))$roots_q05q95_beta_2_2, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i))$roots_q05q95_beta_2_2, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i))$roots_q05q95_beta_2_2, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i))$roots_q05q95_beta_2_5, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i))$roots_q05q95_beta_2_5, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i))$roots_q05q95_beta_2_5, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i))$roots_q05q95_beta_2_5, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i))$roots_q05q95_beta_5_2, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i))$roots_q05q95_beta_5_2, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i))$roots_q05q95_beta_5_2, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i))$roots_q05q95_beta_5_2, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of maximum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f, type of aggregation: location of minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f(x)" & input$Xweights == "Location of minimum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), lower = q05, upper = q95, epsilon = input$epsX, loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv1"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv1"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "difference", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "absolute", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "squared", maximum = TRUE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of maximum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "epsilon", maximum = TRUE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "difference", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "absolute", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "squared", maximum = FALSE)
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f', type of aggregation: location of minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f'(x)" & input$Xweights == "Location of minimum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), lower = q05, upper = q95, epsilon = input$epsX, loss = "epsilon", maximum = FALSE)
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_2, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(2,5)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_2_5, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_2_5, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # here
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: number of roots, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Number of roots" & input$distr == "beta(5,2)" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = nr_roots(get(paste0("roots_truth", i, "_estim", j, "_deriv2"))$roots_q05q95_beta_5_2, get(paste0("roots_truth", i, "_deriv2"))$roots_q05q95_beta_5_2, epsilon = 1, loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "difference", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "absolute", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "squared", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of maximum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of maximum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "epsilon", maximum = TRUE)
          v[[i]][j] = NA
        }
      }
      lb = TRUE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: difference
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Difference" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "difference", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: absolute
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Absolute" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "absolute", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: squared
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Squared" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "squared", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = FALSE
    }
    
    # localization: range, scope of aggregation: between 5% and 95% quantile, axis of aggregation: X, functional characteristic: f'', type of aggregation: location of minimum, loss: epsilon
    if (input$localization == "Range" & input$dimension == "X" &
        input$tier == "f''(x)" & input$Xweights == "Location of minimum" & input$loss == "Epsilon-level accuracy" & input$range == "5_95") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          #v[[i]][j] = location_max_min(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), lower = q05, upper = q95, epsilon = input$epsX, loss = "epsilon", maximum = FALSE)
          v[[i]][j] = NA
        }
      }
      lb = TRUE
    }
    
    
    ###########################################################################################################################
    # 3. Measures with localization = "point" #################################################################################
    ###########################################################################################################################
    
    # localization: point, functional characteristic: f, loss: difference
    if (input$localization == "Point" &
        input$tier == "f(x)" & input$loss == "Difference") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f, loss: absolute
    if (input$localization == "Point" &
        input$tier == "f(x)" & input$loss == "Absolute") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f, loss: squared
    if (input$localization == "Point" &
        input$tier == "f(x)" & input$loss == "Squared") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f, loss: epsilon
    if (input$localization == "Point" &
        input$tier == "f(x)" & input$loss == "Epsilon-level accuracy") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j)), get(paste0("f_truth", i)), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i)), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: point, functional characteristic: f', loss: difference
    if (input$localization == "Point" &
        input$tier == "f'(x)" & input$loss == "Difference") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f', loss: absolute
    if (input$localization == "Point" &
        input$tier == "f'(x)" & input$loss == "Absolute") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f', loss: squared
    if (input$localization == "Point" &
        input$tier == "f'(x)" & input$loss == "Squared") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f', loss: epsilon
    if (input$localization == "Point" &
        input$tier == "f'(x)" & input$loss == "Epsilon-level accuracy") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv1")), get(paste0("f_truth", i, "_deriv1")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv1")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    # localization: point, functional characteristic: f'', loss: difference
    if (input$localization == "Point" &
        input$tier == "f''(x)" & input$loss == "Difference") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "difference")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f'', loss: absolute
    if (input$localization == "Point" &
        input$tier == "f''(x)" & input$loss == "Absolute") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "absolute")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f'', loss: squared
    if (input$localization == "Point" &
        input$tier == "f''(x)" & input$loss == "Squared") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "squared")
        }
      }
      lb = FALSE
    }
    
    # localization: point, functional characteristic: f'', loss: epsilon
    if (input$localization == "Point" &
        input$tier == "f''(x)" & input$loss == "Epsilon-level accuracy") {
      for (i in 1:4) {
        v[[i]] = numeric()
        for (j in 1:5) {
          v[[i]][j] = local_at_x(get(paste0("f_truth", i, "_estim", j, "_deriv2")), get(paste0("f_truth", i, "_deriv2")), x = input$Xloc, epsilon = input$epsY*get(paste0("range_truth", i, "_deriv2")), loss = "epsilon")
        }
      }
      lb = TRUE
    }
    
    v_rounded = v
    for (i in 1:4) {
      for (j in 1:5) {
        v_rounded[[i]][j] = round(v[[i]][j], 5)
      }
    }
    
    if (input$loss == "Difference") {
      v_abs = lapply(v_rounded, abs)
      r = lapply(v_abs, rank)
    } else {
      r = lapply(v_rounded, rank)
    }
    
    
    if (lb) {
      r = lapply(r, function(x) 5 - x + 1) 
    }
    
    for (i in 1:4) {
      for (j in 1:5) {
        v[[i]][j] = round(v[[i]][j], 4)
      }
    }
    
    u = vector(mode='list', length=3)
    names(u) = c("values", "ranks", "formula")
    u[["values"]] = v
    u[["ranks"]] = r
    #u[["formula"]] = form_latex
    
    return(u)
  })
  
  output$formula = renderUI({
    withMathJax(helpText(perf_meas()[["formula"]]))
  })
  
  output$v_truth1_estim1 <- renderText({
    perf_meas()[["values"]][[1]][1]
  })
  
  output$v_truth1_estim2 <- renderText({
    perf_meas()[["values"]][[1]][2]
  })
  
  output$v_truth1_estim3 <- renderText({
    perf_meas()[["values"]][[1]][3]
  })
  
  output$v_truth1_estim4 <- renderText({
    input$which_truth
    perf_meas()[["values"]][[1]][4]
  })
  
  output$v_truth1_estim5 <- renderText({
    perf_meas()[["values"]][[1]][5]
  })
  
  output$v_truth2_estim1 <- renderText({
    perf_meas()[["values"]][[2]][1]
  })
  
  output$v_truth2_estim2 <- renderText({
    perf_meas()[["values"]][[2]][2]
  })
  
  output$v_truth2_estim3 <- renderText({
    perf_meas()[["values"]][[2]][3]
  })
  
  output$v_truth2_estim4 <- renderText({
    perf_meas()[["values"]][[2]][4]
  })
  
  output$v_truth2_estim5 <- renderText({
    perf_meas()[["values"]][[2]][5]
  })
  
  output$v_truth3_estim1 <- renderText({
    perf_meas()[["values"]][[3]][1]
  })
  
  output$v_truth3_estim2 <- renderText({
    perf_meas()[["values"]][[3]][2]
  })
  
  output$v_truth3_estim3 <- renderText({
    perf_meas()[["values"]][[3]][3]
  })
  
  output$v_truth3_estim4 <- renderText({
    perf_meas()[["values"]][[3]][4]
  })
  
  output$v_truth3_estim5 <- renderText({
    perf_meas()[["values"]][[3]][5]
  })
  
  output$v_truth4_estim1 <- renderText({
    perf_meas()[["values"]][[4]][1]
  })
  
  output$v_truth4_estim2 <- renderText({
    perf_meas()[["values"]][[4]][2]
  })
  
  output$v_truth4_estim3 <- renderText({
    perf_meas()[["values"]][[4]][3]
  })
  
  output$v_truth4_estim4 <- renderText({
    perf_meas()[["values"]][[4]][4]
  })
  
  output$v_truth4_estim5 <- renderText({
    perf_meas()[["values"]][[4]][5]
  })
  
  output$r_truth1_estim1 <- renderText({
    perf_meas()[["ranks"]][[1]][1]
  })
  
  output$r_truth1_estim2 <- renderText({
    perf_meas()[["ranks"]][[1]][2]
  })
  
  output$r_truth1_estim3 <- renderText({
    perf_meas()[["ranks"]][[1]][3]
  })
  
  output$r_truth1_estim4 <- renderText({
    input$which_truth
    perf_meas()[["ranks"]][[1]][4]
  })
  
  output$r_truth1_estim5 <- renderText({
    perf_meas()[["ranks"]][[1]][5]
  })
  
  output$r_truth2_estim1 <- renderText({
    perf_meas()[["ranks"]][[2]][1]
  })
  
  output$r_truth2_estim2 <- renderText({
    perf_meas()[["ranks"]][[2]][2]
  })
  
  output$r_truth2_estim3 <- renderText({
    perf_meas()[["ranks"]][[2]][3]
  })
  
  output$r_truth2_estim4 <- renderText({
    perf_meas()[["ranks"]][[2]][4]
  })
  
  output$r_truth2_estim5 <- renderText({
    perf_meas()[["ranks"]][[2]][5]
  })
  
  output$r_truth3_estim1 <- renderText({
    perf_meas()[["ranks"]][[3]][1]
  })
  
  output$r_truth3_estim2 <- renderText({
    perf_meas()[["ranks"]][[3]][2]
  })
  
  output$r_truth3_estim3 <- renderText({
    perf_meas()[["ranks"]][[3]][3]
  })
  
  output$r_truth3_estim4 <- renderText({
    perf_meas()[["ranks"]][[3]][4]
  })
  
  output$r_truth3_estim5 <- renderText({
    perf_meas()[["ranks"]][[3]][5]
  })
  
  output$r_truth4_estim1 <- renderText({
    perf_meas()[["ranks"]][[4]][1]
  })
  
  output$r_truth4_estim2 <- renderText({
    perf_meas()[["ranks"]][[4]][2]
  })
  
  output$r_truth4_estim3 <- renderText({
    perf_meas()[["ranks"]][[4]][3]
  })
  
  output$r_truth4_estim4 <- renderText({
    perf_meas()[["ranks"]][[4]][4]
  })
  
  output$r_truth4_estim5 <- renderText({
    perf_meas()[["ranks"]][[4]][5]
  })
}

shinyApp(ui = ui, server = server)

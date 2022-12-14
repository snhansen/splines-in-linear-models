library(shiny)
library(shinyBS)
library(shinyFeedback)

ui <- fluidPage(
  title = "Splines in linear regression models",
  useShinyFeedback(),
  titlePanel(h1("Splines in linear regression models",
                align = "center")),
  
  fluidRow(
    column(3,
           h3("Parameters"),
           fluidRow(
             column(12,
                    tabsetPanel(id = "tab",
                                tabPanel(title = "Data generation",
                                         value = "data",
                                         fluidRow(
                                           column(12,
                                                  h4("True relationship between X and Y"),
                                                  selectInput("example",
                                                              label = "Choose a pre-specified relationship",
                                                              choices = c("", "Linear", "Piecewise linear", "Quadratic", "Cubic", "Exponential", "Sinusoidal"),
                                                              selected = NULL,
                                                              width = "80%"
                                                              
                                                  ),
                                                  textInput("user_fct",
                                                            label = ".. or specify it yourself:",
                                                            value = "",
                                                            width = "80%"),
                                                  hr(),
                                                  h4("Other parameters"),
                                                  numericInput("n",
                                                               label = "Number of observations:",
                                                               value = 100,
                                                               step = 10,
                                                               min = 1,
                                                               max = 100000,
                                                               width = "80%"),
                                                  numericInput("sd_res",
                                                               label = "Residual variation (sd):",
                                                               value = 1,
                                                               step = 0.1,
                                                               min = 0,
                                                               width = "80%"),
                                                  numericInput("x_min",
                                                               label = "Minimum x-value:",
                                                               value = 0,
                                                               step = 1,
                                                               width = "80%"),
                                                  numericInput("x_max",
                                                               label = "Maximum x-value:",
                                                               value = 10,
                                                               step = 1,
                                                               width = "80%"),
                                                  hr(),
                                                  h4("Hit me!"),
                                                  actionButton("generate",
                                                               label = "Generate data",
                                                               width = "80%")))),
                                tabPanel(title = "Model fitting",
                                         value = "model",
                                         fluidRow(
                                           column(12,
                                                  h4("Categorical fit"),
                                                  numericInput("n_cats",
                                                               label = "Number of categories:",
                                                               value = 2,
                                                               step = 1,
                                                               min = 1,
                                                               max = 10,
                                                               width = "80%"),
                                                  textInput("cats",
                                                            label = ".. or choose your own categories:",
                                                            width = "80%"),
                                                  hr(),
                                                  h4("Restricted cubic spline fit"),
                                                  numericInput("n_knots",
                                                               label = "Number of knots:",
                                                               value = 3,
                                                               step = 1,
                                                               min = 3,
                                                               max = 10,
                                                               width = "80%"),
                                                  textInput("knots",
                                                            label = ".. or choose your own knots:",
                                                            width = "80%")))),
                                tabPanel(title = "Visuals",
                                         value = "visuals",
                                         fluidRow(
                                           column(12,
                                                  h4("Graphical parameters")),
                                           column(6,
                                                  radioButtons("show_lin_fit",
                                                               label = "Linear fit:",
                                                               choices = c("Show", "Hide"),
                                                               selected = "Show",
                                                               inline = TRUE),
                                                  radioButtons("show_spl_fit",
                                                               label = "Spline fit:",
                                                               choices = c("Show", "Hide"),
                                                               selected = "Show",
                                                               inline = TRUE),
                                                  radioButtons("show_cat_fit",
                                                               label = "Categorical fit:",
                                                               choices = c("Show", "Hide"),
                                                               selected = "Show",
                                                               inline = TRUE)
                                                  ),
                                           column(6,
                                                  radioButtons("show_obs",
                                                               label = "Observations:",
                                                               choices = c("Show", "Hide"),
                                                               selected = "Show",
                                                               inline = TRUE),
                                                  radioButtons("show_truth",
                                                               label = "True relationship:",
                                                               choices = c("Show", "Hide"),
                                                               selected = "Show",
                                                               inline = TRUE)
                                                  ),
                                           column(12,
                                                  sliderInput("trans_truth", 
                                                              label = "Transparency (true relationship):",
                                                              min = 0,
                                                              max = 1,
                                                              value = 1,
                                                              step = 0.05,
                                                              width = "80%"),
                                                  sliderInput("trans_obs", 
                                                              label = "Transparency (observations):",
                                                              min = 0,
                                                              max = 1,
                                                              value = 1,
                                                              step = 0.05,
                                                              width = "80%"))))
                    )
             )
           )
    ),
    column(9,
           plotOutput(outputId = "main_plot", height = "700px"),
           span(textOutput(outputId = "spl_warning"), style = "color:red"),
           span(textOutput(outputId = "cat_warning"), style = "color:red"),
           span(textOutput(outputId = "fct_warning"), style = "color:red")
    )
  ),
  bsTooltip(id = "user_fct",
            title = "Needs to be a function in \"x\" written in the R language",
            placement = "right",
            trigger = "hover"),
  bsTooltip(id = "cats",
            title = "e.g. 0,3,10",
            placement = "right",
            trigger = "hover"),
  bsTooltip(id = "knots",
            title = "e.g. 2,5,8",
            placement = "right",
            trigger = "hover"),
)
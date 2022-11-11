library(shiny)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(Hmisc)
library(stringr)
library(RColorBrewer)
library(glue)

# Set your working directory to load the utils.R file.
# setwd("..")
source("R\\utils.R")

server <- function(input, output) {
  max_obs <- 100000
  max_knots <- 10
  max_cats <- 10
  sample_dat <- reactiveValues(df = NULL,
                               n_obs = NULL,
                               xrange = NULL,
                               sd_res = NULL,
                               fct = NULL)
  spl_warning <- reactiveVal()
  cat_warning <- reactiveVal()
  fct_warning <- reactiveVal()
  valid_inputs <- reactiveVal()
  
  # We check the inputs are throw warnings if they are invalid.
  observe({
    bad_sd <- is.na(input$sd_res) | input$sd_res < 0
    feedbackWarning("sd_res", bad_sd, "Residual sd needs to be a non-negative number.")
    bad_n <- is.na(input$n) | input$n < 2 | (input$n %% 1) != 0 | input$n > max_obs
    feedbackWarning("n", bad_n, glue("Number of observations needs to be an integer between 2 and {max_obs}."))
    hideFeedback("x_min")
    hideFeedback("x_max")
    bad_x_min <- is.na(input$x_min)
    bad_x_max <- is.na(input$x_max)
    bad_xs <- !is.na(input$x_max) & !is.na(input$x_min) & input$x_max <= input$x_min
    if (bad_x_min) {
      feedbackWarning("x_min", bad_x_min, "Minimum x-value needs to be a number.")
    }
    if (bad_x_max) {
      feedbackWarning("x_max", bad_x_max, "Maximum x-value needs to be a number.")  
    }
    if (bad_xs) {
      feedbackWarning("x_min", bad_xs, "Minimum x-value must be smaller than maximum x-value.")
      feedbackWarning("x_max", bad_xs, "Minimum x-value must be smaller than maximum x-value.")
    }
    valid_inputs(!bad_sd & !bad_n & !bad_x_min & !bad_x_max & !bad_xs)
  })
  
  observeEvent(input$generate, {
    fct_warning(NULL)
    missing_dgm <- input$example == "" & input$user_fct == ""
    feedbackWarning("example", missing_dgm, text = NULL)
    feedbackWarning("user_fct", missing_dgm, text = NULL)
    # If inputs are bad, set everything to NULL.
    if (!valid_inputs() | missing_dgm) {
      sample_dat$df = NULL
      sample_dat$n_obs = NULL
      sample_dat$xrange = NULL
      sample_dat$sd_res <- NULL
      sample_dat$fct <- NULL
    }
    # Else we proceed to checking the specified function.
    else {
      user_fct <- parse_fct(input$user_fct, c(input$x_min, input$x_max))
      # If specification is valid, we simulate.
      if (user_fct$is_valid) {
        fct_warning(NULL)
        sample_dat$fct <- user_fct$fct
        sample_dat$xrange = c(input$x_min, input$x_max)
        sample_dat$n_obs <- input$n
        sample_dat$sd_res <- input$sd_res
        x <- runif(sample_dat$n_obs, sample_dat$xrange[1], sample_dat$xrange[2])
        y <- sapply(x, sample_dat$fct) + rnorm(sample_dat$n_obs, 0, sample_dat$sd_res)
        sample_dat$df <- tibble(x, y)
      }
      # Otherwise, we set everything to NULL and throw a warning.
      else {
        fct_warning(user_fct$msg)
        sample_dat$df = NULL
        sample_dat$n_obs = NULL
        sample_dat$xrange = NULL
        sample_dat$sd_res <- NULL
        sample_dat$fct <- NULL
      }
    }
  })
  
  pred_dat <- reactive({
    # If no valid sample is available, return NULL.
    if (is.null(sample_dat$df)) {
      return(NULL)
    }
    # Else, we predict the mean outcome along a grid under the various models.
    n_grid <- 10000
    res_dat <- tibble(x = seq(sample_dat$xrange[1], sample_dat$xrange[2], length.out = n_grid))
    res_dat <- res_dat %>% mutate(y = sapply(x, sample_dat$fct))
    
    # Linear relationship.
    model_lin <- lm(y ~ x, sample_dat$df)
    res_dat <- res_dat %>% mutate(pred_lin = predict(model_lin, cur_data()))
    
    # Categorical relationship.
    breaks <- get_breaks(input$n_cats, input$cats, c(sample_dat$xrange[1], sample_dat$xrange[2]), max_cats)
    if (!is.null(breaks)) {
      cat_dat <- sample_dat$df %>% mutate(x_cat = cut(x, breaks = breaks, include.lowest = TRUE))
      n_obs_cats <- cat_dat %>% select(x_cat) %>% n_distinct()
      if (n_obs_cats < length(breaks) - 1) {
        cat_warning("Warning: Empty categories was observed (categorical fit omitted).")
      }
      else {
        cat_warning(NULL)
        model_cat <- sample_dat$df %>%
          mutate(x_cat = cut(x, breaks = breaks, include.lowest = TRUE)) %>%
          lm(y ~ x_cat, data = .)
        res_dat <- res_dat %>% mutate(x_cat = cut(x, breaks = breaks, include.lowest = TRUE))
        res_dat <- res_dat %>% mutate(pred_cat = predict(model_cat, cur_data()))
      }
    }
    else {
      cat_warning(glue("Warning: Number of categories must be an integer between 2 and {max_cats}  (categorical fit omitted)."))
    }
    
    # Spline relationship.
    if (sample_dat$n_obs >= 6) {
      knots <- get_knots(sample_dat$df$x, input$n_knots, input$knots, c(sample_dat$xrange[1], sample_dat$xrange[2]), max_knots)
      if (!is.null(knots)) {
        spl_warning(NULL)
        spline_vars <- rcspline.eval(sample_dat$df$x, knots = knots, inclx = TRUE)
        colnames(spline_vars) <- sprintf("spl%s", seq(1:(length(knots) - 1)))
        spl_dat <- bind_cols(sample_dat$df, spline_vars)
        spl_formula <- formula(paste("y ~", paste("spl", 1:(length(knots) - 1), sep = "", collapse = "+")))
        model_spl <- lm(spl_formula, spl_dat)
        
        spline_vars <- rcspline.eval(res_dat$x, knots = knots, inclx = TRUE)
        colnames(spline_vars) <- sprintf("spl%s", seq(1:(length(knots) - 1)))
        res_dat <- bind_cols(res_dat, spline_vars)
        res_dat <- res_dat %>% mutate(pred_spl = predict(model_spl, cur_data()))
      }
      else {
        spl_warning(glue("Warning: Number of spline knots must be an integer between 3 and {max_knots}  (spline fit omitted)."))
      }
    }
    else {
      spl_warning("Warning: Need at least 6 observation to use splines (spline fit omitted).")
    }
    return(res_dat)
  })
  
  # Some pre-specified examples.
  observeEvent(input$example, {
    if (input$example == "Linear") {
      fct <- "2*x+2"
      x_min <- 0
      x_max <- 10
      sd_res <- 1
    }
    else if (input$example == "Quadratic") {
      fct <- "x^2-2*x+2"
      x_min <- -10
      x_max <- 10
      sd_res <- 10
    }
    else if (input$example == "Cubic") {
      fct <- "-0.2*x^3+3*x^2+2*x-3"
      x_min <- -10
      x_max <- 15
      sd_res <- 20
    }
    else if (input$example == "Exponential") {
      fct <- "exp(x)"
      x_min <- 0
      x_max <- 5
      sd_res <- 10
    }
    else if (input$example == "Sinusoidal") {
      fct <- "sin(x)"
      x_min <- -pi
      x_max <- pi
      sd_res <- 0.1
    }
    else if (input$example == "Piecewise linear") {
      fct <- "x*(x>0)-x*(x<=0)"
      x_min <- -10
      x_max <- 10
      sd_res <- 1
    }
    else if (input$example == "") {
      fct <- ""
      x_min <- 0
      x_max <- 10
      sd_res <- 1
    }
    updateTextInput(inputId = "user_fct", value = fct)
    updateNumericInput(inputId = "sd_res", value = sd_res)
    updateNumericInput(inputId = "x_min", value = x_min)
    updateNumericInput(inputId = "x_max", value = x_max)
  })
  
  # We make a reactive plot
  plot <- reactive({
    make_plot(pred_dat = pred_dat(),
              sample_dat = sample_dat$df,
              points = (input$show_obs == "Show"),
              truth = (input$show_truth == "Show"),
              lin = (input$show_lin_fit == "Show"),
              cat = (input$show_cat_fit == "Show"),
              spl = (input$show_spl_fit == "Show"),
              trans_truth = input$trans_truth,
              trans_obs = input$trans_obs)
  })
  
  # Output the plot.
  output$main_plot <- renderPlot({
    plot()
  })
  
  # Output the warnings.
  output$spl_warning <- renderText(spl_warning())
  output$cat_warning <- renderText(cat_warning())
  output$fct_warning <- renderText(fct_warning())
}
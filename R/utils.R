# Takes a string of comma-separated numbers and returns a
# sorted vector if format is valid.
numvectorize <- function(s) {
  if (s == "") {
    return(NULL)
  }
  nums <- unlist(suppressWarnings(map(str_split(s,","), as.numeric)))
  if (any(is.na(nums))) {
    return(NULL)
  }
  else {
    return(nums)
  }
}

# If user-specified categories are given, breaks are made 
# according to those (if valid), else if the number of categories
# is given, we split the x-axis evenly.
get_breaks <- function(n_cats, cats, xrange, max_cats) {
  if (is.na(n_cats) & cats == "") {
    return(NULL)
  }
  if (cats != "") {
    numvector <- numvectorize(cats)
    if (!is.null(numvector)) {
      breaks <- sort(unique(pmax(pmin(numvector, xrange[2]), xrange[1])))
      # Make sure the number of categories is between 2 and max_cats.
      if ((length(breaks) - 1) > max_cats | length(breaks) - 1  < 2) {
        return(NULL)
      }
      return(breaks)
    }
    else {
      return(NULL)
    }
  }
  else {
    if (n_cats < 2 | n_cats %% 1 != 0 | n_cats > max_cats) {
      return(NULL)
    }
    breaks <- c()
    for (i in 1:(n_cats + 1)) {
      breaks = append(breaks, xrange[1] + (i - 1)*(xrange[2] - xrange[1])/n_cats)
    }  
    return(breaks)
  }
}

# If user-specified knots are given, we use these (if valid),
# else if the number of knots is given, we let rcspline.eval()
# choose them.
get_knots <- function(x, n_knots, knots, xrange, max_knots) {
  if (is.na(n_knots) & knots == "") {
    return(NULL)
  }
  if (knots != "") {
    numvector <- numvectorize(knots)
    if (!is.null(numvector)) {
      num_knots <- sort(unique(pmax(pmin(numvector, xrange[2]), xrange[1])))
      if (length(num_knots) > max_knots | length(num_knots) < 3) {
        return(NULL)
      }
      return(num_knots)
    }
    else {
      return(NULL)
    }
  }
  else {
    if (n_knots < 3 | n_knots %% 1 != 0 | n_knots > max_knots) {
      return(NULL)
    }
    return(rcspline.eval(x, nk = n_knots, knots.only = TRUE))
  }
}

# Parse user-specified function for the X/Y-relationship. We check if this
# is a valid function on the chosen x-range.
parse_fct <- function(s, xrange) {
  if (s == "") {
    return(list("is_valid" = FALSE, "msg" = "Error: Empty functional expression."))
  }
  fct <- function(x) {
    return(eval(parse(text = s)))
  }
  err <- try(fct(xrange), TRUE)
  print(err)
  if (class(err) == "try-error") {
    return(list("is_valid" = FALSE, "msg" = "Error: Invalid functional expression"))
  }
  else {
    if (any(is.na(err))) {
      return(list("is_valid" = FALSE, "msg" = "Error: Invalid functional expression"))
    }
    else {
      return(list("is_valid" = TRUE, "fct" = fct))
    }
  }
}

# Function to make the main plot.
make_plot <- function(pred_dat, sample_dat, points, truth, lin, cat, spl, trans_truth, trans_obs) {
  if (is.null(pred_dat)) {
    return(NULL)
  }
  # cbp <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
  #          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  color_pal <- c("#000000", brewer.pal(4, "Set2"))
  res_plot <-  ggplot(data = pred_dat)  +
    theme_minimal() +
    labs(x = "", y = "") +
    geom_line(aes(x = x,
                  y = y,
                  color = "Truth",
                  linetype = "Truth",
                  size = "Truth"),
              alpha = as.integer(truth)*trans_truth) +
    scale_color_manual(name = "",
                       values = c("Truth" = color_pal[1],
                                  "Linear" = color_pal[2],
                                  "Categorical" = color_pal[3],
                                  "Spline" = color_pal[5])) +
    scale_linetype_manual(name = "",
                          values = c("Truth" = 1,
                                     "Linear" = 2,
                                     "Categorical" = 2,
                                     "Spline" = 2)) +
    scale_size_manual(name = "",
                      values = c("Truth" = 1,
                                 "Linear" = 1.25,
                                 "Categorical" = 1.25,
                                 "Spline" = 1.25)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 25),
          legend.key.size = unit(1.6, 'cm'))
  
  if ("pred_lin" %in% colnames(pred_dat) & lin) {
    res_plot <- res_plot +
      geom_line(aes(x = x,
                y = pred_lin,
                color = "Linear",
                linetype = "Linear",
                size = "Linear"))
  }
  if ("pred_cat" %in% colnames(pred_dat) & cat) {
    res_plot <- res_plot + 
        geom_line(aes(x = x,
                      y = pred_cat,
                      color = "Categorical",
                      linetype = "Categorical",
                      size = "Categorical"))
  }
  if ("pred_spl" %in% colnames(pred_dat) & spl) {
    res_plot <- res_plot + 
        geom_line(aes(x = x,
                      y = pred_spl,
                      color = "Spline",
                      linetype = "Spline",
                      size = "Spline"))
  }
  if (points) {
    res_plot <- res_plot + 
      new_scale_color() + 
      geom_point(data = sample_dat,
                 aes(x = x,
                     y = y,
                     color = "Points"),
                 alpha = trans_obs) + 
      scale_color_manual(name = "",
                         values = c("Points" = color_pal[1])) + 
      guides(color = guide_legend(order = 1))
  }
  else {
    res_plot <- res_plot +
      new_scale_color() +
      geom_point(data = sample_dat,
                 aes(x = x,
                     y = y,
                     color = "Points"),
                 alpha = 0) +
      scale_color_manual(name = "",
                         values = c("Points" = color_pal[1])) + 
      guides(color = guide_legend(order = 1))

  }
  return(res_plot)
}
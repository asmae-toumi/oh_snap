
# library(DALEX)
.predict_logit <- function(model, x) {
  raw_x <- predict(model, x)
  exp(raw_x) / (1 + exp(raw_x))
}
.logit <- function(x) exp(x) / (1 + exp(x))

# ingredients:::plot.ceteris_paribus_explainer
.plot_numerical_ceteris_paribus <- 
  function(all_profiles, facet_ncol = NULL, ...) {
    
    tmp <- as.character(all_profiles$`_vname_`)
    for (i in seq_along(tmp)) {
      all_profiles$`_x_`[i] <- all_profiles[i, tmp[i]]
    }
    p <-
      ggplot(
        all_profiles,
        aes(
          `_x_`,
          `_yhat_`,
          group = paste(`_ids_`, `_label_`)
        )
      ) + 
      geom_line(
        data = all_profiles,
        ...
      ) + 
      facet_wrap(
        ~`_vname_`,
        scales = 'free_x',
        ncol = 1
      )
    p
  }

.check_all_variables <- function(all_variables, variables = NULL) {
  if (!is.null(variables)) {
    all_variables <- intersect(all_variables, variables)
    if (length(all_variables) == 0) {
      stop(
        paste0(
          'variables do not overlap with ',
          paste(all_variables, collapse = ', ')
        )
      )
    }
  }
  all_variables
}

.plot_ceteris_paribus_explainer <- 
  function(x,
           ...,
           color = 'black',
           variables = NULL) {
    
    dfl <- c(list(x))
    all_profiles <- do.call(rbind, dfl)
    class(all_profiles) <- 'data.frame'
    all_profiles$`_ids_` <- factor(all_profiles$`_ids_`)
    all_variables <- na.omit(as.character(unique(all_profiles$`_vname_`)))
    all_variables <- .check_all_variables(all_variables, variables)
    
    is_numeric <- sapply(
      all_profiles[, all_variables, drop = FALSE],
      is.numeric
    )
    vnames <- names(which(is_numeric))
    all_profiles <- all_profiles[
      all_profiles$`_vname_` %in%
        vnames,
    ]
    
    p <-
      .plot_numerical_ceteris_paribus(
        all_profiles = all_profiles,
        facet_ncol = facet_ncol,
        ...
      )
    p
  }

.show_aggregated_profiles <-
  function(x, ..., color = 'red', variables = NULL) {
    dfl <- c(list(x))
    aggregated_profiles <- do.call(rbind, dfl)
    class(aggregated_profiles) <- 'data.frame'
    all_variables <- unique(aggregated_profiles$`_vname_`)
    all_variables <- .check_all_variables(all_variables, variables)
    aggregated_profiles <- aggregated_profiles[aggregated_profiles$`_vname_` %in% all_variables, ]
    res <- 
      geom_line(
        data = aggregated_profiles,
        aes(y = `_yhat_`),
        ...,
        color = color
      )
    res
  }

plot_pdp <- function(x, ...) {
  p <-
    .plot_ceteris_paribus_explainer(x$cp_profiles, ..., alpha = 0.1, size = 0.5, color = '#132f3c') +
    .show_aggregated_profiles(x$agr_profiles, ..., size = 2, color = '#ffa3af')
}

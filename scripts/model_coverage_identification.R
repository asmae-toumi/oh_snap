
library(tidyverse)
source('scripts/read_files.R')
positions <- read_positions()
plays <- read_plays()

weeks <- 1L:17L
features <-
  weeks %>%
  tibble(week = .) %>% 
  mutate(
    data = 
      map(week, 
          ~vroom::vroom(
            file.path(
              'data',
              sprintf('coverage_identification_features_week%d.csv', .x)
            )
          )
      )
  ) %>%
  unnest(data) %>% 
  inner_join(
    positions %>% 
      mutate(across(label, ~if_else(position == 'DB', 'CB', .x))) %>% 
      select(
        side,
        position_category = category,
        position_label = label,
        position
      )
  ) %>% 
  mutate(idx = row_number()) %>% 
  relocate(idx)
features
features %>% count(event)
# plays_w_defender <-
#   plays %>% 
#   select(game_id, play_id, play_description) %>% 
#   filter(play_description %>% str_detect('incomplete')) %>% 
#   filter(play_description %>% str_detect('\\(.*\\)[.]$'))

# library(reticulate)
sklearn <- reticulate::import('sklearn')

.valid_events <- c('postsnap-prethrow') # , 'postthrow-preoutcome')
.valid_position_labels <- c('CB', 'S', 'LB')
.valid_covariance_types <- c('full') # , 'spherical', 'tied', 'diag')
generate_gmm_params_grid <- function() {
  gmm_params_grid <-
    crossing(
      event = .valid_events,
      position_label = .valid_position_labels,
      covariance_type = .valid_covariance_types,
      # covariance_type = c('full', 'diag'),
      # covariance_type = 'full',
      n_components = c(2L, 3L, 4L, 5L) # , 8L)
      # n_components = seq.int(2L, 9L)
    )
}

.select_mat <- function(mat, cols) {
  mat[,cols, drop = FALSE]
}

.unselect_mat <- function(mat, cols) {
  mat[,setdiff(colnames(mat), cols), drop = FALSE]
}

.prepare_results <- function(clf, mat, ..., save_preds = TRUE, save_probs = TRUE) {
  # browser()
  mat_noidx <- mat %>% .unselect_mat('idx')
  bic <- clf$bic(mat_noidx)
  res <- list(bic = bic)
  preds <- clf$predict(mat_noidx)

  mat_idx <- mat %>% .select_mat('idx') %>% as_tibble()
  if(save_preds) {
    preds <- 
      preds %>% 
      tibble(cl_best = .) %>% 
      bind_cols(mat_idx)
    res <- c(res, list(preds = preds))
  }

  if(save_probs) {
    probs <- clf$predict_proba(mat_noidx)
    probs <-
      probs %>% 
      as_tibble() %>%
      set_names(sprintf('%d', 1:ncol(probs))) %>% 
      bind_cols(mat_idx) %>% 
      pivot_longer(
        -c(idx),
        names_to = 'cluster',
        values_to = 'prob'
      ) %>% 
      mutate(across(cluster, as.integer))
    res <- c(res, list(probs = probs))
  }
  res
}

.evaluate_split <- function(clf, split, ...) {
  trn <- split %>% rsample::analysis()
  tst <- split %>% rsample::assessment()
  trn_mat <- trn %>% as.matrix()
  tst_mat <- tst %>% as.matrix()
  clf$fit(trn_mat %>% .unselect_mat('idx'))
  
  res <- map(list('trn' = trn_mat, 'tst' = tst_mat), ~.prepare_results(clf = clf, mat = .x, ...))
  res <- res %>% enframe(name = 'set', value = 'value') %>% unnest_wider(value)
  res
}

.fit_sklearn_gmm_cv <-
  function(data, clf, n_folds = 10L, ...) {
    folds <- 
      rsample::vfold_cv(data, v = n_folds) %>% 
      mutate(across(id, ~str_remove(.x, 'Fold') %>% as.integer())) %>% 
      rename(fold = id)
    data
    # rsample::make_splits()
    res <-
      folds %>% 
      mutate(res = map(splits, ~.evaluate_split(clf = clf, split = .x, ...))) %>% 
      select(-splits)
    res %>% unnest(res)
  }

.prepare_sklearn_gmm <-
  function(features,
           event = .valid_events,
           position_label = .valid_position_labels,
           covariance_type = .valid_covariance_types,
           n_components = 2L,
           ...,
           seed = 0L) {
    # event = 'postsnap-prethrow'
    # position_label = 'CB'
    # covariance_type = 'full'
    # n_components = 3L
    # save_preds = FALSE
    # save_probs = FALSE
    # n_folds = 10L
    # prop = 0.75
    # seed = 0L
    event <- match.arg(event)
    position_label <- match.arg(position_label)
    covariance_type <- match.arg(covariance_type)
    
    data <-
      features %>%
      filter(event == !!event & position_label == !!position_label) %>% 
      select(
        idx,
        week,
        x_mean,
        x_var,
        y_mean,
        y_var,
        s_mean,
        s_var,
        off_mean,
        off_var,
        def_mean,
        rat_mean,
        rat_var,
        off_dir_mean,
        off_dir_var,
        off_o_mean,
        off_o_var
      )
    
    clf <- sklearn$mixture$GaussianMixture(
      n_components = n_components, 
      covariance_type = covariance_type,
      random_state = seed
    )
    res <-
      list(
        data = data,
        clf = clf
      )
    res
  }

fit_sklearn_gmm <-
  function(...,
           # Use 'none' after choosing "best" parameters given CV evaluation.
           # Output format is slightly different for 'cv' compared to the other two `how`s, which I should fix.
           how = c('cv', 'none'),
           prop = 0.8,
           seed = 0L) {
    how <- match.arg(how)

    res_prep <- .prepare_sklearn_gmm(seed = seed, ...)
    data <- res_prep$data
    clf <- res_prep$clf
    # split <- rsample::initial_split(data, prop = prop)
    if(how == 'cv') {
      res <- .fit_sklearn_gmm_cv(clf = clf, data = data, ...)
    } else {
      mat <- data %>% as.matrix()
      clf$fit(mat %>% .unselect_mat('idx'))
      res <- .prepare_results(clf = clf, mat = mat, ...)
    }
    res
  }

gmm_params_grid <- generate_gmm_params_grid()

quietly_fit_sklearn_gmm <- quietly(fit_sklearn_gmm)
res_gmm_cv <-
  gmm_params_grid %>% 
  # slice(1) %>% 
  mutate(
    res = 
      pmap(
        list(event, position_label, covariance_type, n_components), 
        ~quietly_fit_sklearn_gmm(
          features = features, 
          event = ..1, 
          position_label = ..2, 
          covariance_type = ..3, 
          n_components = ..4,
          cv = TRUE
        )
      )
  )
res_gmm_cv

res_gmm_cv %>% unnest_wider(res) %>% unnest(result) %>% select(probs) %>% unnest(probs) # unnest(warnings)

# res_gmm_cv_unnested <-
#   res_gmm_cv %>% 
#   unnest(res)

res_gmm_cv %>% 
  unnest_wider(res) %>% 
  unnest(result) %>% 
  select(event, position_label, covariance_type, n_components, set, probs) 
  

# tic <- tictoc::tic()
res_gmm_cv_agg_by_idx <-
  res_gmm_cv %>% 
  unnest_wider(res) %>% 
  unnest(result) %>% 
  select(event, position_label, covariance_type, n_components, fold, set, probs) %>% 
  unnest(probs) %>% 
  group_by(event, position_label, covariance_type, n_components, set, idx, cluster) %>% 
  summarize(
    # n = n(), # This should be just `n_folds` - 1
    across(prob, mean)
  ) %>% 
  # filter(prob == max(prob)) %>% 
  slice_max(prob, with_ties = FALSE) %>% 
  ungroup()
res_gmm_cv_agg_by_idx
res_gmm_cv_agg_by_idx %>% 
  filter(event == 'postsnap-prethrow', covariance_type == 'full') %>% 
  select(position_label, n_components, idx, cluster, prob) %>% 
  # mutate(across(n_components, ordered)) %>% 
  ggplot() +
  aes(x = prob) +
  geom_histogram(binwidth = 0.05) +
  facet_grid(n_components~position_label, scales = 'free_y')
# toc <- tictoc::toc()

set.seed(42L)
res_gmm_cv_agg_by_idx %>% 
  filter(event == 'postsnap-prethrow', covariance_type == 'full') %>% 
  select(position_label, n_components, idx, cluster, prob) %>% 
  mutate(across(c(n_components, cluster), ordered)) %>% 
  sample_frac(0.01) %>% 
  ggplot() +
  aes(x = prob, y = cluster) +
  geom_point(aes(color = cluster)) +
  facet_grid(position_label~n_components, scales = 'free_y')

.pull_for_ari <- function(data) {
  data %>% 
    select(idx, cluster) %>% 
    arrange(idx) %>% 
    pull(cluster)
}

compute_cv_ari <- function(data) {
  actuals <- data %>% filter(set == 'trn') %>% .pull_for_ari()
  preds <- data %>% filter(set == 'tst') %>% .pull_for_ari()
  # browser()
  ari <- sklearn$metrics$adjusted_rand_score(labels_true = actuals, labels_pred = preds)
  ari
}

gmm_cv_aris <-
  res_gmm_cv_agg_by_idx %>% 
  nest(data = -c(event, position_label, covariance_type, n_components)) %>% 
  mutate(ari = map_dbl(data, compute_cv_ari)) %>% 
  select(-data)
gmm_cv_aris

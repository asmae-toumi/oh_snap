
source('scripts/target_prob_setup.R')

# catch_prob_wts <-
#   tibble(
#     nfl_id = c(2495504, 2495807, 2543769, 2543830, 2556371, 2557887, 2558009),
#     dist = c(19.1, 4.12, 3.42, 3.95, 19.3, 16.2, 19.0),
#     inv_dist_wt = c(0.00275, 0.0589, 0.0856, 0.0641, 0.00269, 0.00379, 0.00278),
#     normalized_wt = c(0.0125, 0.267, 0.388, 0.291, 0.0122, 0.0172, 0.0126)
#   ) %>% 
#   mutate(
#     dist_total = sum(1 / dist^2),
#     inv_dist_wt2 = 1 / dist^2,
#     normalized_wt2 = inv_dist_wt2 / sum(inv_dist_wt2)
#   ) %>% 
#   select(nfl_id, dist, matches('inv_dist'), matches('normalized_wt'))
# catch_prob_wts

save_plot <-
  function(gg,
           file = deparse(substitute(gg)),
           ext = 'png',
           dir = .get_dir_figs(),
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           height = 8,
           width = height,
           ...) {
    ggplot2::ggsave(plot = gg, filename = path, width = width, height = height, type = 'cairo', ...)
  }

plot_defensive_allocation <- function(
  game_id = 2018121600,
  play_id = 1125,
  # frame_id = 36
  plays = import_plays(),
  games = import_games(),
  positions = import_positions(),
  team_colors = TRUE,
  yardmin = NULL,
  yardmax = NULL,
  field_color = 'white',
  line_color = 'black',
  sideline_color = 'white',
  endzone_color = NULL,
  buffer = NULL,
  colors = import_colors(),
  probs_dists = do_combine_target_probs_and_dists(overwrite = F),
  n_defender = 3,
  snapshot = c('throw', 'snap'), # This is specifically for this function. Different than `at`.
  save = TRUE,
  dir = file.path('figs', 'target_prob'),
  ext = 'png',
  filename = sprintf('%s-%s-%s-n_defender=%d.%s', game_id, play_id, snapshot, n_defender, ext),
  path = file.path(dir, filename),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  width = 10,
  height = 8
) {
  
  snapshot <- match.arg(snapshot)
  probs_dists_filt_init <- probs_dists %>% filter(game_id == !!game_id & play_id == !!play_id)
  f <- if(snapshot == 'snap') {
    slice_min
  } else if (snapshot == 'throw') {
    slice_max
  }
  frame_id <- probs_dists_filt_init %>% f(frame_id, with_ties = FALSE) %>% pull(frame_id)
  assertthat::assert_that(length(frame_id) == 1L)
  
  # game_id = 2018090905; play_id = 783
  meta <- tibble(game_id = game_id, play_id = play_id, frame_id = frame_id)
  probs_dists_filt <- probs_dists_filt_init %>% inner_join(meta)
  
  game <- games %>% filter(game_id == !!game_id)
  assertthat::assert_that(nrow(game) == 1L)
  week <- game$week
  
  tracking <- 
    import_tracking(week = week, positions = positions, standardize = FALSE)
  
  tracking <-
    tracking %>%
    inner_join(meta %>% rename(frame_id_clip = frame_id), by = c('game_id', 'play_id')) %>% 
    filter(frame_id <= frame_id_clip) %>% 
    select(-frame_id_clip)
  
  play <- plays %>% inner_join(meta %>% select(game_id, play_id), by = c('game_id', 'play_id'))
  assertthat::assert_that(nrow(play) == 1L)
  
  game <- games %>% filter(game_id == !!game_id)
  assertthat::assert_that(nrow(game) == 1L)
  
  target_id <- play$target_nfl_id
  has_target <- !is.na(target_id)
  
  if(!has_target) {
    .display_warning('No target receiver.')
    # return(ggplot())
    target <- tibble(display_name = '?', jersey_number = -1)
    has_target <- FALSE
  } else {
    target <-
      tracking %>%
      filter(nfl_id == !!target_id) %>%
      distinct(display_name, jersey_number)
    assertthat::assert_that(nrow(target) == 1L)
  }
  
  tracking_clipped <-
    tracking %>%
    arrange(game_id, play_id, nfl_id, frame_id, o)
  
  line_of_scrimmage <- play$absolute_yardline_number
  play_direction <- tracking$play_direction[[1]]
  sign <- ifelse(play_direction == 'left', -1, 1)
  first_down_line <- line_of_scrimmage + sign * play$yards_to_go
  
  target_tracking <-
    tracking %>%
    filter(nfl_id == !!target_id)
  
  nontarget_tracking <-
    tracking %>%
    filter(nfl_id != !!target_id)
  
  target_tracking_clipped <-
    tracking_clipped %>%
    filter(nfl_id == !!target_id)
  
  nontarget_tracking_clipped <-
    tracking_clipped %>%
    filter(nfl_id != !!target_id)
  
  ball <-
    tracking %>%
    distinct(game_id, play_id, frame_id, x = ball_x, y = ball_y, o) %>%
    mutate(nfl_id = NA_integer_) %>% 
    f(frame_id)
  
  if(is.null(yardmin) | is.null(yardmax)) {
    yardminmax <-
      tracking %>%
      summarize(across(x, list(min = min, max = max)))
    
    if(is.null(yardmin)) {
      yardmin <- yardminmax$x_min
      yardmin <- .round_any(yardmin, 10, floor)
    }
    
    if(is.null(yardmax)) {
      yardmax <- yardminmax$x_max
      yardmax <- .round_any(yardmax, 10, ceiling)
    }
  }
  
  max_y <- 160 / 3
  if(is.null(buffer)) {
    yminmax <-
      tracking %>%
      summarize(across(y, list(min = min, max = max)))
    ymin <- yminmax$y_min
    ymin <- .round_any(ymin, 1, floor)
    ymax <- yminmax$y_max
    ymax <- .round_any(ymax, 1, ceiling)
    ymin_bound <- 0 - ymin
    ymax_bound <- ymax - max_y
    buffer <- max(0, ymin_bound, ymax_bound)
  }
  
  home_team <- game$home_team_abbr
  away_team <- game$visitor_team_abbr
  
  if(team_colors) {
    
    home_color <- colors %>% filter(team == !!home_team) %>% pull(color)
    away_color <- colors %>% filter(team == !!away_team) %>% pull(color2)
    # scales::show_col(c(home_color, away_color))
    if(play$possession_team == home_team) {
      offense_color <- home_color
      defense_color <- away_color
    } else {
      offense_color <- away_color
      defense_color <- home_color
    }
  } else {
    offense_color <- 'red'
    defense_color <- 'blue'
    if(play$possession_team == home_team) {
      home_color <- offense_color
      away_color <- defense_color
    } else {
      home_color <- defense_color
      away_color <- offense_color
    }
  }
  
  nontarget_tracking_between <-
    nontarget_tracking %>% 
    anti_join(
      nontarget_tracking_clipped %>% 
        select(game_id, play_id, nfl_id, frame_id, o),
      by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
    )
  
  # new stuff
  tracking_d_clipped <-
    nontarget_tracking_clipped %>%
    filter(side == 'D')
  tracking_d_clipped

  # .power <- 2
  # tracking_d_clipped_annotate <-
  #   tracking_d_clipped %>%
  #   slice_max(frame_id) %>%
  #   select(frame_id, nfl_id_d = nfl_id, display_name_d = display_name, x_d = x, y_d = y) %>%
  #   left_join(
  #     tracking_clipped %>%
  #       filter(side == 'O' & !is.na(route)) %>%
  #       select(frame_id, nfl_id, x, y)
  #   ) %>%
  #   mutate(
  #     dist_d = .dist(x, x_d, y, y_d)
  #   ) %>%
  #   select(-frame_id) %>%
  #   # filter(dist_d < 20) %>%
  #   arrange(dist_d) %>%
  #   filter(dist_d <= 20) %>%
  #   # # Normalize per defensive player
  #   # group_by(nfl_id_d) %>%
  #   # mutate(
  #   #   dist_d_total = sum(1 / dist_d^.power),
  #   #   wt = (1 / dist_d^.power) / dist_d_total
  #   # ) %>%
  #   # ungroup() %>%
  #   # Normalize per defensive player
  #   group_by(nfl_id) %>%
  #   mutate(
  #     # idx = row_number(dist_d),
  #     dist_d_total = sum(1 / dist_d^.power),
  #     wt =  (1 / dist_d^.power),
  #   ) %>%
  #   ungroup() %>%
  #   # Normalize per receiver.
  #   group_by(nfl_id) %>%
  #   mutate(
  #     wt = wt / sum(wt)
  #   ) %>%
  #   ungroup() %>%
  #   filter(nfl_id == target_id)
  # tracking_d_clipped_annotate

  # p +
  #   ggforce::geom_circle(
  #     inherit.aes = FALSE,
  #     data = tracking_d_clipped_annotate,
  #     aes(x0 = x, y0 = y, r = r, fill = r),
  #     alpha = 0.2,
  #     show.legend = FALSE
  #   )
  probs_dists_annotate <-
    probs_dists_filt %>% 
    filter(nfl_id == target_id) %>% 
    select(frame_id, nfl_id_d, dist_d, wt) %>% 
    left_join(tracking_clipped %>% select(frame_id, nfl_id_d = nfl_id, x_d = x, y_d = y)) %>% 
    left_join(target_tracking %>% select(frame_id, x, y)) %>% 
    select(-frame_id) %>% 
    arrange(dist_d)
  probs_dists_annotate
  
  probs_dists_annotate_filt <-
    probs_dists_annotate %>% 
    head(n_defender)
  
  p <-
    ggplot() +
    gg_field(
      yardmin = yardmin,
      yardmax = yardmax,
      buffer = buffer,
      field_color = field_color,
      line_color = line_color,
      sideline_color = sideline_color # ,
      # ...
    ) +
    aes(x = x, y = y) +
    geom_segment(
      data = tibble(x = !!line_of_scrimmage),
      inherit.aes = FALSE,
      aes(x = x, y = 0, xend = x, yend = !!max_y),
      size = 1.25
    ) +
    geom_segment(
      data = tibble(x = !!first_down_line),
      inherit.aes = FALSE,
      aes(x = x, y = 0, xend = x, yend = !!max_y),
      color = '#ffff7f',
      size = 2
    ) +
    geom_segment(
      data = probs_dists_annotate_filt,
      aes(x = x_d, y = y_d, xend = x, yend = y),
      # nudge_x = 0.2,
      # alpha = 0.5,
      linetype = 2,
      size = 1.1
    ) +
    geom_point(
      data = ball,
      inherit.aes = TRUE,
      size = 3,
      color = 'brown'
    ) +
    geom_path(
      data = nontarget_tracking_clipped %>% select(-frame_id),
      aes(color = side, group = nfl_id),
      size = 1,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    geom_path(
      data = nontarget_tracking_between %>% select(-frame_id),
      aes(color = side, group = nfl_id),
      size = 1,
      alpha = 0.3,
      linetype = 2,
      show.legend = FALSE
    ) +
    geom_point(
      data = nontarget_tracking %>% filter(side == 'D') %>% slice_max(frame_id),
      aes(color = side),
      fill = 'white',
      stroke = 2,
      shape = 22,
      size = pts(34),
      show.legend = FALSE
    ) +
    geom_point(
      data = nontarget_tracking %>% filter(side == 'O') %>% slice_max(frame_id),
      aes(color = side),
      fill = 'white',
      stroke = 2,
      shape = 21,
      size = pts(34),
      show.legend = FALSE
    ) +
    geom_text(
      data = nontarget_tracking %>% slice_max(frame_id),
      aes(label = jersey_number),
      color = 'black',
      hjust = 0.5,
      size = pts(16), 
      show.legend = FALSE,
      fontface = 'bold'
    )
  p
  
  if(has_target) {
    
    if(snapshot == 'snap') {
      target_tracking_between <-
        target_tracking %>% 
        slice_max(frame_id)
    } else {
      target_tracking_between <-
        target_tracking %>% 
        anti_join(
          target_tracking_clipped %>% 
            select(game_id, play_id, nfl_id, frame_id),
          by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
        )
    }
    
    p <-
      p +
      geom_path(
        data = target_tracking_clipped %>% select(-frame_id),
        aes(color = side),
        size = 2,
        alpha = 0.3,
        show.legend = FALSE
      ) +
      geom_path(
        data = target_tracking_between %>% select(-frame_id),
        aes(color = side),
        size = 2,
        alpha = 0.3,
        linetype = 2,
        show.legend = FALSE
      ) +
      geom_point(
        data = target_tracking %>% slice_max(frame_id),
        aes(color = side),
        fill = 'white',
        stroke = 2,
        shape = 21,
        size = pts(34),
        show.legend = FALSE
      ) +
      geom_text(
        data = target_tracking %>% slice_max(frame_id),
        aes(label = jersey_number),
        color = 'black',
        hjust = 0.5,
        size = pts(16), 
        show.legend = FALSE,
        fontface = 'bold'
      )
  }

  p <-
    p +
    # scale_fill_manual(values = c('O' = offense_color, 'D' = defense_color)) +
    scale_color_manual(values = c('O' = offense_color, 'D' = defense_color))
  
  title <- ifelse(is.null(title), glue::glue("<b><span style='color:{away_color};'>{away_team}</span></b> @ <b><span style='color:{home_color};'>{home_team}</span></b>, Week {week}"), title)
  caption <- ifelse(is.null(caption), glue::glue('d: distance between defender and receiver,
                                                 w: inverse target probability weight'), caption)
  p <-
    p +
    theme(
      plot.title = ggtext::element_markdown(),
      plot.title.position = 'plot',
      strip.background = element_rect(fill = NA),
      # strip.text = element_text()
      plot.subtitle = element_text(size = 18),
      plot.caption = ggtext::element_markdown(
        size = 12,
        hjust = 0,
        lineheight = 0
      ),
      plot.caption.position = 'plot'
    ) +
    labs(
      subtitle = subtitle,
      title = title,
      caption = caption,
      x = NULL, y = NULL
    )

  p <-
    p +
    ggforce::geom_mark_rect(
      data = probs_dists_annotate_filt,
      aes(x = x_d, y = y_d, group = nfl_id_d, label = glue::glue('d = {scales::number(dist_d, accuracy = 0.1)}, w = {scales::number(wt, accuracy = 0.01)}')),
      label.hjust = c(1, 1),
      label.fontsize = 16
    )

  if(!save) {
    return(p)
  }
  
  if(!dir.exists(dirname(path))) {
    dir.create(dirname(path))
  }
  
  save_plot(
    p,
    path = path,
    height = height,
    width = width
  )
  
}




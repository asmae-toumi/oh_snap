
library(tidyverse)

ggplot2::theme_set(hrbrthemes::theme_ipsum(base_family = '', base_size = 14))
ggplot2::theme_update(
  plot.title = ggplot2::element_text(size = 18),
  plot.title.position = 'plot',
  axis.text = ggplot2::element_text(size = 14),
  axis.title = ggplot2::element_text(size = 14),
  axis.title.x = ggplot2::element_text(size = 14),
  axis.title.y = ggplot2::element_text(size = 14),
  plot.caption.position = 'plot'
)

# 0. general functions ----
.path_x <- function(dir, file, ext = NULL) {
  if(!is.null(ext)) {
    ext <- sprintf('.%s', ext)
  } else {
    ext <- ''
  }
  file.path(dir, sprintf('%s%s', file, suffix, ext))
}
.path_data <- partial(.path_x, dir = 'data', ... = )
.path_data_csv <- partial(.path_data, ext = 'csv', ... = )
.path_data_parquet <- partial(.path_data, ext = 'parquet', ... = )
.path_figs <- partial(.path_x, dir = 'figs', ... = )
.path_figs_png <- partial(.path_figs, ext = 'png', ... = )
.path_figs_gif <- partial(.path_figs, ext = 'gif', ... = )

import_positions <- memoise::memoise({function() {
  path <- .path_data_csv('positions')
  positions <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('side', 'position_category', 'position_label', 'position'),
      col_types = vroom::cols(
        .default = vroom::col_character()
      )
    )
  positions
}})

.extract_n_position <- function(x, position) {
  rgx <- sprintf('(^.*)([0-9])(\\s%s.*$)', toupper(position))
  x %>%
    stringr::str_replace_all(rgx, '\\2') %>%
    as.integer()
}

.drop_bad_plays <- function(plays, positions = import_positions()) {
  players_from_tracking <- bdb2021::players_from_tracking
  targets <-
    plays %>% 
    dplyr::select(.data$game_id, .data$play_id, nfl_id = .data$target_nfl_id, .data$pass_result) %>% 
    dplyr::inner_join(
      players_from_tracking %>% 
        dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$position),
      by = c('game_id', 'play_id', 'nfl_id')
    )
  
  weird_targets <-
    list(
      targets %>% 
        dplyr::filter(pass_result != 'S') %>% 
        dplyr::filter(position == 'QB'),
      targets %>% 
        dplyr::left_join(positions, by = 'position') %>% 
        dplyr::filter(side != 'O')
    ) %>% 
    purrr::reduce(dplyr::bind_rows) %>% 
    dplyr::distinct(game_id, play_id)
  
  res <-
    plays %>%
    dplyr::anti_join(
      plays %>%
        dplyr::filter((.data$n_k > 0L | .data$n_p > 0L)) %>%
        dplyr::select(.data$game_id, .data$play_id),
      by = c('game_id', 'play_id')
    ) %>% 
    dplyr::anti_join(
      weird_targets,
      by = c('game_id', 'play_id')
    )
  res
}

import_plays <- memoise::memoise({function(drop_bad = TRUE) {
  path <- .path_data_csv('plays')
  plays <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('game_id', 'play_id', 'play_description', 'quarter', 'down', 'yards_to_go', 'possession_team', 'play_type', 'yardline_side', 'yardline_number', 'offense_formation', 'personnel_o', 'defenders_in_the_box', 'number_of_pass_rushers', 'personnel_d', 'type_dropback', 'pre_snap_visitor_score', 'pre_snap_home_score', 'game_clock', 'absolute_yardline_number', 'penalty_codes', 'penalty_jersey_numbers', 'pass_result', 'offense_play_result', 'play_result', 'epa', 'is_defensive_pi'),
      col_types = vroom::cols(
        .default = vroom::col_integer(),
        yards_to_go = vroom::col_integer(),
        play_description = vroom::col_character(),
        possession_team = vroom::col_character(),
        play_type = vroom::col_character(),
        yardline_side = vroom::col_character(),
        offense_formation = vroom::col_character(),
        personnel_o = vroom::col_character(),
        personnel_d = vroom::col_character(),
        type_dropback = vroom::col_character(),
        game_clock = vroom::col_time(format = ''),
        penalty_codes = vroom::col_character(),
        penalty_jersey_numbers = vroom::col_character(),
        pass_result = vroom::col_character(),
        epa = vroom::col_double(),
        is_defensive_pi = vroom::col_logical()
      )
    )
  path <- .path_data_csv('targetedReciever')
  target <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('game_id', 'play_id', 'target_nfl_id'),
      col_types = vroom::cols(
        .default = vroom::col_integer()
      )
    )
  plays <-
    plays %>%
    dplyr::inner_join(target, by = c('game_id', 'play_id'))
  plays
  
  suppressWarnings(
    plays <-
      plays %>%
      dplyr::mutate(
        dplyr::across(
          .data$personnel_o,
          list(
            n_p = ~.extract_n_position(.x, 'p'),
            n_k = ~.extract_n_position(.x, 'k')
            .names = '{fn}'
          )
        )
      )
  )
  
  if(!drop_bad) {
    return(plays)
  }
  plays %>% .drop_bad_plays()
  
}})


import_games <- memoise::memoise({function() {
  path <- .path_data_csv('games')
  games <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('game_id', 'game_date', 'game_time_eastern', 'home_team_abbr', 'visitor_team_abbr', 'week'),
      col_types = vroom::cols(
        game_id = vroom::col_integer(),
        game_date = vroom::col_date(format = '%m/%d/%Y'),
        game_time_eastern = vroom::col_character(),
        home_team_abbr = vroom::col_character(),
        visitor_team_abbr = vroom::col_character(),
        week = vroom::col_integer()
      )
    )
  games
}})

import_colors <- memoise::memoise({function() {
  path <- .path_data_csv('teamcolors')
  colors <-
    path %>%
    vroom::vroom(
      skip = 1L,
      progress = FALSE,
      col_names = c('team', 'color', sprintf('color%s', 2:4)),
      col_types = vroom::cols(
        .default = vroom::col_character()
      )
    )
  colors
}})

import_tracking <- function(week = 1, positions = import_positions(), standardize = TRUE) {
  path <- .path_data_csv(sprintf('week%d', week))
  tracking <-
    path %>%
    vroom::vroom(
      # skip = 1L,
      progress = FALSE,
      col_names = c('time', 'x', 'y', 's', 'a', 'dis', 'o', 'dir', 'event', 'nfl_id', 'display_name', 'jersey_number', 'position', 'frame_id', 'team', 'game_id', 'play_id', 'play_direction', 'route'),
      col_types = vroom::cols(
        time = vroom::col_datetime(format = ''),
        x = vroom::col_double(),
        y = vroom::col_double(),
        s = vroom::col_double(),
        a = vroom::col_double(),
        dis = vroom::col_double(),
        o = vroom::col_double(),
        dir = vroom::col_double(),
        event = vroom::col_character(),
        nfl_id = vroom::col_integer(),
        display_name = vroom::col_character(),
        jersey_number = vroom::col_integer(),
        position = vroom::col_character(),
        frame_id = vroom::col_integer(),
        team = vroom::col_character(),
        game_id = vroom::col_integer(),
        play_id = vroom::col_integer(),
        play_direction = vroom::col_character(),
        route = vroom::col_character()
      )
    )
  tracking <- tracking[-1, ]
  tracking <- tracking[, -1] # Never need the time column
  # tracking <- tracking %>% dplyr::select(-.data$time)
  
  tracking <-
    tracking %>%
    dplyr::left_join(
      positions %>%
        dplyr::select(.data$position, .data$side),
      by = 'position'
    )
  
  ball <- tracking %>% dplyr::filter(display_name == 'Football')
  tracking <- tracking %>% dplyr::filter(display_name != 'Football')
  
  tracking <-
    tracking %>%
    dplyr::inner_join(
      ball %>%
        dplyr::select(.data$game_id, .data$play_id, .data$frame_id, ball_x = .data$x, ball_y = .data$y),
      by = c('frame_id', 'game_id', 'play_id')
    )
  
  line_of_scrimmage <-
    tracking %>%
    dplyr::filter(.data$event == 'ball_snap') %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::filter(dplyr::row_number() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$game_id, .data$play_id, los = .data$ball_x) %>%
    dplyr::ungroup()
  
  tracking <-
    tracking %>%
    dplyr::left_join(line_of_scrimmage, by = c('game_id', 'play_id'))
  
  if(!standardize) {
    return(tracking)
  }
  
  x_max <- 120
  y_max <- 160 / 3
  tracking <-
    tracking %>%
    dplyr::mutate(
      dplyr::across(c(.data$x, .data$ball_x, .data$los), ~ dplyr::if_else(.data$play_direction == 'left', !!x_max - .x, .x)),
      dplyr::across(c(.data$y, .data$ball_y), ~ dplyr::if_else(.data$play_direction == 'left', !!y_max - .x, .x))
    )
  tracking
}

.display_info <- function(x, ..., .envir = parent.frame()) {
  
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(x)
}


import_targets <- memoise::memoise({function(plays = import_plays()) {
  plays %>% 
    count(nfl_id = target_nfl_id, name = 'n_target', sort = TRUE) %>% 
    drop_na() %>%
    mutate(rnk_target = row_number(desc(n_target)))
}})

.switch_events_end <- function(at = c('throw', 'end_routes', 'end_rush')) {
  at <- match.arg(at)
  switch(
    at,
    throw = .get_events_throw(),
    end_routes = .get_events_end_routes(),
    end_rush = .get_events_end_rush()
  )
}

clip_tracking_at_events <-
  function(tracking,
           at = 'throw',
           init_cnd = dplyr::quos(.data$event == 'ball_snap')) {
    events <- .switch_events_end(at)
    assertthat::assert_that(rlang::is_quosures(init_cnd))
    tracking %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$nfl_id) %>%
      dplyr::mutate(
        group =
          dplyr::case_when(
            !!!init_cnd ~ 1L,
            dplyr::lag(.data$event) %in% !!events ~ 1L,
            TRUE ~ 0L
          ),
        group = cumsum(.data$group)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$group == 1L) %>%
      dplyr::select(-.data$group)
  }


do_by_week <- 
  function(weeks = 1L,
           f,
           overwrite = FALSE,
           path = .path_data_parquet(''),
           ...) {
    if(file.exists(path) & !overwrite) {
      .display_info('Importing data from `path = "{path}"` and not re-generating.')
      res <- path %>% arrow::read_parquet()
      return(res)
    }
    res <-
      tibble::tibble(week = weeks) %>%
      dplyr::mutate(data = purrr::map(.data$week, f, overwrite = overwrite, ...)) %>%
      tidyr::unnest(.data$data)
    res
  }

save_plot <-
  function(gg,
           file = deparse(substitute(gg)),
           ext = 'png',
           dir = 'figs',
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           height = 8,
           width = height,
           ...) {
    ggplot2::ggsave(plot = gg, filename = path, width = width, height = height, type = 'cairo', ...)
  }

save_animation <-
  function(anim,
           height = 600,
           width = width,
           fps = 10,
           end_pause = fps,
           file = deparse(substitute(anim)),
           ext = 'gif',
           dir = get_ohsnap_dir_figs(),
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           renderer = gganimate::gifski_renderer(path),
           ...) {
    # browser()
    res <-
      gganimate::animate(
        anim,
        fps = fps,
        height = height,
        width = width,
        renderer = renderer,
        end_pause = end_pause,
        ...
      )
  }

animate_play <-
  function(game_id = 2018090600,
           play_id = 75,
           plays = import_plays(),
           games = import_games(),
           positions = import_positions(),
           week = NULL,
           tracking = NULL,
           team_colors = FALSE,
           yardmin = NULL,
           yardmax = NULL,
           field_color = '#b3e3d6',
           line_color = 'black',
           sideline_color = 'white',
           endzone_color = NULL,
           buffer = NULL,
           nearest_defender = FALSE,
           target_prob = TRUE,
           cnd = 'all',
           which = NA_character_,
           target_probs = import_target_probs(cnd = cnd, which = which),
           # target_probs_oob = import_target_probs(cnd = 'all', which = 'oob'),
           clip = TRUE,
           at = 'end_routes', # .get_valid_at_events(),
           init_cnd = dplyr::quos(.data$frame_id == 1L),
           save = TRUE,
           dir = get_ohsnap_dir_figs(),
           ext = 'gif',
           filename = sprintf('%s-%s.%s', game_id, play_id, ext),
           path = file.path(dir, filename),
           dir_tp = dir,
           ext_tp = 'gif',
           filename_tp = sprintf('%s-%s-tp.%s', game_id, play_id, ext_tp),
           path_tp = file.path(dir_tp, filename_tp),
           subtitle = NULL,
           # width = 10,
           # height = 10,
           end_pause = 1,
           height = 600,
           width = 600,
           fps = 10,
           height_tp = height,
           width_tp = width,
           ...) {
    
    # game_id = 2018090905; play_id = 783
    .display_info('Animating `game_id = {game_id}`, `play_id = {play_id}`.')
    meta <- tibble::tibble(game_id = game_id, play_id = play_id)
    
    has_week <- !is.null(week)
    if(!has_week) {
      game <- games %>% dplyr::filter(game_id == !!game_id)
      assertthat::assert_that(nrow(game) == 1L)
      week <- game$week
    }
    
    if(is.null(tracking)) {
      tracking <- import_tracking(week = week, positions = positions, standardize = FALSE)
    }
    
    tracking <-
      tracking %>%
      dplyr::inner_join(meta, by = c('game_id', 'play_id'))
    
    if(nrow(tracking) == 0L) {
      .display_error('Could not identify tracking data for `game_id = {game_id}`, `play_id = {play_id}`.')
    }
    
    play <- plays %>% dplyr::inner_join(meta, by = c('game_id', 'play_id'))
    assertthat::assert_that(nrow(play) == 1L)
    
    game <- games %>% dplyr::filter(game_id == !!game_id)
    assertthat::assert_that(nrow(game) == 1L)
    
    target_id <- play$target_nfl_id
    has_target <- !is.na(target_id)
    
    if(!has_target) {
      .display_warning('No target receiver.')
      # return(ggplot())
      target <- tibble::tibble(display_name = '?', jersey_number = -1)
      has_target <- FALSE
    } else {
      target <-
        tracking %>%
        dplyr::filter(nfl_id == !!target_id) %>%
        dplyr::distinct(display_name, jersey_number)
      assertthat::assert_that(nrow(target) == 1L)
    }
    
    if(nearest_defender | target_prob) {
      personnel_and_rushers <- bdb2021::personnel_and_rushers
      
      personnel_and_rushers_min <-
        personnel_and_rushers %>%
        dplyr::inner_join(meta, by = c('game_id', 'play_id')) %>% 
        dplyr::filter(.data$n_rusher > 0L) %>%
        dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
      
      if(nrow(personnel_and_rushers_min) > 0L) {
        personnel_and_rushers_min <-
          personnel_and_rushers_min %>% 
          dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball)))
      }
    }
    
    tracking_clipped <-
      tracking %>%
      clip_tracking_at_events(at = 'throw', init_cnd = dplyr::quos(.data$frame_id == 1L)) %>%
      dplyr::arrange(game_id, play_id, nfl_id, frame_id)

    
    if(target_prob) {
      probs <-
        target_probs %>% 
        dplyr::semi_join(meta, by = c('game_id', 'play_id'))
      if(nrow(probs) >= 0) {
        probs <-
          probs %>% 
          distinct(game_id, play_id, frame_id, nfl_id, prob = .prob_1_norm)
      } else {

        target_prob <- FALSE
      }
    }
    
    # This is also being clipped, but later than how min distance and target prob is clipped.
    if(clip) {
      tracking <-
        tracking %>%
        clip_tracking_at_events(
          # init_cnd = dplyr::quos(.data$frame_id == 1L),
          init_cnd = init_cnd,
          at = at
        )
    }
    
    # Initialization, in case target probability is added. (This is used for label size.)
    # tracking <- tracking %>% mutate(prob = 1)
    
    line_of_scrimmage <- play$absolute_yardline_number
    play_direction <- tracking$play_direction[[1]]
    sign <- ifelse(play_direction == 'left', -1, 1)
    first_down_line <- line_of_scrimmage + sign * play$yards_to_go
    
    target_tracking <-
      tracking %>%
      dplyr::filter(.data$nfl_id == !!target_id)
    
    nontarget_tracking <-
      tracking %>%
      dplyr::filter(.data$nfl_id != !!target_id)
    
    target_tracking_clipped <-
      tracking_clipped %>%
      dplyr::filter(.data$nfl_id == !!target_id)
    
    nontarget_tracking_clipped <-
      tracking_clipped %>%
      dplyr::filter(.data$nfl_id != !!target_id)
    
    ball <-
      tracking %>%
      dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id, x = .data$ball_x, y = .data$ball_y) %>%
      dplyr::mutate(nfl_id = NA_integer_)
    
    if(is.null(yardmin) | is.null(yardmax)) {
      yardminmax <-
        tracking %>%
        dplyr::summarize(dplyr::across(.data$x, list(min = min, max = max)))
      
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
        dplyr::summarize(dplyr::across(.data$y, list(min = min, max = max)))
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
    
    home_team <- game$home_team_abbr
    away_team <- game$visitor_team_abbr
    
    if(team_colors) {
      
      home_color <- colors %>% dplyr::filter(.data$team == !!home_team) %>% dplyr::pull(.data$color)
      away_color <- colors %>% dplyr::filter(.data$team == !!away_team) %>% dplyr::pull(.data$color2)
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
      dplyr::anti_join(
        nontarget_tracking_clipped %>% 
          dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$frame_id),
        by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
      )
    
    p <-
      ggplot2::ggplot() +
      gg_field(
        yardmin = yardmin,
        yardmax = yardmax,
        buffer = buffer,
        field_color = field_color,
        line_color = line_color,
        sideline_color = sideline_color # ,
        # ...
      ) +
      ggplot2::aes(x = .data$x, y = .data$y) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = !!line_of_scrimmage),
        inherit.aes = FALSE,
        ggplot2::aes(x = .data$x, y = 0, xend = .data$x, yend = !!max_y),
        size = 1.25
      ) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = !!first_down_line),
        inherit.aes = FALSE,
        ggplot2::aes(x = .data$x, y = 0, xend = .data$x, yend = !!max_y),
        color = '#ffff7f',
        size = 2
      ) +
      ggplot2::geom_point(
        data = ball,
        inherit.aes = TRUE,
        size = 3,
        color = 'brown'
      ) +
      ggplot2::geom_path(
        data = nontarget_tracking_clipped %>% dplyr::select(-.data$frame_id),
        ggplot2::aes(color = .data$side, group = .data$nfl_id),
        size = 1,
        alpha = 0.3,
        show.legend = FALSE
      ) +
      ggplot2::geom_path(
        data = nontarget_tracking_between %>% dplyr::select(-.data$frame_id),
        ggplot2::aes(color = .data$side, group = .data$nfl_id),
        size = 1,
        alpha = 0.3,
        linetype = 2,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        data = nontarget_tracking,
        ggplot2::aes(label = .data$jersey_number, color = .data$side),
        # size = pts(14), 
        show.legend = FALSE,
        fontface = 'bold'
      )
    
    if(has_target) {
      
      target_tracking_between <-
        target_tracking %>% 
        dplyr::anti_join(
          target_tracking_clipped %>% 
            dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$frame_id),
          by = c('nfl_id', 'frame_id', 'game_id', 'play_id')
        )
      
      p <-
        p +
        ggplot2::geom_path(
          data = target_tracking_clipped %>% dplyr::select(-.data$frame_id),
          ggplot2::aes(color = .data$side),
          size = 2,
          alpha = 0.3,
          show.legend = FALSE
        ) +
        ggplot2::geom_path(
          data = target_tracking_between %>% dplyr::select(-.data$frame_id),
          ggplot2::aes(color = .data$side),
          size = 2,
          alpha = 0.3,
          linetype = 2,
          show.legend = FALSE
        ) +
        ggplot2::geom_text(
          data = target_tracking,
          ggplot2::aes(label = .data$jersey_number, color = .data$side),
          # size = pts(14),
          show.legend = FALSE,
          fontface = 'bold'
        )
    }

    p <-
      p +
      # ggplot2::scale_size_manual(limits = c(pts(14), pts(20))) +
      ggplot2::scale_color_manual(values = c('O' = offense_color, 'D' = defense_color))
    
    # Theme stuff
    p <-
      p +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(),
        plot.title.position = 'plot',
        strip.background = ggplot2::element_rect(fill = NA),
        # strip.text = ggplot2::element_text()
        plot.caption = ggtext::element_markdown(
          # size = 12,
          hjust = 0,
          lineheight = 0
        ),
        plot.caption.position = 'plot'
      ) +
      ggplot2::labs(
        subtitle = subtitle,
        title = glue::glue("<b><span style='color:{away_color};'>{away_team}</span></b> @ <b><span style='color:{home_color};'>{home_team}</span></b>, Week {week}"),
        caption = glue::glue('Q{play$quarter}: {play$play_description}'),
        x = NULL, y = NULL
      )
    
    anim <- p + gganimate::transition_manual(.data$frame_id)
    
    if(target_prob) {
      
      frame_ids <- tracking_clipped %>% distinct(frame_id) %>% pull(frame_id)
      display_names <- tracking %>% distinct(nfl_id, display_name, jersey_number)
      frame_snap <-
        tracking %>% 
        filter(event == 'ball_snap') %>% 
        distinct(frame_id) %>% 
        pull(frame_id)
      
      grid <-
        crossing(
          frame_id = frame_ids,
          nfl_id = display_names$nfl_id
        )
      grid
      
      probs_aug <-
        grid %>% 
        left_join(probs, by = c('frame_id', 'nfl_id')) %>% 
        arrange(nfl_id, frame_id) %>% 
        group_by(nfl_id) %>% 
        fill(prob, .direction = 'up') %>% 
        ungroup() %>% 
        filter(!is.na(prob)) %>% 
        left_join(tracking %>% select(frame_id, nfl_id, display_name, jersey_number), by = c('frame_id', 'nfl_id')) %>% 
        mutate(
          lab = sprintf('%s (%s)', display_name, jersey_number) %>% factor()
        )
      probs_aug
      
      p_tp <-
        probs_aug %>% 
        ggplot() +
        aes(x = frame_id, y = prob, color = lab) +
        geom_vline(
          # inherit.aes = FALSE,
          data = tibble(x = !!frame_snap),
          aes(xintercept = .data$x)
        ) +
        geom_line(aes(group = lab), size = 1) +
        # guides(color = FALSE) +
        guides(color = guide_legend(title = '', nrow = 2, override.aes = list(size = 3))) +
        scale_y_continuous(labels = scales::percent) +
        # theme_minimal() +
        theme(
          plot.title.position = 'plot',
          plot.caption.position = 'plot',
          plot.caption = ggtext::element_markdown(
            # size = 12,
            hjust = 0,
            lineheight = 0
          ),
          legend.position = 'top'
        ) +
        labs(
          title = 'Target probability',
          # subtitle = 'Fully fit',
          # caption = glue::glue('game_id = {game_id}, play_id = {play_id}'),
          x = NULL, y = NULL
        )
    }
    
    seconds <- ball %>% nrow() %>% {. / 10}
    nframe <- (seconds + end_pause) * fps
    
    if(!save) {
      return(anim)
    }
    
    if(!dir.exists(dirname(path))) {
      dir.create(dirname(path))
    }
    
    res <- 
      save_animation(
        anim,
        nframe = nframe,
        fps = fps,
        height = height,
        width = width,
        path = path,
        renderer = gganimate::gifski_renderer(file = path),
        end_pause = end_pause * fps
      )
    
    if(target_prob) {
      anim_tp <- p_tp + gganimate::transition_reveal(.data$frame_id)
      res_tp <- 
        save_animation(
          anim_tp,
          nframe = nframe,
          fps = fps,
          height = height_tp,
          width = width_tp,
          path = path_tp,
          renderer = gganimate::gifski_renderer(file = path_tp),
          end_pause = end_pause * fps
        )
    }
    
    res
  }

# 1. functions to generate data for features ----
.path_data_parquet_week <- function(prefix, week) {
  .path_data_parquet(sprintf('%s_week%02d', prefix, week))
}

do_identify_personnel_and_rushers <-
  function(week = 1L,
           at = 'end_rush',
           overwrite = FALSE,
           path = .path_data_parquet_week('personnel_and_rushers', week),
           ...) {
    
    if(file.exists(path) & !overwrite) {
      .display_info('Importing data from `path = "{path}"` and not re-generating.')
      res <- path %>% arrow::read_parquet()
      return(res)
    }
    
    .display_info('Identifying personnel and rushers for week {week} at {Sys.time()}.')
    
    tracking <- week %>% import_tracking()
    
    tracking_clipped <- tracking %>% clip_tracking_at_events(at = at)
    
    end_frames <-
      tracking_clipped %>%
      dplyr::group_by(.data$game_id, .data$play_id) %>%
      dplyr::filter(.data$frame_id == max(.data$frame_id)) %>%
      dplyr::ungroup()
    end_frames
    
    def_end_frames <-
      end_frames %>%
      dplyr::filter(.data$side == 'D')
    
    ball_end_frames <-
      end_frames %>%
      dplyr::distinct(game_id, play_id, frame_id, ball_x, ball_y)
    
    potential_rushers <-
      def_end_frames %>%
      dplyr::filter(.data$x < .data$los) %>%
      dplyr::select(game_id, play_id, frame_id, nfl_id, x, y)
    potential_rushers
    
    potential_rushers_ranked <-
      potential_rushers %>%
      dplyr::select(game_id, play_id, nfl_id, x, y) %>%
      dplyr::inner_join(
        ball_end_frames,
        by = c('game_id', 'play_id')
      ) %>%
      dplyr::mutate(
        dist_to_ball_abs = .dist(x, ball_x, y, ball_y) %>% abs()
      ) %>%
      dplyr::arrange(game_id, play_id, dist_to_ball_abs) %>%
      dplyr::group_by(game_id, play_id) %>%
      dplyr::mutate(
        idx_closest_to_ball = dplyr::row_number(dist_to_ball_abs)
      )
    
    def_agg <-
      def_end_frames %>%
      dplyr::select(.data$game_id, .data$play_id) %>%
      dplyr::group_by(.data$game_id, .data$play_id) %>%
      dplyr::summarize(n_d = dplyr::n()) %>%
      dplyr::ungroup()
    
    end_frames %>%
      dplyr::filter(.data$side == 'O' & .data$position != 'QB') %>%
      dplyr::count(position)
    
    pos_n <-
      end_frames %>%
      dplyr::left_join(
        positions %>%
          dplyr::select(.data$side, .data$position, .data$position_category) %>%
          dplyr::mutate(dplyr::across(.data$position_category, tolower))
      ) %>%
      dplyr::count(.data$game_id, .data$play_id, .data$position_category) %>%
      tidyr::pivot_wider(
        names_from = .data$position_category,
        values_from = .data$n,
        names_prefix = 'n_'
      )
    
    off_agg <-
      end_frames %>%
      dplyr::filter(.data$side == 'O' & .data$position != 'QB') %>%
      dplyr::group_by(.data$game_id, .data$play_id) %>%
      dplyr::summarize(
        n_route = sum(!is.na(.data$route))
      ) %>%
      dplyr::ungroup()
    
    personnel_and_rushers <-
      pos_n %>%
      dplyr::left_join(
        off_agg,
        by = c('game_id', 'play_id')
      ) %>%
      dplyr::left_join(
        def_agg,
        by = c('game_id', 'play_id')
      ) %>%
      dplyr::left_join(
        potential_rushers_ranked %>%
          tidyr::nest(
            rushers = -c(.data$game_id, .data$play_id)
          ),
        by = c('game_id', 'play_id')
      ) %>%
      dplyr::mutate(
        # has_rusher = map_lgl(rushers, ~!is.null(.x))
        rushers = purrr::map_if(rushers, is.null, ~tibble::tibble()),
        n_rusher = purrr::map_int(rushers, ~nrow(.x))
      ) %>%
      dplyr::relocate(rushers, .after = dplyr::last_col())
    arrow::write_parquet(personnel_and_rushers, path)
    personnel_and_rushers
  }

do_import_routes <- 
  function(week = 1L, 
           overwrite = FALSE,
           path = .path_data_parquet_week('routes', week),
           ...) {
    
    if(file.exists(path) & !overwrite) {
      .display_info('Importing data from `path = "{path}"` and not re-generating.')
      res <- path %>% arrow::read_parquet()
      return(res)
    }
    
    .display_info('Identifying routes for {week} at {Sys.time()}.')
    tracking <- week %>% import_tracking(standardize = FALSE)
    routes <-
      tracking %>%
      dplyr::filter(!is.na(.data$route)) %>%
      dplyr::distinct(
        .data$game_id, .data$play_id, .data$nfl_id, .data$route
      )
    arrow::write_parquet(routes, path)
    routes
  }

# 2. functions to generate features ----
.select_side <- function(data, side = c('O', 'D'), ...) {
  side <- match.arg(side)
  data %>%
    dplyr::filter(side == !!side) %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$frame_id,
      .data$event,
      .data$nfl_id,
      .data$x,
      .data$y,
      .data$s,
      .data$a,
      .data$dis,
      .data$o,
      .data$dir,
      ...
    )
}

.dist <- function(x1, x2, y1, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

do_generate_features_at_events <- 
  function(week = 1L,
           path = .path_data_parquet_week('target_prob_features', week),
           overwrite = FALSE, 
           targets,
           personnel_and_rushers = arrow::read_parquet(.path_data_parquet('personnel_and_rushers')),
           ...) {
    
    if(file.exists(path) & !overwrite) {
      .display_info('Importing data from `path = "{path}"` and not re-generating.')
      res <- path %>% arrow::read_parquet()
      return(res)
    }
    
    .display_info('Generating target probability features for week {week} at {Sys.time()}.')
    tracking <- week %>% import_tracking(standardize = FALSE)
    
    tracking <-
      tracking %>%
      dplyr::semi_join(
        plays %>%
          dplyr::select(.data$game_id, .data$play_id),
        by = c('game_id', 'play_id')
      )
    
    qb <- tracking %>% dplyr::filter(position == 'QB')
    tracking <- tracking %>% dplyr::filter(position != 'QB')
    
    tracking <-
      tracking %>%
      dplyr::inner_join(
        qb %>%
          dplyr::select(
            .data$game_id,
            .data$play_id,
            .data$frame_id,
            qb_x = .data$x,
            qb_y = .data$y,
            qb_s = .data$s,
            qb_a = .data$a,
            qb_dis = .data$dis,
            qb_o = .data$o,
            qb_dir = .data$dir
          ),
        by = c('frame_id', 'game_id', 'play_id')
      ) %>%
      dplyr::left_join(
        plays %>%
          dplyr::select(.data$game_id, .data$play_id, .data$yards_to_go),
        by = c('game_id', 'play_id')
      ) %>%
      dplyr::mutate(fd = .data$los + dplyr::if_else(.data$play_direction == 'left', -1, 1) * .data$yards_to_go)
    tracking
    
    x_max <- 120
    y_max <- 160 / 3
    tracking <-
      tracking %>%
      dplyr::mutate(
        dplyr::across(c(.data$x, .data$ball_x, .data$qb_x, .data$los), ~ dplyr::if_else(.data$play_direction == 'left', !!x_max - .x, .x)),
        dplyr::across(c(.data$x, .data$ball_x, .data$qb_x), ~.x - los),
        dplyr::across(c(.data$y, .data$ball_y, .data$qb_y), ~ dplyr::if_else(.data$play_direction == 'left', !!y_max - .x, .x))
      )
    
    frames <-
      tracking %>%
      clip_tracking_at_events(at = 'throw')
    
    snap_frames <- tracking %>% dplyr::filter(.data$frame_id == min(.data$frame_id))
    
    snap_frame_ids <- snap_frames %>% dplyr::distinct(.data$game_id, .data$play_id, .data$frame_id)
    
    personnel_and_rushers_week <-
      personnel_and_rushers %>%
      dplyr::filter(.data$week == !!week) %>%
      dplyr::filter(.data$n_rusher > 0L) %>%
      dplyr::mutate(rushers = purrr::map(.data$rushers, ~dplyr::select(.x, .data$nfl_id, .data$idx_closest_to_ball))) %>%
      dplyr::select(.data$game_id, .data$play_id, .data$n_rusher, .data$rushers)
    personnel_and_rushers_week
    
    # One strange source of bad data is play_id-frame_id combos where there is just 1 player.
    frames_n <-
      frames %>%
      dplyr::count(.data$game_id, .data$play_id, .data$frame_id, .data$event)
    
    frames_first_o <-
      frames %>%
      dplyr::filter(.data$side == 'O') %>%
      dplyr::group_by(.data$game_id, .data$play_id) %>%
      dplyr::filter(.data$frame_id == min(.data$frame_id)) %>%
      dplyr::ungroup()
    
    frames_n_o <-
      frames_first_o %>%
      dplyr::count(.data$game_id, .data$play_id)
    
    frames_first_idx_o <-
      frames_first_o %>%
      dplyr::left_join(targets, by = 'nfl_id') %>% 
      dplyr::group_by(.data$game_id, .data$play_id) %>%
      dplyr::mutate(
        dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
        idx_o = dplyr::row_number(.data$rnk_target)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$game_id, .data$play_id, .data$nfl_id, .data$idx_o) %>% 
      dplyr::arrange(.data$game_id, .data$play_id, .data$idx_o)
    frames_first_idx_o
    
    frames_target <-
      frames_first_idx_o %>%
      dplyr::left_join(
        plays %>%
          dplyr::select(
            .data$game_id,
            .data$play_id,
            .data$target_nfl_id
          ),
        by = c('game_id', 'play_id')
      ) %>%
      dplyr::mutate(
        is_target = dplyr::if_else(.data$nfl_id == .data$target_nfl_id, 1L, 0L)
      )
    
    frames_o <-
      frames %>%
      .select_side(side = 'O') %>%
      dplyr::distinct()
    
    frames_o_renamed <-
      frames_o %>%
      dplyr::rename_with(~sprintf('%s_%s', .x, 'o'), -c(.data$game_id, .data$play_id, .data$frame_id, .data$event))
    
    min_dists_naive_o <-
      frames_o %>%
      dplyr::left_join(
        frames_o_renamed,
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_o = .dist(.data$x, .data$x_o, .data$y, .data$y_o)) %>%
      dplyr::filter(.data$dist_o > 0) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, .data$nfl_id) %>%
      dplyr::filter(.data$dist_o == min(.data$dist_o)) %>%
      dplyr::ungroup() %>%
      dplyr::select(game_id, play_id, frame_id, event, nfl_id, dplyr::matches('_o'))
    
    frames_qb <-
      frames %>%
      dplyr::distinct(
        .data$game_id,
        .data$play_id,
        .data$frame_id,
        .data$event,
        .data$qb_x,
        .data$qb_y,
        .data$qb_s,
        .data$qb_a,
        .data$qb_dis,
        .data$qb_o,
        .data$qb_dir
      )
    
    frames_d <-
      frames %>%
      .select_side(side = 'D') %>%
      dplyr::distinct()
    
    frames_d_renamed <-
      frames_d %>%
      dplyr::rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id, event))
    
    min_dists_naive_qb <-
      frames_qb %>%
      dplyr::left_join(
        frames_d %>%
          rename_with(~sprintf('%s_%s', .x, 'rusher'), -c(game_id, play_id, frame_id, event)),
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_rusher = .dist(.data$qb_x, .data$x_rusher, .data$qb_y, .data$y_rusher)) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event) %>%
      dplyr::filter(.data$dist_rusher == min(.data$dist_rusher)) %>%
      dplyr::ungroup()
    
    min_dists_naive_d_init <-
      frames_o %>%
      dplyr::left_join(
        frames_d %>%
          rename_with(~sprintf('%s_%s', .x, 'd'), -c(game_id, play_id, frame_id, event)),
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      dplyr::mutate(dist_d = .dist(.data$x, .data$x_d, .data$y, .data$y_d)) %>%
      dplyr::group_by(.data$game_id, .data$play_id, .data$frame_id, .data$event, nfl_id) %>%
      mutate(idx_closest = row_number(dist_d)) %>%
      ungroup() %>%
      select(game_id, play_id, frame_id, event, nfl_id, matches('_d'), idx_closest)
    
    features <-
      frames_o %>%
      dplyr::full_join(
        frames_target %>% dplyr::rename(nfl_id_target = target_nfl_id),
        by = c('game_id', 'play_id', 'nfl_id')
      ) %>%
      # distinct() %>%
      # Add closest other defender.
      dplyr::full_join(
        min_dists_naive_d_init %>%
          dplyr::filter(idx_closest == 1L) %>%
          dplyr::rename_with(~sprintf('%s1_naive', .x), dplyr::matches('_d$')) %>%
          dplyr::select(-idx_closest),
        by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
      ) %>%
      # count(game_id, play_id, frame_id, event, nfl_id, sort = T)
      dplyr::full_join(
        min_dists_naive_d_init %>%
          dplyr::filter(idx_closest == 2L) %>%
          dplyr::rename_with(~sprintf('%s2_naive', .x), dplyr::matches('_d$')) %>%
          dplyr::select(-idx_closest),
        by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
      ) %>%
      # Add closest offensive players.
      dplyr::left_join(
        min_dists_naive_o,
        by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
      ) %>%
      # Add closest defender to qb.
      dplyr::left_join(
        min_dists_naive_qb,
        by = c('game_id', 'play_id', 'frame_id', 'event')
      ) %>%
      # Add "static" info for each frame.
      dplyr::left_join(
        frames %>%
          dplyr::select(
            .data$game_id,
            .data$play_id,
            .data$frame_id,
            .data$event,
            .data$nfl_id,
            .data$ball_x,
            .data$ball_y,
            # .data$fd,
            .data$yards_to_go,
            .data$los
          ),
        by = c('game_id', 'play_id', 'frame_id', 'event', 'nfl_id')
      ) %>%
      dplyr::left_join(targets, by = 'nfl_id') %>% 
      dplyr::mutate(
        dist_ball = .dist(.data$x, .data$ball_x, .data$y, .data$ball_y),
        dist_ball_o = .dist(.data$x_o, .data$ball_x, .data$y_o, .data$ball_y),
        dist_ball_d1_naive = .dist(.data$x_d1_naive, .data$ball_x, .data$y_d1_naive, .data$ball_y),
        dist_ball_d2_naive = .dist(.data$x_d2_naive, .data$ball_x, .data$y_d2_naive, .data$ball_y),
        dist_qb = .dist(.data$x, .data$qb_x, .data$y, .data$qb_y),
        dist_qb_o = .dist(.data$x_o, .data$qb_x, .data$y_o, .data$qb_y),
        dist_ball_d1_naive = .dist(.data$x_d1_naive, .data$qb_x, .data$y_d1_naive, .data$qb_y),
        dist_ball_d2_naive = .dist(.data$x_d2_naive, .data$qb_x, .data$y_d2_naive, .data$qb_y),
        dist_los = .data$x -  .data$los
      )
    arrow::write_parquet(features, path)
    features
  }

# TODO: find the code to generate this 
import_min_dists_naive_target <- function() {
  .path_data_parquet('min_dists_naive_target') %>%
    arrow::read_parquet() 
}

# 3. functions for target prob ----
.augment_target_probs <- function(v, features_df, features, col_y, export = TRUE, path) {
  # browser()
  col_y_sym <- col_y %>% dplyr::sym()
  probs_init <-
    v %>% 
    dplyr::tibble(.prob_1 = .) %>% 
    dplyr::mutate(.prob_0 = 1 - .prob_1) %>% 
    dplyr::bind_cols(
      features_df %>% 
        dplyr::select(
          idx,
          game_id, 
          play_id, 
          nfl_id, 
          frame_id, 
          !!col_y
        )
    ) %>% 
    dplyr::inner_join(
      features %>%
        dplyr::select(
          # idx,
          game_id,
          play_id,
          nfl_id,
          frame_id
        )
    )
  
  probs_init_bad <-
    probs_init %>% 
    dplyr::count(idx) %>% 
    dplyr::filter(n > 1L)
  
  probs_init <-
    probs_init %>% 
    dplyr::group_by(game_id, play_id, frame_id) %>% 
    dplyr::mutate(dplyr::across(.prob_1, list(norm = ~ .x / sum(.x)))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.prob_0_norm = 1 - .prob_1_norm) %>%
    dplyr::select(
      idx,
      game_id,
      play_id,
      frame_id,
      dplyr::matches('nfl_id'),
      !!col_y_sym,
      dplyr::matches('prob_0'),
      dplyr::matches('prob_1')
    )
  probs_init
  
  probs_init <- probs_init %>% dplyr::anti_join(probs_init_bad)
  
  probs_max <-
    probs_init %>% 
    dplyr::group_by(game_id, play_id) %>% 
    # filter(.prob_1_norm == max(.prob_1_norm)) %>% 
    dplyr::slice_max(.prob_1_norm, with_ties = FALSE) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(.prob_class = 1L)
  probs_max
  
  probs <-
    probs_init %>%
    dplyr::left_join(
      probs_max %>% 
        dplyr::select(game_id, play_id, nfl_id, .prob_class)
    ) %>% 
    dplyr::mutate(
      dplyr::across(.prob_class, ~dplyr::coalesce(.x, 0L) %>% factor()),
      dplyr::across(!!col_y_sym, factor)
    )
  if(!export) {
    return(probs)
  }
  probs %>% arrow::write_parquet(path)
  probs
}

#' @seealso \url{https://cran.r-project.org/web/packages/yardstick/vignettes/custom-metrics.html}
mse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  
  mse_impl <- function(truth, estimate) {
    mean((truth - estimate) ^ 2)
  }
  
  yardstick::metric_vec_template(
    metric_impl = mse_impl,
    truth = truth, 
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
  
}

mse <- function(data, ...) {
  UseMethod("mse")
}

mse <- yardstick::new_numeric_metric(mse, direction = "minimize")

mse.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  
  yardstick::metric_summarizer(
    metric_nm = "mse",
    metric_fn = mse_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate), 
    na_rm = na_rm,
    ...
  )
  
}

import_target_probs <- function(cnd = 'all', which = c(NA_character_, 'oob') {
  which <- match.arg(which)
  .path_data_parquet(sprintf('probs_%starget_prob_%s', ifelse(is.na(which), '', sprintf('%s_', which)), cnd)) %>%
    arrow::read_parquet() 
}

do_fit_target_prob_model <- function(cnd = 'all', plays = import_plays()) {

  .suffix <- if(cnd %in% c('start', 'end')) {
    'minmax'
  } else {
    'all'
  }
  
  features <- 
    import_target_prob_features(suffix = .suffix) %>%
    dplyr::semi_join(plays %>% dplyr::select(game_id, play_id), by = c('game_id', 'play_id')) %>% 
    dplyr::mutate(idx = dplyr::row_number()) %>% 
    dplyr::relocate(idx)
  
  features_start <-
    features %>% 
    dplyr::filter(event == 'ball_snap')
  
  if (cnd == 'start') {
    extra_features <- NULL
    features_x <- features_start
    subtitle <- 'At the snap'
  } else if(cnd == 'end') {
    extra_features <- 'sec'
    features_x <- 
      features %>% 
      dplyr::filter(event != 'ball_snap')
    
    subtitle <- 'At the throw'
  } else if(cnd == 'all') {
    extra_features <- 'sec'
    set.seed(42)
    # Data is too large to tune on everything
    features_x <- features %>% dplyr::sample_frac(0.1)
    subtitle <- 'All frames between snap and throw'
  }
  suffix <- cnd
  
  .path_data_x <- function(file, ext = NULL) {
    if(!is.null(ext)) {
      ext <- sprintf('.%s', ext)
    } else {
      ext <- ''
    }
    .path_data(file = sprintf('%s_target_prob_%s%s', file, suffix), ext = ext)
  }
  .path_figs_png_x <- function(file) {
    .path_figs_png(file = printf('%s_target_prob_%s', file, suffix))
  }
  .path_data_parquet_x <- purrr::partial(.path_data_x, ext = 'parquet', ... = )
  
  path_res_tune_cv <- .path_data_x('res_tune_cv', ext = 'rds')
  path_fit <- .path_data_x('fit')
  path_res_cv <- .path_data_parquet_x('res_cv')
  path_probs <- .path_data_parquet_x('probs')
  path_probs_oob <- .path_data_parquet_x('probs_oob')
  path_shap <- .path_data_parquet_x('shap')
  
  path_roc_curve <- .path_figs_png_x('viz_roc_curve')
  path_roc_curve_compare <- .path_figs_png_x('viz_roc_curve_compare')
  path_shap_swarm <- .path_figs_png_x('viz_shap_swarm')
  path_shap_agg <- .path_figs_png_x('viz_shap_agg')
  
  path_metrics <- .path_data_x('metrics', ext = 'csv')
  path_metrics_gt <- .path_figs_png_x('metrics', ext = 'png')
  path_metrics_compare_gt <- .path_figs_png_x('metrics_compare', ext = 'png')
  
  cols_lst <-
    list(
      col_y = 'is_target',
      cols_id = c('game_id', 'play_id', 'frame_id', 'nfl_id'),
      cols_id_model = c('idx'),
      cols_keep = c(
        'is_target',
        'idx_o',
        'game_id',
        'play_id',
        'nfl_id',
        'frame_id',
        'idx',
        'los',
        'qb_o',
        'qb_x',
        'qb_y',
        'x_rusher',
        'y_rusher',
        'dist_rusher',
        'idx_o',
        'x',
        'y',
        'o',
        'o_d1_naive',
        'dist_ball',
        'dist_d1_naive',
        'dist_ball_d1_naive',
        'dist_d2_naive',
        'dist_ball_d2_naive',
        'rnk_target',
        extra_features
      )
    )
  
  features_df <-
    features_x %>% 
    dplyr::left_join(
      features_start %>% 
        dplyr::select(game_id, play_id, nfl_id, frame_id_start = frame_id)
    ) %>% 
    dplyr::mutate(sec = 0.1 * (frame_id - frame_id_start)) %>% 
    dplyr::select(dplyr::any_of(cols_lst$cols_keep)) %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(idx = dplyr::row_number()) %>% 
    dplyr::relocate(idx)
  features_df
  
  features_mat <- 
    model.matrix(
      ~.+0, 
      data = 
        features_df %>%
        dplyr::select(dplyr::one_of(cols_lst$cols_keep)) %>% 
        dplyr::select(-dplyr::one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_id_model)))
    )
  features_mat
  
  features_dmat <-
    xgboost::xgb.DMatrix(
      features_mat,
      label = features_df[[cols_lst$col_y]]
    )
  .nrounds <- 500
  .booster <- 'gbtree'
  .objective <- 'binary:logistic'
  .eval_metrics <- list('auc')
  
  # `folds` comes up in 2 places, so don't put it in an if clause.
  set.seed(42)
  play_ids <- features_df %>% dplyr::distinct(game_id, play_id) %>% dplyr::mutate(idx_play = dplyr::row_number())
  # caret::createFolds(features_df[[cols_lst$col_y]], k = 10, list = TRUE, returnTrain = FALSE) %>% flatten_int() %>% length()
  folds_ids <- caret::createFolds(play_ids$idx_play, k = 10, list = FALSE, returnTrain = FALSE)
  # names(folds_ids) <- NULL
  
  folds <-
    play_ids %>% 
    dplyr::bind_cols(dplyr::tibble(fold = folds_ids)) %>% 
    dplyr::left_join(features_df %>% dplyr::select(game_id, play_id, idx)) %>% 
    dplyr::select(fold, idx) %>% 
    split(.$fold) %>% 
    purrr::map(~dplyr::select(.x, -fold) %>% dplyr::pull(idx))
  
  # folds %>% flatten_int() %>% max()
  
  if(!file.exists(path_res_tune_cv)) {
    
    n_row <- 20
    grid_params <- 
      dials::grid_latin_hypercube(
        dials::finalize(dials::mtry(), features_df),
        dials::min_n(),
        dials::tree_depth(),
        dials::learn_rate(),
        dials::loss_reduction(),
        sample_size = dials::sample_prop(),
        size = n_row
      ) %>% 
      dplyr::mutate(
        learn_rate = 0.1 * ((1:n_row) / n_row),
        mtry = mtry / length(features_df)
      )
    grid_params
    
    .get_metrics <-
      function(data,
               row = 1,
               path = .path_data(sprintf('res_tune_cv_target_prob_%02d', row), ext = 'rds'),
               overwrite = FALSE) {
        # row = 1; data <- grid_params %>% slice(row)
        
        path_exists <- path %>% file.exists()
        if(path_exists & !overwrite) {
          .display_info('Returning early for `row = {row}`.')
          return(readr::read_rds(path))
        }
        
        params <-
          list(
            booster = .booster,
            objective = .objective,
            eval_metric = .eval_metrics,
            eta = data$learn_rate,
            gamma = data$loss_reduction,
            subsample = data$sample_size,
            colsample_bytree = data$mtry,
            max_depth = data$tree_depth,
            min_child_weight = data$min_n
          )
        
        fit_cv <-
          xgboost::xgb.cv(
            data = features_dmat, 
            params = params, 
            nrounds = .nrounds,
            folds = folds, 
            metrics = .eval_metrics,
            early_stopping_rounds = 10,
            print_every_n = 10
          )
        
        res <- params
        res$iter = fit_cv$best_iteration
        res$error_trn = fit_cv$evaluation_log[res$iter]$train_error_mean
        res$error_tst = fit_cv$evaluation_log[res$iter]$test_error_mean
        res$auc_trn = fit_cv$evaluation_log[res$iter]$train_auc_mean
        res$auc_tst = fit_cv$evaluation_log[res$iter]$test_auc_mean
        
        res[['eval_metric']] <- NULL
        res <- dplyr::bind_rows(res)
        readr::write_rds(res, path)
        res
      }
    
    res_tune_cv <- purrr::map_df(1:n_row, function(i) {
      
      cat(glue::glue('Row {cli::bg_cyan(i)} (of {cli::bg_cyan(n_row)})'), sep = '\n')
      .get_metrics(grid_params %>% dplyr::slice(i), row = i)
      
    })
    res_tune_cv %>% readr::write_rds(path_res_tune_cv)
  } else {
    res_tune_cv <- path_res_tune_cv %>% readr::read_rds()
  }
  
  # These can be used in the next 2 ifelse clauses. Easiest to just always run this.
  res_cv_best <- res_tune_cv %>% dplyr::slice_max(auc_tst)
  
  .f <- function(x) {
    res_cv_best %>% purrr::pluck(x)
  }
  params_best <-
    list(
      booster = .booster,
      objective = .objective,
      eval_metric = 'auc',
      eta = .f('eta'),
      gamma = .f('gamma'),
      subsample = .f('subsample'),
      colsample_bytree = .f('colsample_bytree'),
      max_depth = .f('max_depth'),
      min_child_weight = .f('min_child_weight')
    )
  
  # For the 'all' condition, 10% of the data set was used for tuning (since it's ginormous).
  # Now use all of the data for the final model (`fit)` + final out of bag models (`fit_cv`) + shap
  if(cnd == 'all') {
    features_x <- features
    features_df <-
      features_x %>% 
      dplyr::left_join(
        features_start %>% 
          dplyr::select(game_id, play_id, nfl_id, frame_id_start = frame_id)
      ) %>% 
      dplyr::mutate(sec = frame_id - frame_id_start) %>% 
      dplyr::select(dplyr::any_of(cols_lst$cols_keep)) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(idx = dplyr::row_number()) %>% 
      dplyr::relocate(idx)
    features_df
    
    features_mat <- 
      model.matrix(
        ~.+0, 
        data = 
          features_df %>%
          dplyr::select(dplyr::one_of(cols_lst$cols_keep)) %>% 
          dplyr::select(-dplyr::one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_id_model)))
      )
    features_mat
    
    features_dmat <-
      xgboost::xgb.DMatrix(
        features_mat,
        label = features_df[[cols_lst$col_y]]
      )
    
    set.seed(42)
    play_ids <- features_df %>% dplyr::distinct(game_id, play_id) %>% dplyr::mutate(idx_play = dplyr::row_number())
    # caret::createFolds(features_df[[cols_lst$col_y]], k = 10, list = TRUE, returnTrain = FALSE) %>% flatten_int() %>% length()
    folds_ids <- caret::createFolds(play_ids$idx_play, k = 10, list = FALSE, returnTrain = FALSE)
    # names(folds_ids) <- NULL
    
    folds <-
      play_ids %>% 
      dplyr::bind_cols(dplyr::tibble(fold = folds_ids)) %>% 
      dplyr::left_join(features_df %>% dplyr::select(game_id, play_id, idx)) %>% 
      dplyr::select(fold, idx) %>% 
      split(.$fold) %>% 
      purrr::map(~dplyr::select(.x, -fold) %>% dplyr::pull(idx))
  }
  
  if(!file.exists(path_fit)) {
    fit <- 
      xgboost::xgboost(
        params = params_best, 
        data = features_dmat, 
        nrounds = .nrounds, 
        early_stopping_rounds = 10,
        print_every_n = 10,
        verbose = 2
      )
    xgboost::xgb.save(fit, path_fit)
  } else {
    fit <- xgboost::xgb.load(path_fit)
  }
  
  .augment_target_probs_x <-
    purrr::partial(
      .augment_target_probs,
      features_df = features_df,
      features = features_x,
      col_y = cols_lst$col_y,
      export = TRUE,
      ... =
    )
  
  if(!file.exists(path_probs)) {
    # debugonce(.augment_target_probs)
    probs <-
      fit %>% 
      predict(features_dmat, type = 'prob') %>% 
      .augment_target_probs_x(path = path_probs)
    probs 
  } else {
    probs <- path_probs %>% arrow::read_parquet()
  }
  
  if(!file.exists(path_res_cv) | !file.exists(path_probs_oob)) {
    
    fit_cv <-
      xgboost::xgb.cv(
        prediction = TRUE,
        data = features_dmat, 
        params = params_best, 
        nrounds = .nrounds,
        folds = folds, 
        metrics = .eval_metrics,
        early_stopping_rounds = 10,
        print_every_n = 10
      )
    
    res_cv <-
      fit_cv$evaluation_log %>% 
      dplyr::as_tibble()
    res_cv %>% arrow::write_parquet(path_res_cv)
    
    probs_oob <-
      fit_cv$pred %>% 
      .augment_target_probs_x(path = path_probs_oob)
    probs_oob
  } else {
    res_cv <- path_res_cv %>% arrow::read_parquet()
    probs_oob <- path_probs_oob %>% arrow::read_parquet()
    
  }
  
  col_y_sym <- cols_lst$col_y %>% dplyr::sym()
  if(!file.exists(path_roc_curve) | !file.exists(path_roc_curve_compare)) {
    
    roc_curve <-
      dplyr::bind_rows(
        probs %>% 
          yardstick::roc_curve(!!col_y_sym, .prob_0) %>% 
          dplyr::mutate(set = 'Full data'),
        probs_oob %>% 
          yardstick::roc_curve(!!col_y_sym, .prob_0) %>% 
          dplyr::mutate(set = 'Out-of-bag')
      )
    
    viz_roc_curve <-
      roc_curve %>% 
      dplyr::filter(set == 'Full data') %>% 
      ggplot2::ggplot() +
      ggplot2::aes(x = 1 - specificity, y = sensitivity) +
      ggplot2::geom_path(size = 1) +
      ggplot2::geom_abline(size = 1, lty = 2) +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = 'Target probability ROC Curve',
        subtitle = subtitle
      )
    
    viz_roc_curve_compare <-
      roc_curve %>% 
      ggplot2::ggplot() +
      ggplot2::aes(x = 1 - specificity, y = sensitivity) +
      ggplot2::geom_path(aes(color = set, group = set), size = 1) +
      ggplot2::geom_abline(size = 1, lty = 2) +
      ggplot2::guides(
        color = guide_legend('', override.aes = list(size = 3))
      ) +
      ggplot2::coord_equal() +
      ggplot2::theme(
        legend.position = 'top'
      ) +
      ggplot2::labs(
        title = 'Target probability ROC Curve',
        subtitle = subtitle
      )
    
    save_plot(
      viz_roc_curve_compare,
      path = path_roc_curve_compare
    )
    
    save_plot(
      viz_roc_curve,
      path = path_roc_curve
    )
  }
  
  if(!file.exists(path_shap)) {
    
    # Downsize to just 10% of the data again for shap stuff.
    if(cnd == 'all') {
      set.seed(42)
      # browser()
      probs <- probs %>% dplyr::sample_frac(0.1)
      set.seed(42)
      features_df <-
        features_x %>% 
        dplyr::sample_frac(0.1) %>% 
        dplyr::left_join(
          features_start %>% 
            dplyr::select(game_id, play_id, nfl_id, frame_id_start = frame_id)
        ) %>% 
        dplyr::mutate(sec = frame_id - frame_id_start) %>% 
        dplyr::select(dplyr::any_of(cols_lst$cols_keep)) %>% 
        tidyr::drop_na() %>% 
        dplyr::mutate(idx = dplyr::row_number()) %>% 
        dplyr::relocate(idx)
      features_df
      
      features_mat <- 
        model.matrix(
          ~.+0, 
          data = 
            features_df %>%
            dplyr::select(dplyr::one_of(cols_lst$cols_keep)) %>% 
            dplyr::select(-dplyr::one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_id_model)))
        )
      features_mat
    }
    
    feature_values_init <-
      features_mat %>%
      as.data.frame() %>%
      dplyr::mutate_all(scale) %>%
      tidyr::gather('feature', 'feature_value') %>% 
      dplyr::as_tibble()
    feature_values_init
    
    feature_values <-
      feature_values_init %>% 
      dplyr::pull(feature_value)
    feature_values
    
    shap_init <-
      fit %>% 
      predict(newdata = features_mat, predcontrib = TRUE) %>%
      as.data.frame() %>%
      dplyr::as_tibble() %>% 
      dplyr::select(-dplyr::matches('BIAS'))
    shap_init
    
    shap <-
      shap_init %>%
      dplyr::bind_cols(features_df %>% dplyr::select(idx)) %>%
      dplyr::left_join(
        probs %>%
          dplyr::select(prob = .prob_1_norm) %>%
          dplyr::mutate(idx = dplyr::row_number())
      ) %>%
      tidyr::pivot_longer(-c(idx, prob), names_to = 'feature', values_to = 'shap_value')
    shap
    
    shap %>% arrow::write_parquet(path_shap)
  } else {
    shap <- path_shap %>% arrow::read_parquet()
  }
  
  shap_agg_by_feature <-
    shap %>% 
    dplyr::group_by(feature) %>% 
    dplyr::summarize(
      dplyr::across(shap_value, ~mean(abs(.x))),
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      dplyr::across(shap_value, list(rnk = ~dplyr::row_number(dplyr::desc(.x))))
    ) %>% 
    dplyr::arrange(shap_value_rnk)
  shap_agg_by_feature
  
  .prep_viz_data <- function(data) {
    data %>% 
      dplyr::filter(feature != 'rnk_target') %>% 
      dplyr::mutate(
        dplyr::across(
          feature, ~forcats::fct_reorder(.x, -shap_value_rnk)
        )
      )
  }
  
  if(!file.exists(path_shap_swarm)) {

    set.seed(42)
    shap_sample <- shap %>% dplyr::group_by(feature) %>% dplyr::sample_frac(0.1) %>% dplyr::ungroup()
    viz_shap_swarm <-
      shap_sample %>%
      dplyr::left_join(shap_agg_by_feature %>% dplyr::select(feature, shap_value_rnk)) %>%
      .prep_viz_data() %>%
      ggplot2::ggplot() +
      ggplot2::aes(y = feature, x = shap_value) +
      ggbeeswarm::geom_quasirandom(
        ggplot2::aes(color = prob),
        alpha = 0.5,
        groupOnX = FALSE,
        varwidth = TRUE,
      ) +
      ggplot2::scale_color_gradient2(
        limits = c(0, 1),
        midpoint = 0.25,
        low = 'blue',
        mid = 'grey80',
        high = 'red'
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend('Probability', override.aes = list(size = 3, alpha = 1))
      ) +
      # theme_minimal() +
      ggplot2::theme(
        legend.position = 'top'
      ) +
      ggplot2::labs(
        title = 'Target probability model feature importance',
        subtitle = subtitle,
        x = 'SHAP value',
        y = NULL
      )
    # viz_shap_swarm
    
    save_plot(
      viz_shap_swarm,
      path = path_shap_swarm
    )
  }
  
  if(!file.exists(path_shap_agg)) {
    viz_shap_agg <- 
      shap_agg_by_feature %>% 
      .prep_viz_data() %>% 
      ggplot2::ggplot() +
      ggplot2::aes(y = feature, x = shap_value) +
      ggplot2::geom_col() +
      # theme_minimal() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        title = 'Target probability model feature importance',
        subtitle = subtitle,
        x = 'mean(|SHAP value|)',
        y = NULL
      )
    viz_shap_agg
    
    save_plot(
      viz_shap_agg,
      path = path_shap_agg
    )
  }
  
  if(!file.exists(path_metrics) | !file.exists(path_metrics_gt) | !file.exists(path_metrics_compare_gt)) {
    
    acc <-
      probs %>% 
      yardstick::accuracy(!!col_y_sym, .prob_class)
    acc
    
    acc_oob <-
      probs_oob %>% 
      yardstick::accuracy(!!col_y_sym, .prob_class)
    acc_oob
    
    auc <-
      probs %>% 
      yardstick::roc_auc(!!col_y_sym, .estimate = .prob_0_norm)
    auc
    
    auc_oob <-
      probs_oob %>% 
      yardstick::roc_auc(!!col_y_sym, .estimate = .prob_0_norm)
    auc_oob
    
    brier <-
      probs %>% 
      dplyr::mutate(dplyr::across(!!col_y_sym, binary_fct_to_int)) %>% 
      mse(!!col_y_sym, .prob_1)
    
    brier_oob <-
      probs_oob %>% 
      dplyr::mutate(dplyr::across(!!col_y_sym, binary_fct_to_int)) %>% 
      mse(!!col_y_sym, .prob_1)
    
    n_metric <- 3
    metrics <-
      dplyr::tibble(
        set = c(rep('full', n_metric), rep('oob', n_metric)),
        value = list(acc, auc, brier, acc_oob, auc_oob, brier_oob)
      ) %>% 
      tidyr::unnest(value) %>% 
      dplyr::arrange(.metric, set)
    metrics
    
    metrics_gt <-
      metrics %>% 
      dplyr::filter(set == 'full') %>% 
      dplyr::select(-.estimator, -set) %>% 
      gt::gt() %>% 
      gt::cols_label(
        .list = 
          list(
            .metric = gt::md('**Metric**'),
            .estimate = gt::md('**Estimate**')
          )
      ) %>% 
      gt::fmt_number(columns = dplyr::vars(.estimate), decimals = 2)
    
    metrics_gt_compare <-
      metrics %>% 
      dplyr::select(-.estimator) %>% 
      dplyr::mutate(
        dplyr::across(
          set,
          ~dplyr::case_when(
            .x == 'full' ~ 'Full data',
            TRUE ~ 'Out-of-bag'
          )
        )
      ) %>% 
      gt::gt() %>% 
      gt::cols_label(
        .list = 
          list(
            .metric = gt::md('**Metric**'),
            .estimate = gt::md('**Estimate**'),
            set = gt::md('**Set**')
          )
      ) %>% 
      gt::fmt_number(columns = dplyr::vars(.estimate), decimals = 2)
    
    gt::gtsave(metrics_gt, path_metrics_gt)
    gt::gtsave(metrics_gt_compare, path_metrics_compare_gt)
    readr::write_csv(metrics, path_metrics)
  }
  fit
}

.augment_catch_probs <- function(v, features_df, features, col_y, export = TRUE, path) {
  # browser()
  col_y_sym <- col_y %>% dplyr::sym()
  probs_init <-
    v %>% 
    dplyr::tibble(.prob_1 = .) %>% 
    dplyr::mutate(.prob_0 = 1 - .prob_1) %>% 
    dplyr::bind_cols(
      features_df %>% 
        dplyr::select(
          idx,
          game_id, 
          play_id, 
          nfl_id, 
          frame_id, 
          !!col_y
        )
    ) %>% 
    dplyr::inner_join(
      features %>%
        dplyr::select(
          # idx,
          game_id,
          play_id,
          nfl_id,
          frame_id
        )
    )
  
  probs_init_bad <-
    probs_init %>% 
    dplyr::count(idx) %>% 
    dplyr::filter(n > 1L)
  
  probs_init <-
    probs_init %>% 
    dplyr::select(
      idx,
      game_id,
      play_id,
      frame_id,
      dplyr::matches('nfl_id'),
      !!col_y_sym,
      dplyr::matches('prob_0'),
      dplyr::matches('prob_1')
    )
  probs_init
  
  probs_init <- probs_init %>% dplyr::anti_join(probs_init_bad)
  
  probs <-
    probs_init %>%
    dplyr::mutate(
      dplyr::across(!!col_y_sym, factor)
    )
  if(!export) {
    return(probs)
  }
  probs %>% arrow::write_parquet(path)
  probs
}

.filter_target <- function(data) {
  data %>% 
    inner_join(
      plays %>% 
        filter(pass_result %in% c('C', 'I', 'IN')) %>% 
        mutate(is_catch = if_else(pass_result == 'C', 1L, 0L)) %>% 
        select(game_id, play_id, is_catch, nfl_id = target_nfl_id)
    )
}

do_fit_catch_prob_model <- function(cnd = 'all', plays = import_plays()) {
  
  .suffix <- if(cnd %in% c('start', 'end')) {
    'minmax'
  } else {
    'all'
  }
  
  features_init <- import_target_prob_features(suffix = .suffix)
  
  features_catch <- 
    features_init %>% 
    group_by(game_id, play_id, nfl_id) %>% 
    slice_max(frame_id, with_ties = FALSE) %>% 
    ungroup() %>% 
    left_join(
      plays %>% 
        filter(pass_result %in% c('C', 'I', 'IN')) %>% 
        mutate(is_catch = if_else(pass_result == 'C', 1L, 0L)) %>% 
        select(game_id, play_id, is_catch, nfl_id = target_nfl_id)
    )
  features_catch
  
  features <-
    features_init %>% 
    left_join(
      features_catch %>% 
        select(game_id, play_id, frame_id, is_catch, nfl_id)
    ) %>% 
    mutate(idx = row_number()) %>% 
    relocate(idx)
  features
  
  features_start <-
    features %>% 
    filter(event == 'ball_snap')
  
  if(cnd == 'end') {
    features_x <- 
      features %>% 
      filter(event != 'ball_snap')
    
    subtitle <- 'At the throw'
  } else if(cnd == 'all') {
    features_x <- features
    subtitle <- 'All frames between snap and throw'
  }
  suffix <- cnd
  
  .path_data_x <- function(file, ext = NULL) {
    if(!is.null(ext)) {
      ext <- sprintf('.%s', ext)
    } else {
      ext <- ''
    }
    .path_data(file = sprintf('%s_target_prob_%s%s', file, suffix), ext = ext)
  }
  .path_figs_png_x <- function(file) {
    .path_figs_png(file = printf('%s_target_prob_%s', file, suffix))
  }
  .path_data_parquet_x <- purrr::partial(.path_data_x, ext = 'parquet', ... = )
  
  path_res_tune_cv <- .path_data_x('res_tune_cv', ext = 'rds')
  path_fit <- .path_data_x('fit')
  path_res_cv <- .path_data_parquet_x('res_cv')
  path_probs <- .path_data_parquet_x('probs')
  
  cols_lst <-
    list(
      col_y = 'is_catch',
      cols_id = c('game_id', 'play_id', 'frame_id', 'nfl_id'),
      cols_id_model = c('idx'),
      cols_keep = c(
        'is_catch',
        'idx_o',
        'game_id',
        'play_id',
        'nfl_id',
        'frame_id',
        'idx',
        'los',
        'qb_o',
        'qb_x',
        'qb_y',
        'x_rusher',
        'y_rusher',
        'dist_rusher',
        'idx_o',
        'x',
        'y',
        'o',
        'o_d1_naive',
        'dist_ball',
        'dist_d1_naive',
        'dist_ball_d1_naive',
        'dist_d2_naive',
        'dist_ball_d2_naive',
        'rnk_target',
        'sec'
      )
    )
  
  features_df <-
    features_x %>% 
    left_join(
      features_start %>% 
        select(game_id, play_id, nfl_id, frame_id_start = frame_id)
    ) %>% 
    mutate(sec = 0.1 * (frame_id - frame_id_start)) %>% 
    select(any_of(cols_lst$cols_keep)) %>% 
    # drop_na() %>% 
    filter(!is.na(idx_o)) %>% 
    filter(!is.na(qb_o)) %>% 
    mutate(idx = row_number()) %>% 
    relocate(idx)
  features_df
  
  features_mat <- 
    model.matrix(
      ~.+0, 
      data = 
        features_df %>% 
        select(one_of(cols_lst$cols_keep)) %>% 
        select(-one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_id_model)))
    )
  features_mat
  
  features_dmat <-
    xgboost::xgb.DMatrix(
      features_mat,
      label = features_df[[cols_lst$col_y]]
    )
  
  features_df_filt <-
    features_df %>% 
    .filter_target() %>% 
    mutate(idx = row_number()) %>% 
    relocate(idx)
  
  features_mat_filt <- 
    model.matrix(
      ~.+0, 
      data = 
        features_df_filt %>% 
        select(one_of(cols_lst$cols_keep)) %>% 
        select(-one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_id_model)))
    )
  
  features_dmat_filt <-
    xgboost::xgb.DMatrix(
      features_mat_filt,
      label = features_df_filt[[cols_lst$col_y]]
    )
  
  .nrounds <- 500
  .booster <- 'gbtree'
  .objective <- 'binary:logistic'
  .eval_metrics <- list('auc')
  
  # `folds` comes up in 2 places, so don't put it in an if clause.
  set.seed(42)
  play_ids <- features_df_filt %>% distinct(game_id, play_id) %>% mutate(idx_play = row_number())
  # caret::createFolds(features_df[[cols_lst$col_y]], k = 10, list = TRUE, returnTrain = FALSE) %>% flatten_int() %>% length()
  folds_ids <- caret::createFolds(play_ids$idx_play, k = 10, list = FALSE, returnTrain = FALSE)
  # names(folds_ids) <- NULL
  
  folds <-
    play_ids %>% 
    bind_cols(tibble(fold = folds_ids)) %>% 
    left_join(features_df_filt %>% select(game_id, play_id, idx)) %>% 
    select(fold, idx) %>% 
    split(.$fold) %>% 
    map(~select(.x, -fold) %>% pull(idx))
  folds
  # n_fold <- folds %>% flatten_int() %>% max()
  # n_fold
  
  if(!file.exists(path_res_tune_cv)) {
    
    n_row <- 20
    grid_params <- 
      dials::grid_latin_hypercube(
        dials::finalize(dials::mtry(), features_df),
        dials::min_n(),
        dials::tree_depth(),
        dials::learn_rate(),
        dials::loss_reduction(),
        sample_size = dials::sample_prop(),
        size = n_row
      ) %>% 
      mutate(
        learn_rate = 0.1 * ((1:n_row) / n_row),
        mtry = mtry / length(features_df)
      )
    grid_params
    
    .get_metrics <-
      function(data,
               i = 1,
               path = path_ohsnap_data_x(sprintf('res_tune_cv_catch_prob_%02d', i), ext = 'rds'),
               overwrite = FALSE) {
        # i = 1; data = grid_params %>% slice(i); path = path_ohsnap_data_x(sprintf('res_tune_cv_catch_prob_%02d', i), ext = 'rds'); overwrite = FALSE
        path_exists <- path %>% file.exists()
        if(path_exists & !overwrite) {
          .display_info('Returning early for `i = {i}`.')
          return(read_rds(path))
        }
        
        params <-
          list(
            booster = .booster,
            objective = .objective,
            eval_metric = .eval_metrics,
            eta = data$learn_rate,
            gamma = data$loss_reduction,
            subsample = data$sample_size,
            colsample_bytree = data$mtry,
            max_depth = data$tree_depth,
            min_child_weight = data$min_n
          )
        
        fit_cv <-
          xgboost::xgb.cv(
            data = features_dmat_filt, 
            params = params, 
            nrounds = .nrounds,
            folds = folds, 
            metrics = .eval_metrics,
            early_stopping_rounds = 10,
            print_every_n = 10
          )
        
        res <- params
        res$iter = fit_cv$best_iteration
        res$error_trn = fit_cv$evaluation_log[res$iter]$train_error_mean
        res$error_tst = fit_cv$evaluation_log[res$iter]$test_error_mean
        res$auc_trn = fit_cv$evaluation_log[res$iter]$train_auc_mean
        res$auc_tst = fit_cv$evaluation_log[res$iter]$test_auc_mean
        
        res[['eval_metric']] <- NULL
        res <- bind_rows(res)
        write_rds(res, path)
        res
      }
    
    res_tune_cv <- map_df(1:n_row, function(i) {
      
      cat(glue::glue('Row {cli::bg_cyan(i)} (of {cli::bg_cyan(n_row)})'), sep = '\n')
      .get_metrics(grid_params %>% slice(i), i)
      
    })
    res_tune_cv %>% write_rds(path_res_tune_cv)
  } else {
    res_tune_cv <- path_res_tune_cv %>% read_rds()
  }
  
  # These can be used in the next 2 ifelse clauses. Easiest to just always run this.
  # res_best <- res %>% slice_min(error_tst)
  res_cv_best <- res_tune_cv %>% slice_max(auc_tst)
  
  params_best <-
    list(
      booster = .booster,
      objective = .objective,
      eval_metric = 'auc',
      eta = res_cv_best %>% pluck('eta'),
      gamma = res_cv_best %>% pluck('gamma'),
      subsample = res_cv_best %>% pluck('subsample'),
      colsample_bytree = res_cv_best %>% pluck('colsample_bytree'),
      max_depth = res_cv_best %>% pluck('max_depth'),
      min_child_weight = res_cv_best %>% pluck('min_child_weight')
    )
  
  if(!file.exists(path_fit)) {
    fit <- 
      xgboost::xgboost(
        params = params_best, 
        data = features_dmat_filt, 
        nrounds = .nrounds, 
        verbose = 2
      )
    xgboost::xgb.save(fit, path_fit)
  } else {
    fit <- xgboost::xgb.load(path_fit)
  }
  
  .augment_catch_probs_x <-
    partial(
      .augment_catch_probs,
      features_df = features_df,
      features = features_x,
      col_y = cols_lst$col_y,
      export = TRUE,
      ... =
    )
  
  if(!file.exists(path_probs)) {
    # debugonce(.augment_catch_probs)
    probs <-
      fit %>% 
      predict(features_dmat, type = 'prob') %>% 
      .augment_catch_probs_x(path = path_probs)
    probs
  } else {
    probs <- path_probs %>% arrow::read_parquet()
  }
}

# 99. main ---- 
personnel_and_rushers <- 
  weeks %>%
  do_by_week(
    f = do_identify_personnel_and_rushers, 
    path = .path_data_parquet('personnel_and_rushers')
  )
routes <- 
  weeks %>% 
  do_by_week(
    f = do_import_routes, 
    path = .path_data_parquet('routes')
  )

features <-
  weeks %>% 
  do_by_week(
    f = do_generate_features_at_events,
    path = .path_data_parquet('target_prob_features'),
    targets = targets,
    personnel_and_rushers = personnel_and_rushers
  )

do_fit_target_prob_model()
do_fit_catch_prob_model()

# players stuff ----

library(tidyverse)
theme_set_and_update_ohsnap()
data('players_from_tracking', package = 'bdb2021')
positions <-
  import_positions() %>% 
  mutate(
    across(position_label, ~case_when(.x == 'DB' ~ 'CB', TRUE ~ .x))
  )
plays <- import_plays()

path_probs_dists_bad <- path_ohsnap_data('probs_dists_bad.parquet')
path_probs_dists_wt <- path_ohsnap_data('probs_dists_wt.parquet')

if(!file.exists(path_probs_dists_wt)) {
# if(TRUE) {
  catch_probs <- 
    import_catch_probs() %>% 
    select(game_id, play_id, frame_id, nfl_id, catch_prob = .prob_1)
  
  probs <-
    crossing(
      cnd = 'all',
      which = c(NA_character_, 'oob')
    ) %>% 
    mutate(
      data = map2(cnd, which, import_target_probs)
    ) %>% 
    select(cnd, which, data) %>% 
    unnest(data) %>% 
    distinct() %>% 
    # select(-matches('^[.]prob_0'), -.prob_1, -.prob_class) %>% 
    rename(prob = .prob_1_norm) %>% 
    left_join(catch_probs)
  probs
  
  probs_grp <- 
    probs %>% 
    group_by(cnd, which, game_id, play_id, nfl_id)
  
  probs_start <-
    probs_grp %>% 
    slice_min(frame_id, with_ties = FALSE) %>% 
    ungroup()
  
  probs_end <-
    probs_grp %>%
    slice_max(frame_id, with_ties = FALSE) %>%
    ungroup()
  
  secs <-
    full_join(
      probs_start %>% distinct(game_id, play_id, frame_id_start = frame_id),
      probs_end %>% distinct(game_id, play_id, frame_id_end = frame_id)
    ) %>%
    mutate(
      frame_id_diff = frame_id_end - frame_id_start
    )
  
  probs_aug <-
    probs %>% 
    # Drop the first frame
    # anti_join(probs_start %>% select(which, game_id, play_id, nfl_id, frame_id)) %>% 
    # Get the prob of the first frame
    left_join(
      probs_start %>% 
        select(which, game_id, play_id, nfl_id, catch_prob, prob) %>% 
        rename_with(~sprintf('%s_%s', .x, 'start'), -c(which, game_id, play_id, nfl_id))
    ) %>% 
    # Add how many frames are in the play
    left_join(secs %>% select(game_id, play_id, frame_id_diff))
  probs_aug
  rm('probs_grp', 'probs_start', 'probs_end', 'secs')
  
  .power <- 2
  ids <- probs_aug %>% distinct(game_id, play_id)
  
  min_dists_init <- 
    import_min_dists_naive_target() %>%
    semi_join(ids) %>% 
    distinct(game_id, play_id, frame_id, nfl_id, nfl_id_d, dist_d)
  min_dists_init
  
  min_dists <-
    min_dists_init %>% 
    group_by(game_id, play_id, frame_id, nfl_id_d) %>%
    mutate(
      dist_d_total = sum(1 / dist_d^.power),
      wt = (1 / dist_d^.power) / dist_d_total
    ) %>%
    ungroup() %>% 
    select(-dist_d_total)
  min_dists
  
  min_dists_start <-
    min_dists %>% 
    group_by(game_id, play_id, nfl_id, nfl_id_d) %>% 
    slice_min(frame_id, with_ties = FALSE) %>% 
    ungroup()
  
  min_dists_aug <-
    min_dists %>% 
    left_join(
      min_dists_start %>% 
        select(game_id, play_id, nfl_id, nfl_id_d, wt, dist_d) %>% 
        rename_with(~sprintf('%s_%s', .x, 'start'), -c(game_id, play_id, nfl_id, nfl_id_d))
    )
  rm('min_dists', 'min_dists_init', 'min_dists_start')
  
  probs_dists <-
    probs_aug %>%
    select(
      cnd,
      which,
      game_id,
      play_id,
      frame_id,
      frame_id_diff,
      nfl_id,
      is_target,
      matches('prob')
    ) %>%
    inner_join(min_dists_aug)
  probs_dists
  
  if(!file.exists(path_probs_dists_bad)) {
    probs_dists_n <-
      probs_dists %>% 
      count(cnd, which, game_id, play_id, nfl_id, frame_id, nfl_id_d)
    probs_dists_n
    
    probs_dists_bad <-
      probs_dists_n %>% 
      filter(n > 1L)
    probs_dists_bad
    rm('probs_dists_n')
    probs_dists_bad %>% arrow::write_parquet(path_probs_dists_bad)
  } else {
    probs_dists_bad <- path_probs_dists_bad %>% arrow::read_parquet()
  }
  rm('probs_aug', 'probs')
  
  probs_dists_wt <-
    probs_dists %>%
    anti_join(probs_dists_bad) %>%
    semi_join(plays) %>% 
    arrange(game_id, play_id, frame_id, nfl_id) %>%
    select(-matches('^[.]prob')) %>% 
    select(
      which,
      game_id,
      play_id,
      nfl_id,
      nfl_id_d,
      is_target,
      frame_id,
      frame_id_diff,
      matches('dist_d'),
      matches('wt'),
      matches('prob')
    ) %>%
    mutate(
      across(is_target, binary_fct_to_lgl)
    )
  probs_dists_wt %>% arrow::write_parquet(path_probs_dists_wt)
} else {
  probs_dists_wt <- path_probs_dists_wt %>% arrow::read_parquet()
}

probs_dists_wt <-
  probs_dists_wt %>%
  mutate(
    prob_wt = wt * prob * catch_prob,
    prob_diff = prob - prob_start,
    prob_diff_wt_old = (wt * prob) - (wt_start * prob_start),
    prob_diff_wt = (wt * catch_prob * prob) - (wt_start * catch_prob_start * prob_start)
  ) %>% 
  left_join(
    players_from_tracking %>%
      select(-week) %>% 
      rename_with(
        ~ sprintf('%s_d', .x),
        c(nfl_id, position, display_name, jersey_number)
      )
  ) %>%
  left_join(
    positions %>%
      select(position, position_label) %>%
      rename_with(
        ~ sprintf('%s_d', .x), c(position, position_label)
      )
  )

probs_dists_wt_end <-
  probs_dists_wt %>% 
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id,
    nfl_id_d,
    position_label_d,
    display_name_d,
    jersey_number_d
  ) %>% 
  slice_max(frame_id, with_ties = FALSE) %>%
  ungroup()

if(FALSE) {
  game_id_example <- 2018123010
  play_id_example <- 3585
  animate_play(game_id = game_id_example, play_id = play_id_example, target_prob = TRUE)
  
  path_play <- path_ohsnap_figs(sprintf('%s-%s.gif', game_id_example, play_id_example))
  path_tp <- path_ohsnap_figs(sprintf('%s-%s-tp.gif', game_id_example, play_id_example))
  path_res <- path_ohsnap_figs(sprintf('%s-%s-combined.gif', game_id_example, play_id_example))
  play_mgif <- magick::image_read(path_play)
  tp_mgif <- magick::image_read(path_tp)
  
  res_gif <- magick::image_append(c(play_mgif[1], tp_mgif[1]), stack = FALSE)
  for(i in 2:54){
    combo_gif <- magick::image_append(c(play_mgif[i], tp_mgif[i]), stack = FALSE)
    res_gif <- c(res_gif, combo_gif)
  }
  magick::image_write(res_gif, path = path_res)
  
  players_from_tracking_slim <-
    players_from_tracking %>% 
    select(game_id, play_id, nfl_id, display_name, jersey_number)
  
  viz_tp_example <-
    probs_dists_wt %>% 
    filter(which %>% is.na()) %>% 
    filter(game_id == !!game_id_example, play_id == !!play_id_example) %>% 
    select(frame_id, nfl_id, nfl_id_d, dist = dist_d, wt, catch_prob, target_prob_wt = prob_wt) %>% 
    pivot_longer(
      -matches('^(nfl|frame)_id')
    ) %>% 
    left_join(
      players_from_tracking_slim
    ) %>%
    left_join(
      players_from_tracking_slim %>% 
        rename_with(
          ~ sprintf('%s_d', .x),
          c(nfl_id, display_name, jersey_number)
        )
    ) %>%
    mutate(
      across(value, ~coalesce(.x, 0)),
      lab = sprintf('%s (%s)', display_name, jersey_number),
      lab_d = sprintf('%s (%s)', display_name_d, jersey_number_d),
      across(c(lab, lab_d), factor),
      across(name, ~ordered(.x, levels = c('dist', 'wt', 'catch_prob', 'target_prob_wt')))
    ) %>% 
    ggplot() +
    aes(x = frame_id, y = value, group = lab_d, color = lab_d) +
    geom_line(size = 1) +
    # guides(color = FALSE) +
    guides(color = guide_legend(title = '', override.aes = list(size = 3))) +
    # theme_minimal() +
    theme(
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14),
      plot.caption = ggtext::element_markdown(
        # size = 12,
        hjust = 0,
        lineheight = 0
      ),
      legend.position = 'top'
    ) +
    labs(
      title  = 'Coverage skill factors',
      x = NULL, y = NULL
    ) +
    facet_grid(name ~ lab, scales = 'free_y')
  viz_tp_example
  
  save_plot(
    viz_tp_example, 
    path = path_ohsnap_figs(sprintf('target_prob_factors-%s-%s.png'), game_id_example, play_id_exaxmple), 
    width = 10, 
    height = 10
  )
}

diffs_by_play <-
  probs_dists_wt_end %>%
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id_d,
    position_label_d,
    display_name_d,
    jersey_number_d
  ) %>% 
  summarize(
    n_receiver = n(),
    across(c(matches('^prob')), sum, na.rm = TRUE)
  ) %>%
  ungroup()

diffs_by_player <-
  diffs_by_play %>%
  # probs_dists_wt_end %>% 
  group_by(which, nfl_id_d, display_name_d) %>% 
  mutate(
    n = sum(!is_target)
  ) %>%
  ungroup() %>% 
  group_by(which, is_target, nfl_id = nfl_id_d, grp = position_label_d, display_name = display_name_d, n) %>%
  summarize(
    # n = n(),
    across(matches('^prob_diff'), list(sum = sum))
  ) %>%
  ungroup() %>%
  # filter(n > 200) %>% 
  group_by(which, is_target) %>%
  mutate(
    # rnk_sum = row_number(prob_diff_sum),
    # rnk_wt_old_sum = row_number(prob_diff_wt_old_sum),
    rnk = row_number(prob_diff_wt_sum)
  ) %>%
  ungroup() %>%
  group_by(which, is_target, grp) %>%
  mutate(
    # rnk_position_sum = row_number(prob_diff_sum),
    # rnk_wt_old_position_sum = row_number(prob_diff_wt_old_sum),
    rnk_grp = row_number(prob_diff_wt_sum)
  ) %>%
  ungroup() %>%
  rename(tpoe = prob_diff_wt_sum) %>% 
  arrange(rnk)
diffs_by_player

# diffs_by_player %>% filter(display_name == 'Ha Ha Clinton-Dix')
diffs_by_player_wide <-
  diffs_by_player %>% 
  filter(which %>% is.na()) %>% 
  select(
    nfl_id,
    display_name,
    grp,
    is_target,
    n,
    tpoe,
    # rnk,
    rnk_grp
  ) %>% 
  mutate(across(is_target, ~if_else(.x, 'target', 'not_target'))) %>% 
  pivot_wider(
    names_from = is_target,
    values_from = c(tpoe, rnk_grp),
    values_fill = 0
  ) %>% 
  mutate(
    tpoe = tpoe_target + tpoe_not_target,
    rnk = row_number(tpoe)
  ) %>% 
  relocate(nfl_id, display_name, grp, n, tpoe, rnk)
diffs_by_player_wide
write_csv(diffs_by_player_wide, .path_data_csv('tpoe_player_rankings.csv'), na = '')

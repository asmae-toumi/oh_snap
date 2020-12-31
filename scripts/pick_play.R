
library(tidyverse)
source("scripts/gg_field.R")
source("scripts/read_files.R")
source('scripts/plot_helpers.R')

# # Used this to determine `cutoff` for `x_side`
# snap_frames_cleaned %>% 
#   filter(position == "RB") %>% 
#   inner_join(plays) %>% 
#   mutate(x_relative = x - ball_x) %>% 
#   ggplot() +
#   aes(x = x_relative) +
#   geom_histogram(binwidth = 1) +
#   facet_grid(offense_formation~play_direction)
add_x_side_col <- function(data, cutoff = 2.5) {
  data %>% 
    mutate(
      # x_side = 
      #   case_when(
      #     play_direction == "left" & (x + !!cutoff) > ball_x ~ "backfield", 
      #     play_direction == "right" & (x + !!cutoff) < ball_x ~ "backfield", 
      #     TRUE ~ NA_character_
      #   )
      x_side = 
        if_else(
          (x + !!cutoff) < ball_x,
          "backfield",
          "los"
        )
    )
}

# # Used this to determine `cutoff` for `y_side`
# snap_frames_cleaned %>% 
#   filter(position == "TE") %>% 
#   inner_join(plays) %>% 
#   mutate(
#     y_relative = y - ball_y,
#     across(y_relative, ~cut(.x, breaks = seq(-8, 8, by = 0.5)))
#   ) %>% 
#   ggplot() +
#   aes(x = y_relative) +
#   geom_bar() +
#   facet_grid(offense_formation~.)
add_y_side_col <- function(data, cutoff = 3.5) {
  data %>% 
    mutate(
      y_side = 
        case_when(
          play_direction == "left" & (y + !!cutoff) < ball_y ~ "left", 
          play_direction == "left" & (y - !!cutoff) >= ball_y ~ "right", 
          play_direction == "left" ~ "mid",
          play_direction == "right" & (y + !!cutoff) <= ball_y ~ "right", 
          play_direction == "right" & (y - !!cutoff) >= ball_y ~ "left", 
          play_direction == "right" ~ "mid",
          TRUE ~ NA_character_
        )
    )
}

add_side_cols <- function(data, cutoff_x = 2.5, cutoff_y = 3.5) {
  data %>% 
    add_x_side_col(cutoff = cutoff_x) %>% 
    add_y_side_col(cutoff = cutoff_y)
}

add_idx_y_col <- function(data) {
  data %>% 
    filter(side == "O" & !is.na(route) & y_side != "mid" & x_side != "backfield") %>%
    group_by(game_id, play_id, frame_id, y_side) %>% 
    mutate(
      idx_y =
        case_when(
          y_side == "left" ~ row_number(y), 
          y_side == "right" ~ row_number(-y), 
          TRUE ~ NA_integer_
        )
    ) %>% 
    ungroup()
}

identify_intersection_xy <- function(data, pad_backwards = TRUE, pad_x = 1) {
  nfl_ids <- data %>% distinct(nfl_id) %>% pull(nfl_id)
  if(length(nfl_ids) <= 1L) {
    return(tibble())
  }
  
  if(pad_backwards) {
    first_x <-
      data %>% 
      group_by(nfl_id) %>% 
      mutate(x = dplyr::first(x)) %>% 
      ungroup()
    padded_x <-
      first_x %>% 
      mutate(x = x - !!pad_x)
    data <-
      bind_rows(padded_x, data)
  }
  
  data_trans <-
    data %>% 
    nest(data = -c(nfl_id)) %>% 
    mutate(line = map(data, ~select(.x, x, y) %>% as.matrix() %>% sf::st_linestring())) %>% 
    select(nfl_id, line)
  res <-
    crossing(
      nfl_id = nfl_ids,
      nfl_id_intersect = nfl_ids
    ) %>% 
    filter(nfl_id != nfl_id_intersect) %>% 
    inner_join(data_trans, by = "nfl_id") %>% 
    inner_join(data_trans %>% rename_all(~sprintf("%s_intersect", .x)), by = "nfl_id_intersect") %>% 
    mutate(
      intersection = map2(line, line_intersect, ~sf::st_intersection(..1, ..2) %>% as.numeric()),
      has_intersection = map_lgl(intersection, ~length(.x) > 0L)
    ) %>% 
    filter(has_intersection)
  
  if(nrow(res) == 0L) {
    return(tibble())
  }
  
  res %>% 
    mutate(
      x_intersect = map_dbl(intersection, ~.x[[1]]),
      y_intersect = map_dbl(intersection, ~.x[[2]])
    ) %>% 
    select(nfl_id, nfl_id_intersect, x_intersect, y_intersect)
}

identify_intersection <- function(data, pad_backwards = TRUE, pad_x = 1) {
  nfl_ids <- data %>% distinct(nfl_id) %>% pull(nfl_id)
  if(length(nfl_ids) <= 1L) {
    return(tibble())
  }
  
  if(pad_backwards) {
    first_x <-
      data %>% 
      group_by(nfl_id) %>% 
      mutate(x = dplyr::first(x)) %>% 
      ungroup()
    padded_x <-
      first_x %>% 
      mutate(x = x - !!pad_x)
    data <-
      bind_rows(padded_x, data)
  }
  
  data_trans <-
    data %>% 
    nest(data = -c(nfl_id)) %>% 
    mutate(line = map(data, ~select(.x, x, y) %>% as.matrix() %>% sf::st_linestring())) %>% 
    select(nfl_id, line)
  # browser()
  # line1 <- data_trans %>% slice(1) %>% pull(line) %>% pluck(1)
  # line2 <- data_trans %>% slice(2) %>% pull(line) %>% pluck(1)
  # sf::st_intersects(line1, line2) %>% as.integer()
  res <-
    crossing(
      nfl_id = nfl_ids,
      nfl_id_intersect = nfl_ids
    ) %>% 
    filter(nfl_id != nfl_id_intersect) %>% 
    inner_join(data_trans, by = "nfl_id") %>% 
    inner_join(data_trans %>% rename_all(~sprintf("%s_intersect", .x)), by = "nfl_id_intersect") %>% 
    mutate(
      intersection = map2_int(line, line_intersect, ~sf::st_intersects(..1, ..2) %>% as.integer()),
      has_intersection = map_lgl(intersection, ~.x > 0L)
    ) %>% 
    filter(has_intersection)
  
  if(nrow(res) == 0L) {
    return(tibble())
  }
  
  res %>% 
    select(nfl_id, nfl_id_intersect)
}


# # Create a data.frame with all possible pick play route combos.
# # TODO: Eliminate this since it's a pretty fragile way to identify pick routes.
# generate_short_pick_route_combos <- memoise::memoise({function() {
#   short_in_routes <- c("SLANT", "IN")
#   short_out_routes <- c("FLAT", "OUT")
#   all_routes <- c("HITCH", "OUT", "FLAT", "CROSS", "GO", "SLANT", "SCREEN", "CORNER", "IN", "ANGLE", "POST", "WHEEL")
#   # `idx_y_out` marks the receiver doing the "out" part of the combo, which is typically the desired receiver.
#     list(
#       crossing(
#         route1 = short_in_routes,
#         route2 = short_out_routes
#       ) %>% 
#         mutate(idx_y_in = 1L, idx_y_out = 2L, n_route = 2L),
#       crossing(
#         route1 = all_routes,
#         route2 = short_in_routes,
#         route3 = short_out_routes
#       ) %>% 
#         mutate(idx_y_in = 2L, idx_y_out = 3L, n_route = 3L),
#       crossing(
#         route1 = short_in_routes,
#         route2 = short_out_routes,
#         route3 = all_routes
#       ) %>% 
#         mutate(idx_y_in = 1L, idx_y_out = 2L, n_route = 3L),
#       crossing(
#         route1 = all_routes,
#         route2 = all_routes,
#         route3 = short_in_routes,
#         route4 = short_out_routes
#       ) %>% 
#         mutate(idx_y_in = 3L, idx_y_out = 4L, n_route = 4L),
#       crossing(
#         route1 = all_routes,
#         route2 = short_in_routes,
#         route3 = short_out_routes,
#         route4 = all_routes
#       ) %>% 
#         mutate(idx_y_in = 2L, idx_y_out = 3L, n_route = 4L),
#       crossing(
#         route1 = short_in_routes,
#         route2 = short_out_routes,
#         route3 = all_routes,
#         route4 = all_routes
#       ) %>% 
#         mutate(idx_y_in = 1L, idx_y_out = 2L, n_route = 4L)
#     ) %>% 
#     reduce(bind_rows) %>% 
#     unite('route_combo', matches('^route'), sep = '-') %>% 
#     mutate(
#       across(route_combo, ~str_replace_all(.x, c('-NA' = '', '^NA' = '')))
#     )
# }})


plot_pick_route_play <-
  function(game_id = 2018090600,
           play_id = 75,
           tracking = read_week(week = 1),
           positions = read_positions(),
           plays = read_plays(),
           games = read_games(),
           team_colors = FALSE,
           yardmin = NULL,
           yardmax = NULL,
           field_color = "#b3e3d6",
           line_color = "black",
           sideline_color = "white",
           endzone_color = NULL,
           buffer = NULL,
           ...,
           save = TRUE,
           dir = "figs",
           filename = sprintf("%s-%s.png", game_id, play_id),
           path = file.path(dir, filename),
           width = 10,
           height = 12) {
    
    # game_id = 2018090905; play_id = 783
    meta <- tibble::tibble(game_id = game_id, play_id = play_id)
    
    tracking <-
      tracking %>% 
      dplyr::left_join(
        positions %>% 
          dplyr::select(position, position_category = category, side), 
        by = "position"
      ) %>%
      dplyr::inner_join(meta, by = c("game_id", "play_id"))
    
    if(nrow(tracking) == 0L) {
      stop(sprintf("Could not identify tracking data for `game_id = %s`, `play_id = %s`", game_id, play_id), call. = FALSE)
    }
    
    ball <- tracking %>% dplyr::filter(display_name == "Football")
    
    tracking <-
      tracking %>%
      dplyr::filter(display_name != "Football") %>%
      dplyr::inner_join(
        ball %>% dplyr::select(game_id, play_id, frame_id, ball_x = x, ball_y = y), 
        by = c("frame_id", "game_id", "play_id")
      )
    
    play <- plays %>% dplyr::inner_join(meta, by = c("game_id", "play_id"))
    assertthat::assert_that(nrow(play) == 1L)
    
    game <- games %>% dplyr::inner_join(meta, by = "game_id")
    assertthat::assert_that(nrow(game) == 1L)
    
    target_id <- play$target_nfl_id
    target <- 
      tracking %>% 
      filter(nfl_id == !!target_id) %>% 
      distinct(display_name, jersey_number)
    assertthat::assert_that(nrow(target) == 1L)
    
    events_throw <- c(
      "pass_forward",
      "pass_shovel"
    )
    
    snap_frames <- tracking %>% dplyr::filter(event == "ball_snap")
    
    throw_frames <- 
      tracking %>% 
      dplyr::filter(event %in% events_throw) %>% 
      dplyr::mutate(event = "pass_throw")
    
    frames <- dplyr::bind_rows(snap_frames, throw_frames)
    
    min_dists <-
      frames %>%
      dplyr::filter(position != "QB") %>%
      dplyr::filter(position_category != "DL") %>% 
      dplyr::select(game_id, play_id, event, nfl_id, side, x, y) %>%
      tidyr::nest(data = -c(game_id, play_id, event)) %>%
      dplyr::mutate(data = purrr::map(data, compute_min_distances)) %>%
      tidyr::unnest(data)
    
    min_dists_wide <-
      min_dists %>% 
      select(game_id, play_id, event, nfl_id_o, nfl_id_d) %>% 
      pivot_wider(names_from = event, values_from = nfl_id_d, names_prefix = "nfl_id_d_") %>% 
      dplyr::mutate(is_same_defender = dplyr::if_else(nfl_id_d_ball_snap == nfl_id_d_pass_throw, TRUE, FALSE))
    min_dists_wide
    
    pick_route_candidates <-
      snap_frames %>% 
      add_side_cols() %>% 
      add_idx_y_col() %>% 
      select(game_id, play_id, nfl_id, y_side, idx_y, route)
    
    route_combos <-
      pick_route_candidates %>%
      select(game_id, play_id, y_side, idx_y, route) %>% 
      pivot_wider(names_from = idx_y, values_from = route, names_prefix = "rec") %>% 
      unite("route_combo", matches("rec"), sep = "-") %>% 
      # TODO: Fix this. Doesn't seem to capture all cases.
      mutate(
        across(route_combo, ~str_replace_all(.x, c("-NA" = "", "^NA" = ""))),
        n_route = route_combo %>% str_count("-") + 1L
      )

    defenders <-
      min_dists_wide %>%
      inner_join(
        pick_route_candidates %>% 
          rename(nfl_id_o = nfl_id), 
        by = c("game_id", "play_id", "nfl_id_o")
      ) %>%
      inner_join(
        snap_frames %>% 
          filter(side == "O") %>% 
          select(
            game_id,
            play_id,
            nfl_id_o = nfl_id,
            display_name_o = display_name,
            jersey_number_o = jersey_number
          ),
        by = c("game_id", "play_id", "nfl_id_o")
      ) %>% 
      inner_join(
        snap_frames %>% 
          filter(side == "D") %>% 
          select(
            game_id,
            play_id,
            nfl_id_d_ball_snap = nfl_id,
            display_name_d_ball_snap = display_name,
            jersey_number_d_ball_snap = jersey_number
          ),
        by = c("game_id", "play_id", "nfl_id_d_ball_snap")
      ) %>% 
      inner_join(
        snap_frames %>% 
          filter(side == "D") %>% 
          select(
            game_id,
            play_id,
            nfl_id_d_pass_throw = nfl_id,
            display_name_d_pass_throw = display_name,
            jersey_number_d_pass_throw = jersey_number
          ),
        by = c("game_id", "play_id", "nfl_id_d_pass_throw")
      )
    defenders
    
    defenders_changed <-
      defenders %>% 
      filter(!is_same_defender)
    
    if(nrow(defenders_changed) == 0L) {
      defenders_caption <- "All defenders had the same receiver at the snap and throw."
    } else {
      routes_w_leading_vowel <- c("OUT", "IN", "ANGLE")
      defenders_caption <-
        defenders_changed %>% 
        mutate(lab = sprintf("#%s running %s%s on the %s has a different defender at the time of the throw (#%s) compared to the snap (#%s).", jersey_number_o, ifelse(route %in% routes_w_leading_vowel, "an ", ""), route, y_side, jersey_number_d_pass_throw, jersey_number_d_ball_snap)) %>% 
        pull(lab) %>% 
        paste(collapse = "<br />", sep = "")
    }
    
    line_of_scrimmage <- play$absolute_yardline_number
    play_direction <- tracking$play_direction[[1]]
    first_down_line <- line_of_scrimmage + ifelse(play_direction == "left", -1, 1) * play$yards_to_go
    
    events_end_route <-
      c(
        "pass_outcome_caught",
        "pass_outcome_incomplete",
        "qb_sack",
        "pass_outcome_interception",
        "pass_outcome_touchdown",
        "qb_strip_sack",
        "qb_spike"
      )
    
    route_frames <-
      tracking %>%
      dplyr::group_by(game_id, play_id, nfl_id) %>%
      dplyr::mutate(
        group = dplyr::case_when(
          # frame_id == min(frame_id) ~ 1L,
          event == "ball_snap" ~ 1L,
          dplyr::lag(event) %in% events_end_route ~ 1L,
          TRUE ~ 0L
        ),
        group = cumsum(group)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(group == 1L) %>%
      dplyr::select(-group) %>% 
      dplyr::select(game_id, play_id, nfl_id, frame_id, event, side, x, y)
    
    
    target_route_frames <-
      route_frames %>% 
      filter(nfl_id == !!target_id)
    
    nontarget_route_frames <-
      route_frames %>% 
      filter(nfl_id != !!target_id)
    
    ball <-
      frames %>%
      dplyr::distinct(game_id, play_id, event, x = ball_x, y = ball_y) %>%
      dplyr::mutate(nfl_id = NA_real_)
    
    if(is.null(yardmin) | is.null(yardmax)) {
      yardminmax <-
        tracking %>% 
        dplyr::summarize(dplyr::across(x, list(min = min, max = max)))
      
      if(is.null(yardmin)) {
        yardmin <- yardminmax$x_min
        yardmin <- round_any(yardmin, 10, floor)
      }
      
      if(is.null(yardmax)) {
        yardmax <- yardminmax$x_max
        yardmax <- round_any(yardmax, 10, ceiling)
      }
    }
    
    max_y <- 160 / 3
    if(is.null(buffer)) {
      yminmax <-
        tracking %>% 
        dplyr::summarize(dplyr::across(y, list(min = min, max = max)))
      ymin <- yminmax$y_min
      ymin <- round_any(ymin, 1, floor)
      ymax <- yminmax$y_max
      ymax <- round_any(ymax, 1, ceiling)
      ymin_bound <- 0 - ymin
      ymax_bound <- ymax - max_y
      buffer <- max(0, ymin_bound, ymax_bound)
    }
    
    home_team <- game$home_team_abbr
    away_team <- game$visitor_team_abbr
    
    home_team <- game$home_team_abbr
    away_team <- game$visitor_team_abbr
    
    if(team_colors) {
      colors <- read_colors()
      home_color <- colors %>% dplyr::filter(team == home_team) %>% dplyr::pull(color)
      away_color <- colors %>% dplyr::filter(team == away_team) %>% dplyr::pull(color)
      if(play$possession_team == home_team) {
        offense_color <- home_color
        defense_color <- away_color
      } else {
        offense_color <- away_color
        defense_color <- home_color
      }
    } else {
      offense_color <- "red"
      defense_color <- "blue"
      if(play$possession_team == home_team) {
        home_color <- offense_color
        away_color <- defense_color
      } else {
        home_color <- defense_color
        away_color <- offense_color
      }
    }
    
    events <- c("ball_snap", "pass_throw")
    
    p <-
      frames %>%
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
      ggplot2::facet_wrap(
        ~event, 
        nrow = 2L, 
        strip.position = "left", 
        labeller = ggplot2::labeller(event = function(x) ifelse(x == "ball_snap", "Snap", "Throw"))
      ) +
      ggplot2::aes(x = x, y = y, group = nfl_id) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = line_of_scrimmage, event = events),
        inherit.aes = FALSE,
        ggplot2::aes(x = x, y = 0, xend = x, yend = !!max_y),
        size = 1.25
      ) +
      ggplot2::geom_segment(
        data = tibble::tibble(x = first_down_line, event = events),
        inherit.aes = FALSE,
        ggplot2::aes(x = x, y = 0, xend = x, yend = !!max_y),
        color = "#ffff7f",
        size = 2
      ) +
      ggplot2::geom_point(
        data = ball,
        inherit.aes = TRUE,
        size = 3,
        color = "brown"
      ) +
      ggplot2::geom_segment(
        data = min_dists,
        inherit.aes = FALSE,
        size = 1,
        color = "black",
        # linetype = 2,
        ggplot2::aes(x = x_o, y = y_o, xend = x_d, yend = y_d)
      ) +
      ggplot2::geom_path(
        data = 
          nontarget_route_frames %>% 
          # filter(frame_id == max(frame_id)) %>% 
          mutate(event = "pass_throw") %>% 
          arrange(frame_id),
        aes(color =  side),
        size = 0.5,
        alpha = 0.5,
        show.legend = FALSE
      ) +
      ggplot2::geom_path(
        data = 
          target_route_frames %>% 
          # filter(frame_id == max(frame_id)) %>% 
          mutate(event = "pass_throw") %>% 
          arrange(frame_id),
        aes(color =  side),
        size = 2,
        alpha = 0.5,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = "\u25A0", color = side, angle = o),
        # size = 8,
        size = pts(32),
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = "\u2039", color = side, angle = o + 90),
        # size = 8,
        size = pts(32),
        vjust = 0.3,
        hjust = 2,
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = jersey_number, angle = o),
        color = "white",
        # size = 3,
        size = pts(12),
        vjust = 1,
      ) +
      ggplot2::scale_color_manual(values = c("O" = offense_color, "D" = defense_color))
    
    # Theme stuff
    p <-
      p +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(),
        plot.title.position = 'plot',
        strip.background = ggplot2::element_rect(fill = NA),
        # strip.text = ggplot2::element_text()
        plot.caption = ggtext::element_markdown(
          hjust = 0, 
          lineheight = 0
        ),
        # plot.caption = ggplot2::element_text(hjust = 0),
        plot.caption.position = 'plot'
      ) +
      ggplot2::labs(
        title = glue::glue("<b><span style='color:{away_color};'>{away_team}</span></b> @ <b><span style='color:{home_color};'>{home_team}</span></b>, Week {game$week}"),
        # subtitle = "",
        caption = glue::glue("Q{play$quarter}: {play$play_description}<br />
                             Intended receiver: {target$display_name} ({target$jersey_number})<br />
                             {defenders_caption}<br />
                             game_id = {game$game_id}, play_id = {play$play_id}"),
        x = NULL, y = NULL
      )
    
    if(!save) {
      return(p)
    }
    
    if(!dir.exists(dirname(path))) {
      dir.create(dirname(path))
    }
    
    ggplot2::ggsave(
      plot = p,
      filename = path,
      width = width,
      height = height,
      type = "cairo"
    )
  }

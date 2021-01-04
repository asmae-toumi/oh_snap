
source('scripts/target_prob_setup.R')
tpoe <- file.path('data', 'target_prob', 'small', 'tpoe_player_rankings.csv') %>% readr::read_csv()
dpoe <- file.path('data', 'catch_prob', 'dpoe_arrival_player_rankings.csv') %>% readr::read_csv()
# roster <- nflfastR::fast_scraper_roster(seasons = 2018)
# roster %>% filter(team == 'NE', position %in% c('SS', 'FS'))
db_grps <- import_nflfastr_cb_groups()

labels <-
  tpoe %>% 
  filter(grp %in% c('CB', 'S')) %>% 
  distinct(nfl_id, display_name, grp, team)
labels

xpoe <-
  bind_rows(
    tpoe %>%
      select(nfl_id, xpoe = tpoe) %>% 
      mutate(prefix = 't'),
    dpoe %>% 
      select(nfl_id, xpoe = dpoe_arr) %>% 
      mutate(prefix = 'c')
  ) %>% 
  left_join(labels) %>% 
  filter(grp %in% c('CB', 'S')) %>% 
  group_by(grp, prefix) %>% 
  mutate(grp_rnk = row_number(xpoe)) %>% 
  ungroup()
xpoe

xpoe_wide <-
  xpoe %>%
  pivot_wider(
    names_from = prefix,
    values_from = c(xpoe, grp_rnk)
  ) %>% 
  group_by(grp) %>% 
  mutate(grp_rnk = row_number(grp_rnk_t + grp_rnk_c)) %>% 
  ungroup() %>% 
  arrange(grp, grp_rnk)
xpoe_wide

colors <-
  c('#ffa3af',
    '#bf83a8',
    '#7a6991',
    '#3d4d6a',
    '#132f3c')
tb <-
  xpoe_wide %>% 
  filter(grp_rnk <= 10 | display_name %in% c('Jonathan Jones', 'Jason McCourty', 'Patrick Chung', 'Devin McCourty')) %>% 
  # arrange(grp, grp_rnk) %>% 
  mutate(label = sprintf('%s (%s)', display_name, team)) %>% 
  select(
    label,
    # display_name,
    # team,
    # grp,
    xpoe_t,
    xpoe_c,
    grp_rnk_t,
    grp_rnk_c,
    grp_rnk
  ) %>% 
  gt::gt(rowname_col = 'label') %>% 
  gt::cols_label(
    .list = 
      list(
        # display_name = gt::md('**Name (Team)**'),
        # team = gt::md('**Team**'),
        # grp = gt::md('**Group**'),
        xpoe_t = gt::md('**dTPOE**'),
        xpoe_c = gt::md('**dCPOE**'),
        grp_rnk_t = gt::md('**dTPOE Rank**'),
        grp_rnk_c = gt::md('**dCPOE Rank**'),
        grp_rnk = gt::md('**Overall Rank**')
      )
  ) %>% 
  gt::data_color(
    columns = vars(xpoe_t),
    colors = scales::col_numeric(
      palette = colors,
      # palette = as.character(paletteer::paletteer_c('scico::berlin', n = 5, direction = -1)),
      domain = c(5, -21)
    )
  ) %>% 
  gt::data_color(
    columns = vars(xpoe_c),
    colors = scales::col_numeric(
      palette = colors,
      # palette = rev(c('#ffffff', '#f2fbd2', '#c9ecb4', '#93d3ab', '#35b0ab')),
      
      # palette = as.character(paletteer::paletteer_c('scico::berlin', n = 5, direction = -1)),
      domain = c(6, -6)
    )
  ) %>% 
  gt::tab_row_group(
    group = 'Safety', # gt::md('Safety'),
    rows = 13:24
  ) %>% 
  gt::tab_row_group(
    group = 'Cornerback',
    rows = 1:12
  ) %>% 
  gt::fmt_number(columns = vars(xpoe_t), decimals = 1) %>% 
  gt::fmt_number(columns = vars(xpoe_c), decimals = 2) %>% 
  gt::tab_footnote(
    locations = gt::cells_column_labels(
      columns = vars(grp_rnk)
    ),
    footnote = 'NE players included (regardless of rank) for comparison.'
  ) %>% 
  gt::tab_options(
    row_group.font.weight = 'bold'
  )
tb
gt::gtsave(tb, filename = file.path('figs', 'final_player_rankings.png'))

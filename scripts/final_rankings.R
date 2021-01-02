

source('scripts/target_prob_setup.R')
tpoe <- file.path('data', 'target_prob', 'small', 'tpoe_player_rankings.csv') %>% readr::read_csv()
dpoe <- file.path('data', 'catch_prob', 'dpoe_arrival_player_rankings.csv') %>% readr::read_csv()
roster <- nflfastR::fast_scraper_roster(seasons = 2018)
roster %>% filter(team == 'NE', position %in% c('SS', 'FS'))

labels <-
  tpoe %>% 
  filter(grp %in% c('CB', 'S')) %>% 
  distinct(nfl_id, display_name, grp) %>% 
  inner_join(
    roster %>% 
      filter(position %in% c('CB', 'DB', 'S', 'FS', 'SS')) %>% 
      select(display_name = full_name, team, position)
  )
labels
# roster %>% filter(team == 'NE', position %in% c('CB', 'DB'))
# labels %>% filter(n > 1)
xpoe <-
  bind_rows(
    tpoe %>%
      select(nfl_id, xpoe = tpoe) %>% 
      mutate(prefix = 't'),
    dpoe %>% 
      select(nfl_id, xpoe = dpoe_arr) %>% 
      mutate(prefix = 'd')
  ) %>% 
  left_join(labels) %>% 
  filter(grp %in% c('CB', 'S')) %>% 
  group_by(nfl_id, prefix) %>% 
  filter(row_number(xpoe) == 1L) %>% 
  ungroup() %>% 
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
  mutate(grp_rnk = row_number(grp_rnk_t + grp_rnk_d)) %>% 
  ungroup() %>% 
  arrange(grp, grp_rnk)
xpoe_wide

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
    xpoe_d,
    grp_rnk_t,
    grp_rnk_d,
    grp_rnk
  ) %>% 
  gt::gt(rowname_col = 'label') %>% 
  gt::cols_label(
    .list = 
      list(
        # display_name = gt::md('**Name (Team)**'),
        # team = gt::md('**Team**'),
        # grp = gt::md('**Group**'),
        xpoe_t = gt::md('**TPOE**'),
        xpoe_d = gt::md('**DPOE**'),
        grp_rnk_t = gt::md('**TPOE Rank**'),
        grp_rnk_d = gt::md('**DPOE Rank**'),
        grp_rnk = gt::md('**Overall Rank**')
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
  gt::fmt_number(columns = vars(xpoe_d), decimals = 2) %>% 
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

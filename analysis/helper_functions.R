# This R file contains some helper functions

# TABLES
# count how many times basic has greater gain than its superordinate
b_greater_than_s <- function(d) {
  d %>%
    filter(level != "subordinate", level != "intermediate") %>%
    group_by(domain) %>%
    mutate(super_gain = first(gain[level == "superordinate"])) %>%
    filter(level == "basic", !is.na(super_gain)) %>%
    summarise(
      b_minus_s   = mean(gain - super_gain, na.rm = TRUE),
      b_greater_than_s = sum(gain > super_gain, na.rm = TRUE),
      n_basics = n(),
      prop = round(b_greater_than_s / n_basics, 2),
      .groups = "drop") %>%
    rename(category=domain)
}

# count how many times terminal has greater gain than its starting
t_greater_than_s <- function(d) {
  d %>%
    filter(level != "subordinate", level != "intermediate") %>%
    group_by(domain) %>%
    mutate(s_gain = first(gain[level == "starting"])) %>%
    filter(level == "terminal", !is.na(s_gain)) %>%
    summarise(
      t_minus_s   = mean(gain - s_gain, na.rm = TRUE),
      t_greater_than_s = sum(gain > s_gain, na.rm = TRUE),
      n_terminals = n(),
      prop = round(t_greater_than_s / n_terminals, 2),
      .groups = "drop") %>%
    rename(category=domain)
}

# a version for Corter and Gluck's category utility measure
b_greater_than_s_cu <- function(d) {
  d %>%
    filter(level != "subordinate", level != "intermediate") %>%
    group_by(domain) %>%
    mutate(super_cu = first(cu[level == "superordinate"])) %>%
    filter(level == "basic", !is.na(super_cu)) %>%
    summarise(
      b_minus_s   = mean(cu - super_cu, na.rm = TRUE),
      b_greater_than_s = sum(cu > super_cu, na.rm = TRUE),
      n_basics = n(),
      prop = round(b_greater_than_s / n_basics, 2),
      .groups = "drop") %>%
    rename(category=domain)
}

t_greater_than_s_cu <- function(d) {
  d %>%
    filter(level != "subordinate", level != "intermediate") %>%
    group_by(domain) %>%
    mutate(s_cu = first(cu[level == "starting"])) %>%
    filter(level == "terminal", !is.na(s_cu)) %>%
    summarise(
      t_minus_s   = mean(cu - s_cu, na.rm = TRUE),
      t_greater_than_s = sum(cu > s_cu, na.rm = TRUE),
      n_terminals = n(),
      prop = round(t_greater_than_s / n_terminals, 2),
      .groups = "drop") %>%
    rename(category=domain)
}

# count how many times basic has greater gain than its subordinates
b_greater_than_c <- function(d) {
  d_mut <- d %>%
    filter(level != "superordinate") %>%
    left_join(read_csv(here("data", "d_rosch.csv"), show_col_types = FALSE) %>%
                select(concept, category) %>%
                filter(concept != category, !(category %in% c("instrument", "furniture", "fruit"))) %>%
                rename(category = concept, basic = category), by = "category") %>%
    mutate(basic = ifelse(is.na(basic), category, basic))

  d_mut %>%
    group_by(basic, .add = TRUE) %>%
    mutate(basic_gain = first(gain[level == "basic"])) %>%
    filter(level == "subordinate", !is.na(basic_gain)) %>%
    summarise(
      b_minus_c   = mean(basic_gain - gain, na.rm = TRUE),
      b_greater_than_c = sum(basic_gain > gain, na.rm = TRUE),
      n_subordinates = n(),
      prop = round(b_greater_than_c / n_subordinates, 2),
      .groups = "drop_last")  %>%
    rename(category=basic)
}

# a version for Corter and Gluck's category utility measure
b_greater_than_c_cu <- function(d) {
  d_mut <- d %>%
    filter(level != "superordinate") %>%
    left_join(read_csv(here("data", "d_rosch.csv"), show_col_types = FALSE) %>%
                select(concept, category) %>%
                filter(concept != category, !(category %in% c("instrument", "furniture", "fruit"))) %>%
                rename(category = concept, basic = category), by = "category") %>%
    mutate(basic = ifelse(is.na(basic), category, basic))

  d_mut %>%
    group_by(basic, .add = TRUE) %>%
    mutate(basic_cu = first(cu[level == "basic"])) %>%
    filter(level == "subordinate", !is.na(basic_cu)) %>%
    summarise(
      b_minus_c   = mean(basic_cu - cu, na.rm = TRUE),
      b_greater_than_c = sum(basic_cu > cu, na.rm = TRUE),
      n_subordinates = n(),
      prop = round(b_greater_than_c / n_subordinates, 2),
      .groups = "drop_last")  %>%
    rename(category=basic)
}

# count how many times basic has greater gain than its intermediates
t_greater_than_i <- function(d) {
  d_mut <- d %>%
    filter(level != "starting") %>%
    left_join(read_csv(here("data", "d_leuven.csv"), show_col_types = FALSE) %>%
                select(concept, category) %>%
                filter(concept != category, !(category %in% c("animals", "artifacts"))) %>%
                rename(category = concept, intermediate = category), by = "category") %>%
    mutate(intermediate = ifelse(is.na(intermediate), category, intermediate))

  d_mut %>%
    group_by(intermediate, .add = TRUE) %>%
    mutate(intermediate_gain = first(gain[level == "intermediate"])) %>%
    filter(level == "terminal", !is.na(intermediate_gain)) %>%
    summarise(
      t_minus_i   = mean(gain - intermediate_gain, na.rm = TRUE),
      t_greater_than_i = sum(gain > intermediate_gain, na.rm = TRUE),
      n_terminals = n(),
      prop = round(t_greater_than_i / n_terminals, 2),
      .groups = "drop_last")   %>%
    rename(category=intermediate)
}

# a version for Corter and Gluck's category utility measure
t_greater_than_i_cu <- function(d) {
  d_mut <- d %>%
    filter(level != "starting") %>%
    left_join(read_csv(here("data", "d_leuven.csv"), show_col_types = FALSE) %>%
                select(concept, category) %>%
                filter(concept != category, !(category %in% c("animals", "artifacts"))) %>%
                rename(category = concept, intermediate = category), by = "category") %>%
    mutate(intermediate = ifelse(is.na(intermediate), category, intermediate))

  d_mut %>%
    group_by(intermediate, .add = TRUE) %>%
    mutate(intermediate_cu = first(cu[level == "intermediate"])) %>%
    filter(level == "terminal", !is.na(intermediate_cu)) %>%
    summarise(
      t_minus_i   = mean(cu - intermediate_cu, na.rm = TRUE),
      t_greater_than_i = sum(cu > intermediate_cu, na.rm = TRUE),
      n_terminals = n(),
      prop = round(t_greater_than_i / n_terminals, 2),
      .groups = "drop_last")   %>%
    rename(category=intermediate)
}

# FIGURES
my_theme <- theme(
  axis.text.x  = element_text(size = 6, angle = 90, hjust = 1),
  axis.text.y  = element_text(size = 6),
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  strip.background = element_blank(),
  strip.text   = element_text(size = 6),
  legend.title = element_text(size = 8),
  legend.position = "top",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.spacing = unit(1, "pt")
)

my_gradient_colors <- list(low = "#67a9cf",high = "#ef8a62",mid = "#f7f7f7")
my_binary_super_colors <- c("superordinate" = "#67a9cf", "equal" = "#f7f7f7", "basic"= "#ef8a62")
my_binary_start_colors <- c("starting" = "#67a9cf", "equal" = "#f7f7f7", "terminal"= "#ef8a62")
my_binary_inter_colors <- c("intermediate" = "#67a9cf", "equal" = "#f7f7f7", "terminal"= "#ef8a62")
my_binary_sub_colors <- c("subordinate" = "#67a9cf", "equal" = "#f7f7f7", "basic"= "#ef8a62")

# plot feature model prediction results
plot_gradient_b_minus_s_withstar <- function(d, star_df = NULL) {

  p <- d %>%
    rename(`p[s-b]` = p_s_b, `p[b]` = p_b) %>%
    ggplot(aes(f_b, f_s_b, fill = b_minus_s)) +
    geom_tile() +
    facet_grid(
      `p[s-b]` ~ `p[b]`,
      labeller = label_bquote(
        rows = p[s-b]*": "*.(`p[s-b]`),
        cols = p[b]*": "*.(`p[b]`)
      )
    ) +
    theme_minimal() +
    scale_fill_gradient2(
      low = my_gradient_colors$low,
      high = my_gradient_colors$high,
      mid = my_gradient_colors$mid,
      midpoint = 0,
      name = expression(Delta*G[b-s] * " (bits):    ")
    ) +
    coord_equal() +
    ylab(expression(f[s-b])) +
    xlab(expression(f[b])) +
    theme(
      legend.text  = element_text(size = 6),
      legend.key.height = unit(6, "pt")
    ) +
    my_theme

  if (!is.null(star_df)) {
    p <- p +
      geom_point(
        data = star_df,
        aes(x = f_b, y = f_s_b, shape = which),
        inherit.aes = FALSE,
        color = "black",
        size = 0.5
      ) +
      scale_shape_manual(
        values = c("Rosch" = 8, "Leuven" = 4),
        guide = "none"
      )
  }

  p
}

plot_binary_b_minus_s_withstar <- function(d, star_df = NULL) {

  p <- d %>%
    rename(`p[s-b]` = p_s_b, `p[b]` = p_b) %>%
    mutate(pref = case_when(
      b_minus_s <  0 ~ "superordinate",
      b_minus_s == 0 ~ "equal",
      TRUE           ~ "basic"
    )) %>%
    ggplot(aes(f_b, f_s_b, fill = pref)) +
    geom_tile() +
    scale_fill_manual(
      values = my_binary_super_colors,
      breaks = c("basic", "superordinate"),
      name = "Preference:"
    ) +
    facet_grid(
      `p[s-b]` ~ `p[b]`,
      labeller = label_bquote(
        rows = p[s-b]*": "*.(`p[s-b]`),
        cols = p[b]*": "*.(`p[b]`)
      )
    ) +
    theme_minimal() +
    coord_equal() +
    ylab(expression(f[s-b])) +
    xlab(expression(f[b])) +
    theme(
      legend.text  = element_text(size = 8),
      legend.key.height = unit(2, "pt")
    ) +
    my_theme

  if (!is.null(star_df)) {
    p <- p +
      geom_point(
        data = star_df,
        aes(x = f_b, y = f_s_b, shape = which),
        inherit.aes = FALSE,
        color = "black",
        size = 0.5
      ) +
      scale_shape_manual(
        values = c("Rosch" = 8, "Leuven" = 4),
        guide = "none"
      )
  }

  p
}

# plot non-linguistic context results
add_theme <- theme(
  panel.background = element_rect(fill = "white"),
  legend.text  = element_text(size = 7),
  legend.key.height = unit(2, "pt"),
  axis.title.x = element_text(size=11),
  axis.title.y = element_text(size=11),
  strip.text = element_text(size=7)
)

plot_binary_b_minus_s_nonling <- function(d, ncol=3) {

  d %>%
    mutate(pref = case_when(
      b_minus_s <  0 ~ "superordinate",
      b_minus_s >  0 ~ "basic",
      TRUE           ~ "equal"
    )) %>%
    group_by(category, p_s, p_u) %>%
    summarise(
      pref = names(which.max(table(pref))),  # majority class in this tile
      .groups = "drop"
    ) %>%
    ggplot(aes(p_s, p_u, fill = pref)) +
    geom_tile() +
    scale_fill_manual(
      values = my_binary_super_colors,
      breaks = c("basic", "superordinate"),
      name = "Preference:"
    ) +
    #facet_wrap(~ category, ncol = ncol) +
    facet_wrap(~ category,ncol = ncol, labeller = labeller(category = \(x) label_wrap_gen(width = 10)(gsub("_", " ", x)))) +
    coord_equal() +
    my_theme +
    xlab(expression(p[s])) +
    ylab(expression(p[u]))  +
    add_theme
}

plot_binary_t_minus_s_nonling <- function(d, ncol=3) {
  d %>%
    mutate(pref = case_when(
      t_minus_s <  0 ~ "starting",
      t_minus_s >  0 ~ "terminal",
      TRUE           ~ "equal"
    )) %>%
    group_by(category, p_s, p_u) %>%
    summarise(
      pref = names(which.max(table(pref))),  # majority class in this tile
      .groups = "drop"
    ) %>%
    ggplot(aes(p_s, p_u, fill = pref)) +
    geom_tile() +
    scale_fill_manual(
      values = my_binary_start_colors,
      breaks = c("terminal", "starting"),
      name = "Preference:"
    ) +
    #facet_wrap(~ category, ncol = ncol) +
    facet_wrap(~ category,ncol = ncol, labeller = labeller(category = \(x) label_wrap_gen(width = 10)(gsub("_", " ", x)))) +
    coord_equal() +
    my_theme +
    xlab(expression(p[s])) +
    ylab(expression(p[u]))  +
    add_theme
}

plot_binary_t_minus_i_nonling <- function(d, ncol=30) {

  d %>%
    mutate(pref = case_when(
      t_minus_i <  0 ~ "intermediate",
      t_minus_i >  0 ~ "terminal",
      TRUE           ~ "equal"
    )) %>%
    group_by(category, p_i, p_s) %>%
    summarise(
      pref = names(which.max(table(pref))),  # majority class in this tile
      .groups = "drop"
    ) %>%
    ggplot(aes(p_i, p_s, fill = pref)) +
    geom_tile() +
    scale_fill_manual(
      values = my_binary_inter_colors,
      breaks = c("terminal", "intermediate"),
      name = "Preference:"
    ) +
    #facet_wrap(~ category, ncol = ncol) +
    facet_wrap(~ category,ncol = ncol, labeller = labeller(category = \(x) label_wrap_gen(width = 10)(gsub("_", " ", x)))) +
    coord_equal() +
    my_theme +
    xlab(expression(p[i])) +
    ylab(expression(p[s]))  +
    add_theme
}

# plot non-linguistic context results summarised across concepts
plot_binary_b_minus_s_summary_nonling <- function(d) {

  d %>%
    group_by(p_s, p_u, p_b) %>%
    summarise(b_minus_s = mean(b_minus_s), .groups="drop") %>%
    mutate(pref = case_when(
      b_minus_s <  0 ~ "superordinate",
      b_minus_s >  0 ~ "basic",
      TRUE           ~ "equal"
    )) %>%
    ggplot(aes(p_s, p_u, fill = pref)) +
    geom_tile() +
    scale_fill_manual(
      values = my_binary_super_colors,
      breaks = c("basic", "superordinate"),
      name = "Preference:"
    ) +
    coord_equal()  +
    xlab(expression(p[s])) +
    ylab(expression(p[u])) +
    my_theme +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7),
      legend.key.height = unit(6, "pt"),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6)
    )
}

plot_binary_t_minus_s_summary_nonling <- function(d) {

  d %>%
    group_by(p_i, p_u, p_s) %>%
    summarise(t_minus_s = mean(t_minus_s), .groups="drop") %>%
    mutate(pref = case_when(
      t_minus_s <  0 ~ "starting",
      t_minus_s >  0 ~ "terminal",
      TRUE           ~ "equal"
    )) %>%
    ggplot(aes(p_i, p_u, fill = pref)) +
    geom_tile() +
    scale_fill_manual(
      values = my_binary_start_colors,
      breaks = c("terminal", "starting"),
      name = "Preference:"
    ) +
    coord_equal() +
    xlab(expression(p[i])) +
    ylab(expression(p[u])) +
    my_theme +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7),
      legend.key.height = unit(6, "pt"),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6)
    )
}

plot_binary_t_minus_i_summary_nonling <- function(d) {

  d %>%
    group_by(p_i, p_s, p_u) %>%
    summarise(t_minus_i = mean(t_minus_i), .groups="drop") %>%
    mutate(pref = case_when(
      t_minus_i <  0 ~ "intermediate",
      t_minus_i >  0 ~ "terminal",
      TRUE           ~ "equal"
    )) %>%
    ggplot(aes(p_i, p_s, fill = pref)) +
    geom_tile() +
    scale_fill_manual(
      values = my_binary_inter_colors,
      breaks = c("terminal", "intermediate"),
      name = "Preference:"
    ) +
    coord_equal() +
    xlab(expression(p[i])) +
    ylab(expression(p[s])) +
    my_theme +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7),
      legend.key.height = unit(6, "pt"),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6)
    )
}

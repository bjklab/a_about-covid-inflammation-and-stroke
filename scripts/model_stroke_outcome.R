#' load libraries and set seed
library(tidyverse)
library(readxl)
library(gt)
library(tidybayes)
library(brms)

set.seed(16)


#' load data

read_csv("./data/ac_stroke_dat_combined.csv.gz") %>%
  identity() -> dat
dat


#' #########################
#' UNIVARIABLE ferritin model
#' #########################

dat %>%
  filter(!is.na(lab_ferritin)) %>%
  filter(lab_ferritin < 1e4) %>%
  mutate_at(.vars = vars(contains("lab")), .funs = list("scale" = ~ scale(.x)[,1])) %>%
  select(subject_id, stroke, contains("scale")) %>%
  pivot_longer(cols = contains("lab"), names_to = "lab_name", values_to = "lab_value") %>%
  group_by(subject_id) %>%
  summarise(stroke = unique(stroke),
            ferritin_scale = max(lab_value[lab_name == "lab_ferritin_scale"], na.rm = TRUE),
            inflam_marker_scale = max(lab_value, na.rm = TRUE),
            ) %>%
  ungroup() %>%
  distinct() %>%
  identity() -> dat_ferritin
dat_ferritin


dat_ferritin %>%
  brm(data = ., family = bernoulli(),
      stroke ~ 1 + ferritin_scale,
      # prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
      #           prior(student_t(3, 0, 2.5), class = b)
      # ),
      iter = 2000,
      warmup = 1000,
      chains = 4,
      cores = 4,
      control = list("adapt_delta" = 0.99, max_treedepth = 16),
      backend = "cmdstanr",
      seed = 16) -> m_stroke_ferritin_uni_brms


m_stroke_ferritin_uni_brms %>% write_rds(file = "./models/m_stroke_ferritin_uni_brms.rds.gz", compress = "gz")
m_stroke_ferritin_uni_brms$fit %>% write_rds(file = "./models/m_stroke_ferritin_uni_brms_stanfit.rds.gz", compress = "gz")
m_stroke_ferritin_uni_brms <- read_rds(file = "./models/m_stroke_ferritin_uni_brms.rds.gz")

m_stroke_ferritin_uni_brms$formula

pp_check(m_stroke_ferritin_uni_brms)
m_stroke_ferritin_uni_brms$fit %>% 
  rstan::check_hmc_diagnostics(.)


#  model results
m_stroke_ferritin_uni_brms %>%
  brms::posterior_summary() %>%
  as_tibble(rownames = "param") %>%
  mutate(typeS.05 = (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0)) %>%
  mutate_at(.vars = vars(contains("Q")), .funs = list("exp" = ~ exp(.x))) %>%
  identity() -> precis_stroke_ferritin_uni_brms


precis_stroke_ferritin_uni_brms %>%
  select(param,typeS.05,everything()) %>%
  gt::gt() %>%
  gt::fmt_number(columns = 3:8, n_sigfig = 3) %>%
  gt::data_color(
    columns = c(Estimate, Q2.5, Q97.5),
    colors = scales::col_bin(
      palette = c("salmon","white","lightcyan"),
      domain = NULL,
      na.color = NA,
      bins = c(-Inf,0,Inf)
    )
  )



m_stroke_ferritin_uni_brms$data %>%
  tibble() %>%
  expand(ferritin_scale = modelr::seq_range(ferritin_scale, n = 100),
  ) %>% 
  mutate(ferritin = ferritin_scale*sd(dat$lab_ferritin, na.rm = TRUE) + mean(dat$lab_ferritin, na.rm = TRUE)) %>%
  tidybayes::add_fitted_draws(model = m_stroke_ferritin_uni_brms) %>%
  ungroup() %>%
  ggplot(data = ., aes(x = ferritin, y = .value)) +
  tidybayes::stat_lineribbon() +
  scale_fill_brewer() +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.title = ggtext::element_markdown(),
        legend.background = element_rect(color = "black", size = 0.25, fill = "white"),
        axis.text.x = ggtext::element_markdown(color = "black"),
        axis.text.y = ggtext::element_markdown(color = "black"),
        axis.title.x = ggtext::element_markdown(),
        strip.text = ggtext::element_markdown(color = "black")
  ) +
  labs(x = "Ferritin (mcg / L)",
       y = "Probability of Stroke",
       fill = "Posterior<br>Credible<br>Interval"
  ) -> p_stroke_ferritin_uni_brms
p_stroke_ferritin_uni_brms



p_stroke_ferritin_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_ferritin_uni_brms.pdf", height = 6, width = 8, units = "in")
p_stroke_ferritin_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_ferritin_uni_brms.svg", height = 6, width = 8, units = "in")
p_stroke_ferritin_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_ferritin_uni_brms.png", height = 6, width = 8, units = "in", dpi= 600)





#' #########################
#' UNIVARIABLE inflam model
#' #########################

dat %>%
  mutate_at(.vars = vars(contains("lab")), .funs = list("scale" = ~ scale(.x)[,1])) %>%
  select(subject_id, stroke, contains("scale")) %>%
  pivot_longer(cols = contains("lab"), names_to = "lab_name", values_to = "lab_value") %>%
  group_by(subject_id) %>%
  summarise(stroke = unique(stroke),
            ferritin_scale = max(lab_value[lab_name == "lab_ferritin_scale"], na.rm = TRUE),
            inflam_marker_scale = max(lab_value, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  distinct() %>%
  filter(abs(inflam_marker_scale) != Inf) %>%
  identity() -> dat_inflam
dat_inflam


dat_inflam %>%
  brm(data = ., family = bernoulli(),
      stroke ~ 1 + inflam_marker_scale,
      # prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
      #           prior(student_t(3, 0, 2.5), class = b)
      # ),
      iter = 2000,
      warmup = 1000,
      chains = 4,
      cores = 4,
      control = list("adapt_delta" = 0.99, max_treedepth = 16),
      backend = "cmdstanr",
      seed = 16) -> m_stroke_inflam_uni_brms


m_stroke_inflam_uni_brms %>% write_rds(file = "./models/m_stroke_inflam_uni_brms.rds.gz", compress = "gz")
m_stroke_inflam_uni_brms$fit %>% write_rds(file = "./models/m_stroke_inflam_uni_brms_stanfit.rds.gz", compress = "gz")
m_stroke_inflam_uni_brms <- read_rds(file = "./models/m_stroke_inflam_uni_brms.rds.gz")

m_stroke_inflam_uni_brms$formula

pp_check(m_stroke_inflam_uni_brms)
m_stroke_inflam_uni_brms$fit %>% 
  rstan::check_hmc_diagnostics(.)


#  model results
m_stroke_inflam_uni_brms %>%
  brms::posterior_summary() %>%
  as_tibble(rownames = "param") %>%
  mutate(typeS.05 = (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0)) %>%
  mutate_at(.vars = vars(contains("Q")), .funs = list("exp" = ~ exp(.x))) %>%
  identity() -> precis_stroke_inflam_uni_brms


precis_stroke_inflam_uni_brms %>%
  select(param,typeS.05,everything()) %>%
  gt::gt() %>%
  gt::fmt_number(columns = 3:8, n_sigfig = 3) %>%
  gt::data_color(
    columns = c(Estimate, Q2.5, Q97.5),
    colors = scales::col_bin(
      palette = c("salmon","white","lightcyan"),
      domain = NULL,
      na.color = NA,
      bins = c(-Inf,0,Inf)
    )
  )




m_stroke_inflam_uni_brms$data %>%
  tibble() %>%
  expand(inflam_marker_scale = modelr::seq_range(inflam_marker_scale, n = 100),
  ) %>% 
  tidybayes::add_fitted_draws(model = m_stroke_inflam_uni_brms) %>%
  ungroup() %>%
  ggplot(data = ., aes(x = inflam_marker_scale, y = .value)) +
  tidybayes::stat_lineribbon() +
  scale_fill_brewer() +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.title = ggtext::element_markdown(),
        legend.background = element_rect(color = "black", size = 0.25, fill = "white"),
        axis.text.x = ggtext::element_markdown(color = "black"),
        axis.text.y = ggtext::element_markdown(color = "black"),
        axis.title.x = ggtext::element_markdown(),
        strip.text = ggtext::element_markdown(color = "black")
  ) +
  labs(x = "Marker of Inflammation (scaled)",
       y = "Probability of Stroke",
       fill = "Posterior<br>Credible<br>Interval"
  ) -> p_stroke_inflam_uni_brms
p_stroke_inflam_uni_brms



p_stroke_inflam_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_inflam_uni_brms.pdf", height = 6, width = 8, units = "in")
p_stroke_inflam_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_inflam_uni_brms.svg", height = 6, width = 8, units = "in")
p_stroke_inflam_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_inflam_uni_brms.png", height = 6, width = 8, units = "in", dpi= 600)





#' #########################
#' MULTIVARIABLE symptom model
#' #########################

dat %>%
  select(subject_id, stroke, ams, contains("symp")) %>%
  select(-subject_id) %>%
  identity() -> dat_sx
dat_sx


dat_sx %>%
  brm(data = ., family = bernoulli(),
      stroke ~ .,
      # prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
      #           prior(student_t(3, 0, 2.5), class = b)
      # ),
      iter = 2000,
      warmup = 1000,
      chains = 4,
      cores = 4,
      control = list("adapt_delta" = 0.99, max_treedepth = 16),
      backend = "cmdstanr",
      seed = 16) -> m_stroke_sx_multi_brms


m_stroke_sx_multi_brms %>% write_rds(file = "./models/m_stroke_sx_multi_brms.rds.gz", compress = "gz")
m_stroke_sx_multi_brms$fit %>% write_rds(file = "./models/m_stroke_sx_multi_brms_stanfit.rds.gz", compress = "gz")
m_stroke_sx_multi_brms <- read_rds(file = "./models/m_stroke_sx_multi_brms.rds.gz")

m_stroke_sx_multi_brms$formula

pp_check(m_stroke_sx_multi_brms)
m_stroke_sx_multi_brms$fit %>% 
  rstan::check_hmc_diagnostics(.)


#  model results
m_stroke_sx_multi_brms %>%
  brms::posterior_summary() %>%
  as_tibble(rownames = "param") %>%
  mutate(typeS.05 = (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0)) %>%
  mutate_at(.vars = vars(contains("Q"), contains("Estimate")), .funs = list("exp" = ~ exp(.x))) %>%
  identity() -> precis_stroke_sx_multi_brms


precis_stroke_sx_multi_brms %>%
  select(param,typeS.05,everything()) %>%
  gt::gt() %>%
  gt::fmt_number(columns = 3:9, n_sigfig = 3) %>%
  gt::data_color(
    columns = c(Estimate, Q2.5, Q97.5),
    colors = scales::col_bin(
      palette = c("salmon","white","lightcyan"),
      domain = NULL,
      na.color = NA,
      bins = c(-Inf,0,Inf)
    )
  )






#' #########################
#' MULTIVARIABLE symptom + inflam model
#' #########################

dat %>%
  select(subject_id, stroke, ams, contains("symp")) %>%
  left_join(select(dat_inflam, subject_id, inflam_marker_scale), by = "subject_id") %>%
  identity() -> dat_sx_inflam
dat_sx_inflam


dat_sx_inflam %>%
  brm(data = ., family = bernoulli(),
      stroke ~ .,
      # prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
      #           prior(student_t(3, 0, 2.5), class = b)
      # ),
      iter = 2000,
      warmup = 1000,
      chains = 4,
      cores = 4,
      control = list("adapt_delta" = 0.99, max_treedepth = 16),
      backend = "cmdstanr",
      seed = 16) -> m_stroke_sx_inflam_multi_brms


m_stroke_sx_inflam_multi_brms %>% write_rds(file = "./models/m_stroke_sx_inflam_multi_brms.rds.gz", compress = "gz")
m_stroke_sx_inflam_multi_brms$fit %>% write_rds(file = "./models/m_stroke_sx_inflam_multi_brms_stanfit.rds.gz", compress = "gz")
m_stroke_sx_inflam_multi_brms <- read_rds(file = "./models/m_stroke_sx_inflam_multi_brms.rds.gz")

m_stroke_sx_inflam_multi_brms$formula

pp_check(m_stroke_sx_inflam_multi_brms)
m_stroke_sx_inflam_multi_brms$fit %>% 
  rstan::check_hmc_diagnostics(.)


#  model results
m_stroke_sx_inflam_multi_brms %>%
  brms::posterior_summary() %>%
  as_tibble(rownames = "param") %>%
  mutate(typeS.05 = (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0)) %>%
  mutate_at(.vars = vars(-param), .funs = list("exp" = ~ exp(.x))) %>%
  identity() -> precis_stroke_sx_inflam_multi_brms


precis_stroke_sx_inflam_multi_brms %>%
  gt::gt() %>%
  gt::fmt_number(columns = 2:5, n_sigfig = 3) %>%
  gt::data_color(
    columns = c(Estimate, Q2.5, Q97.5),
    colors = scales::col_bin(
      palette = c("salmon","white","lightcyan"),
      domain = NULL,
      na.color = NA,
      bins = c(-Inf,0,Inf)
    )
  )







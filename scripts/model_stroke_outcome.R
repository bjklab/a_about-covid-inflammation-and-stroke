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



#' UNIVARIABLE ferritin model

dat %>%
  filter(!is.na(lab_ferritin)) %>%
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


dat_ferritin %>%
  brm(data = ., family = binomial,
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


m_stroke_ferritin_uni_brms %>% write_rds(file = "./models/linear/m_stroke_ferritin_uni_brms.rds.gz", compress = "gz")
m_stroke_ferritin_uni_brms$fit %>% write_rds(file = "./models/linear/m_stroke_ferritin_uni_brms_stanfit.rds.gz", compress = "gz")
m_stroke_ferritin_uni_brms <- read_rds(file = "./models/linear/m_stroke_ferritin_uni_brms.rds.gz")

m_stroke_ferritin_uni_brms$formula

pp_check(m_stroke_ferritin_uni_brms)
m_stroke_ferritin_uni_brms$fit %>% 
  rstan::check_hmc_diagnostics(.)


# linear model results
m_stroke_ferritin_uni_brms %>%
  brms::posterior_summary() %>%
  as_tibble(rownames = "param") %>%
  mutate(typeS.05 = (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0)) %>%
  mutate(lineage = ifelse(grepl("lineage",param),gsub("b_lineage","",param),NA)) %>%
  identity() -> precis_stroke_ferritin_uni_brms


precis_stroke_ferritin_uni_brms %>%
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



m_stroke_ferritin_uni_brms$data %>%
  tibble() %>%
  expand(min_ct = modelr::seq_range(min_ct, n = 100),
         lineage = unique(lineage)
  ) %>% 
  tidybayes::add_fitted_draws(model = m_stroke_ferritin_uni_brms) %>%
  ungroup() %>%
  ggplot(data = ., aes(x = lineage, y = .value, fill = stat(y < precis_stroke_ferritin_uni_brms$Q2.5[1]))) +
  geom_hline(yintercept = c(precis_stroke_ferritin_uni_brms$Q2.5[1],precis_stroke_ferritin_uni_brms$Q97.5[1]), linetype = 2) +
  tidybayes::stat_halfeye(.width = c(0.5, 0.95)) +
  scale_fill_manual(values = c("gray80", "skyblue"), guide = FALSE) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.title = ggtext::element_markdown(),
        legend.background = element_rect(color = "black", size = 0.25, fill = "white"),
        axis.text.x = ggtext::element_markdown(color = "black", angle = 270, hjust = 0, vjust = 0.5),
        axis.text.y = ggtext::element_markdown(color = "black"),
        axis.title.x = ggtext::element_markdown(),
        strip.text = ggtext::element_markdown(color = "black")
  ) +
  labs(x = "",
       y = "Expected Cycle Threshold (Ct) Value"
  )


m_stroke_ferritin_uni_brms$data %>%
  tibble() %>%
  expand(min_ct = modelr::seq_range(min_ct, n = 100),
         lineage = unique(lineage)
  ) %>% 
  left_join(filter(select(precis_stroke_ferritin_uni_brms,lineage,typeS.05),!is.na(lineage)), by = "lineage") %>%
  mutate(typeS.05 = replace(typeS.05, is.na(typeS.05), FALSE)) %>%
  tidybayes::add_fitted_draws(model = m_stroke_ferritin_uni_brms) %>%
  ungroup() %>%
  mutate(lineage = factor(lineage),
         lineage = forcats::fct_reorder(.f = lineage, .x = .value, .fun = mean)) %>%
  ggplot(data = ., aes(y = lineage, x = .value, color = typeS.05)) +
  #geom_vline(xintercept = c(precis_stroke_ferritin_uni_brms$Q2.5[1],precis_stroke_ferritin_uni_brms$Q97.5[1]), linetype = 2) +
  tidybayes::stat_pointinterval(.width = c(0.95), size = 0.5) +
  scale_y_discrete(limits = rev, drop = TRUE) +
  scale_x_continuous(expand = expansion(add = 1)) +
  scale_color_manual(values = c("black","dodgerblue"), guide = FALSE) +
  theme_bw() +
  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(),
        plot.title.position = "plot",
        plot.caption = ggtext::element_markdown(),
        plot.caption.position = "plot",
        legend.title = ggtext::element_markdown(),
        legend.background = element_rect(color = "black", size = 0.25, fill = "white"),
        #axis.text.x = ggtext::element_markdown(color = "black", angle = 270, hjust = 0, vjust = 0.5),
        axis.text.y = ggtext::element_markdown(color = "black", hjust = 0),
        axis.title.x = ggtext::element_markdown(),
        strip.text = ggtext::element_markdown(color = "black")
  ) +
  labs(y = "",
       x = "Expected Cycle Threshold (Ct) Value",
       title = "Linear Model - Unadjusted",
       caption = "(posterior median and 95%CI shown; lineages with expected Ct significantly reduced relative to B.1 noted in <span style='color:#1E90FF;'>blue</span>)"
  ) -> p_stroke_ferritin_uni_brms
p_stroke_ferritin_uni_brms



p_stroke_ferritin_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_ferritin_uni_brms.pdf", height = 6, width = 8, units = "in")
p_stroke_ferritin_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_ferritin_uni_brms.svg", height = 6, width = 8, units = "in")
p_stroke_ferritin_uni_brms %>%
  ggsave(filename = "./figs/p_stroke_ferritin_uni_brms.png", height = 6, width = 8, units = "in", dpi= 600)



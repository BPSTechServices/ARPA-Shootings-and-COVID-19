source("./scripts/global.R")
working_data <- readRDS("./data/working_data.rds")



## Filter for NA values to test for data completeness
working_data %>% 
  select(shooting_rate, shooting_rate_jitter, covid_rate, housing_index_quantile, covid_index_quantile, equity_index_quantile, pct_m15t54) %>%
  filter_all(any_vars(is.na(.))) %>%
  mapview(.)

## Correlation matrix
working_data %>%
  st_drop_geometry() %>% 
  mutate(log_shooting_rate = ifelse(log(shooting_rate) %in% c(-Inf, Inf), NA_integer_, log(shooting_rate)),
         log_pop_dens = log(pop_density_livable)) %>% 
  select(shooting_rate, covid_rate, equity_index_quantile, cn_score_mean) %>% 
  # ggcorr()
  ggpairs(.) 

m1 <- glm(shooting_rate ~ covid_rate + equity_index_quantile + pct_m15t54 + camps_reported + cn_score_mean + pop_density_livable, data = working_data)
summary(m1)

m2 <- glm(shooting_rate_jitter ~ covid_rate + equity_index_quantile + pct_m15t54, data = working_data)
summary(m2)

m3 <- glm(shooting_rate_jitter ~ covid_rate, data = working_data)
summary(m3)


# Other useful functions
coefficients(m1) # model coefficients
confint(m1, level=0.95) # CIs for model parameters
fitted(m1) # predicted values
residuals(m1) # residuals
anova(m1) # anova table
vcov(m1) # covariance matrix for model parameters
influence(m1) # regression diagnostics

# ## PCA scree plot
# working_data %>%
#   st_drop_geometry() %>%
#   select(shooting_rate, covid_rate, equity_index_quantile, pct_m15t54) %>% 
#   prcomp(., scale = TRUE) %>%
#   factoextra::fviz_eig(.)
#   # factoextra::fviz_pca_ind(., repel = T)

m1.effects <- standardize_parameters(m1, method = "refit") # https://cran.r-project.org/web/packages/effectsize/effectsize.pdf
m1.effects %>% as.tibble() %>%
  ggplot(aes(x = Parameter, y = Std_Coefficient, ymin = CI_low, ymax = CI_high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Shootings as a function of COVID-19 infection rate and other parameters",
       subtitle = "95% CI effect sizes of preliminary model parameters")

m2.effects <- standardize_parameters(m2, method = "refit") # https://cran.r-project.org/web/packages/effectsize/effectsize.pdf
m2.effects %>% as.tibble() %>%
  ggplot(aes(x = Parameter, y = Std_Coefficient, ymin = CI_low, ymax = CI_high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Shootings as a function of COVID-19 infection rate and other parameters",
       subtitle = "95% CI effect sizes of preliminary model parameters")

compare_models <- rbind(
  m1.effects %>% as.tibble() %>% mutate(model = "Standard Location"),
  m2.effects %>% as.tibble() %>% mutate(model = "Locational Jitter")
)

compare_models %>%
  ggplot(aes(x = Parameter, y = Std_Coefficient, ymin = CI_low, ymax = CI_high, color = model)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  facet_wrap(~model) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Shootings as a function of COVID-19 infection rate and other parameters",
       subtitle = "95% CI effect sizes of preliminary model parameters")


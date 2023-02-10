library(dplyr)
library(ggplot2)
library(rstan)
library(brms)
library(patchwork)

set.seed(1)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

temp <- rnorm(1000, 20,5) %>% round(1) 
temp
holiday <- rbinom(1000, 1, 2/7)
shop <- runif(1000, 1, 4) %>% round()
shop
shop_r <- rnorm(4, 0, 1)
shop_r
data　<- data.frame(temp, holiday, shop) %>% 
  mutate(r=ifelse(shop==1, shop_r[1], ifelse(shop==2, shop_r[2],
                                             ifelse(shop==3, shop_r[3], shop_r[4] ))))%>%
  mutate(lambda=exp(-2+0.3*temp+0.4*holiday+r)) %>%
  mutate(holiday=factor(holiday))
data$sale <- rpois(1000, data$lambda)
data %>% select(temp, holiday, shop, sale) %>% head()

plot <- ggplot() +
  geom_point(data=data,aes(x=temp, y=sale, color=factor(holiday))) + 
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text=element_text(size=20))+
  labs(x="temp", y="sale", title="data") +
  scale_color_manual("holiday",values=c("red","blue")) + 
  facet_wrap(.~ shop)
plot
glm_pois_brms <- brm(
  formula = sale ~ temp + holiday,  
  family = poisson(),                         
  data = data,                     
  seed = 1
)
glm_pois_brms
exp(0.10)
exp(0.0)
eff <- marginal_effects(glm_pois_brms, 
                        method="predict",
                        effects = "temp:holiday")
plot(eff, points = TRUE)

get_prior(
  formula = sale ~ temp + holiday + (1|shop),
  family = poisson(),
  data=data
)
glmm_pois_brms <- brm(
  data = data,
  formula = sale ~ temp + holiday + (1|shop),
  family = poisson(),
  seed = 1,
  prior=c(set_prior("normal(20,10)",class="b",coef="temp")),
  iter = 2000, warmup = 200, chains = 4, thin=2)
prior_summary(glmm_pois_brms)
print(glmm_pois_brms)
ranef(glmm_pois_brms)

theme_set(theme_classic(base_size = 10, base_family = "HiraKakuProN-W3"))
plot(glmm_pois_brms)

condition <- data.frame(shop=1:4)
plot(conditional_effects(mcmc_result, effects="気温:休日",re_formula=NULL,
                         conditions = condition), points=TRUE, ncol=2) %>%
  wrap_plots() + plot_annotation(title="λの推定結果")

plot(conditional_effects(mcmc_result, effects="気温:休日",re_formula=NULL,
                         conditions = condition, method="predict"), points=TRUE, ncol=2)%>%
  wrap_plots() + plot_annotation(title="予測分布")
library(dplyr)
library(ggplot2)
library(rstan)  
library(bayesplot)
library(brms)
library(patchwork)
library(loo)
set.seed(1)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
#データ作成
temp <- rnorm(2000, 20,5) %>% round(1) 
temp
holiday <- rbinom(2000, 1, 2/7)
holiday
shop <- runif(2000, 1, 4) %>% round()
shop
shop_r <- rnorm(4, 0, 1)
shop_r
data<-data.frame(temp,holiday,shop)
data
r=ifelse(shop==1, shop_r[1], ifelse(shop==2, shop_r[2],
                                    ifelse(shop==3, shop_r[3],shop_r[4])))
r 
lambda=exp(-2+0.3*temp+0.4*holiday+r)
lambda

data$sale<-rpois(2000,lambda)
data$holiday<-factor(holiday)
data$shop<-factor(shop)
data %>% head()


summary(data)
plot <- ggplot( 
  data=data,
  mapping=aes(x=temp, y=sale)) + 
  geom_point(aes(color=holiday))
labs(title="sale")
plot
data
#stan design_mat　GLM
formula<-formula(sale~temp+holiday)
design_mat<-model.matrix(formula,data)
design_mat
N<-nrow(data)
K<-3
Y<-data$sale
X<-design_mat
data_list_design<-list(N=N,K=K,Y=Y,X=X)
data_list_design
glm_pois_design <-stan(
  file="GLM_1_design_mat.stan",
  data=data_list_design,
  seed=1
)
print(glm_pois_design,pars=c("b"))
log_lik1 <- extract_log_lik(glm_pois_design)
log_lik1
waic1 <- waic(log_lik1)
print(waic1 , digits = 4)
glm_pois_brms <- brm(
  formula = sale ~ temp + holiday ,  
  family = poisson(),                         
  data = data,                     
  seed = 1)
glm_pois_brms
#信用区間
eff <- conditional_effects(glm_pois_brms, 
                           effects = "temp:holiday")
plot(eff, points = TRUE)
#予測区間
set.seed(1)
eff_pre <- conditional_effects(glm_pois_brms, 
                               method="predict",
                               effects="temp:holiday",
                               probs=c(0.005, 0.995))
plot(eff_pre, points = TRUE)


#一般化線型モデル　係数 shop
glm_pois_brms <- brm(
  formula = sale ~ temp + holiday + shop,  
  family = poisson(),                         
  data = data,                     
  seed = 1,
  prior=c(set_prior("",class="Intercept"))
)
glm_pois_brms
prior_summary(glm_pois_brms)
exp(0.21)
exp(0.55)
#信用区間
eff <- conditional_effects(glm_pois_brms, 
                           effects = "temp:shop")
plot(eff, points = TRUE)
#予測区間
set.seed(1)
eff_pre <- conditional_effects(glm_pois_brms, 
                               method="predict",
                               effects="temp:shop",
                               probs=c(0.005, 0.995))
plot(eff_pre, points = TRUE)

#ランダム切片モデル　stan
formula_random<-formula(sale~temp+holiday)
design_mat_random<-model.matrix(formula_random,data)
design_mat_random
N_s<- shop %>% max
N_s
data_list_design_random<-list(N=N,N_s=N_s,K=K,Y=Y,X=design_mat_random,shop=shop)
data_list_design_random
glmm_pois_design <-stan(
  file="GLMM_1_design_mat.stan",
  data=data_list_design_random,
  seed=1,
  chains = 4,                            
  iter = 5000,                           
  warmup = 4000,                        
  thin = 1
)
print(glmm_pois_design,
      pars=c("b","sigma_r"))
traceplot(glmm_pois_design, pars = c("b", "r", "sigma_r"))
plot(glmm_pois_design)
mcmc_rhat(rhat(glmm_pois_design))
#ランダム切片モデル　brms
get_prior(
  formula=sale ~ temp + holiday + (1|shop) ,  
  family = poisson(), 
  data = data
)
glmm_pois_brms <- brm(
  formula = sale ~ temp + holiday + (1|shop) ,  
  family = poisson(),                         
  data = data,                     
  seed = 1,
  
  )
prior_summary(glmm_pois_brms)
glmm_pois_brms
plot(glmm_pois_brms)
mcmc_rhat(rhat(glmm_pois_brms))
ranef(glmm_pois_brms)

prior_summary(glmm_pois_brms)
ranef(glmm_pois_brms)
conditions<-data.frame(
  shop=c(1:10))
conditions
eff_glmm_brms<-conditional_effects(
  glmm_pois_brms,
  effects="temp:holiday",
  re_formula = NULL,
  conditions=conditions
)

plot(eff_glmm_brms,points=TRUE)



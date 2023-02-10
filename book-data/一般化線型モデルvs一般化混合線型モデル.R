library(rstan)
library(brms)
library(bayesplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fish_num_climate <- read.csv("3-8-1-fish-num-1.csv")
id <- rep(c("1","2","3","4","5","6"),length=10000)
num<-rep(fish_num_climate$fish_num,length=10000)
weather<-rep(fish_num_climate$weather,length=10000)
temperature<-rep(fish_num_climate$temperature,length=10000)
data<-data.frame(FISH_NUM=num,ID=id,WEATHER=weather,TEMPERATURE=temperature)
data[data$ID=="1",1] <- data[data$ID=="1",1] * rpois(n = 1, lambda = 10)
data[data$ID=="2",1] <- data[data$ID=="2",1] * rpois(n = 1, lambda = 5)
data[data$ID=="3",1] <- data[data$ID=="2",1] * rpois(n = 1, lambda = 4)
data[data$ID=="4",1] <- data[data$ID=="2",1] * rpois(n = 1, lambda = 3)
data[data$ID=="5",1] <- data[data$ID=="2",1] * rpois(n = 1, lambda = 2)
data[data$ID=="6",1] <- data[data$ID=="2",1] * rpois(n = 1, lambda = 1)
data
head(data, 3)
summary(data)
ggplot(data = data, 
       mapping = aes(x = TEMPERATURE, y = FISH_NUM)) +
  geom_point(aes(color = weather)) +
  labs(title = "釣獲尾数と気温・天気の関係")
glm_pois_brms <- brm(
  formula = FISH_NUM ~ WEATHER + TEMPERATURE,  
  family = poisson(),                         
  data = data,                     
  seed = 1,                                   
  prior = c(set_prior("", class = "Intercept"))
)
glm_pois_brms
exp(-0.16)
exp(0.01)

set.seed(1)
eff_pre <- marginal_effects(glm_pois_brms, 
                            method = "predict",
                            effects = "TEMPERATURE:WEATHER",
                            probs = c(0.005, 0.995))
plot(eff_pre, points = TRUE)
#brmsランダム切片モデル
glmm_pois_brms<-brm(
  formula=FISH_NUM~WEATHER+TEMPERATURE+(1|ID),
  family=poisson(),
  data=data,
  seed=1,
  prior=c(set_prior("",class="Intercept"),
          set_prior("",class="sd"))
)
plot(glmm_pois_brms)
stanplot(glmm_pois_brms,type="rhat")
glmm_pois_brm
eff_pre2 <- marginal_effects(glmm_pois_brms, 
                            method = "predict",
                            effects = "TEMPERATURE:WEATHER",
                            probs = c(0.005, 0.995))
plot(eff_pre2, points = TRUE)
#brmsランダム係数モデル
glmm_pois_brms<-brm(
  formula=FISH_NUM~WEATHER*TEMPERATURE*ID,
  family=poisson(),
  data=data,
  seed=1,
  prior=c(set_prior("",class="Intercept"),
          set_prior("",class="sd"))
)
plot(glmm_pois_brms)
stanplot(glmm_pois_brms,type="rhat")
glmm_pois_brm
eff_pre2 <- marginal_effects(glmm_pois_brms, 
                             method = "predict",
                             effects = "TEMPERATURE:WEATHER",
                             probs = c(0.005, 0.995))
plot(eff_pre2, points = TRUE)



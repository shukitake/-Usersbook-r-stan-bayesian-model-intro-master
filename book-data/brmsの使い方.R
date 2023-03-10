library(rstan)
library(brms)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
file_beer_sales_2<-read.csv("3-2-1-beer-seles-2.csv")
simple_lm_brms<-brm(
  formula=sales~temperature,
  family=gaussian(link="identity"),
  data=file_beer_sales_2,
  seed=1
)
simple_lm_brms
plot(simple_lm_brms)
simple_lm_formula<-bf(sales~temperature)
simple_lm_brms_2<-brm(
  formula=simple_lm_formula,
  family=gaussian(),
  data=file_beer_sales_2,
  seed=1,
  chains=4,
  iter=2000,
  warmup=1000,
  thin=1
)
prior_summary(simple_lm_brms)
simple_lm_brms_3<-brm(
  formula=simple_lm_formula,
  family=gaussian(),
  data=file_beer_sales_2,
  seed=1,
  prior=c(set_prior("",class="Intercept"),
           set_prior("",class="sigma"),
          set_prior("normal(0,100000)",class="b",coef="temperature")
          )
)
get_prior(
  formula=sales~temperature,
  family=gaussian(),
  data=file_beer_sales_2
)
stancode(simple_lm_brms_3)
standata(simple_lm_brms_3)
stanplot(simple_lm_brms,
         type="intervals",
         pars="^b_",
         prob=0.8,
         prob_outer=0.95)
new_data<-data.frame(temperature=20)
fitted(simple_lm_brms,new_data)
set.seed(1)
predict(simple_lm_brms,new_data)
eff<-marginal_effects(simple_lm_brms)
plot(eff,points=TRUE)
set.seed(1)
eff<-marginal_effects(simple_lm_brms,method="predict")
plot(eff,points=TRUE)
eff<-marginal_effects(simple_lm_brms,effects="temperature")
plot(eff,points=TRUE)

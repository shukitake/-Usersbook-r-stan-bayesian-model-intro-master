install.packages("rstan")
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.core=parallel::detectCores())
file_beer_sales_1<-read.csv("2-4-1-beer-sales-1.csv")
head(file_beer_sales_1,n=3)
sample_size<-nrow(file_beer_sales_1)
sample_size
data_list<-list(sales=file_beer_sales_1$sales, N=sample_size)
data_list
mcmc_result <- stan(
  file = "2-4-1-calc-mean-variance.stan",
  data = data_list,                       
  seed = 1,                               
  chains = 4,                             
  iter = 2000,                            
  warmup = 1000,                          
  thin = 1                               
)

print(
  mcmc_result,                   
  probs = c(0.025, 0.5, 0.975)  
)
traceplot(mcmc_result)
traceplot(mcmc_result,inc_warmup=T)
mcmc_sample<-rstan::extract(mcmc_result,permuted=FALSE)
mcmc_sample
class(mcmc_sample)
dim(mcmc_sample)
dimnames(mcmc_sample)
mcmc_sample[1,"chain:1","mu"]
mcmc_sample[,"chain:1","mu"]
length(mcmc_sample[,,"mu"])
dim(mcmc_sample[,,"mu"])
class(mcmc_sample[,,"mu"])
mu_mcmc_vec<-as.vector(mcmc_sample[,,"mu"])
median(mu_mcmc_vec)
mean(mu_mcmc_vec)
quantile(mu_mcmc_vec,probs=c(0.025,0.975))
install.packages("ggfortify")
library(ggfortify)
autoplot(ts(mcmc_sample[,,"mu"]),
         facets=F,
         ylab="mu",
         main="トレースプロット")
mu_df<-data.frame(
  mu_mcmc_sample=mu_mcmc_vec
)
ggplot(data=mu_df,mapping=aes(x=mu_mcmc_sample))+geom_density(size=1.5)
install.packages("bayesplot")
library(bayesplot)
mcmc_hist(mcmc_sample,pars=c("mu","sigma"))
mcmc_dens(mcmc_sample,pars=c("mu","sigma"))
mcmc_trace(mcmc_sample,pars=c("mu","sigma"))
mcmc_combo(mcmc_sample,pars=c("mu","sigma"))
mcmc_intervals(
  mcmc_sample,pars=c("mu","sigma"),
  prob=0.8,
  prob_outer=0.95
)
mcmc_areas(
  mcmc_sample,pars=c("mu","sigma"),
  prob=0.8,
  prob_outer=0.99
)
mcmc_acf_bar(mcmc_sample,pars=c("mu","sigma"))

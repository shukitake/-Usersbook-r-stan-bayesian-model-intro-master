library(rstan)
library(bayesplot)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

file_beer_sales_1<-read.csv("2-4-1-beer-sales-1.csv")

sample_size<-nrow(file_beer_sales_1)

data_list<-list(sales = file_beer_sales_1$sales, N = sample_size)
data_list

mcmc_result_1 <- stan(
  file = "2-4-1-calc-mean-variance.stan",
  data = data_list,
  seed = 1
)
print(mcmc_result_1)
mcmc_result_2<-stan(
  file="normal-prior.stan",
  data=data_list,
  seed=1
)
print(mcmc_result_2)

mcmc_result_3<-stan(
  file="lp.stan",
  data=data_list,
  seed=1
)
print(mcmc_result_3)

mcmc_result_4<-stan(
  file="lp-normal-prior-vec.stan",
  data=data_list,
  seed=1
)
print(mcmc_result_4)
file_beer_sales_ab<-read.csv("2-6-1-beer-sales-ab.csv")
head(file_beer_sales_ab,n=3)
ggplot(data=file_beer_sales_ab,
       mapping=aes(x=sales,y=..density..,color=beer_name,fill=beer_name))+
  geom_histogram(alpha=0.5,position="identity")+
  geom_density(alpha=0.5,size=0)
sales_a<-file_beer_sales_ab$sales[1:100]
sales_b<-file_beer_sales_ab$sales[101:200]

data_list_ab<-list(
  sales_a=sales_a,
  sales_b=sales_b,
  N=100
)
data_list_ab
mcmc_result6<-stan(
  file="difference-mean.stan",
  data=data_list_ab,
  seed=1
)
print(mcmc_result6)
mcmc_sample<-rstan::extract(mcmc_result6,permuted=FALSE)
mcmc_dens(mcmc_sample,pars="diff")

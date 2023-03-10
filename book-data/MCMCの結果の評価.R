animal_num<-read.csv("2-5-1-animal-num.csv")
head(animal_num,n=3)
sample_size<-nrow(animal_num)
data_list<-list(animal_num=animal_num$animal_num,N=sample_size)
data_list
mcmc_normal<-stan(
  file="normal-dist.stan",
  data=data_list,
  seed=1
)

mcmc_poisson<-stan(
  file="poisson-dist.stan",
  data=data_list,
  seed=1
)
y_rep_normal<-rstan::extract(mcmc_normal)$pred
y_rep_poisson<-rstan::extract(mcmc_poisson)$pred
dim(y_rep_normal)
dim(y_rep_poisson)
y_rep_normal[1,]
y_rep_poisson[1,]
hist(animal_num$animal_num)
hist(y_rep_normal[1,])
hist(y_rep_poisson[1,])
ppc_hist(y=animal_num$animal_num,y_rep_normal[1:5,])
ppc_hist(y=animal_num$animal_num,y_rep_poisson[1:5,])
ppc_dens(y=animal_num$animal_num,y_rep_poisson[1:5,])
ppc_dens_overlay(y=animal_num$animal_num,y_rep_poisson[1:5,])
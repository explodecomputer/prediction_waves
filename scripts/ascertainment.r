library(dplyr)
library(parallel)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# Large population
# Variable SES
# SES is caused by genetics (a)
# SES is caused by environment (b)

# Sample from the population
# Vary:
# - Extent of SES overrepresentation
# - Sample size
# - Causal effects


popn <- 500000
splits <- 10000
popdat <- tibble(id = 1:popn, a = rnorm(popn), b = rnorm(popn))
b_as <- 3
b_bs <- 3
popdat$ses <- range01(popdat$a * b_as + popdat$b * b_bs + rnorm(nrow(popdat)))

param <- expand.grid(
	sample_size = seq(splits, popn-splits, by=splits),
	ascertainment = c(0, 1, 2),
	sim = 1,
	mean_ses = NA,
	cor_ab = NA,
	pval_ab = NA
)
dim(param)

create_sample <- function(popdat, param)
{
	sampledat <- popdat[sample(popdat$id, param$sample_size, prob=(popdat$ses / max(popdat$ses))^param$ascertainment), ]
	param$mean_ses <- mean(sampledat$ses)
	param$cor_ab <- cor(sampledat$a, sampledat$b)
	mod <- summary(lm(a ~ b, sampledat))
	param$pval_ab <- mod$coefficients[2,4]
	param$beta_ab <- mod$coefficients[2,1]
	param$se_ab <- mod$coefficients[2,2]
	return(param)
}

set.seed(12345)
param <- mclapply(1:nrow(param), function(x) 
{
	message(x)
	create_sample(popdat, param[x,])
}, mc.cores=16) %>% bind_rows()

save(param, file="ascertainment.rdata")

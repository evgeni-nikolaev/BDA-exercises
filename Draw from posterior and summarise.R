rm(list=ls())
cat("\n\n\n")

### DESCRIPTION ###
# Knowledge item: Minimal prediction example
# Quality level: Work in progress
#
# Summary of fit for a poisson!
# This runs Gaussian and poisson model with simple predictions through sampling

### INPUT ###
N = 100 # datapoints
beta.x = 1 # fixed effect size
off = 0 # offset / intercept
# link function is assumed to be exponential

n.samples = 10000

### SIMULATE ###
set.seed(2016)
x = runif(N)
x = scale(x) # this will solve any problems with NA values and intercepts I think

exposure = runif(N, min = 1, max = 10)

eta.pois = beta.x*x + off
y.pois = rpois(n = N, lambda = exposure*exp(eta.pois))

### INFER ###
formula.pois = y ~ x

res.p = inla(formula.pois,
             data = data.frame(x = x, y = y.pois),
             E = exposure,
             family = 'poisson',
             control.compute = list(config = TRUE),
             control.predictor = list(compute = TRUE))


### SUMMARISE ###
summary(res.p)


### PREDICTIVE DISTRIBUTION POISSON ###

samples = inla.posterior.sample(n.samples, res.p)
eta.pred.samples = sapply(samples, function(x) {
                    return(x$latent[1:N, 1]) })
# - each row is a predictor
# - each column is a sample

exposure = res.p$.args$E


## Linear predictor
plot(density(eta.pred.samples[1, ]))
# - plots the samples for the first datapoint
lines(res.p$marginals.linear.predictor[[paste('Predictor', 1, sep='.')]], col='green')

## Data level
# Do not be surprised if this is a mismatch, since we have not added observation likelihood variance

marginal.1 = inla.tmarginal(res.p$marginals.linear.predictor[[paste('Predictor', 1, sep='.')]], fun = function(x) exposure[1]*exp(x))
# - mean of observation likelihood
# - first data point
plot(marginal.1, xlim=c(-.1, 10), type="l")
abline(v = y.pois[1])
marginal.2 = inla.tmarginal(res.p$marginals.linear.predictor[[paste('Predictor', 1, sep='.')]], fun = function(x) exposure[2]*exp(x))
lines(marginal.2, xlim=c(-.1, 10), col='green')
abline(v = y.pois[2], col="green")
marginal.3 = inla.tmarginal(res.p$marginals.linear.predictor[[paste('Predictor', 1, sep='.')]], fun = function(x) exposure[3]*exp(x))
lines(marginal.3, xlim=c(-.1, 10), col="red")
abline(v = y.pois[3], col="red")

## Data level samples
# To include observation likelihood variance

y.post.samples.before.poisson = exp(eta.pred.samples)*exposure
y.post.samples = apply(y.post.samples.before.poisson, MARGIN = 2, FUN = function(x) rpois(N, lambda = x))
# - can be done several times
hist(y.post.samples[1, ])
abline(v = y.pois[1])

oldpar = par(mfrow=c(2,2), mai=c(1,1,1,1)*.2)
hist(y.pois)
apply(X=y.post.samples[, 1:3], MARGIN = 2, FUN = hist)
# - plots all the data for the first simulation
# - quite close to the first histogram
par(oldpar)

### SUMMARY measures of fit ###
fun.1 = function(x) mean(x)
y.fun1 = fun.1(y.pois)
posterior.fun1.samples = apply(X = y.post.samples, 2, FUN = fun.1)
hist(posterior.fun1.samples, main="dataset mean compared to posterior samples")
abline(v = y.fun1)

fun.2 = function(x) sd(x)
y.fun2 = fun.2(y.pois)
posterior.fun2.samples = apply(X = y.post.samples, 2, FUN = fun.2)
hist(posterior.fun2.samples)
abline(v = y.fun2)


fun.3 = function(x) mean(x/exposure)
y.fun3 = fun.3(y.pois)
posterior.fun3.samples = apply(X = y.post.samples, 2, FUN = fun.3)
hist(posterior.fun3.samples)
abline(v = y.fun3)
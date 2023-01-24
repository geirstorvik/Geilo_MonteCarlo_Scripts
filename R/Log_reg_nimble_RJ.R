library(nimble)
#Siumulate data - logistic regression
set.seed(34534)
n = 200
p = 2
x= matrix(rnorm(n*p),ncol=p)
X = cbind(1,x)
p = ncol(X)
beta = matrix(c(1,0.5,0.2),ncol=1)
eta = X%*%beta
pr = 1/(1+exp(-eta))
y = rbinom(n,1,prob=pr)
d = data.frame(x=X,y=y)

x1 = X[,2]
x2 = X[,3]
Y=y
N=n

simpleCode1 <- nimbleCode({
  beta0 ~ dnorm(0, sd = 100)
  beta1 ~ dnorm(0, sd = 100)
  beta2 ~ dnorm(0, sd = 100)
  sigma ~ dunif(0, 100)
  z1 ~ dbern(0.5)  ## indicator variable for including beta2
  z2 ~ dbern(0.5)  ## indicator variable for including beta2
  beta1z1 <- beta1 * z1
  beta2z2 <- beta2 * z2
  for(i in 1:N) {
    logit(p[i]) <- beta0 + beta1z1 * x1[i] + beta2z2 * x2[i]
    Y[i] ~ dbin(p[i], 1)
  }
})

simpleModel1 <- nimbleModel(simpleCode1,
                            data = list(Y = Y, x1 = x1, x2 = x2),
                            constants = list(N = N),
                            inits = list(beta0 = 0, beta1 = 0, beta2 = 0, sigma = sd(Y), z1=1,z2 = 1))

RW_sampler_nonzero_indicator <- nimbleFunction(
  contains = sampler_BASE,
  setup = function(model, mvSaved, target, control) {
    regular_RW_sampler <- sampler_RW(model, mvSaved, target = target, control = control$RWcontrol)
    indicatorNode <- control$indicator
  },
  run = function() {
    if(model[[indicatorNode]] == 1) regular_RW_sampler$run()
  },
  methods = list(
    reset = function() {regular_RW_sampler$reset()}
  ))


RJindicatorSampler <- nimbleFunction(
  contains = sampler_BASE,
  setup = function( model, mvSaved, target, control ) {
    ## target should be the name of the indicator node, 'z2' above
    ## control should have an element called coef for the name of the corresponding coefficient, 'beta2' above.  
    coefNode <- control$coef
    scale <- control$scale
    calcNodes <- model$getDependencies(c(coefNode, target))
  },
  run = function( ) { ## The reversible-jump updates happen here.
    currentIndicator <- model[[target]]
    currentLogProb <- model$getLogProb(calcNodes)
    if(currentIndicator == 1) {
      ## propose removing it
      currentCoef <- model[[coefNode]]
      logProbReverseProposal <- dnorm(0, currentCoef, sd = scale, log = TRUE)
      model[[target]] <<- 0
      model[[coefNode]] <<- 0
      proposalLogProb <- model$calculate(calcNodes)
      log_accept_prob <- proposalLogProb - currentLogProb + logProbReverseProposal
    } else {
      ## propose adding it
      proposalCoef <- rnorm(1, 0, sd = scale)
      model[[target]] <<- 1
      model[[coefNode]] <<- proposalCoef
      logProbForwardProposal <- dnorm(0, proposalCoef, sd = scale, log = TRUE)
      proposalLogProb <- model$calculate(calcNodes)
      log_accept_prob <- proposalLogProb - currentLogProb - logProbForwardProposal
    }
    accept <- decide(log_accept_prob)
    if(accept) {
      copy(from = model, to = mvSaved, row = 1, nodes = calcNodes, logProb = TRUE)
    } else {
      copy(from = mvSaved, to = model, row = 1, nodes = calcNodes, logProb = TRUE)
    }
  },
  methods = list(reset = function() {
  })
)



mcmcConf1 <- configureMCMC(simpleModel1)
mcmcConf1$removeSamplers('z1')
mcmcConf1$addSampler(target = 'z1',
                     type = RJindicatorSampler,
                     control = list(scale = 1, coef = 'beta1'))
mcmcConf1$removeSamplers('z2')
mcmcConf1$addSampler(target = 'z2',
                     type = RJindicatorSampler,
                     control = list(scale = 1, coef = 'beta2'))
mcmcConf1$removeSamplers('beta1')
mcmcConf1$addSampler(target = 'beta1',
                     type = 'RW_sampler_nonzero_indicator',
                     control = list(indicator = 'z1',
                                    RWcontrol = list(adaptive = TRUE,
                                                     adaptInterval = 100,
                                                     scale = 1,
                                                     log = FALSE,
                                                     reflective = FALSE)))
mcmcConf1$removeSamplers('beta2')
mcmcConf1$addSampler(target = 'beta2',
                     type = 'RW_sampler_nonzero_indicator',
                     control = list(indicator = 'z2',
                                    RWcontrol = list(adaptive = TRUE,
                                                     adaptInterval = 100,
                                                     scale = 1,
                                                     log = FALSE,
                                                     reflective = FALSE)))

mcmc1 <- buildMCMC(mcmcConf1)
compiled1 <- compileNimble(simpleModel1, mcmc1)
compiled1$mcmc1$run(10000)

samples1 <- as.matrix(compiled1$mcmc1$mvSamples)

plot(samples1[,'z2'])

show(table(samples1[,'z1'],samples1[,'z2']))
show(table(samples1[,'z2']))

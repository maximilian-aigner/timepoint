# Scratch:
# A multidimensional Gamma-renewal process
# with random intercepts (frailties)

# Data generating process
set.seed(1)
d <- 10
sd.frail <- 0.5
shape.0 <- 1
rate.0 <- 2
beta <- c(0.75)

# X <- cbind(rep(1, d), rep(c(0, 1, 2), times = c(4, 3, 3)))
X <- cbind(rep(c(0, 1, 2), times = c(4, 3, 3)))

bT <- 10

frailties <- rnorm(d, sd = sd.frail)

comp <- function(t, f=0, shape = shape.0, rate = rate.0) {
  -pgamma(t, shape, rate, lower.tail = FALSE, log = TRUE) * exp(f)
}

icomp <- function(y, f=0, shape = shape.0, rate = rate.0) {
  if (abs(exp(-y*exp(-f))) < 0.0005) return(Inf)
  qgamma(1-exp(-y * exp(-f)), shape, rate)
}

hgamma <- function(t, shape = shape.0, rate = rate.0) {
  dgamma(t,
         shape = shape, rate = rate) /
    pgamma(t,
           shape = shape, rate = rate, lower.tail = FALSE)
}

pts <- vector(mode = "list", length = d)
ft <- frailties + X%*%beta
#pts <- sapply(1:d, function(j)) 
for (j in 1:d) {
  # simulate points
  u <- rexp(1)
  # ft <- frailties[j] + x[j]*beta
  pts[[j]] <- icomp(u, ft[j])
  i <- 1
  while ( pts[[j]][i] < bT ) {
    iat_exp <- rexp(1)
    iat_gamm <- icomp(iat_exp, ft[j])
    #if (next_pt == pts[[j]][i]) browser()
    pts[[j]] <- c(pts[[j]], pts[[j]][i] + iat_gamm)
    i <- i + 1
  }
  pts[[j]] <- pts[[j]][pts[[j]]<bT]
}

plot.new()
plot.window(xlim = c(-2, bT + 1), ylim = c(0, d))
points(x = unlist(pts), y = rep(1:d, times = lengths(pts)))
segments(x0 = 0, y0 = 1:d, x1 = bT)

text(x = -.75, y = 1:d, round(frailties, digits = 2),
     adj = 1)

text(x = bT, y = 1:d, round(X%*%beta, digits = 2),
     adj = 0)


# Inference

# Numerical integration
ll_num <- function(iat, std, linpred, shape, rate) {
  # f(th|y) = \int_u f(th|y,u) f(u|y) du
  ldens <- dgamma(iat, shape = shape, rate = rate, log = TRUE)
  chaz <- -pgamma(iat, shape = shape, rate = rate, lower.tail = FALSE,
                  log.p = TRUE)
  condll <- function(u) {
    lhood <- prod(hgamma(iat, shape = shape, rate = rate)) * exp(linpred + u) * prod(pgamma(iat, shape = shape, rate = rate, lower.tail = FALSE))^(exp(linpred+u))
    out <- lhood * dnorm(u,mean=0,sd=std)
    # lhood <- sum(ldens) + linpred + u - sum(chaz) * exp(linpred + u)
    # out <- exp(lhood) * dnorm(u, mean = 0, sd = std)
    if (any(is.infinite(out))) browser()
    return(out)
  }
  #browser()
  out <- integrate(condll, lower = -5, upper = 5, subdivisions = 10000)
  #print(out$message)
  return(out$value)
}

num_likelihood <- function(pts, beta, shape, rate, std) {
  #print(paste(beta,shape,rate,std, sep = "-"))
  if(std<0|rate<0|shape<0) return(-99999999999)
  iats <- lapply(pts, diff)
  preds <- X%*%beta
  sum(sapply(1:length(pts), function(j) {
    log(ll_num(iats[[j]], std, preds[j], shape, rate))
  }))
}

# true param vals are (1, 0, 1, 2, 0.5)
fit <- optim(runif(4, min = 0, max = 1), \(par)
  -num_likelihood(pts, beta = par[1], shape = par[2],
                      rate = par[3],
                      std = par[4]))

# TOO COMPLICATED:
# MC-inside -EM algorithm
mcem <- function(pts, beta0 = c(1, 0), sd0 = 0.5,
                 sh0 = 1, rate0 = 1,
                 maxit = 100, tol = 1e-4,
                 N = 10000, prop.sd = 1) {
  d <- length(pts)
  iats <- lapply(pts, diff)
  beta <- beta0
  std <- sd0
  shape <- sh0
  rate <- rate0
  
  for (i in 1:maxit) {
    cat(paste("EM step",i,"\n"))
    # MH step
    uv <- matrix(nrow = 10 + N, ncol = d)
    uv[1,] <- rnorm(d, mean = 0, sd = std)
    #uv[1,10] <- -sum(uv[1,1:9])
    cat("burn-in...")
    acceptance <- numeric(length = 10 + N)
    for (k in 1:(10 + N - 1)) { # 10 burn-in steps 
      if ( k == 11) cat("sampling...")
      
      # THIS SHOULD BE GIBBS SAMPLING !
      proposal <- uv[k,]
      
      #covs <- X%*%beta + uv[k,]
      #proposal <- rnorm(d, mean = uv[k,], sd = prop.sd)
      #proposal[10] <- -sum(proposal[1:9])
      
      #covs_prop <- X%*%beta + proposal
      # 
      #term0 <- dnorm(proposal, mean = 0, sd = std) - dnorm(uv[k,], mean = 0, sd = std)
      #term1 <- sum(lengths(iats) * (uv[k,] - proposal))
      # term2 <- 0
      # for (j in 1:d) {
      #   inner <- 0
      #   for (i in 1:length(iats[[j]])) {
      #     inner <- inner - pgamma(iats[[j]][i], shape = shape,
      #                             rate = rate,
      #                             log = TRUE,
      #                             lower.tail = FALSE)
      #   }
      #   inner <- inner * exp(X[j,]%*%beta) * (exp(proposal[j]) - exp(uv[k,j]))
      #   term2 <- term2 + inner
      # }
      #term2 <- sum(sapply(1:d, function(j) {
      # compsum <- sum(-pgamma(iats[[j]], shape = shape, rate = rate, log = TRUE,
      #                        lower.tail = FALSE)) * exp(X[j,]%*%beta)
      # compsum * (exp(proposal[j]) - exp(uv[k,j]))
      #}))
     # termlast <- dnorm(proposa, mean = 0, )
      #loga <- term1 + term2
      acceptance[k] <- exp(loga)
      if (is.nan(loga)) browser()
      if(k==2)browser()
      # browser()
      #loga <- sum(sapply(1:d, \(j) one_ll_ratio_given_u(iats[[j]], lp1 = covs[j],
      #                                              lp2 = covs_prop[j], lpars = c(shape, rate))))
      #loga <- sum(sapply(1:d, \(j) one_ll_given_u(iats[[j]], lp = covs[j], lpars = c(shape, rate))) -
      #  sapply(1:d, \(j) one_ll_given_u(iats[[j]], lp = covs_prop[j], lpars = c(shape, rate))))
      # browser()
      # ap <- 1
      #for (j in 1:d) {
      # W_ji ~ lambda_0(t) * exp(x'beta + u)
      #  tts <- iats[[j]]
      #  upper <- covs +
      #    sum(log(hgamma(tts,shape = shape,rate = rate))) -
      #    sum(comp(tts, f = covs[j], shape = shape, rate = rate))
      #  
      #  lower <- covs_prop +
      #    sum(log(hgamma(tts, shape = shape, rate = rate))) -
      #    sum(comp(tts, f = covs_prop[j], shape = shape, rate = rate))
      #  
      #  ap <- ap * exp(sum(upper-lower))
      #  print(ap)
      #}
      #if (length(ap) == 0 | is.na(ap)) browser()
      # if (ap > 1 | is.nan(ap)) ap <- 1
      u <- runif(1)
      if (log(u) < loga) {
        # accept
        uv[k+1,] <- proposal
      } else {
        uv[k+1,] <- uv[k,]
      }
    }
    uv <- uv[11:(10+N),] # remove burn-in
    cat("done sampling.\n")
    browser()
    # approximate expectations using uv
    # p = (beta, shape, rate)
    optimf <- function(p) {
      if (shape < 0 | rate < 0) return(-9999999999)
      sum(sapply(1:d, function(j) {
        vals <- sapply(1:N, function(k) {
          covs <- x*p[1] + uv[k,]
          covs +
            sum(log(hgamma(tts, shape = p[2],rate = p[3]))) -
            sum(comp(tts, f = covs[j], shape = p[2], rate = p[3]))
        })
        return(sum(vals))
      }))
    }
    par0 <- c(beta, shape, rate) # dim = 4
    print(paste("starting optimisation at", paste0(par0,collapse=",")))
    pars <- optim(par0, optimf, control = list(fnscale = -1))$par
    beta = pars[1]
    shape = pars[2]
    rate = pars[3]
    std <- sd(as.vector(uv))
    browser()
  }
}

mcem(pts, prop.sd = 0.5)

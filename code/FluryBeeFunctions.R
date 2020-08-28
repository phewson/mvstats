## collection of functions based upon Bernard Flury's Book
## Originally written by Marco Bee


## Calculate the standard error of a principal component solution.

lpc <- function(X){
  UseMethod("lpc", X)
}

lpc.default <- function(X)
{

#  procedure LPC  
#  procedure LPC  

# input:     X        N by p  data matrix

# output:    B        orthogonal p by p matrix, with eigenvectors of the
#                     covariance matrix of X as columns
#            eig$values   N-vector, with eigenvalues in descending order
#            stdeB    p by p matrix, containing normal theory standard
#                      errors of the coefficients in B
#            stdelam  normal theory standard errors of the eigenvalues


#  +++ NOTE: the calculation of standard errors will fail if any two
#         eigenvalues are equal, or if any eigenvalue is zero       +++   

p <- dim(X)[2] # number of variables
N <- dim(X)[1] # number of observations 
eig <- eigen(var(X)) # eigenvalues and eigenvectors 
Ones <- matrix(1, p, p) 
Lambda <- Ones * eig$values
Q <- (t(Lambda) - Lambda + diag(p))^(-2) - diag(p) # nifty trick
Theta1 <- sweep(Q, 2, eig$values, FUN="*") 
Theta <- Theta1 * eig$values # compute matrix of theta-coefficients
stdeB <- matrix(0,p,p)       # open matrix for standard errors 
h <- 1
while (h <= p)               # start loop over eigenvectors 
{
V <- eig$vectors %*% (Theta[, h] * t(eig$vectors))
                             # V-matrix for h-th eigenvector 

stdeB[, h] <- sqrt(diag(V)/N)# standard errors for h-th eigenvector 
h <- h + 1 
}                            # end of loop 
stdelam <- sqrt(2/N) * eig$values   # standard errors of eigenvalues 
results <- list("eigenvectors" = eig$vectors, "eigenvalues" = eig$values,
"stdeB" = stdeB, "stdelam" = stdelam)
class(results) <- "lpc"
results
}


print.lpc <- function(x, alpha = NULL, bonferroni = FALSE) {
if (!is.null(alpha)){ ## calculate ci if asked
  if (bonferroni == TRUE) {alpha = alpha / length(x[[2]])}
  z <- abs(qnorm((1-alpha)/2))
}
print(x[1]) ## eigenvectors

if (!is.null(alpha)){
cat(round(alpha * 100), "\% CI: \n ")
veclo <- x[[1]] - z * x[[3]]
vechi <- x[[1]] + z * x[[3]]
print(veclo)
print(vechi)
cat("\n")
} 

print(x[2]) ## eigenvalues

if (!is.null(alpha)){
cat(round(alpha * 100), "\% CI: \n ")
vallo <- x[[2]] - z * x[[4]]
valhi <- x[[2]] + z * x[[4]]
print(vallo)
print(valhi)
cat("\n")
} 

cat("standard errors for eigenvector coefficients:")
print(x[3])

cat("standard errors for eigenvalues:")
print(x[4])
cat("\n\n")
invisible(x)
}



# S-PLUS instructions for graphing an ellipse 
# First define all parameters 

ellips <- function(A = matrix(c(1,1,1,2),2,2), m = c(3,4), const = 1, k = 1000)
{
# input:  A	positive definite symmetric matrix of dimension 2 by 2
#         m	column vector of length 2, center of ellipse
#         const	positive constant
#         k	number of points on ellipse (must be at least 2)
# output: x 	a (k by 2) matrix whose rows are the coordinates
#		of k points on the ellipse (y-m)'*A^{-1}*(y-m) = c^2

r <- A[1, 2]/sqrt(A[1, 1] * A[2, 2])
Q <- matrix(0, 2, 2)			  # construct matrix Q for
Q[1, 1] <- sqrt(A[1, 1] %*% (1+r)/2)	# transformation of circle
Q[1, 2] <- -sqrt(A[1, 1] %*% (1-r)/2)		      # to ellipse
Q[2, 1] <- sqrt(A[2, 2] %*% (1+r)/2)
Q[1, 1] <- sqrt(A[2, 2] %*% (1-r)/2)
alpha <- seq(0, by = (2 * pi)/k, length = k)	   # define angles
Z <- cbind(cos(alpha), sin(alpha)) 	   # points on unit circle
X <- t(m + const * Q %*% t(Z))  # coordinates of points on ellipse
X
}					 # end of procedure ellips

# call procedure ellips


X <- ellips()		   
plot(X)

# graph the results

win.graph()
plot(X[,1], X[,2])



A <- matrix(c(1,1,1,2), ncol = 2)		 # define matrix A
m <- c(3, 4)					 # define vector m
const <- 1					 # define constant 
k <- 1000		  # define number of points on the ellipse

# procedure ELLIPS
# procedure ELLIPS
X <- ellips(A, m, const, k)		   

# graph the results

win.graph()
plot(X[,1], X[,2])


#  procedure UNORMIX  
#  procedure UNORMIX  

# input:   y       N-vector of observed data
#          prior   2-vector of initial prior probabilities
#          mu      2-vector of initial means
#          var     2-vector of initial variances
#          pool    indicator for equality of variances:
#                  pool<-0: unequal variances; pool<-1: equal variances

#  output  prior   final initial probabilities
#          mu      final means
#          var     final variances
#          loglik  value of (complete data) log-likelihood
#          nit     number of iterations of EM-algorithm


unormix <- function(y, prior, mu, var, pool)
{
N <- dim(y)[1]                                     # number of observations 
eps <- 10^(-10)                                   # convergence criterion 
change <- 1                           # initial test value for convergence 
maxiter <- 1000                             # maximum number of iterations 
nit <- 0                                    # initialize iteration counter 
param <- c(prior, mu, var)            # arrange all parameters in a vector 
post <- matrix(0, N, 2)		# open matrix for posterior probabilities
while (change > eps && nit <= maxiter)             # start iterations 
{
options(digits = 6)	    # format for the display of numerical results
parold <- param                               # store old parameter values 
f1 <- exp(-0.5 * (y - mu[1])^2 / var[1] ) / sqrt(2*pi*var[1])   # evaluate 
f2 <- exp(-0.5 * (y - mu[2])^2 / var[2] ) / sqrt(2*pi*var[2])  # component 
f <- prior[1] * f1 + prior[2] * f2       # densities and mixture density 
loglik <- sum(log(f))                  # evaluate log-likelihood function 

# comment next line out if you don't want a protocol of the algorithm 
cat(format(c(nit, param, loglik), justify = "right"), fill = T)
post[,1] <- (prior[1] * f1) / f
post[,2] <- (prior[2] * f2) / f
prior <- apply(post, 2, mean)              # M-step: prior probabilities 

mu <- t(post) %*% y
mu[1] <- mu[1] / (N * prior[1])				# M-step: means 
mu[2] <- mu[2] / (N * prior[2])				
var1 <- t(post[, 1]) %*% (y-mu[1])^2 / (N*prior[1]) # M-step: variances 
var2 <- t(post[, 2]) %*% (y-mu[2])^2 / (N*prior[2])
var <- c(var1, var2)
							
if (pool ==1)
{
var1 <- prior[1] * var1 + prior[2] * var2
var2 <- var1						# M-step: common variance 
}
param <- c(prior, mu, var)              # arrange all parameters in a vector 
change <- max(abs(param - parold))           # test value for convergence 
nit <- nit + 1                               # increase interation counter 
}
if (nit >= maxiter)
cat("algorithm did not converge in ")   # warning message if convergence 
maxiter                        			    # is not reached 
" iterations"
results <- list("prior" = prior, "mu" = mu, "var " = var,
"loglik" = loglik, "nit" = nit)
results
}

#  end of procedure UNORMIX  
#  end of procedure UNORMIX



#### example

##wingfreq <- matrix(scan("c:\\multidat\\tab1_3.dat"), ncol = 2, byrow = T)
wingfreq <- pipits

wing <- wingfreq[, 1]
freq <- wingfreq[, 2]
y <- wing[1] * matrix(1, freq[1], 1)
i <- 2
while (i <= 17)
{
if (freq[i] != 0)
y <- rbind(y, wing[i] * matrix(1, freq[i], 1))
i <- i + 1
}
##cat(y, file = "birdwing.dat", append = F)

 prior <- rbind(.2,.8)

##Next, assign initial means and variances:

mu <- rbind(84,96)
var <- rbind(10,10)
pool <- 0
results <- unormix(y, prior, mu, var, pool)



## and a print method is needed.

> cat("prior probabilities: ", t(results$prior), "\n")
> cat("means: ", t(results$mu), "\n")
> cat("variances: ", t(results$var), "\n")
> cat("standard deviations: ", sqrt(t(results$var)), "\n")
> cat("log-likelihood: ", results$loglik, "\n")
> cat("# iterations: ", results$nit, "\n")


############## bootstrapping

##rawdat <- matrix(scan("c:\\multidat\\tab1_1.dat"), ncol=3,
	  byrow = T)

rawdat <- midge
D <- 100 * rawdat[1:9, 2:3]	               # data of Af midges
N <- 9				   	  # number of observations
B <- 10000     		     	     # number of bootstrap samples
boot <- function()			    # define function boot
{
results <- matrix(0, B, 2)     # open matrix for bootstrap results
it <- 1					    # start bootstrap loop
while(it <= B) {
	items <- ceiling(runif(N, min = 0, max = 1) * N)  
			    # indices of items in bootstrap sample

	Dboot <- D[items, ] 	       # generate bootstrap sample
	ave <- apply(Dboot, 2, mean) 
			 # compute statistic from bootstrap sample

	results[it,  ] <- t(ave) 		   # store results
	it <- it + 1			   # end of bootstrap loop
	##cat(format(results, justify = "right"), file = "boot.out",
	##fill = 25)				   # store results
	}
return(results)
}

results <- boot()					 # run the function "boot"
##results <- matrix(scan("boot.out"), ncol = 2, byrow = T)
					  # read results in S-PLUS

sd.x <- function(x) sqrt(var(x)) 	# define the function sd.x
stderr <- apply(results, 2, sd.x) 			  # use it
cat("\nstandard errors are\n", stderr, file = "boot.out",
append = T)					   # store results


sd.x <- function(x) sqrt(var(x)) 	# define the function sd.x
stderr <- apply(results, 2, sd.x) 			  # use it
cat("\nstandard errors are\n", stderr)
##, file = "boot.out",
##append = T)





## an em algorithm for the sake of having one 4.4


y1 <- 5					    # observed data values
y2 <- 3
y3 <- 2
y23 <- 10               
N <- y1 + y2 + y3 + y23
theta <- c(0.6, 0.1, 0.3)	     # initialize parameter values
iter <- 0				   # set iteration counter
epsilon <- 10^(-6)		       # set convergence criterion
di <- 1
	 # set test value for convergence to some number > epsilon

while (di > epsilon) 				# start iterations
{
ellstar <- (y1 * log(theta[1]) + y23 * log(1 - theta[1])
+ y2 * log(theta[2]) + y3 * log(theta[3]))
		   # compute value of observed-data log-likelihood

cat(format(c(iter, t(theta), ellstar), justify = "right"),
fill = 60)	   # display iteration number and parameter values

# E-step

Ex2 <- y23 * (theta[2] / (theta[2] + theta[3]))
					    # expected value of x2

Ex3 <- y23 * (theta[3] / (theta[2] + theta[3]))
					    # expected value of x3

# M-step

thetaold <- theta 		      # store old parameter values
theta[1] <- y1 / N 				   # update theta1
theta[2] <- (y2 + Ex2) / N 			   # update theta2
theta[3] <- (y3 + Ex3) / N 			   # update theta3

di <- max(abs(theta - thetaold))	      # compute test value
iter <- iter + 1 		      # increase iteration counter
} 						# end of iteration

#-------------------------------------------------------------------------------------------------------------------
# Author		: Marcel Goic
# Usage			: source("yogurt logit - v01.r", print.eval=TRUE)
# Description	: Maximum Likelihood Estimation for a logit model of yogurt brand choice
#						    - v.0.0 (04/20/2011). simplest model
#						    - v.0.1 (06/02/2014). compare against mlogit
# Notes			: - Educational purpose (code not optimized for computational efficiency nor scalability)
#						  - Explanatory Variables: Intercept, price, feature
# 						- Heterogeneity model: None
#-------------------------------------------------------------------------------------------------------------------

rm(list=ls())       # clear the list of objects
graphics.off()			# clear the list of graphs
options(digits = 3)	# number of digits to display
library(mlogit)			# load package to compare

### READ AND FORMAT THE DATA ---------------------------------------------------------------------------------------

paneldata <- read.table(file="./Tarea2/Class_15_Estimation_of_logit_and_probit/yogurt data.txt", header=TRUE, sep="")		#complete data file loaded in a matrix*/

idcust <- paneldata[,1]							# idcust is the sub matrix containing all rows and the first column (IdPanel)
choice <- paneldata[,2:5]						# choice is the sub matrix containing all rows and the 07-10 column (1 if bought the brand)
featur <- paneldata[,6:9]						# featur is the sub matrix containing all rows and the 11-14 column (Feature 4 brands)
prices <- paneldata[,10:13]					# prices is the sub matrix containing all rows and the 15-18 column (Price 4 brands)

n		 <- nrow(paneldata)						# returns the number of rows in the complete matrix
ncus <- length(unique(idcust))		# returns number of customers in the panel
nbra <- ncol(choice)							# returns number of brands in the set
ones <- rep(1,n)	          			# a column vector of n ones, one for each row (useful for intercepts)

yoplai <- cbind(ones,prices[,1],featur[,1])		# matrix for yop-brand: concatenation of ones, prices and features     
danone <- cbind(prices[,2],featur[,2])			    # matrix for dan-brand: concatenation of ones, prices and features
hiland <- cbind(ones,prices[,3],featur[,3])		# matrix for hil-brand: concatenation of ones, prices and features
wwatch <- cbind(ones,prices[,4],featur[,4])		# matrix for wwt-brand: prices and features, asumes constant 0

### FUNCTION DEFINITION ---------------------------------------------------------------------------------------------

loglikel <- function(beta){
		# log likelihood function

	expVyop <- exp(yoplai %*% beta[c(1,4:5)] )		# expUyop is the exponential of the utility of yoplait-brand
	expVdan <- exp(danone %*% beta[c(4:5)] )			# expUdan is the exponential of the utility of danone-brand
	expVhil <- exp(hiland %*% beta[c(2,4:5)] )		# expUhil is the exponential of the utility of hiland-brand
	expVwwt <- exp(wwatch %*% beta[c(3,4:5)] )		# expUwwt is the exponential of the utility of wweight watchers-brand

	denom <- expVyop+expVdan+expVhil+expVwwt		# sum exponential utilities, we need it as the denominador of purchase probability

	Pryop <- expVyop / denom								# purchase probability of yop-bran
	Prdan <- expVdan / denom								# purchase probability of dan-bran
	Prhil <- expVhil / denom								# purchase probability of hil-bran
	Prwwt <- expVwwt / denom								# purchase probability of wwt-bran

	# individual level contribution to the likelihood
	lli  <- log( (Pryop*choice[,1])+(Prdan*choice[,2])+(Prhil*choice[,3])+(Prwwt*choice[,4]) ) 
	# total likelihood - sum over all individual contributions
	return(-sum(lli))        						
}

### LIKELIHOOD MAXIMIZATION ---------------------------------------------------------------------------------------------

beta.start <- 0.01*rep(1,5)
mymle <- optim(par=beta.start, fn=loglikel, hessian=TRUE, method="BFGS", control = list(maxit=1000, trace=TRUE, REPORT=10))

# very basic reporting
mle <- mymle$par
se  <- sqrt(diag(solve(mymle$hessian)))
cbind(mle,se)

### REPORTING -----------------------------------------------------------------------------------------------------------

source("report.basic.r", print.eval=TRUE)				# generate a basic set of reports: 
source("report.fit.r", print.eval=TRUE)					# analize the fit of the model
source("report.simulation.r", print.eval=TRUE)			# analize market shares in several situations

### MLOGIT ---------------------------------------------------------------------------------------------------------------

# format the data according to what mlogit requires
tmpchoice <- apply(choice,1,which.max)
mychoice <- array(NA, dim=nrow(paneldata))

for(i in 1:nrow(paneldata)){
	if (tmpchoice[i] == 1) mychoice[i]="yoplait"
	if (tmpchoice[i] == 2) mychoice[i]="danone" 
	if (tmpchoice[i] == 3) mychoice[i]="hiland" 
	if (tmpchoice[i] == 4) mychoice[i]="weightw"	
}
rm(tmpchoice)
levels(mychoice) <- c("yoplait", "danone", "hiland", "weightw")

myyogurt <- data.frame(ch=mychoice, 
					  price.yoplait=prices[,1], price.danone=prices[,2], price.hiland=prices[,3], price.weightw=prices[,4],
					  featr.yoplait=featur[,1], featr.danone=featur[,2], featr.hiland=featur[,3], featr.weightw=featur[,4])

myfrmyogurt <- mlogit.data(myyogurt, shape="wide", varying=2:9, choice="ch")		#format the data

#run logit model
cat("mlogit - logit\n")
yogurtlogit <- mlogit(ch ~ price + featr, data = myfrmyogurt)					        
summary(yogurtlogit)

#run probit model
cat("mlogit - probit\n")
yogurtprobit <- mlogit(ch ~ price + featr, data = myfrmyogurt, probit = TRUE, print.level=1)					        
summary(yogurtprobit)

# by default mlogit fix the null utility for the alternative with largest market share, 
# in this case is danone. If you fix any other alternative as the baseline you might need
# to take differences to recover what mlogit is doing. 

# probit takes a while. by adding print.level you can check the evolution
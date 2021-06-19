#-------------------------------------------------------------------------------------------------------------------
# Author			: Marcel Goic
# Usage			: source("report.fit.r", print.eval=TRUE)
# Description	: Model fit Reporting for a logit model of yogurt brand choice
#						- v.0.0 (04/29/2011). show the fit of the model 
# Notes			:  - Educational purpose (code not optimized for computational efficiency nor scalability)
#-------------------------------------------------------------------------------------------------------------------

# rho, bic, aic
npar = length(mle)	
rho = 1 - ( -mymle$value / -loglikel(beta=rep(0,npar)) )	;	cat("\nrho =", rho, "\n")
aic = -2*(-mymle$value) + 2*length(npar)						;	cat("aic =", aic, "\n")
bic = -2*(-mymle$value) + length(npar)*log(n)				;	cat("bic =", bic, "\n\n")
	
# Boxplots
expUyop = exp(yoplai %*% mle[c(1,4:5)] )		# expUyop is the exponential of the utility of yoplait-brand
expUdan = exp(danone %*% mle[4:5] )				# expUdan is the exponential of the utility of danone-brand
expUhil = exp(hiland %*% mle[c(2,4:5)] )		# expUhil is the exponential of the utility of hiland-brand
expUwwt = exp(wwatch %*% mle[c(3,4:5)] )		# expUwwt is the exponential of the utility of wweight watchers-brand
denom  = expUyop+expUdan+expUhil+expUwwt		# sum exponential utilities, we need it as the denominador of purchase probability

Pryop = expUyop / denom								# purchase probability of yop-bran
Prdan = expUdan / denom								# purchase probability of dan-bran
Prhil = expUhil / denom								# purchase probability of hil-bran
Prwwt = expUwwt / denom								# purchase probability of wwt-bran

fityoplai = list('No Purchase' = Pryop[ choice[,1]==0 ], 'Purchase' = Pryop[ choice[,1]==1 ] )
fitdanone = list('No Purchase' = Prdan[ choice[,2]==0 ], 'Purchase' = Prdan[ choice[,2]==1 ] )
fithiland = list('No Purchase' = Prhil[ choice[,3]==0 ], 'Purchase' = Prhil[ choice[,3]==1 ] )
fitwwatch = list('No Purchase' = Prwwt[ choice[,4]==0 ], 'Purchase' = Prwwt[ choice[,4]==1 ] )

x11(width=4, height=5);	boxplot(fityoplai, main = "yoplait", ylim=c(0,1))
x11(width=4, height=5);	boxplot(fitdanone, main = "danone", ylim=c(0,1))
x11(width=4, height=5);	boxplot(fithiland, main = "hiland", ylim=c(0,1))
x11(width=4, height=5);	boxplot(fitwwatch, main = "weight watchers", ylim=c(0,1))



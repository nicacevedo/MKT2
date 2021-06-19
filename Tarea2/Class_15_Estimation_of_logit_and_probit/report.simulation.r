#-------------------------------------------------------------------------------------------------------------------
# Author			: Marcel Goic
# Usage			: source("report.simulation.r", print.eval=TRUE)
# Description	: Compute some simulations on the logit model 
#						- v.0.0 (04/29/2011). Descriptive statistics of the Data and 
# Notes			:  - Educational purpose (code not optimized for computational efficiency nor scalability)
#-------------------------------------------------------------------------------------------------------------------

# compute market share for average prices and no feature

prices.mean = apply(prices, 2, mean)
intercepts  = c(mle[1],0, mle[2:3])

exp.ubase = array(NA, dim=nbra)
exp.udecr = array(NA, dim=nbra)

for (b in 1:nbra){
	exp.ubase[b] = exp( intercepts[b] + mle[4]*prices.mean[b])
	exp.udecr[b] = exp( intercepts[b] + mle[4]*0.9*prices.mean[b])
}
share.base = exp.ubase / sum(exp.ubase)
pie(share.base, labels=c("yoplait", "danone", "hiland", "weigth watchers"), main="Market Share for Average Prices")

# compute variation of market share for brand i when brand j decreases prices 10%

share.delta = array(NA, dim=c(nbra,nbra))		# variation of market shares

for (b1 in 1:nbra){
	exp.unewc = exp.ubase															# all equals	
	exp.unewc[b1] = exp.udecr[b1]													# except b1
	share.delta[b1,] = ( exp.unewc/sum(exp.unewc) ) - share.base 		#compute diference
}

cat("variation of market share for brand i when brand j decreases prices 10% \n")
print(share.delta)
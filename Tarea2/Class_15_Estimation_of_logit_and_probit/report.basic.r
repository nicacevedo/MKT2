#-------------------------------------------------------------------------------------------------------------------
# Author			: Marcel Goic
# Usage			: source("report.basic.r", print.eval=TRUE)
# Description	: Basic Resporting for a logit model of yogurt brand choice
#						- v.0.0 (04/29/2011). Descriptive statistics of the Data and 
# Notes			:  - Educational purpose (code not optimized for computational efficiency nor scalability)
#-------------------------------------------------------------------------------------------------------------------

# histogram number of times that each customer buys
nbuy = array(NA, dim=ncus)
for (i in 1:ncus){
	nbuy[i] = length(idcust[ idcust == i])
}
hist(nbuy, main="Number or Purchase Occasions", xlab="number of occasions")

# empirical market shares, feature frecuency and prices
cat("brand, share, feature, price, price paid, \n", file="descstats.csv", append=FALSE)
cat("yoplait,", mean(choice[,1]), ",", mean(featur[,1]), ",", mean(prices[,1]), ",", mean(prices[ choice[,1] == 1, 1 ]), "\n", file="descstats.csv", append=TRUE)
cat("danone ,", mean(choice[,2]), ",", mean(featur[,2]), ",", mean(prices[,2]), ",", mean(prices[ choice[,2] == 1, 2 ]), "\n", file="descstats.csv", append=TRUE)
cat("hiland ,", mean(choice[,3]), ",", mean(featur[,3]), ",", mean(prices[,3]), ",", mean(prices[ choice[,3] == 1, 3 ]), "\n", file="descstats.csv", append=TRUE)
cat("wwatch ,", mean(choice[,4]), ",", mean(featur[,4]), ",", mean(prices[,4]), ",", mean(prices[ choice[,4] == 1, 4 ]), "\n", file="descstats.csv", append=TRUE)

# write a file with maximum likelihood estimators and their standar deviations
cat("Maximum Likelihood Estimators \n\n", file="mle.txt", append=FALSE)
cat("intercept.yoplait  \t", mle[1], "(", se[1], ")\n", file="mle.txt", append=TRUE)
cat("intercept.danone   \t", mle[2], "(", se[2], ")\n", file="mle.txt", append=TRUE)
cat("intercept.hiland   \t", mle[3], "(", se[3], ")\n", file="mle.txt", append=TRUE)
cat("price coefficient 	\t", 	mle[4], "(", se[4], ")\n", file="mle.txt", append=TRUE)
cat("feature coefficient\t",  mle[5], "(", se[5], ")\n", file="mle.txt", append=TRUE)


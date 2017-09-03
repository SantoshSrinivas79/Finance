require(quantmod)
require(PerformanceAnalytics)
 
stepwiseCorRank <- function(corMatrix, startNames=NULL, stepSize=1, bestHighestRank=FALSE) {
  #edge cases
  if(dim(corMatrix)[1] == 1) {
    return(corMatrix)
  } else if (dim(corMatrix)[1] == 2) {
    ranks <- c(1.5, 1.5)
    names(ranks) <- colnames(corMatrix)
    return(ranks)
  }
   
  if(is.null(startNames)) {
    corSums <- rowSums(corMatrix)
    corRanks <- rank(corSums)
    startNames <- names(corRanks)[corRanks <= stepSize]
  }
  nameList <- list()
  nameList[[1]] <- startNames
  rankList <- list()
  rankCount <- 1
  rankList[[1]] <- rep(rankCount, length(startNames))
  rankedNames <- do.call(c, nameList)
   
  while(length(rankedNames) < nrow(corMatrix)) {
    rankCount <- rankCount+1
    subsetCor <- corMatrix[, rankedNames]
    if(class(subsetCor) != "numeric") {
      subsetCor <- subsetCor[!rownames(corMatrix) %in% rankedNames,]
      if(class(subsetCor) != "numeric") {
        corSums <- rowSums(subsetCor)
        corSumRank <- rank(corSums)
        lowestCorNames <- names(corSumRank)[corSumRank <= stepSize]
        nameList[[rankCount]] <- lowestCorNames
        rankList[[rankCount]] <- rep(rankCount, min(stepSize, length(lowestCorNames)))
      } else { #1 name remaining
        nameList[[rankCount]] <- rownames(corMatrix)[!rownames(corMatrix) %in% names(subsetCor)]
        rankList[[rankCount]] <- rankCount
      }
    } else {  #first iteration, subset on first name
      subsetCorRank <- rank(subsetCor)
      lowestCorNames <- names(subsetCorRank)[subsetCorRank <= stepSize]
      nameList[[rankCount]] <- lowestCorNames
      rankList[[rankCount]] <- rep(rankCount, min(stepSize, length(lowestCorNames)))
    }    
    rankedNames <- do.call(c, nameList)
  }
   
  ranks <- do.call(c, rankList)
  names(ranks) <- rankedNames
  if(bestHighestRank) {
    ranks <- 1+length(ranks)-ranks
  }
  ranks <- ranks[colnames(corMatrix)] #return to original order
  return(ranks)
}
 
 
FAAreturns <- function(prices, monthLookback = 4,
                       weightMom=1, weightVol=.5, weightCor=.5, 
                       riskFreeName="VFISX", bestN=3,
                       stepCorRank = FALSE, stepStartMethod=c("best", "default")) {
  stepStartMethod <- stepStartMethod[1]
  returns <- Return.calculate(prices)
  monthlyEps <- endpoints(prices, on = "months")
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  tmp <- list()
  dates <- list()
   
  for(i in 2:(length(monthlyEps) - monthLookback)) {
     
    #subset data
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthLookback],]
     
    #perform computations
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    priceData <- priceData[, momentum > 0] #remove securities with momentum < 0
    returnsData <- returnsData[, momentum > 0]
    momentum <- momentum[momentum > 0]
    names(momentum) <- colnames(returnsData)
    vol <- as.numeric(-sd.annualized(returnsData))
     
    if(length(momentum) > 1) {
       
      #perform ranking
      if(!stepCorRank) {
        sumCors <- -colSums(cor(returnsData, use="complete.obs"))
        stats <- data.frame(cbind(momentum, vol, sumCors))
        ranks <- data.frame(apply(stats, 2, rank))
        weightRankSum <- weightMom*ranks$momentum + weightVol*ranks$vol + weightCor*ranks$sumCors
        names(weightRankSum) <- rownames(ranks)
      } else {
        corMatrix <- cor(returnsData, use="complete.obs")
        momRank <- rank(momentum)
        volRank <- rank(vol)
        compositeMomVolRanks <- weightMom*momRank + weightVol*volRank
        maxRank <- compositeMomVolRanks[compositeMomVolRanks==max(compositeMomVolRanks)]
        if(stepStartMethod=="default") {
          stepCorRanks <- stepwiseCorRank(corMatrix=corMatrix, startNames = NULL, 
                                          stepSize = 1, bestHighestRank = TRUE)
        } else {
          stepCorRanks <- stepwiseCorRank(corMatrix=corMatrix, startNames = names(maxRank), 
                                          stepSize = 1, bestHighestRank = TRUE)
        }
        weightRankSum <- weightMom*momRank + weightVol*volRank + weightCor*stepCorRanks
      }
       
      totalRank <- rank(weightRankSum)
       
      #find top N values, from http://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column
      #thanks to Dr. Rob J. Hyndman
      upper <- length(names(returnsData))
      lower <- max(upper-bestN+1, 1)
      topNvals <- sort(totalRank, partial=seq(from=upper, to=lower))[c(upper:lower)]
       
      #compute weights
      longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
      longs <- longs/sum(longs) #equal weight all candidates
      longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
      names(longs) <- names(totalRank)
       
    } else if(length(momentum) == 1) { #only one security had positive momentum 
      longs <- 1/bestN
      names(longs) <- names(momentum)
    } else { #no securities had positive momentum 
      longs <- 1
      names(longs) <- riskFreeName
    }
     
    #append removed names (those with momentum < 0)
    removedZeroes <- rep(0, ncol(returns)-length(longs))
    names(removedZeroes) <- names(returns)[!names(returns) %in% names(longs)]
    longs <- c(longs, removedZeroes)
     
    #reorder to be in the same column order as original returns/prices
    longs <- data.frame(t(longs))
    longs <- longs[, names(returns)]
     
    #append lists
    tmp[[i]] <- longs
    dates[[i]] <- index(returnsData)[nrow(returnsData)]
  }
   
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = FALSE)
  colnames(strategyReturns) <- paste(monthLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
}
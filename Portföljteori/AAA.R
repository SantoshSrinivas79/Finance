weightMom <- seq(0, 1, by=.5)
weightVol <- c(0, .5, 1)
weightCor <- c(0, .5, 1)
monthLookback=c(3, 4, 6, 10)
permutations <- expand.grid(weightMom, weightVol, weightCor, monthLookback)
colnames(permutations) <- c("wMom", "wVol", "wCor", "monthLookback")
 
require(doMC)
registerDoMC(detectCores())
t1 <- Sys.time()
out <- foreach(i = 1:nrow(permutations), .combine = cbind) %dopar% {
  FAAreturns(prices=adPrices, 
             monthLookback = permutations$monthLookback[i], 
             weightMom = permutations$wMom[i], 
             weightCor = permutations$wCor[i], 
             weightVol=permutations$wVol[i])
}
t2 <- Sys.time()
print(t2-t1)
 
out <- out["1998-10::"] #start at 1999 due to NAs with data
 
FAAwalkForward <- function(portfolios, applySubset = apply.quarterly, applyFUN = Return.cumulative) {
  metrics <- applySubset(portfolios, applyFUN)
  weights <- list()
  for(i in 1:nrow(metrics)) {
    row <- metrics[i,]
    winners <- row==max(row)
    weight <- winners/rowSums(winners) #equal weight all top performers
    weights[[i]] <- weight
  }
  weights <- do.call(rbind, weights)
  returns <- Return.rebalancing(portfolios, weights)
  return(returns)
}
 
WFQtrRets <- FAAwalkForward(portfolios = out, applySubset = apply.quarterly, applyFUN = Return.cumulative)
WFYrRets <- FAAwalkForward(portfolios = out, applySubset = apply.yearly, applyFUN = Return.cumulative)
WFMoRets <- FAAwalkForward(portfolios = out, applySubset = apply.monthly, applyFUN = Return.cumulative)
 
WF <- cbind(WFQtrRets, WFYrRets, WFMoRets)
colnames(WF) <- c("quarterly", "annually", "monthly")
WF <- WF["1999::"]
 
original <- FAAreturns(adPrices)
original <- original["1999::"]
WF <- cbind(WF, original)
colnames(WF)[4] <- "original"
charts.PerformanceSummary(WF)
 
 
Return.annualized(WF)
maxDrawdown(WF)
SharpeRatio.annualized(WF)
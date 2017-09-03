mutualFunds <- c("VTSMX", #Vanguard Total Stock Market Index
                 "FDIVX", #Fidelity Diversified International Fund
                 "VEIEX", #Vanguard Emerging Markets Stock Index Fund
                 "VFISX", #Vanguard Short-Term Treasury Fund
                 "VBMFX", #Vanguard Total Bond Market Index Fund
                 "QRAAX", #Oppenheimer Commodity Strategy Total Return 
                 "VGSIX" #Vanguard REIT Index Fund
)
 
#mid 1997 to end of 2012
getSymbols(mutualFunds, from="1997-06-30", to="2012-12-31")
tmp <- list()
for(fund in mutualFunds) {
  tmp[[fund]] <- Ad(get(fund))
}
 
#always use a list hwne intending to cbind/rbind large quantities of objects
adPrices <- do.call(cbind, args = tmp)
colnames(adPrices) <- gsub(".Adjusted", "", colnames(adPrices))
 
original <- FAAreturns(adPrices, stepCorRank=FALSE)
originalSWCbest <- FAAreturns(adPrices, stepCorRank=TRUE)
originalSWCdefault <- FAAreturns(adPrices, stepCorRank=TRUE, stepStartMethod="default")
stepMaxDecorBest <- FAAreturns(adPrices, weightMom=.05, weightVol=.025, 
                               weightCor=1, riskFreeName="VFISX", 
                               stepCorRank = TRUE, stepStartMethod="best")
stepMaxDecorDefault <- FAAreturns(adPrices, weightMom=.05, weightVol=.025, 
                                  weightCor=1, riskFreeName="VFISX", 
                                  stepCorRank = TRUE, stepStartMethod="default")
w311 <- FAAreturns(adPrices, weightMom=3, weightVol=1, weightCor=1, stepCorRank=TRUE)
originalMaxDecor <- FAAreturns(adPrices, weightMom=0, weightVol=1, stepCorRank=FALSE)
tmp <- cbind(original, originalSWCbest, originalSWCdefault, 
             stepMaxDecorBest, stepMaxDecorDefault, w311, originalMaxDecor)
names(tmp) <- c("original", "originalSWCbest", "originalSWCdefault", "SMDB", 
                "SMDD", "w311", "originalMaxDecor")
charts.PerformanceSummary(tmp, colorset=c("black", "orange", "blue", "purple", "green", "red", "darkred"))
 
 
statsTable <- data.frame(t(rbind(Return.annualized(tmp)*100, maxDrawdown(tmp)*100, SharpeRatio.annualized(tmp))))
statsTable$ReturnDrawdownRatio <- statsTable[,1]/statsTable[,2]
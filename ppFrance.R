################################# PREPROCESSING #########################################################
pp = FALSE
# PP France
if (pp == TRUE){
  # Need to infer quarterly EL, FR and PT
  dataQ["FR.EL.PT"] = dataQ[,"EA12"] -rowSums(dataQ[,-c(1,2,4,7,9,14,17,16)])
  
  # Convert into Time Series
  startQ = c(as.numeric(substr(rownames(dataQ)[1], 1, 4)), as.numeric(substr(rownames(dataQ)[1], 6, 6)))
  startAnn = c(as.numeric(rownames(dataAnn)[1]))
  
  dataQ = ts(dataQ, frequency = 4, start = startQ)
  dataAnn = ts(dataAnn, frequency = 1, start = startAnn)
  
  # Now, separate the aggregate into separate countries by weighting according
  # to the annual observations
  weights <- dataAnn[,c("FR","EL","PT")] / rowSums(dataAnn[,c("FR","EL","PT")])
  
  for (years in time(dataAnn)){
    k = which(time(dataQ) == years)
    j = which(time(dataAnn) == years)
    if (length(k)==1){
      dataQ[k:(k+3),"FR"] <- dataQ[k:(k+3),"FR.EL.PT"] * weights[j,"FR"]
      dataQ[k:(k+3),"EL"] <- dataQ[k:(k+3),"FR.EL.PT"] * weights[j,"EL"]
      dataQ[k:(k+3),"PT"] <- dataQ[k:(k+3),"FR.EL.PT"] * weights[j,"PT"]
    }
  }
  
  # Remove FR.EL.PT
  dataQ = dataQ[,-18]
  
  ### Infer quarterly observations for Finland
  FI.annual <-dataAnn[,"FI"]
  ts.plot(FI.annual)
  k <- which(time(FI.annual) == 1989)
  
  FI.annual.f <- window(FI.annual, end = 1989)
  
  FI.q        <- ts(rev(dataQ[-(1:60),"FI"]), frequency = 4)
  FI.q.f      <- forecast(arima(
    FI.q,
    order = c(1,0,0),
    seasonal = c(1,0,0)
  ), h = 10*4)$mean
  
  # Define hierarchy
  FI.q.f <- sepQ(FI.q.f, type = "matrix")
  FI.a <- cbind(FI.annual.f[which(!is.na(FI.annual.f))], FI.q.f)
  
  nodes = list(4)
  
  FI.f <- c(t(TdFp(FI.a,nodes)))
  
  write.xlsx(FI.f, file = "FI_q.xlsx")
  
  ### Infer quarterly observations for Denmark
  DK.annual <-dataAnn[,"DK"]
  ts.plot(DK.annual)
  k <- which(time(DK.annual) == 1994)
  
  DK.annual.f <- window(DK.annual, end = 1994)
  
  DK.q        <- ts(rev(dataQ[-(1:80),"DK"]), frequency = 4)
  DK.q.f      <- forecast(arima(
    DK.q,
    order = c(1,0,0),
    seasonal = c(1,0,0),
    xreg = time(DK.q)
  ), h = 20*4, xreg = seq(from = 24, by = 0.25, length = 20*4))$mean
  
  # Define hierarchy
  DK.q.f <- sepQ(DK.q.f, type = "matrix")
  DK.a <- cbind(DK.annual.f[which(!is.na(DK.annual.f))], DK.q.f)
  
  nodes = list(4)
  
  DK.f <- c(t(TdFp(DK.a,nodes)))
  
  write.xlsx(DK.f, file = "DK_q.xlsx")
  
  ### Infer quarterly observations for France
  FR.annual <-dataAnn[,"FR"]
  ts.plot(FR.annual)
  k <- which(time(FR.annual) == 1994)
  
  FR.annual.f <- window(FR.annual, end = 1994)
  
  FR.q        <- ts(rev(dataQ[-(1:80),"FR"]), frequency = 4)
  FR.q.f      <- forecast(arima(
    FR.q,
    order = c(1,0,0),
    seasonal = c(1,0,0),
    xreg = time(FR.q)
  ), h = 20*4, xreg = seq(from = 24, by = 0.25, length = 20*4))$mean
  
  # Define hierarchy
  FR.q.f <- sepQ(FR.q.f, type = "matrix")
  FR.a <- cbind(FR.annual.f[which(!is.na(FR.annual.f))], FR.q.f)
  
  nodes = list(4)
  
  FR.f <- c(t(TdFp(FR.a,nodes)))
  
  write.xlsx(FR.f, file = "FR_q.xlsx")
  
}
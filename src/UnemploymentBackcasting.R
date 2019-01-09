######################################################
############# UNEMPLOYMENT BACKCASTING #################
######################################################
# rewrite but dont use gts function type data for forecasts - use oroginal data because of rounding errors
# Filename locations
library(openxlsx)
library(rlist)
library(hts)
source("backcastingFunctions.R")
filename = "../data/unemployment.xlsx"


# Read in the data
dataQ = read.xlsx(
  xlsxFile = filename,
  colNames = T,
  rowNames = T,
  sheet    = 2
)

Q.rownames = rownames(dataQ)

dataAnn = read.xlsx(
  xlsxFile = filename,
  colNames = T,
  rowNames = T,
  sheet    = 1
)
A.rownames = rownames(dataAnn)

dataAnn = 12 * dataAnn

# Load region defs
load("data/EUregions.dat")

source("backcastingFunctions.R")

###########################################################################################################
# We have 15 countries plus EU15 and EA12
###########################################################################################################
# Convert into Time Series
startQ = c(as.numeric(substr(rownames(dataQ)[1], 1, 4)), as.numeric(substr(rownames(dataQ)[1], 7, 7)))
startAnn = c(as.numeric(rownames(dataAnn)[1]))

dataQ = ts(dataQ, frequency = 12, start = startQ)
dataAnn = ts(dataAnn, frequency = 1, start = startAnn)

# Define the hierachical or group structure
countryNum = 15
month = rep(c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12"), each = countryNum)
country = rep(EUregions[["EU15"]],4)

gc <- rbind(month,country)
rownames(gc) <- c("Month", "Country")

# Save EA12
dataQEA12 = dataQ[,c("EA12")]
dataAEA12 = dataAnn[,c("EA12")]

# Drop EA12
dataQ    <- dataQ[,c("EU15",EUregions[["EU15"]])]
dataAnn  <- dataAnn[,c("EU15",EUregions[["EU15"]])]

# Make sure we finish on a complete year and save the last obs
dataQcrop <-  window(dataQ,   start = 2018)

dataQ    <- window(dataQ, end = 2017.917 )
dataAnn  <- window(dataAnn, end = 2017)

# Find the earliest date for which we have data for all countries
dataAv     <- !is.na(dataQ) * 1
dataRow  <- rowSums(dataAv)
startIndexQ <- which(dataRow == dim(dataQ)[2])[1]
startDateQ  <- time(dataQ)[startIndexQ]
startIndexA  <- which(time(dataAnn) == floor(startDateQ)) 

# plot the time series along with earliest date
ts.plot(dataQ)
abline(v = time(dataQ)[startIndexQ], col  = "red")
ts.plot(dataAnn)
abline(v = time(dataAnn)[startIndexA], col = "red")

############################## Backcast a year (four quarters) ahead #################################
# Reverse the time series and backcast the lower level time series
#(Note: these forecacsts need to be done external to the gts structure as to 
# preserve the seasonality)
#################### INITIALISE #############################

# List with all of the data
dataTemp <- c(list(dataAnn[,"EU15"]), list(dataQ[,"EU15"]), split(dataAnn[, EUregions[["EU15"]]], col(dataAnn[, EUregions[["EU15"]]])), split(dataQ[, EUregions[["EU15"]]], col(dataQ[, EUregions[["EU15"]]])))
f        <- c(1,12,rep(1,15), rep(12,15))

models = NULL
library(forecast)

# Reverse all of the time series and make sure we preserve seasonality
for (i in 1:length(dataTemp)){
  dataTemp[[i]] <- ts(rev(dataTemp[[i]]), frequency = f[i])
}

# Either backcast all of the time series or select the truth
years    =  as.vector(sapply(dataTemp, FUN = numNA)/f)
len      =  sapply(dataTemp, FUN = numNAnot)
maxYears = ceiling(max(years))

backcastModels = as.list(rep(NA, length(dataTemp)))
backcasts      = as.list(rep(NA, length(dataTemp)))
for (i in 1:length(dataTemp)) {
  if ( i == 6 || i ==21){
    backcastModels[[i]] <-
      auto.arima(
        window(dataTemp[[i]], end = time(dataTemp[[i]])[len[i]]),
        max.order = 5,
        stepwise = F,
        max.p=5,
        max.q=5,
        max.P=5,
        max.Q=5
      )
  }else if (i == 16){
    backcastModels[[i]] <-
      auto.arima(
        window(dataTemp[[i]], end = time(dataTemp[[i]])[len[i]]),
        max.order = 5,
        stepwise = F,
        max.p=5,
        max.q=5,
        max.P=5,
        max.Q=5,
        allowmean = F
      )
  }else if ( i ==2 ){
    backcastModels[[i]] <-
      auto.arima(
        window(dataTemp[[i]], start = time(dataTemp[[i]])[70],end = time(dataTemp[[i]])[len[i]]),
        stationary = T,
        xreg = time(dataTemp[[i]])[70:len[i]],
        max.order = 5,
        max.p=5,
        max.q=5,
        max.P=5,
        max.Q=5
      )
    }else if ( i == 7 || i == 22){
      backcastModels[[i]] <-
        auto.arima(
          window(dataTemp[[i]], start =time(dataTemp[[i]])[7], end = time(dataTemp[[i]])[len[i]]),
          stationary = T,
          xreg = time(dataTemp[[i]])[7:len[i]],
          max.order = 5,
          stepwise = F,
          max.p=5,
          max.q=5,
          max.P=5,
          max.Q=5,
          lambda = 0,
          biasadj = T
        )
    }else if ( i == 31){
      backcastModels[[i]] <-
        auto.arima(
          window(dataTemp[[i]], end = time(dataTemp[[i]])[len[i]]),
          max.order = 5,
          stepwise = F,
          max.p=5,
          max.q=5,
          max.P=5,
          max.Q=5,
          allowmean = F
        )
    }else{
    backcastModels[[i]] <-
      auto.arima(
        window(dataTemp[[i]], end = time(dataTemp[[i]])[len[i]]),
        stationary = T,
        xreg = time(dataTemp[[i]])[1:len[i]],
        max.order = 5,
        stepwise = F,
        max.p=5,
        max.q=5,
        max.P=5,
        max.Q=5
      )
  }
  if (years[i] != 0 & years[i] !=maxYears){
    if (i == 6 || i == 31 || i ==21){
      backcasts[[i]]      <-
        c(dataTemp[[i]][(len[i]+1-abs(f[i] * (years[i] - maxYears))):(len[i])],
          forecast(
            backcastModels[[i]],
            h = years[i] * f[i]
          )$mean)
    }else if (i == 16){
      backcasts[[i]]      <-
        c(dataTemp[[i]][(len[i]+1-abs(f[i] * (years[i] - maxYears))):(len[i])],
          forecast(
            backcastModels[[i]],
            h = years[i] * f[i]
          )$mean)
    }else{
      backcasts[[i]]      <-
        c(dataTemp[[i]][(len[i]+1-abs(f[i] * (years[i] - maxYears))):(len[i])],
          forecast(
            backcastModels[[i]],
            h = years[i] * f[i],
            xreg = time(dataTemp[[i]])[(len[i] + 1):(len[i] + years[i] * f[i])]
          )$mean)
    }

  } else if (years[i] == 0) {
    backcasts[[i]]      <-
      c(dataTemp[[i]][(len[i]+1-abs(f[i] * (years[i] - maxYears))):(len[i])])
  } else if (years[i] == maxYears) {
    
    backcasts[[i]]      <-  forecast(backcastModels[[i]],
                                       h = years[i] * f[i],
                                       xreg = time(dataTemp[[i]])[(len[i] + 1):(len[i] + years[i] * f[i])])$mean 

  }
}

#backcasts <- lapply(backcasts, round)
# Transform backcasts into correct format
allf <- flattenHier(backcasts, type = "M", h = maxYears)

# Flatten data Temp too
dataTemp2 <- flattenHier(dataTemp, type = "M", h = 35)

# Now determine the weights based on MSE of the forecasts
# Calculate mean square error for all of the forecasts we have done
type   <- c("A","M",rep("A",15), rep("M",15))

mse <- mapply(FUN  = meanSqE, model = backcastModels, type = type)
mse <- flattenHier(mse, h = 1, type = "M")

# weights to send to combinef function
weights <- (1/mse)

library(Matrix)
groups = gc

rownames(groups) <- NULL
gmat <- GmatrixG(groups)
smat <- SmatrixM(gmat)

totalts <- sum(Mlevel(gmat))
S = smat


# Now iterate through each step-ahead and combine the forecasting
data.f <- matrix(NA, nrow = dim(allf)[1], ncol = dim(allf)[2])
start = dim(dataTemp2)[1] - startIndexA + 1


for (i in 1:maxYears){
  k = which(dataTemp2[start+i,]!=0)
  known.values = k
  
  weightsTemp = c(weights)
  weightsTemp[k] <- 10^6
  seqts <- 1:totalts
  weightsTemp <- sparseMatrix(i = seqts, j = seqts, 
                          x = 1/weightsTemp) # diagonal matrix of 1/weights
  
  
  fcasts = allf[i,, drop=F]
  fcasts <- stats::as.ts(fcasts)
  tspx <- stats::tsp(fcasts)
  fcasts <- t(fcasts)
  
  data.f[i,] <- LU(fcasts = fcasts, S=smat, weights = weightsTemp, known.values= known.values)
}


#Put back into orignal format
data.f <- buildHier(data.f, type = "M")

# Append onto original time series ensuring that we don't overwrite the 'truth'
dataFinal = list(rep(NA, 32))
for (i in 1:32) {
  if (years[i] != 0 & years[i] != maxYears) {
    dataFinal[[i]] <-
      c(dataTemp[[i]][1:len[i]], data.f[[i]][((maxYears - years[i]) * f[i] +
                                                1):(maxYears * f[i])])
  } else if (years[i] == 0) {
    dataFinal[[i]] <- dataTemp[[i]]
  } else if (years[i] == maxYears) {
    dataFinal[[i]] <- c(dataTemp[[i]][1:len[i]],data.f[[i]])
  }
}

# Reverse the time series
dataFinal <- lapply(dataFinal, FUN = rev)

# Put back into separate Annual and Quarterly time series
dataFinalA <- do.call("cbind", dataFinal[c(1,3:17)])
dataFinalQ <- do.call("cbind", dataFinal[c(2,18:32)])

# Put back the quarter we chopped off
dataFinalQ <- rbind(dataFinalQ, dataQcrop)

# Put EA12 back
dataFinalA <- cbind(dataFinalA[,1], dataAEA12, dataFinalA[,-1])
dataFinalQ <- cbind(dataFinalQ[,1], dataQEA12, dataFinalQ[,-1])

# Give them collum names again
colnames(dataFinalA) <- colnames(dataFinalQ) <- c("EU15", "EA12",colnames(dataAnn)[-1]) 

# Infer missing EA12
dataFinalA[which(is.na(dataFinalA[,2])),2] <- rowSums(dataFinalA[which(is.na(dataFinalA[,2])), EUregions[["EA12"]]])
dataFinalQ[which(is.na(dataFinalQ[,2])),2] <- rowSums(dataFinalQ[which(is.na(dataFinalQ[,2])), EUregions[["EA12"]]])

# Divide annual back through by 12
dataFinalA <- dataFinalA/12

# Give it row names back
rownames(dataFinalA) <- A.rownames
rownames(dataFinalQ) <- Q.rownames

# Write to Excel
write.xlsx(x = list("Annual" = dataFinalA,"Quarterly" = dataFinalQ), file = "../data/unemploymentBackcast.xlsx", rowNames = T)


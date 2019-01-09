###############################################
######### Retail Trade backcasting ############
###############################################


# Filename locations
library(openxlsx)
library(rlist)
library(hts)
source("backcastingFunctions.R")
filename = "../Database/retailTrade.xlsx"

# Load region defs
load("EUregions.dat")

# Read in the CA data
data = read.xlsx(
  xlsxFile = filename,
  colNames = T,
  rowNames = T,
  sheet    = 1
)

data.rownames = rownames(data)

# Read in the weights
weights.retail <- read.xlsx(
  xlsxFile = filename,
  colNames = T,
  rowNames = F,
  sheet    = 3
)

weights.retail <- weights.retail[colnames(data)[-c(1,2)]]

# Multiply EU15 and EA12 by 100
data[,1:2] <- data[,1:2] * 100

# Convert into Time Series
start = c(as.numeric(substr(rownames(data)[1], 1, 4)), as.numeric(substr(rownames(data)[1], 7, 7)))
data = ts(data, frequency = 12, start = start)

# Define the hierachical structure
nodes = list(15)

# Save EA12
dataEA12 = data[,c("EA12")]

# Drop EA12
data  <- data[,c("EU15",EUregions[["EU15"]])]

# Make sure we finish on a complete year and save the last obs
dataCrop <-  window(data,   start = 2018)
data  <- window(data, end = 2017.917)

# Find the earliest date for which we have data for all countries
dataAv     <- !is.na(data) * 1
dataRow  <- rowSums(dataAv)
startIndex <- which(dataRow == dim(data)[2])[1]
startDate  <- time(data)[startIndex]

# plot the time series along with earliest date
ts.plot(data)
abline(v = time(data)[startIndex], col  = "red")


# List with all of the data
dataTemp <- c(list(data[,"EU15"]), split(data[, EUregions[["EU15"]]], col(data[, EUregions[["EU15"]]])))
f        <- rep(12,16)

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

for (i in 1:length(dataTemp)){
  backcastModels[[i]] <-
    auto.arima(
      window(dataTemp[[i]], start = time(dataTemp[[i]])[108],end = time(dataTemp[[i]])[len[i]]),
      max.order = 5,
      max.p = 5,
      max.q = 5,
      max.P = 5,
      max.Q = 5,
      D = 1
    )
  if (years[i] != 0 & years[i] != maxYears) {
    backcasts[[i]]      <-
      c(dataTemp[[i]][(len[i] + 1 - abs(f[i] * (years[i] - maxYears))):(len[i])],
        forecast(
          backcastModels[[i]],
          h = years[i] * f[i]
        )$mean)
  } else if (years[i] == 0) {
    backcasts[[i]]      <-
      c(dataTemp[[i]][(len[i] + 1 - abs(f[i] * (years[i] - maxYears))):(len[i])])
  } else if (years[i] == maxYears) {
    backcasts[[i]]      <-  forecast(backcastModels[[i]],
                                     h = years[i] * f[i])$mean
  }
}

# Transform backcasts into correct format
allf <- matrix(unlist(backcasts), nrow = maxYears*12)

# Flatten data Temp too
dataTemp2 <- matrix(unlist(dataTemp), ncol = length(dataTemp))

# Now determine the weights based on MSE of the forecasts
# Calculate mean square error for all of the forecasts we have done
mse <- mapply(FUN  = meanSqE, model = backcastModels, type = "A")

# weights to send to combinef function
weights <- (1/mse)

# Get summation matrix
library(Matrix)
gmat <- GmatrixH(nodes)
smat <- SmatrixM(gmat)

# Replace diagonal with the weights for the PEEI
smat[1,] <- as.vector(unlist(weights.retail[,EUregions["EU15"][[1]]][1,]))

totalts <- sum(Mnodes(nodes))
S = smat

# Now iterate through each step-ahead and combine the forecasting
data.f <- matrix(NA, nrow = dim(allf)[1], ncol = dim(allf)[2])
start = dim(dataTemp2)[1] - startIndex + 1

for (i in 1:(maxYears*12)){
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
data.f <- split(data.f, col(data.f))

# Append onto original time series ensuring that we don't overwrite the 'truth'
dataFinal = list(rep(NA, totalts))
for (i in 1:totalts) {
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

# Put into matrix
dataFinal <- do.call("cbind", dataFinal)

# Put back the months we chopped off
dataFinal <- rbind(dataFinal, dataCrop)

# Put EA12 back
dataFinal <- cbind(dataFinal[,1], dataEA12, dataFinal[,-1])

# Give them collum names again
colnames(dataFinal) <- c("EU15", "EA12",EUregions["EU15"][[1]]) 

# Infer missing EA12
dataFinal[which(is.na(dataFinal[, 2])), 2] <- rowSums(sweep(
  dataFinal[which(is.na(dataFinal[, 2])), EUregions[["EA12"]]],
  MARGIN = 2,
  STATS =  unlist(weights.retail[EUregions[["EA12"]]][2,]),
  FUN = "*"))

# Divide EU15 and EA12 back through by 100
dataFinal[,1:2] <- dataFinal[,1:2]/100

# Give it row names back
rownames(dataFinal) <- data.rownames

# Write to Excel
write.xlsx(x = list("CA" = dataFinal), file = "../Database/retailBackcast.xlsx", rowNames = T)

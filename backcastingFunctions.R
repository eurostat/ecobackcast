#############################################
####### Backcasting functions ###############
#############################################

# Separate the quarters
sepQ <- function(data, type  = "list") {
  k  = length(data)
  dataQ1 <- data[seq(1, k, 4)]
  dataQ2 <- data[seq(2, k, 4)]
  dataQ3 <- data[seq(3, k, 4)]
  dataQ4 <- data[seq(4, k, 4)]
  
  if (type == "list") {
    data = list(dataQ1, dataQ2, dataQ3, dataQ4)
  }
  if (type == "matrix") {
    data = cbind(dataQ1, dataQ2, dataQ3, dataQ4)
  }
  return(data)
}
# Separate the months
sepM <- function(data, type = "list") {
  k  = length(data)
  dataM = as.list(rep(NA,12))
  for (i in 1:12){
    dataM[[i]] <- data[seq(i, k, 12)]
  }
  
  if (type == "list") {
    data = dataM
  }
  if (type == "matrix") {
    data = do.call(cbind,dataM)
  }
  return(data)
}
# Calculate the mean squared
meanSq <- function(x) {
  return(mean(x ^ 2, na.rm = T))
}
# Calculate the mean squared error
meanSqE <- function(model, type = "A") {
  if (type == "A") {
    return(meanSq(model$residuals))
  }
  if (type == "Q") {
    #resid <- sepQ(model$residuals)
    #return(sapply(resid, FUN = meanSq))
    return(rep(meanSq(model$residuals),4))
    
  }
  if (type == "M") {
    # resid <- sepM(model$residuals)
    # return(sapply(resid, FUN = meanSq))
    return(rep(meanSq(model$residuals),12))
  }
}

# Collapse list of individual ts into a matrix for hierarchy
flattenHier <- function(data, h, type = "Q") {
  if (type == "Q" && length(data) == 32) {
    fH <- matrix(nrow = h, ncol = 80)
    # Annual EU15
    fH[, 1]   <- data[[1]]
    
    # Quarterly EU15
    fH[, 2:5] <- sepQ(data[[2]], type = "matrix")
    
    # Annnual counties
    fH[, 6:20] <- do.call(cbind, data[3:17])
    
    # Quarterly countries
    for (i in 18:32) {
      data[[i]] <- sepQ(data[[i]], type = "list")
    }
    fH[, 21:35] <- do.call(cbind, lapply(data[18:32], "[[", 1)) #Q1
    fH[, 36:50] <- do.call(cbind, lapply(data[18:32], "[[", 2)) #Q2
    fH[, 51:65] <- do.call(cbind, lapply(data[18:32], "[[", 3)) #Q3
    fH[, 66:80] <- do.call(cbind, lapply(data[18:32], "[[", 4)) #Q4
    
  }
  if (type == "M" && length(data) == 32) {
    fH <- matrix(nrow = h, ncol = 208)
    # Annual EU15
    fH[, 1]   <- data[[1]]
    
    # Monthly EU15
    fH[, 2:13] <- sepM(data[[2]], type = "matrix")
    
    # Annnual counties
    fH[, 14:28] <- do.call(cbind, data[3:17])
    
    # Monthly countries
    for (i in 18:32) {
      data[[i]] <- sepM(data[[i]], type = "list")
    }
    for (i in 1:12){
      fH[, (14 + (i*15)):(13 +(i+1)*15)] <- do.call(cbind, lapply(data[18:32], "[[", i)) #Q1
      
    }
  }
  if (type == "M" && length(data) == 16) {
    fH <- matrix(nrow = h, ncol = 16*12)
    # Monthly EU15
    fH[, 1:12] <- sepM(data[[1]], type = "matrix")
    
    # Monthly countries
    for (i in 2:16) {
      data[[i]] <- sepM(data[[i]], type = "list")
    }
    for (i in 1:12){
      fH[, (-2 + (i*15)):(-3 +(i+1)*15)] <- do.call(cbind, lapply(data[2:16], "[[", i))
    }
  }
  return(fH)
}
# Rebuild into original format - Go from Matrix to list
buildHier <- function(data, type = "Q") {
  if (type == "Q") {
    bH <- list(rep(NA, 32))
    # Annual EU15
    bH[[1]] <- data[, 1]
    
    # Quarterly EU15
    bH[[2]] <- c(t(cbind(data[, 2:5])))
    
    # Anual Countries
    for (i in 3:17) {
      bH[[i]] <- data[, i + 3]
    }
    
    # Quarterly Countries
    for (i in 18:32) {
      bH[[i]] <- c(t(data[, seq(3 + i, 80, 15)]))
    }
  }
  if (type == "M" && dim(data)[2] == 208) {
    bH <- list(rep(NA, 32))
    # Annual EU15
    bH[[1]] <- data[, 1]
    
    # Monthly EU15
    bH[[2]] <- c(t(cbind(data[, 2:13])))
    
    # Anual Countries
    for (i in 3:17) {
      bH[[i]] <- data[, i + 11]
    }
    
    # Monthly Countries
    for (i in 18:32) {
      bH[[i]] <- c(t(data[, seq(11 + i, 208, 15)]))
    }  
  }
  if (type == "M" && dim(data)[2] == 192) {
    bH <- list(rep(NA, 16))

    # Monthly EU15
    bH[[1]] <- c(t(cbind(data[, 1:12])))
    
    # Monthly Countries
    for (i in 2:16) {
      bH[[i]] <- c(t(data[, seq(-4 + i, 208, 15)]))
    }  
  }
  
  return(bH)
}

# Function to return the number of NAs 
numNA <- function(x){
  return(sum(is.na(x)))
}

# Function to return the number of not NAs
numNAnot <- function(x){
  return(sum(!is.na(x)))
}

# Convert to time series from Excel
convertTS <- function(data, type = c("A","Q", "M")){
  if (type == "A"){
    data = ts(data, frequency = 1, start = c(as.numeric(substr(rownames(data)[1], 1, 4))))
  }
  if (type == "Q"){
    data = ts(data, frequency = 4, start = c(as.numeric(substr(rownames(data)[1], 1, 4)), as.numeric(substr(rownames(data)[1], 6, 6))))
  }
  if (type == "M"){
    data = ts(data, frequency = 12, start = c(as.numeric(substr(rownames(data)[1], 1, 4)), as.numeric(substr(rownames(data)[1], 7, 7))))
  }
  return(data)
}

# A function to calculate No. of groups at each level
Mlevel <- function(xgroup) {
  m <- apply(xgroup, 1, function(x) length(unique(x)))
  return(m)
}

# A function to return the NO. of nodes at each level
Mnodes <- function(xlist) {
  m <- c(unlist(lapply(xlist, length)), sum(xlist[[length(xlist)]]))
  return(m)
}
# A function to convert groups to gmatrix
GmatrixG <- function(xmat) {
  if (is.character(xmat)) {
    # Convert character to integer
    gmat <- t(apply(xmat, 1, function(x) as.integer(factor(x, unique(x)))))
  } else {
    gmat  <- xmat
  }
  # Insert the first & last rows
  nc.xmat <- ncol(xmat)
  gmat <- rbind(
    if (all(gmat[1,] == rep(1L, nc.xmat))) NULL else rep(1L, nc.xmat),
    gmat,
    if (all(gmat[NROW(gmat),] == seq(1L, nc.xmat))) NULL else seq(1L, nc.xmat)
  )
  #gmat <- gmat[!duplicated(gmat), , drop = FALSE] # Remove possible duplicated... make smarter above.
  return(structure(gmat, class = "gmatrix"))
}
# A function to convert the nodes list to gmatrix
GmatrixH <- function(xlist) {
  l.xlist <- length(xlist)
  num.bts <- sum(xlist[[l.xlist]])
  nlist <- unlist(lapply(xlist, length))
  # Create an empty matrix to contain the gmatrix
  gmat <- matrix(, nrow = l.xlist, ncol = num.bts)
  # Insert the bottom level
  gmat[nrow(gmat), ] <- seq(1L, num.bts)
  # Insert the middle levels in the reverse order
  if (l.xlist > 1L) {
    repcount <- xlist[[l.xlist]]
    for (i in (l.xlist - 1L):1L) {
      gmat[i, ] <- rep(1L:nlist[i + 1], repcount)
      repcount <- rowsum(repcount, rep(1L:nlist[i], xlist[[i]]))
    }
  }
  # Insert the top level
  gmat <- rbind(rep(1L, num.bts), gmat)
  
  dimnames(gmat) <- list(paste("Level", 0L:(nrow(gmat) - 1L)), colnames(xlist))
  class(gmat) <- "gmatrix"
  return(gmat)
}
# This function returns a sparse matrix supported by Matrix pkg
SmatrixM <- function(gmat) { 
  # Sparse matrices stored in coordinate format
  # gmatrix contains all the information to generate smatrix
  num.bts <- ncol(gmat)
  sparse.S <- apply(gmat, 1L, function(x) {
    ia <- as.integer(x)
    ra <- as.integer(rep(1L, num.bts))
    ja <- as.integer(1L:num.bts)
    s <- sparseMatrix(i = ia, j = ja, x = ra)
  })
  sparse <- do.call("rbind", sparse.S)
  return(sparse)
}

# Try Catch on the solve function to prevent errors
solveLUQR <- function(lhs.l, rhs.l) {
  tryCatch(solve(lhs.l, rhs.l), error=function(cond){
    
    #browser()
    warning("An error in LU decomposition occurred, the message was the following:\n",
            cond$message, "\n Trying QR decomposition instead...")
    solve(qr(lhs.l), rhs.l)
  })
}

# LU factorization (Matrix pkg)
LU <- function(fcasts, S=smat, weights, known.values) {
  nts <- nrow(S) # total number of time series
  nbts <- ncol(S) # total number of bottom level time series
  nagg <- nts - nbts # number of aggregate time series
  seqagg <- 1L:nagg # sequence 1:#aggtime series
  
  known.values.b <- known.values[known.values>nagg]
  unknown.values.b <- setdiff((nagg+1):nts, known.values.b)
  
  known.values.agg <- known.values[known.values<=nagg]
  unknown.values.agg <- setdiff( 1:nagg,known.values.agg)
  
  utmat <- cbind2(sparseMatrix(i = seqagg, j = seqagg, x = 1),
                  -1 * S[1L:nagg, ]) ## Identity matrix for the top level series appended with the corresponding summation matrix that will act on the lowest level time series
  
  jmat <- sparseMatrix(i = 1L:nbts, j = (nagg + 1L):nts, x = rep(1L, nbts),
                       dims = c(nbts, nts)) # when this is applied to fcasts, we get the bottom level ofrecasts
  
  rhs.l <-  methods::as(utmat %*% fcasts, "CsparseMatrix") # The discrepancy at the higher aggregate series, fom summing the lowest level series
  
  lhs.l <- utmat %*% weights%*% t(utmat)  ## utmat %*% t(utmat) is of dim #agg x # agg ...   %*% weights
  lhs.l <- (t(lhs.l) + lhs.l)/2            ## if there are no weights, this does not change lhs.l - this is making sure the matrix is symmetric
  
  
  lin.sol <- solveLUQR(lhs.l, rhs.l )
  
  p1 <- jmat %*% fcasts- (jmat %*% weights %*% t(utmat) %*% lin.sol) ## the bottom level forecasts minus the redistributed discrepancy from the top levels :this is beta in the paper
  #### This can only have zeros where we don't force the lower level time series   
  
  # Replace any lower level forecasts with known values
  p1[known.values.b - nagg,,drop = F] <- fcasts[known.values.b,,drop = F]
  p1 <- round(p1)
  
  # Check there are no less than zero values
  p1[which(p1<0, arr.ind = T)] <- 0
  
  comb <- as.matrix(S %*% p1)
  
  return(comb)
}

## TOP down forecsts based on forecast proportions
TdFp <- function(fcasts, nodes) {
  # Top-down forecasts using forecast proportions
  levels <- cumsum(Mnodes(nodes))
  # Split fcasts to a list
  l.levels <- length(levels)
  flist <- lapply(2L:l.levels, function(x) {
    fcasts[, seq(levels[x - 1L] + 1L, levels[x])]
  })
  flist <- c(list(fcasts[, 1L]), flist)
  if (is.vector(flist[[2L]])) {  # In case of h = 1
    new.flist <- vector(length = l.levels - 1L, mode = "list")
    for (j in 1L:(l.levels - 1L)) {
      repcount <- rep(1:length(nodes[[j]]), nodes[[j]])
      new.flist[[j]] <- rowsum(flist[[j + 1L]], repcount)
    }
    
    # Calculate proportions
    prop <- c(flist[[2L]]) / c(new.flist[[1L]])
    if (l.levels > 2L) {
      for (k in 2L:(l.levels - 1L)) {
        prop <- rep(prop, nodes[[k]])
        newprop <- rep(new.flist[[k]], nodes[[k]])
        prop <- prop * flist[[k + 1L]]/newprop
      }
    }
    out <- t(fcasts[, 1L] * prop)
  } else {
    # Create the sum of the h-step-ahead base forecasts at l level above node j
    new.flist <- vector(length = l.levels - 1L, mode = "list")
    for (j in 1L:(l.levels - 1L)) {
      repcount <- rep(1:length(nodes[[j]]), nodes[[j]])
      new.flist[[j]] <- t(apply(flist[[j + 1L]], 1, 
                                function(x) rowsum(x, repcount)))
    }
    
    # Calculate proportions
    prop <- apply(flist[[2L]], 2, function(x) x/new.flist[[1L]])
    if (l.levels > 2L) {
      for (k in 2L:(l.levels - 1L)) {
        prop <- t(apply(prop, 1, function(x) rep(x, nodes[[k]])))
        newprop <- t(apply(new.flist[[k]], 1, function(x) rep(x, nodes[[k]])))
        prop <- prop * flist[[k + 1L]]/newprop
      }
    }
    out <- fcasts[, 1L] * prop
  }
  return(out)
}


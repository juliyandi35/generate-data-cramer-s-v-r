set.seed(6)
strong.pertubation <- function(X,Y) {
  initial_cramer_v <- cramerV(table(X, Y))
  
  # Initialize variables
  changed_indices <- c()
  current_cramer_v <- initial_cramer_v
  
  # Loop through the indices of 0s in X
  for (i in which(X == 0)) {
    # Change the 0 to 1
    X[i] <- 1
    
    # Calculate Cramer's V with the updated X
    new_cramer_v <- cramerV(table(X, Y))
    
    # If Cramer's V is higher, keep the change, otherwise revert back
    if (new_cramer_v > current_cramer_v) {
      current_cramer_v <- new_cramer_v
      changed_indices <- c(changed_indices, i)
    } else {
      X[i] <- 0
    }
    if (new_cramer_v >= 0.15){
      break
    }
  }
  
  # Return the final X vector, changed indices, initial and final Cramer's V
  return(list(X = X, changed_indices = changed_indices, initial_cramer_v = initial_cramer_v, current_cramer_v = current_cramer_v))
}

medium.pertubation <- function(X,Y) {
  initial_cramer_v <- cramerV(table(X, Y))
  
  # Initialize variables
  changed_indices <- c()
  current_cramer_v <- initial_cramer_v
  
  # Loop through the indices of 0s in X
  for (i in which(X == 0)) {
    # Change the 0 to 1
    X[i] <- 1
    
    # Calculate Cramer's V with the updated X
    new_cramer_v <- cramerV(table(X, Y))
    
    # If Cramer's V is higher, keep the change, otherwise revert back
    if (new_cramer_v > current_cramer_v) {
      current_cramer_v <- new_cramer_v
      changed_indices <- c(changed_indices, i)
    } else {
      X[i] <- 0
    }
    if (new_cramer_v >= 0.1){
      break
    }
  }
  
  # Return the final X vector, changed indices, initial and final Cramer's V
  return(list(X = X, changed_indices = changed_indices, initial_cramer_v = initial_cramer_v, current_cramer_v = current_cramer_v))
}

weak.pertubation <- function(X,Y) {
  initial_cramer_v <- cramerV(table(X, Y))
  
  # Initialize variables
  changed_indices <- c()
  current_cramer_v <- initial_cramer_v
  
  # Loop through the indices of 0s in X
  for (i in which(X == 0)) {
    # Change the 0 to 1
    X[i] <- 1
    
    # Calculate Cramer's V with the updated X
    new_cramer_v <- cramerV(table(X, Y))
    
    # If Cramer's V is higher, keep the change, otherwise revert back
    if (new_cramer_v > current_cramer_v) {
      current_cramer_v <- new_cramer_v
      changed_indices <- c(changed_indices, i)
    } else {
      X[i] <- 0
    }
    if (new_cramer_v >= 0.05){
      break
    }
  }
  
  # Return the final X vector, changed indices, initial and final Cramer's V
  return(list(X = X, changed_indices = changed_indices, initial_cramer_v = initial_cramer_v, current_cramer_v = current_cramer_v))
}

# Untuk n = 500
n <- 500
Y <- rbinom(n,1,0.2)

# Ulangan 1
X1 <- rbinom(n,1,0.5)
X2 <- rbinom(n,1,0.5)
X3 <- rbinom(n,1,0.5)
X4 <- rbinom(n,1,0.5)
X5 <- rbinom(n,1,0.5)
X6 <- rbinom(n,1,0.5)
X7<- rbinom(n,1,0.5)
X8 <- rbinom(n,1,0.5)
X9 <- rbinom(n,1,0.5)
X10 <- rbinom(n,1,0.5)
X11 <- rbinom(n,1,0.5)
X12 <- rbinom(n,1,0.5)

library(rcompanion)
X1.Pert <- weak.pertubation(X = X1,Y = Y)
X2.Pert <- weak.pertubation(X = X2,Y = Y)
X3.Pert <- weak.pertubation(X = X3,Y = Y)
X4.Pert <- weak.pertubation(X = X4,Y = Y)
X5.Pert <- medium.pertubation(X = X5,Y = Y)
X6.Pert <- medium.pertubation(X = X6,Y = Y)
X7.Pert <- medium.pertubation(X = X7,Y = Y)
X8.Pert <- medium.pertubation(X = X8,Y = Y)
X9.Pert <- strong.pertubation(X = X9,Y = Y)
X10.Pert <- strong.pertubation(X = X10,Y = Y)
X11.Pert <- strong.pertubation(X = X11,Y = Y)
X12.Pert <- strong.pertubation(X = X12,Y = Y)

Z <- X1.Pert$X*X1.Pert$current_cramer_v+X2.Pert$X*X2.Pert$current_cramer_v+
  X3.Pert$X*X3.Pert$current_cramer_v+X4.Pert$X*X4.Pert$current_cramer_v+
  X5.Pert$X*X5.Pert$current_cramer_v+X6.Pert$X*X6.Pert$current_cramer_v+
  X7.Pert$X*X7.Pert$current_cramer_v+X8.Pert$X*X8.Pert$current_cramer_v+
  X9.Pert$X*X9.Pert$current_cramer_v+X10.Pert$X*X10.Pert$current_cramer_v+
  X11.Pert$X*X11.Pert$current_cramer_v+X12.Pert$X*X12.Pert$current_cramer_v
Z
Pi <- 1/(1+exp(-Z))
Y.New <- rbinom(n,size = 1,prob=mean(Pi))
CramersV.Values <- data.frame(X1= X1.Pert$current_cramer_v,X2 = X2.Pert$current_cramer_v,
                              X3 = X3.Pert$current_cramer_v, X4 = X4.Pert$current_cramer_v,
                              X5 = X5.Pert$current_cramer_v, X6 = X6.Pert$current_cramer_v, 
                              X7 = X7.Pert$current_cramer_v, X8 = X8.Pert$current_cramer_v,
                              X9 = X9.Pert$current_cramer_v, X10 = X10.Pert$current_cramer_v,
                              X11= X11.Pert$current_cramer_v, X12 = X12.Pert$current_cramer_v)
Dataset <- data.frame(Y.Before = Y, Y.After = Y.New,
                      X1 = X1.Pert$X,X2 = X2.Pert$X,
                      X3 = X3.Pert$X, X4 = X4.Pert$X,
                      X5 = X5.Pert$X, X6 = X6.Pert$X, 
                      X7 = X7.Pert$X, X8 = X8.Pert$X,
                      X9 = X9.Pert$X, X10 = X10.Pert$X,
                      X11= X11.Pert$X, X12 = X12.Pert$X)

library(openxlsx)
# Create a workbook
wb <- createWorkbook()
addWorksheet(wb, "Dataset 500")
writeData(wb, "Dataset 500", Dataset)
addWorksheet(wb, "CV 500")
writeData(wb, "CV 500", CramersV.Values)

# Untuk n = 1000
n <- 1000
Y <- rbinom(n,1,0.2)

# Ulangan 1
X1 <- rbinom(n,1,0.5)
X2 <- rbinom(n,1,0.5)
X3 <- rbinom(n,1,0.5)
X4 <- rbinom(n,1,0.5)
X5 <- rbinom(n,1,0.5)
X6 <- rbinom(n,1,0.5)
X7 <- rbinom(n,1,0.5)
X8 <- rbinom(n,1,0.5)
X9 <- rbinom(n,1,0.5)
X10 <- rbinom(n,1,0.5)
X11 <- rbinom(n,1,0.5)
X12 <- rbinom(n,1,0.5)

library(rcompanion)
X1.Pert <- weak.pertubation(X = X1,Y = Y)
X2.Pert <- weak.pertubation(X = X2,Y = Y)
X3.Pert <- weak.pertubation(X = X3,Y = Y)
X4.Pert <- weak.pertubation(X = X4,Y = Y)
X5.Pert <- medium.pertubation(X = X5,Y = Y)
X6.Pert <- medium.pertubation(X = X6,Y = Y)
X7.Pert <- medium.pertubation(X = X7,Y = Y)
X8.Pert <- medium.pertubation(X = X8,Y = Y)
X9.Pert <- strong.pertubation(X = X9,Y = Y)
X10.Pert <- strong.pertubation(X = X10,Y = Y)
X11.Pert <- strong.pertubation(X = X11,Y = Y)
X12.Pert <- strong.pertubation(X = X12,Y = Y)

Z <- X1.Pert$X*X1.Pert$current_cramer_v+X2.Pert$X*X2.Pert$current_cramer_v+
  X3.Pert$X*X3.Pert$current_cramer_v+X4.Pert$X*X4.Pert$current_cramer_v+
  X5.Pert$X*X5.Pert$current_cramer_v+X6.Pert$X*X6.Pert$current_cramer_v+
  X7.Pert$X*X7.Pert$current_cramer_v+X8.Pert$X*X8.Pert$current_cramer_v+
  X9.Pert$X*X9.Pert$current_cramer_v+X10.Pert$X*X10.Pert$current_cramer_v+
  X11.Pert$X*X11.Pert$current_cramer_v+X12.Pert$X*X12.Pert$current_cramer_v
Z
Pi <- 1/(1+exp(-Z))
Y.New <- rbinom(n,size = 1,prob=mean(Pi))
CramersV.Values <- data.frame(X1= X1.Pert$current_cramer_v,X2 = X2.Pert$current_cramer_v,
                              X3 = X3.Pert$current_cramer_v, X4 = X4.Pert$current_cramer_v,
                              X5 = X5.Pert$current_cramer_v, X6 = X6.Pert$current_cramer_v, 
                              X7 = X7.Pert$current_cramer_v, X8 = X8.Pert$current_cramer_v,
                              X9 = X9.Pert$current_cramer_v, X10 = X10.Pert$current_cramer_v,
                              X11= X11.Pert$current_cramer_v, X12 = X12.Pert$current_cramer_v)
Dataset <- data.frame(Y.Before = Y, Y.After = Y.New,
                      X1= X1.Pert$X,X2 = X2.Pert$X,
                      X3 = X3.Pert$X, X4 = X4.Pert$X,
                      X5 = X5.Pert$X, X6 = X6.Pert$X, 
                      X7 = X7.Pert$X, X8 = X8.Pert$X,
                      X9 = X9.Pert$X, X10 = X10.Pert$X,
                      X11= X11.Pert$X, X12 = X12.Pert$X)

# Create a workbook
addWorksheet(wb, "Dataset 1000")
writeData(wb, "Dataset 1000", Dataset)
addWorksheet(wb, "CV 1000")
writeData(wb, "CV 1000", CramersV.Values)

# Untuk n = 2000
n <- 2000
Y <- rbinom(n,1,0.2)

# Ulangan 1
X1 <- rbinom(n,1,0.5)
X2 <- rbinom(n,1,0.5)
X3 <- rbinom(n,1,0.5)
X4 <- rbinom(n,1,0.5)
X5 <- rbinom(n,1,0.5)
X6 <- rbinom(n,1,0.5)
X7 <- rbinom(n,1,0.5)
X8 <- rbinom(n,1,0.5)
X9 <- rbinom(n,1,0.5)
X10 <- rbinom(n,1,0.5)
X11 <- rbinom(n,1,0.5)
X12 <- rbinom(n,1,0.5)

library(rcompanion)
X1.Pert <- weak.pertubation(X = X1,Y = Y)
X2.Pert <- weak.pertubation(X = X2,Y = Y)
X3.Pert <- weak.pertubation(X = X3,Y = Y)
X4.Pert <- weak.pertubation(X = X4,Y = Y)
X5.Pert <- medium.pertubation(X = X5,Y = Y)
X6.Pert <- medium.pertubation(X = X6,Y = Y)
X7.Pert <- medium.pertubation(X = X7,Y = Y)
X8.Pert <- medium.pertubation(X = X8,Y = Y)
X9.Pert <- strong.pertubation(X = X9,Y = Y)
X10.Pert <- strong.pertubation(X = X10,Y = Y)
X11.Pert <- strong.pertubation(X = X11,Y = Y)
X12.Pert <- strong.pertubation(X = X12,Y = Y)

Z <- X1.Pert$X*X1.Pert$current_cramer_v+X2.Pert$X*X2.Pert$current_cramer_v+
  X3.Pert$X*X3.Pert$current_cramer_v+X4.Pert$X*X4.Pert$current_cramer_v+
  X5.Pert$X*X5.Pert$current_cramer_v+X6.Pert$X*X6.Pert$current_cramer_v+
  X7.Pert$X*X7.Pert$current_cramer_v+X8.Pert$X*X8.Pert$current_cramer_v+
  X9.Pert$X*X9.Pert$current_cramer_v+X10.Pert$X*X10.Pert$current_cramer_v+
  X11.Pert$X*X11.Pert$current_cramer_v+X12.Pert$X*X12.Pert$current_cramer_v
Z
Pi <- 1/(1+exp(-Z))
Y.New <- rbinom(n,size = 1,prob=mean(Pi))
CramersV.Values <- data.frame(X1= X1.Pert$current_cramer_v,X2 = X2.Pert$current_cramer_v,
                              X3 = X3.Pert$current_cramer_v, X4 = X4.Pert$current_cramer_v,
                              X5 = X5.Pert$current_cramer_v, X6 = X6.Pert$current_cramer_v, 
                              X7 = X7.Pert$current_cramer_v, X8 = X8.Pert$current_cramer_v,
                              X9 = X9.Pert$current_cramer_v, X10 = X10.Pert$current_cramer_v,
                              X11= X11.Pert$current_cramer_v, X12 = X12.Pert$current_cramer_v)
Dataset <- data.frame(Y.Before = Y, Y.After = Y.New,
                      X1= X1.Pert$X,X2 = X2.Pert$X,
                      X3 = X3.Pert$X, X4 = X4.Pert$X,
                      X5 = X5.Pert$X, X6 = X6.Pert$X, 
                      X7 = X7.Pert$X, X8 = X8.Pert$X,
                      X9 = X9.Pert$X, X10 = X10.Pert$X,
                      X11= X11.Pert$X, X12 = X12.Pert$X)

# Create a workbook
addWorksheet(wb, "Dataset 2000")
writeData(wb, "Dataset 2000", Dataset)
addWorksheet(wb, "CV 2000")
writeData(wb, "CV 2000", CramersV.Values)

# Untuk n = 3000
n <- 3000
Y <- rbinom(n,1,0.2)

# Ulangan 1
X1 <- rbinom(n,1,0.5)
X2 <- rbinom(n,1,0.5)
X3 <- rbinom(n,1,0.5)
X4 <- rbinom(n,1,0.5)
X5 <- rbinom(n,1,0.5)
X6 <- rbinom(n,1,0.5)
X7 <- rbinom(n,1,0.5)
X8 <- rbinom(n,1,0.5)
X9 <- rbinom(n,1,0.5)
X10 <- rbinom(n,1,0.5)
X11 <- rbinom(n,1,0.5)
X12 <- rbinom(n,1,0.5)

library(rcompanion)
X1.Pert <- weak.pertubation(X = X1,Y = Y)
X2.Pert <- weak.pertubation(X = X2,Y = Y)
X3.Pert <- weak.pertubation(X = X3,Y = Y)
X4.Pert <- weak.pertubation(X = X4,Y = Y)
X5.Pert <- medium.pertubation(X = X5,Y = Y)
X6.Pert <- medium.pertubation(X = X6,Y = Y)
X7.Pert <- medium.pertubation(X = X7,Y = Y)
X8.Pert <- medium.pertubation(X = X8,Y = Y)
X9.Pert <- strong.pertubation(X = X9,Y = Y)
X10.Pert <- strong.pertubation(X = X10,Y = Y)
X11.Pert <- strong.pertubation(X = X11,Y = Y)
X12.Pert <- strong.pertubation(X = X12,Y = Y)

Z <- X1.Pert$X*X1.Pert$current_cramer_v+X2.Pert$X*X2.Pert$current_cramer_v+
  X3.Pert$X*X3.Pert$current_cramer_v+X4.Pert$X*X4.Pert$current_cramer_v+
  X5.Pert$X*X5.Pert$current_cramer_v+X6.Pert$X*X6.Pert$current_cramer_v+
  X7.Pert$X*X7.Pert$current_cramer_v+X8.Pert$X*X8.Pert$current_cramer_v+
  X9.Pert$X*X9.Pert$current_cramer_v+X10.Pert$X*X10.Pert$current_cramer_v+
  X11.Pert$X*X11.Pert$current_cramer_v+X12.Pert$X*X12.Pert$current_cramer_v
Z
Pi <- 1/(1+exp(-Z))
Y.New <- rbinom(n,size = 1,prob=mean(Pi))
CramersV.Values <- data.frame(X1= X1.Pert$current_cramer_v,X2 = X2.Pert$current_cramer_v,
                              X3 = X3.Pert$current_cramer_v, X4 = X4.Pert$current_cramer_v,
                              X5 = X5.Pert$current_cramer_v, X6 = X6.Pert$current_cramer_v, 
                              X7 = X7.Pert$current_cramer_v, X8 = X8.Pert$current_cramer_v,
                              X9 = X9.Pert$current_cramer_v, X10 = X10.Pert$current_cramer_v,
                              X11= X11.Pert$current_cramer_v, X12 = X12.Pert$current_cramer_v)
Dataset <- data.frame(Y.Before = Y, Y.After = Y.New,
                      X1= X1.Pert$X,X2 = X2.Pert$X,
                      X3 = X3.Pert$X, X4 = X4.Pert$X,
                      X5 = X5.Pert$X, X6 = X6.Pert$X, 
                      X7 = X7.Pert$X, X8 = X8.Pert$X,
                      X9 = X9.Pert$X, X10 = X10.Pert$X,
                      X11= X11.Pert$X, X12 = X12.Pert$X)

# Create a workbook
addWorksheet(wb, "Dataset 3000")
writeData(wb, "Dataset 3000", Dataset)
addWorksheet(wb, "CV 3000")
writeData(wb, "CV 3000", CramersV.Values)

# Untuk n = 4000
n <- 4000
Y <- rbinom(n,1,0.2)

# Ulangan 1
X1 <- rbinom(n,1,0.5)
X2 <- rbinom(n,1,0.5)
X3 <- rbinom(n,1,0.5)
X4 <- rbinom(n,1,0.5)
X5 <- rbinom(n,1,0.5)
X6 <- rbinom(n,1,0.5)
X7 <- rbinom(n,1,0.5)
X8 <- rbinom(n,1,0.5)
X9 <- rbinom(n,1,0.5)
X10 <- rbinom(n,1,0.5)
X11 <- rbinom(n,1,0.5)
X12 <- rbinom(n,1,0.5)

library(rcompanion)
X1.Pert <- weak.pertubation(X = X1,Y = Y)
X2.Pert <- weak.pertubation(X = X2,Y = Y)
X3.Pert <- weak.pertubation(X = X3,Y = Y)
X4.Pert <- weak.pertubation(X = X4,Y = Y)
X5.Pert <- medium.pertubation(X = X5,Y = Y)
X6.Pert <- medium.pertubation(X = X6,Y = Y)
X7.Pert <- medium.pertubation(X = X7,Y = Y)
X8.Pert <- medium.pertubation(X = X8,Y = Y)
X9.Pert <- strong.pertubation(X = X9,Y = Y)
X10.Pert <- strong.pertubation(X = X10,Y = Y)
X11.Pert <- strong.pertubation(X = X11,Y = Y)
X12.Pert <- strong.pertubation(X = X12,Y = Y)

Z <- X1.Pert$X*X1.Pert$current_cramer_v+X2.Pert$X*X2.Pert$current_cramer_v+
  X3.Pert$X*X3.Pert$current_cramer_v+X4.Pert$X*X4.Pert$current_cramer_v+
  X5.Pert$X*X5.Pert$current_cramer_v+X6.Pert$X*X6.Pert$current_cramer_v+
  X7.Pert$X*X7.Pert$current_cramer_v+X8.Pert$X*X8.Pert$current_cramer_v+
  X9.Pert$X*X9.Pert$current_cramer_v+X10.Pert$X*X10.Pert$current_cramer_v+
  X11.Pert$X*X11.Pert$current_cramer_v+X12.Pert$X*X12.Pert$current_cramer_v
Z
Pi <- 1/(1+exp(-Z))
Y.New <- rbinom(n,size = 1,prob=mean(Pi))
CramersV.Values <- data.frame(X1= X1.Pert$current_cramer_v,X2 = X2.Pert$current_cramer_v,
                              X3 = X3.Pert$current_cramer_v, X4 = X4.Pert$current_cramer_v,
                              X5 = X5.Pert$current_cramer_v, X6 = X6.Pert$current_cramer_v, 
                              X7 = X7.Pert$current_cramer_v, X8 = X8.Pert$current_cramer_v,
                              X9 = X9.Pert$current_cramer_v, X10 = X10.Pert$current_cramer_v,
                              X11= X11.Pert$current_cramer_v, X12 = X12.Pert$current_cramer_v)
Dataset <- data.frame(Y.Before = Y, Y.After = Y.New,
                      X1= X1.Pert$X,X2 = X2.Pert$X,
                      X3 = X3.Pert$X, X4 = X4.Pert$X,
                      X5 = X5.Pert$X, X6 = X6.Pert$X, 
                      X7 = X7.Pert$X, X8 = X8.Pert$X,
                      X9 = X9.Pert$X, X10 = X10.Pert$X,
                      X11= X11.Pert$X, X12 = X12.Pert$X)

# Create a workbook
addWorksheet(wb, "Dataset 4000")
writeData(wb, "Dataset 4000", Dataset)
addWorksheet(wb, "CV 4000")
writeData(wb, "CV 4000", CramersV.Values)

# Untuk n = 5000
n <- 5000
Y <- rbinom(n,1,0.2)

# Ulangan 1
X1 <- rbinom(n,1,0.5)
X2 <- rbinom(n,1,0.5)
X3 <- rbinom(n,1,0.5)
X4 <- rbinom(n,1,0.5)
X5 <- rbinom(n,1,0.5)
X6 <- rbinom(n,1,0.5)
X7 <- rbinom(n,1,0.5)
X8 <- rbinom(n,1,0.5)
X9 <- rbinom(n,1,0.5)
X10 <- rbinom(n,1,0.5)
X11 <- rbinom(n,1,0.5)
X12 <- rbinom(n,1,0.5)

library(rcompanion)
X1.Pert <- weak.pertubation(X = X1,Y = Y)
X2.Pert <- weak.pertubation(X = X2,Y = Y)
X3.Pert <- weak.pertubation(X = X3,Y = Y)
X4.Pert <- weak.pertubation(X = X4,Y = Y)
X5.Pert <- medium.pertubation(X = X5,Y = Y)
X6.Pert <- medium.pertubation(X = X6,Y = Y)
X7.Pert <- medium.pertubation(X = X7,Y = Y)
X8.Pert <- medium.pertubation(X = X8,Y = Y)
X9.Pert <- strong.pertubation(X = X9,Y = Y)
X10.Pert <- strong.pertubation(X = X10,Y = Y)
X11.Pert <- strong.pertubation(X = X11,Y = Y)
X12.Pert <- strong.pertubation(X = X12,Y = Y)

Z <- X1.Pert$X*X1.Pert$current_cramer_v+X2.Pert$X*X2.Pert$current_cramer_v+
  X3.Pert$X*X3.Pert$current_cramer_v+X4.Pert$X*X4.Pert$current_cramer_v+
  X5.Pert$X*X5.Pert$current_cramer_v+X6.Pert$X*X6.Pert$current_cramer_v+
  X7.Pert$X*X7.Pert$current_cramer_v+X8.Pert$X*X8.Pert$current_cramer_v+
  X9.Pert$X*X9.Pert$current_cramer_v+X10.Pert$X*X10.Pert$current_cramer_v+
  X11.Pert$X*X11.Pert$current_cramer_v+X12.Pert$X*X12.Pert$current_cramer_v
Z
Pi <- 1/(1+exp(-Z))
Y.New <- rbinom(n,size = 1,prob=mean(Pi))
CramersV.Values <- data.frame(X1= X1.Pert$current_cramer_v,X2 = X2.Pert$current_cramer_v,
                              X3 = X3.Pert$current_cramer_v, X4 = X4.Pert$current_cramer_v,
                              X5 = X5.Pert$current_cramer_v, X6 = X6.Pert$current_cramer_v, 
                              X7 = X7.Pert$current_cramer_v, X8 = X8.Pert$current_cramer_v,
                              X9 = X9.Pert$current_cramer_v, X10 = X10.Pert$current_cramer_v,
                              X11= X11.Pert$current_cramer_v, X12 = X12.Pert$current_cramer_v)
Dataset <- data.frame(Y.Before = Y, Y.After = Y.New,
                      X1= X1.Pert$X,X2 = X2.Pert$X,
                      X3 = X3.Pert$X, X4 = X4.Pert$X,
                      X5 = X5.Pert$X, X6 = X6.Pert$X, 
                      X7 = X7.Pert$X, X8 = X8.Pert$X,
                      X9 = X9.Pert$X, X10 = X10.Pert$X,
                      X11= X11.Pert$X, X12 = X12.Pert$X)

# Create a workbook
addWorksheet(wb, "Dataset 5000")
writeData(wb, "Dataset 5000", Dataset)
addWorksheet(wb, "CV 5000")
writeData(wb, "CV 5000", CramersV.Values)

# Untuk n = 6000
n <- 6000
Y <- rbinom(n,1,0.2)

# Ulangan 1
X1 <- rbinom(n,1,0.5)
X2 <- rbinom(n,1,0.5)
X3 <- rbinom(n,1,0.5)
X4 <- rbinom(n,1,0.5)
X5 <- rbinom(n,1,0.5)
X6 <- rbinom(n,1,0.5)
X7 <- rbinom(n,1,0.5)
X8 <- rbinom(n,1,0.5)
X9 <- rbinom(n,1,0.5)
X10 <- rbinom(n,1,0.5)
X11 <- rbinom(n,1,0.5)
X12 <- rbinom(n,1,0.5)

library(rcompanion)
X1.Pert <- weak.pertubation(X = X1,Y = Y)
X2.Pert <- weak.pertubation(X = X2,Y = Y)
X3.Pert <- weak.pertubation(X = X3,Y = Y)
X4.Pert <- weak.pertubation(X = X4,Y = Y)
X5.Pert <- medium.pertubation(X = X5,Y = Y)
X6.Pert <- medium.pertubation(X = X6,Y = Y)
X7.Pert <- medium.pertubation(X = X7,Y = Y)
X8.Pert <- medium.pertubation(X = X8,Y = Y)
X9.Pert <- strong.pertubation(X = X9,Y = Y)
X10.Pert <- strong.pertubation(X = X10,Y = Y)
X11.Pert <- strong.pertubation(X = X11,Y = Y)
X12.Pert <- strong.pertubation(X = X12,Y = Y)

Z <- X1.Pert$X*X1.Pert$current_cramer_v+X2.Pert$X*X2.Pert$current_cramer_v+
  X3.Pert$X*X3.Pert$current_cramer_v+X4.Pert$X*X4.Pert$current_cramer_v+
  X5.Pert$X*X5.Pert$current_cramer_v+X6.Pert$X*X6.Pert$current_cramer_v+
  X7.Pert$X*X7.Pert$current_cramer_v+X8.Pert$X*X8.Pert$current_cramer_v+
  X9.Pert$X*X9.Pert$current_cramer_v+X10.Pert$X*X10.Pert$current_cramer_v+
  X11.Pert$X*X11.Pert$current_cramer_v+X12.Pert$X*X12.Pert$current_cramer_v
Z
Pi <- 1/(1+exp(-Z))
Y.New <- rbinom(n,size = 1,prob=mean(Pi))
CramersV.Values <- data.frame(X1= X1.Pert$current_cramer_v,X2 = X2.Pert$current_cramer_v,
                              X3 = X3.Pert$current_cramer_v, X4 = X4.Pert$current_cramer_v,
                              X5 = X5.Pert$current_cramer_v, X6 = X6.Pert$current_cramer_v, 
                              X7 = X7.Pert$current_cramer_v, X8 = X8.Pert$current_cramer_v,
                              X9 = X9.Pert$current_cramer_v, X10 = X10.Pert$current_cramer_v,
                              X11= X11.Pert$current_cramer_v, X12 = X12.Pert$current_cramer_v)
Dataset <- data.frame(Y.Before = Y, Y.After = Y.New,
                      X1= X1.Pert$X,X2 = X2.Pert$X,
                      X3 = X3.Pert$X, X4 = X4.Pert$X,
                      X5 = X5.Pert$X, X6 = X6.Pert$X, 
                      X7 = X7.Pert$X, X8 = X8.Pert$X,
                      X9 = X9.Pert$X, X10 = X10.Pert$X,
                      X11= X11.Pert$X, X12 = X12.Pert$X)

# Create a workbook
addWorksheet(wb, "Dataset 6000")
writeData(wb, "Dataset 6000", Dataset)
addWorksheet(wb, "CV 6000")
writeData(wb, "CV 6000", CramersV.Values)

# Save the workbook
saveWorkbook(wb, "Ulangan 6.xlsx", overwrite = TRUE)

xtotal = 0
ytotal = 0
products <- c(0,0,0)
defrates <- c(0,0,0)
product_defrate <- c(0,0,0)

while(xtotal!=1){
  for(i in 1:3){
    products[i] <- as.numeric(readline(paste("Input a value for product ", i, " (0.1 <= input <= 0.4): ")))
    while(products[i]>0.4 || products[i]<0.1){
      products[i] <- as.numeric(readline(paste("Input a value for product ", i, " (0.1 <= input <= 0.4): ")))
    }
  }
  xtotal <- sum(products)
}
while(ytotal!=0.12){
  for(i in 1:3){
    defrates[i] <- as.numeric(readline(paste("Input a value for the defective rate of product ", i, " (0.01 <= input <= 0.05): ")))
    while(defrates[i]>0.05 || defrates[i]<0.01){
      defrates[i] <- as.numeric(readline(paste("Input a value for the defective rate of product ", i, " (0.01 <= input <= 0.05): ")))
    }
  }
  ytotal <- sum(defrates)
}
for(i in 1:3){
  cat("Product 1 Percentage = ", products[i], "; Defective Rate: ", defrates[i], "\n")
  product_defrate[i] <- products[i]*defrates[i]
}
product_defrate[1]
product_defrate[2]
product_defrate[3]
random_defrate <- sum(product_defrate)
random_defrate
cat("The probability that a randomly selected product is defective is ", random_defrate)
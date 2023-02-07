### French Loan ###

# uses nominal rate in unitary terms (i.e: 0.01)
# term in years

loan <- function(principal, rate, term) {
  monthly_rate <- rate / 12
  total_payments <- term * 12
  payment <- principal * (monthly_rate / (1 - (1 + monthly_rate)^(-total_payments)))
  
  results <- data.frame(Payment_Number = c(0, 1:total_payments), 
                        Payment = c(0, round(rep(payment, total_payments), 3)), 
                        Principal_Paid = c(0, numeric(total_payments)), 
                        Interest_Paid = c(0, numeric(total_payments)), 
                        Principal_Remaining = c(principal, numeric(total_payments)))
  
  for (i in 1:total_payments) {
    results$Interest_Paid[i+1] <- round(principal * monthly_rate, 3)
    results$Principal_Paid[i+1] <- round(payment - results$Interest_Paid[i+1], 3)
    principal <- principal - results$Principal_Paid[i+1]
    results$Principal_Remaining[i+1] <- round(principal, 3)
  }
  
  tolerance <- 0.1
  results$Principal_Remaining[results$Principal_Remaining < tolerance] <- 0
  
  return(results)
  
}

# example: one loan

(loan_table <- loan(10000,0.05,5))

# chart of remaining principal

plot(y=loan_table$Principal_Remaining,
     x=loan_table$Payment_Number,'b',
     xlab='Payments',ylab='Remaining Principal',main='Loan Payment')

#chart of payment composition

slices <- c(sum(loan_table$Interest_Paid),sum(loan_table$Principal_Paid))

etq <- c('Interest','Principal')

pie(slices,labels = etq, main = 'Composition of Payment')

# example: several loans

# data frame with principals, rates and terms

loans_data <- data.frame(principal = c(1000, 2000, 3000), 
                    rate = c(0.05, 0.06, 0.07), 
                    term = c(5, 4, 3))

loan_tables <- lapply(1:nrow(loans_data), function(i) {
  loan(loans_data$principal[i], loans_data$rate[i], loans_data$term[i])
})

names(loan_tables) <- paste0("Loan", 1:nrow(loans_data))





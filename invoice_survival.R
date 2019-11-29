library(survival)

invoices = read.csv('invoice_age.csv')
invoices$paid = as.logical(invoices$paid)

# Exclude invoices for 0 dollars, or ones that got paid 
# immediately. These were not real invoices
invoices = invoices[invoices$age != 0,]
invoices = invoices[invoices$amount != 0,]

# Bigger invoices may be take longer to get paid than smaller
# invoices, let's capture that with a log scale
invoices$log_amount = log(invoices$amount)

# This is or starting model. It has a single covariate, size of
# the invoice. We can make this more sophisticated in the
# future
fit = coxph(Surv(age, paid) ~ log_amount, data=invoices)

# Now we want to predict whether our current outstanding invoices
# will be paid in the next 30 days. We first create our data frame to
# predict off of
next_30 = invoices[invoices$paid == FALSE,]
next_30$age <- 30

# Here are the individual probabilities that a invoice will be paid
# in the next 30 days
pr_paid = 1 - exp(-predict(fit, next_30, type='expected'))

# Finally, are actually interested in knowing how much revenue we will
# realize in the next 30 days, which is a product of an invoice getting
# paid and the size of the invoice.
#
# We simulate many outcomes and graph the resulting distribution
runs = c()
for (j in 1:100000) {
    expected_cash = 0
    for (i in 1:nrow(next_30)) {
        pr = pr_paid[i]
        paid = sample(c(0,1), 1, prob=c(1-pr, pr))
        expected_cash = expected_cash + paid * next_30[i, "amount"]
    }
    runs[j] = expected_cash
}

pdf(file='expected_realized_cash.pdf')
hist(runs, main='Realized cash from current Accounts Receivable, 30 days')
dev.off()

summary(runs)

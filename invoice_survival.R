library(survival)
library(timeDate)

n_business_days = function(start_date, end_date) {
    date.sequence <- timeSequence(start_date, end_date)

    # holidays in the period
    years.included <- unique(as.integer(format(x=date.sequence, format="%Y")))
    holidays <- holidayNYSE(years.included)

    # Subset business days
    business.days <- date.sequence[isBizday(date.sequence, holidays)]
    return(length(business.days))
}


invoices = read.csv('invoice_age.csv')
invoices$paid = as.logical(invoices$paid)

# Bigger invoices may be take longer to get paid than smaller
# invoices, let's capture that with a log scale
invoices$log_amount = log(invoices$amount)

# Age in business days
invoices$age[invoices$paid == TRUE] = mapply(
    n_business_days,
    as.Date(invoices$invoice_created[invoices$paid == TRUE]),
    as.Date(invoices$invoice_closed[invoices$paid == TRUE]))

invoices$age[invoices$paid == FALSE] = mapply(
    n_business_days,
    as.Date(invoices$invoice_created[invoices$paid == FALSE]),
    Sys.Date())

# Exclude invoices for 0 dollars, or ones that got paid 
# immediately. These were not real invoices
invoices = invoices[invoices$age != 0,]
invoices = invoices[invoices$amount != 0,]

# This is or starting model. It has a single covariate, size of
# the invoice. We can make this more sophisticated in the
# future
fit = survival::coxph(survival::Surv(age, paid) ~ log_amount, data=invoices)
summary(fit)

# Now we want to predict whether our current outstanding invoices
# will be paid in the next 30 days. We first create our data frame to
# predict off of
next_30 = invoices[invoices$paid == FALSE,]
next_30$age <- n_business_days(Sys.Date(), Sys.Date() + 30)

# Here are the individual probabilities that a invoice will be paid
# in the next 30 days
pr_paid = 1 - exp(-predict(fit, next_30, type='expected'))

# Finally, we are actually interested in knowing how much revenue we will
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

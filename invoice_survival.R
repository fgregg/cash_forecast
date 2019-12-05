library(survival)
library(timeDate)
library(coxme)

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

invoices$id = row.names(invoices)
invoices$paid = as.logical(invoices$paid)

invoices$invoice_created = as.Date(invoices$invoice_created)

invoices$invoice_closed[invoices$paid == FALSE] = NA
invoices$invoice_closed = as.Date(invoices$invoice_closed)

# Bigger invoices may be take longer to get paid than smaller
# invoices, let's capture that with a log scale
invoices$log_amount = log(invoices$amount)

# Age in business days
invoices$age[invoices$paid == TRUE] = mapply(
    n_business_days,
    invoices$invoice_created[invoices$paid == TRUE],
    invoices$invoice_closed[invoices$paid == TRUE])

invoices$age[invoices$paid == FALSE] = mapply(
    n_business_days,
    invoices$invoice_created[invoices$paid == FALSE],
    Sys.Date())

invoices$time_0 = 0
invoices$time_1 = invoices$age

# Exclude invoices for 0 dollars, or ones that got paid 
# immediately. These were not real invoices
invoices = invoices[invoices$age > 1,]
invoices = invoices[invoices$amount != 0,]

# Exclude invoices paid with credit card
invoices = invoices[invoices$payment_type != 'credit card',]

# Reset the organization factor
invoices$organization = factor(invoices$organization)

# Get the Kaplan Meier fit
km_fit = survival::survfit(survival::Surv(time_0, time_1, paid) ~ 1, data=invoices)

pdf(file='km.pdf')
plot(km_fit,
     xlim=c(0, 70),
     xlab='Business Days',
     main='Proportion of Invoices Open After N Business Days, Historic')
dev.off()

# This is or starting model. It has a single covariate, size of
# the invoice. We can make this more sophisticated in the
# future
fit = survival::coxph(survival::Surv(time_0, time_1, paid) ~ log_amount, data=invoices)
summary(fit)

# Now we want to predict whether our current outstanding invoices
# will be paid in the next 30 days. We first create our data frame to
# predict off of
next_30 = invoices[invoices$paid == FALSE,]
next_30$time_0 = next_30$time_1
next_30$time_1 = next_30$time_1 + n_business_days(Sys.Date(), Sys.Date() + 30)

# Here are the individual probabilities that a invoice will be paid
# in the next 30 days
pr_paid = 1 - exp(-predict(fit, next_30, type='expected'))

# Finally, we are actually interested in knowing how much revenue we will
# realize in the next 30 days, which is a product of an invoice getting
# paid and the size of the invoice.
#
# We simulate many outcomes and graph the resulting distribution
runs = c()
for (j in 1:1000) {
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

# We want to estimate cient effects, so we used a random effects
# model for the clients. We'll rank the clients in order of
# their individual contribution to getting an invoice payed promptly
# from slow to fast
client_fit = coxme::coxme(survival::Surv(age, paid) ~ log_amount + (1 | organization), data=invoices)
summary(client_fit)

client_effects = ranef(client_fit)
client_effects = data.frame(name=names(client_effects$organization),
                            score=client_effects$organization)
row.names(client_effects) = NULL

client_effects[order(client_effects$score),]

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


OVERDUE_DAYS = 30

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

# Exclude invoices for 0 dollars, or ones that got paid 
# immediately. These were not real invoices
invoices = invoices[invoices$age > 1,]
invoices = invoices[invoices$amount != 0,]

# Get the Kaplan Meier fit
km_fit = survival::survfit(survival::Surv(age, paid) ~ 1, data=invoices)

pdf(file='km.pdf')
plot(km_fit,
     xlab='Business Days',
     main='Proportion of Invoices Open After N Business Days, Historic')
dev.off()

invoices$age_due = mapply(
    n_business_days,
    invoices$invoice_created,
    invoices$invoice_created + OVERDUE_DAYS)

invoices$overdue = invoices$age > invoices$age_due

timely = invoices[invoices$overdue == FALSE, ]
timely$time_0 = 0
timely$time_1 = timely$age
timely$overdue = FALSE

due = invoices[invoices$overdue == TRUE, ]
due$time_0 = 0
due$time_1 = due$age_due
due$overdue = FALSE
due$paid = FALSE

overdue = invoices[invoices$overdue == TRUE, ]
overdue$time_0 = overdue$age_due
overdue$time_1 = overdue$age
overdue$overdue = TRUE

complete = rbind(timely, due, overdue)

# This is or starting model. It has a single covariate, size of
# the invoice. We can make this more sophisticated in the
# future
fit = survival::coxph(survival::Surv(time_0, time_1, paid) ~ log_amount, data=complete)
summary(fit)

fit_2 = survival::coxph(survival::Surv(age, paid) ~ log_amount, data=invoices)
summary(fit_2)

# Now we want to predict whether our current outstanding invoices
# will be paid in the next 30 days. We first create our data frame to
# predict off of
next_30 = complete[complete$paid == FALSE & complete$time_1 == complete$age,]
next_30$time_1 = next_30$time_0 + n_business_days(Sys.Date(), Sys.Date() + 30)

# Here are the individual probabilities that a invoice will be paid
# in the next 30 days
pr_paid = 1 - exp(-predict(fit, next_30, type='expected'))

# Finally, we are actually interested in knowing how much revenue we will
# realize in the next 30 days, which is a product of an invoice getting
# paid and the size of the invoice.
#
# We simulate many outcomes and graph the resulting distribution
runs = c()
for (j in 1:10000) {
    expected_cash = 0
    for (i in 1:nrow(next_30)) {
        pr = pr_paid[i]
        paid = sample(c(0,1), 1, prob=c(1-pr, pr))
        expected_cash = expected_cash + paid * next_30[i, "amount"]
    }
    runs[j] = expected_cash
}

#pdf(file='expected_realized_cash.pdf')
hist(runs, main='Realized cash from current Accounts Receivable, 30 days')
#dev.off()

summary(runs)

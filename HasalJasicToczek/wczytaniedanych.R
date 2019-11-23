data <- read.table("german.data")

colnames(data) <- c("Account_status",
                    "Duration_in_month",
                    "Credit_history",
                    "Puprose",
                    "Credit_ammount",
                    "Savings_account/bonds",
                    "Present_employment since",
                    "Rate",
                    "Status/Sex",
                    "Debtors/guarantors",
                    "Present_residence_since",
                    "Property",
                    "Age",
                    "Other_installment_plans",
                    "Housing",
                    "Existing_credit_at_this_bank",
                    "Job",
                    "Maintenance",
                    "Telephone",
                    "Foreign_worker",
                    "Decision")

data_positive <- data[data$Decision == 1 , -21]
data_negative <- data[data$Decision == 2 , -21]

#Tylko przykład co bedziemy robić :)
hist(data_positive$Age, ylim = c(0, 150))
hist(data_negative$Age, ylim = c(0, 150))
#Chcielibyśmy aby te dwa histogramy byly na jednym obrazku ale nie wiemy jak :(

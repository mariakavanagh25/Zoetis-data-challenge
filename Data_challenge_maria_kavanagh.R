install.packages("readxl")
install.packages("sqldf")
install.packages("fuzzyjoin")
library(readxl)
library(dplyr)
library(sqldf)
library(fuzzyjoin)
library(tidyr)

### Read in files
customer_account <- read.table("CUSTOMER_ACCOUNT.txt", sep = "|", header = TRUE, quote= "")
customer_sales <- read_xlsx("CUSTOMER_SALES.xlsx", sheet="Sales_data")

customer_account$CUSTOMER_NAME_acc <- paste(customer_account$CUSTOMER_FIRST_NAME,
                                        customer_account$CUSTOMER_LAST_NAME)



### Join files - since no unique identifier will merge based on customer name

### Joining based on wild cards gets just 797 matches out of 1000
acc_sales <- sqldf('SELECT *
                    FROM customer_account acc
                    LEFT JOIN customer_sales s 
                    ON s.CUSTOMER_NAME like 
                              "%" || acc.CUSTOMER_FIRST_NAME || "%" || acc.CUSTOMER_LAST_NAME || "%" 
                   ') 
acc_sales <- acc_sales %>% filter(CUSTOMER_NAME !='NA')

### Check if all have matched ###
print(paste(nrow(customer_account) - nrow (acc_sales), "still to match"))

### find those records that were not matched and try again (203 records)
account_unmatched <- subset(customer_account, !(CUSTOMER_NAME_acc %in% acc_sales$CUSTOMER_NAME_acc))
account_unmatched <- rename(account_unmatched,CUSTOMER_NAME = CUSTOMER_NAME_acc)

acc_sales2 <- stringdist_left_join(account_unmatched, customer_sales, by="CUSTOMER_NAME", max_dist = 2)
acc_sales2 <- acc_sales2 %>% filter(CUSTOMER_NAME.y !='NA')

### Check if all have matched ###
print(paste(nrow(customer_account) - nrow (acc_sales) - nrow (acc_sales2), "still to match"))



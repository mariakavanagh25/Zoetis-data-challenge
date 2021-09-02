library(readxl)
library(dplyr)
library(sqldf)
library(fuzzyjoin)
library(tidyr)

### Read in files
account <- read.table("CUSTOMER_ACCOUNT.txt", sep = "|", header = TRUE, quote= "")
sales <- read_xlsx("CUSTOMER_SALES.xlsx", sheet="Sales_data")

account$CUSTOMER_NAME_x <- paste(account$CUSTOMER_FIRST_NAME, account$CUSTOMER_LAST_NAME)
sales <- rename (sales, CUSTOMER_NAME_y = CUSTOMER_NAME )

#initialise data frame for joined files
acc_sales <- data.frame()

### First JOIN based on wildcards (797 matches)

x <- sqldf('SELECT *
            FROM account acc
            LEFT JOIN sales s 
            ON s.CUSTOMER_NAME_y like 
                "%" || acc.CUSTOMER_FIRST_NAME || "%" || acc.CUSTOMER_LAST_NAME || "%" 
            ')

acc_sales <- rbind(acc_sales, x%>% filter(CUSTOMER_NAME_y !='NA') )
acc_sales <- rename(acc_sales, CUSTOMER_NAME.x = CUSTOMER_NAME_x)
acc_sales <- rename(acc_sales, CUSTOMER_NAME.y = CUSTOMER_NAME_y)

### find those records that were not matched and try again (203 records)
sales_unmatched <- subset(sales, !(CUSTOMER_NAME_y %in% acc_sales$CUSTOMER_NAME.y ))
account_unmatched <- subset(account, !(CUSTOMER_NAME_x %in% acc_sales$CUSTOMER_NAME.x))

account_unmatched <- rename(account_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_x)
sales_unmatched <- rename(sales_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_y)

### Second JOIN using fuzzyjoin package 
x <- stringdist_left_join(account_unmatched, sales_unmatched, by="CUSTOMER_NAME", max_dist = 1)
acc_sales <- rbind(acc_sales, x %>% filter(CUSTOMER_NAME.y !='NA'))


### find those records that were not matched and try again (60 records)
sales_unmatched <- subset(sales, !(CUSTOMER_NAME_y %in% acc_sales$CUSTOMER_NAME.y ))
account_unmatched <- subset(account, !(CUSTOMER_NAME_x %in% acc_sales$CUSTOMER_NAME.x))

account_unmatched <- rename(account_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_x)
sales_unmatched <- rename(sales_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_y)

x <- stringdist_left_join(account_unmatched, sales_unmatched, by="CUSTOMER_NAME", max_dist = 2)
acc_sales <- rbind(acc_sales, x %>% filter(CUSTOMER_NAME.y !='NA'))

### find those records that were not matched and try again (17 records)
sales_unmatched <- subset(sales, !(CUSTOMER_NAME_y %in% acc_sales$CUSTOMER_NAME.y ))
account_unmatched <- subset(account, !(CUSTOMER_NAME_x %in% acc_sales$CUSTOMER_NAME.x))

account_unmatched <- rename(account_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_x)
sales_unmatched <- rename(sales_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_y)

x <- stringdist_left_join(account_unmatched, sales_unmatched, by="CUSTOMER_NAME", max_dist = 3)
acc_sales <- rbind(acc_sales, x %>% filter(CUSTOMER_NAME.y !='NA'))

### find those records that were not matched and try again (11 records)
sales_unmatched <- subset(sales, !(CUSTOMER_NAME_y %in% acc_sales$CUSTOMER_NAME.y ))
account_unmatched <- subset(account, !(CUSTOMER_NAME_x %in% acc_sales$CUSTOMER_NAME.x))

account_unmatched <- rename(account_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_x)
sales_unmatched <- rename(sales_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_y)

x <- stringdist_left_join(account_unmatched, sales_unmatched, by="CUSTOMER_NAME", max_dist = 4)
acc_sales <- rbind(acc_sales, x %>% filter(CUSTOMER_NAME.y !='NA'))

### find those records that were not matched and try again (6 records)
sales_unmatched <- subset(sales, !(CUSTOMER_NAME_y %in% acc_sales$CUSTOMER_NAME.y ))
account_unmatched <- subset(account, !(CUSTOMER_NAME_x %in% acc_sales$CUSTOMER_NAME.x))

account_unmatched <- rename(account_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_x)
sales_unmatched <- rename(sales_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_y)

x <- stringdist_left_join(account_unmatched, sales_unmatched, by="CUSTOMER_NAME", max_dist = 5)
acc_sales <- rbind(acc_sales, x %>% filter(CUSTOMER_NAME.y !='NA'))

### find those records that were not matched and try again (2 records)
sales_unmatched <- subset(sales, !(CUSTOMER_NAME_y %in% acc_sales$CUSTOMER_NAME.y ))
account_unmatched <- subset(account, !(CUSTOMER_NAME_x %in% acc_sales$CUSTOMER_NAME.x))

account_unmatched <- rename(account_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_x)
sales_unmatched <- rename(sales_unmatched, CUSTOMER_NAME = CUSTOMER_NAME_y)

x <- stringdist_left_join(account_unmatched, sales_unmatched, by="CUSTOMER_NAME", max_dist = 6)
acc_sales <- rbind(acc_sales, x %>% filter(CUSTOMER_NAME.y !='NA'))

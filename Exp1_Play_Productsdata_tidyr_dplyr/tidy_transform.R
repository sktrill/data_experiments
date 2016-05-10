library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)

# load raw data from csv file
setwd("D:/Github/Experiments_Data_R_Py/Exp1_Play_Productsdata_tidyr_dplyr")
products <- tbl_df(read.csv("refine_original.csv",stringsAsFactors=FALSE))

# store products in separate tables for clean up
# if memory was a priority then would be better to change directly in the dataframe using:
# e.g. products$company[1:6] = "philips"
phil <- products %>% filter(grepl("^(p|P|f|F)",company)) %>% mutate(company = "philips")
akzo <- products %>% filter(grepl("^(a|A)",company)) %>% mutate(company = "akzo")
vanh <- products %>% filter(grepl("^(v|V)",company)) %>% mutate(company = "van houten")
univ <- products %>% filter(grepl("^(u|U)",company)) %>% mutate(company = "unilever")

# right bind tables into cleaned up dataframe and remove potential duplicates
products2 <- rbind(univ,akzo)
products2 <- rbind(products2,phil)
products2 <- rbind(products2,vanh)
products2 <- products2[!duplicated(products2),]

# create table for product code to categories mapping
prod_cat <- data.frame(product_code=c("p","v","x","q"), cat=c("Smartphone","TV","Laptop","Tablet"))

# tidy up product code and number into separate variable columns and left join with category table above
names(products2)[2] <- "prodcode"
products2 <- separate(products2,"prodcode",c("product_code","product_number"),sep="-")
left_join(products2,prod_cat) # joining by product_code

# add full address for geocoding
products2 <- products2 %>% mutate(full_address = paste(address,city,country,sep=", "))

# create dummy variables for company and product categories
products2 <- products2 %>% mutate(company_philips = company == "philips")
products2 <- products2 %>% mutate(company_akzo = company == "akzo")
products2 <- products2 %>% mutate(company_van_houten = company == "van houten")
products2 <- products2 %>% mutate(company_unilever = company == "unilever")
products2 <- products2 %>% mutate(product_smartphone = product_code == "p")
products2 <- products2 %>% mutate(product_tv = product_code == "v")
products2 <- products2 %>% mutate(product_laptop = product_code == "x")
products2 <- products2 %>% mutate(product_tablet = product_code == "q")

# create 'refine_clean.csv' file
write.table(products2,file="refine_clean.csv",sep = ",", row.names=FALSE,qmethod="escape", quote=TRUE)
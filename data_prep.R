library(tidyverse)
#Reading in the data
retail <- read.csv("/Users/lawrence/Downloads/quant/online_retail_II.csv")

#Preparing the data
retail %>% 
  separate(InvoiceDate, into=c('Date'), sep = " ", remove=F) %>% 
  separate(InvoiceDate, into=c('MonthYear'), sep = c(7), remove=F) %>%
  mutate(Sales = retail$Quantity * retail$Price)-> retail

# #Select returned items
retail %>% filter(Sales < 0) %>% select(StockCode,Customer.ID,Sales,Invoice,MonthYear) %>% mutate (Sales = abs(Sales))-> returns
retail %>% filter(Sales > 0) -> no_returns
# 
# #Only consider returns within 3 months after the dataset collection started which is reasonable as most returns policies will only allow returns within 30-60 days. Otherwise would take too long on my computer and not really necessary
returns <- returns[(returns$MonthYear=='2009-12' | returns$MonthYear=='2010-01' | returns$MonthYear=='2010-02'),]
# 
# #Check if the original purchase with the same metrics (Customer.ID, StockCode and +ive Sales) exists, if not remove from retail dataframe
apply(returns,MARGIN = 1, FUN = function(x){
  if(nrow(no_returns[(no_returns$StockCode==x[1] & no_returns$Customer.ID==as.double(x[2]) & no_returns$Sales==as.double(x[3])),]) == 0){
    print(retail[retail$Invoice==x[4],]$InvoiceDate)
    retail<<-retail[!(retail$Invoice==x[4]),] #Accesses and modifies global variable retail by removing the appropriate entries
  }
})

write.csv(retail,"/Users/lawrence/Downloads/retail.csv", row.names = FALSE)

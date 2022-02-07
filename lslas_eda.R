library(data.table)
library(ggplot2)

datasources<-fread("export/datasources.csv")
deals<-fread("export/deals.csv")
investors<-fread("export/investors.csv")
locations<-fread("export/locations.csv")
contracts<-fread("export/contracts.csv")

deals[,`:=`(`Deal ID`, as.integer(`Deal ID`))]
deals<-deals[grepl("Contract signed",`Negotiation status`, fixed = TRUE)]

"Displacement of people"                                                                                                        
"Number of people actually displaced"                                                                                           
"Number of households actually displaced"                                                                                       
"Number of people displaced out of their community land"                                                                        
"Number of people displaced staying on community land"   
"Comment on displacement of people"  
"Export" 
"Intention of investment"
"Size under contract (leased or purchased area, in ha)" 
"Deal scope"

deals_ts<-

lsla<-merge.data.table(deals, contracts, by="Deal ID",all.x = TRUE)

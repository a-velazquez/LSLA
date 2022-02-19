library(data.table)
library(ggplot2)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


domestic_deals <- fread("raw/domestic/deals.csv")
transnational_deals <- fread("raw/transnational/deals.csv")


clean_deals <- function(df, original, keep_countries){
  df[,`:=`(`Deal ID`, as.integer(`Deal ID`))]
  df<-df[grepl("Contract signed",`Negotiation status`, fixed = TRUE)]
  
  df[,`:=`("date"=gsub("[^0-9]","", `Negotiation status`))]
  df[,`:=`("date"=sub("#.*", "",
                         `Size under contract (leased or purchased area, in ha)`))]
  
  df[,`:=`("year"=substr(date,1,4))]
  df_tm <- df[year != ""]
  
  df_tm[, `:=`("year"=as.Date(paste0(df_tm$year, "-12-31"), "%Y-%m-%d"))]
  
  if(keep_countries){
    by_vec <- c("year", "Deal scope", "Target country")
  }else{
    by_vec <- c("year", "Deal scope")
  }
  
  df_tm<-df_tm[,.N, by=by_vec]
  
  if(original){
    return(df)
  }
  
  else{
    return(df_tm)
  }
}

domestic_deals <- clean_deals(domestic_deals, TRUE)
domestic_tm <- clean_deals(domestic_deals, FALSE, TRUE)


transnational_deals <- clean_deals(transnational_deals, TRUE)
transnational_tm <- clean_deals(transnational_deals, FALSE, TRUE)

deals_tm <- rbind(transnational_tm, domestic_tm) 
deals_tm <- deals_tm[,sum(N), by=c("year", "Deal scope")]

# datasources<-fread("raw/datasources.csv")
# deals<-fread("raw/deals.csv")
# investors<-fread("raw/investors.csv")
# locations<-fread("raw/locations.csv")
# contracts<-fread("raw/contracts.csv")

deals[,`:=`(`Deal ID`, as.integer(`Deal ID`))]
deals<-deals[grepl("Contract signed",`Negotiation status`, fixed = TRUE)]


# Variables related to outcomes of interest:
# "Displacement of people"                                                                                                        
# "Number of people actually displaced"                                                                                           
# "Number of households actually displaced"                                                                                       
# "Number of people displaced out of their community land"                                                                        
# "Number of people displaced staying on community land"   
# "Comment on displacement of people"  
# "Export" 
# "Intention of investment"
# "Size under contract (leased or purchased area, in ha)" 
# "Deal scope"

head(deals$`Negotiation status`)
# deals[,`:=`("date"=gsub("[^0-9]","", `Negotiation status`))]
deals[,`:=`("date"=gsub("[^0-9]","", `Negotiation status`))]
deals[,`:=`("date"=sub("#.*", "", `Size under contract (leased or purchased area, in ha)`))]
head(deals$date, 50)

deals[,`:=`("year"=substr(date,1,4))]

# Deals with no date attached
nrow(deals)-nrow(deals[year != ""])

# View(deals[year == ""])
# View(deals[grepl("abandoned",`Implementation status`,ignore.case = TRUE)])

abandoned<-deals[grepl("abandoned",`Implementation status`,ignore.case = TRUE)]
deals_tm <- deals[year != ""]

deals_tm[, `:=`("year"=as.Date(paste0(deals_tm$year, "-12-31"), "%Y-%m-%d"))]
deals_tm<-deals_tm[,.N, by=year]

# ggplot(deals_tm, aes(x=year, y=N))+ 
#   geom_line()+
#   geom_point()+
#   theme_minimal()+
#   labs(title="Large Scale Land Acquisitions\n",
#        x="Year", y="Number of Contracts Signed\n")+
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
#   scale_y_continuous(n.breaks = 10)+
#   theme(plot.title = element_text(hjust = 0.5))


ggplot(deals_tm[(year(year) > 1990 & year(year) <= 2022)], aes(x=year, y=V1, color =`Deal scope` ))+ 
  geom_col(data = deals_tm[(year(year) > 1990 & year(year) <= 2022),.(sum(V1)), by=year], aes(x=year, y=V1), 
           color = "#cbbeb5", fill = "#cbbeb5",inherit.aes = TRUE, alpha=0.7)+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(title="Large Scale Land Acquisitions\n",
       x="Year", y="Number of Contracts Signed\n")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(n.breaks = 10)+
  scale_color_manual(labels=c("Domestic", "Transnational"), values = c("#ff6666","#525266"))+
  theme(plot.title = element_text(hjust = 0.5))


# lsla<-merge.data.table(deals, contracts, by="Deal ID",all.x = TRUE)

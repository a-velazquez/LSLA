library(data.table)
library(ggplot2)
library(lubridate)
library(sf) 
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####  Load data

# Load deals
domestic_deals <- fread("raw/domestic/deals.csv")
transnational_deals <- fread("raw/transnational/deals.csv")


# Load investors
domestic_investors <- fread("raw/domestic/investors.csv")
transnational_investors <- fread("raw/transnational/investors.csv")

# Load locations
domestic_locations <- fread("raw/domestic/locations.csv")
transnational_locations <- fread("raw/transnational/locations.csv")

#### Summarize key information

## Transnational Deals

# Deal size
summary(transnational_deals$`Deal size`)
# Negotiation status
unique(transnational_deals$`Current negotiation status`)
# Implementation status
unique(transnational_deals$`Current implementation status`)
unique(transnational_deals$`Intention of investment`)
unique(transnational_deals$`Target country`)
summary(transnational_deals$`Fully updated`)
length(unique(transnational_deals$`Deal ID`))


summary(domestic_deals$`Deal size`)

unique(domestic_deals$`Current negotiation status`)
unique(domestic_deals$`Current implementation status`)
unique(domestic_deals$`Intention of investment`)
unique(domestic_deals$`Target country`)
length(unique(domestic_deals$`Deal ID`))

clean_deals <- function(df, original, add_items=c()){
  # Make deal id integer
  df[,`:=`(`Deal ID`, as.integer(`Deal ID`))]
  
  # Subset only to concluded deals
  # df<-df[grepl("Contract signed",`Negotiation status`, fixed = TRUE)]
  
  # Create "date" column using size under contract information
  # df[,`:=`("date"=gsub("[^0-9]","", `Negotiation status`))]
  df[,`:=`("date"=sub("#.*", "",
                         `Size under contract (leased or purchased area, in ha)`))]
  
  df[,`:=`("year"=substr(date,1,4))]
  df_tm <- df[year != ""]
  
  df_tm[, `:=`("year"=as.Date(paste0(df_tm$year, "-12-31"), "%Y-%m-%d"))]
  
  by_vec <- c(c("year", "Deal scope"), add_items)

  df_tm<-df_tm[,.N, by=by_vec]
  
  if(original){
    return(df)
  }
  
  else{
    return(df_tm)
  }
}


transnational_deals <- clean_deals(transnational_deals, TRUE)
domestic_deals <- clean_deals(domestic_deals, TRUE)

Ndeals_domestic <- clean_deals(domestic_deals, FALSE)
Ndeals_transnational <- clean_deals(transnational_deals, FALSE)

countryDeals_domestic <- clean_deals(domestic_deals, FALSE, 
                                     add_items = c("Target country"))
countryDeals_transnational <- clean_deals(transnational_deals, FALSE, 
                                     add_items = c("Target country"))

negtiationDeals_transnational <- clean_deals(domestic_deals, FALSE, 
                                             add_items = c("Current negotiation status"))
nrgotiationDeals_domestic <- clean_deals(transnational_deals, FALSE, 
                                         add_items = c("Current negotiation status"))

implemDeals_transnational <- clean_deals(domestic_deals, FALSE, 
                                         add_items = c("Current implementation status"))
implemDeals_domestic <- clean_deals(transnational_deals, FALSE, 
                                    add_items = c("Current implementation status"))

allDeals_domestic <- clean_deals(domestic_deals, FALSE, 
                                 add_items = c("Target country",
                                               "Current negotiation status",
                                               "Current implementation status"))
allDeals_transnational <- clean_deals(transnational_deals, FALSE, 
                                      add_items = c("Target country",
                                                    "Current negotiation status",
                                                    "Current implementation status"))
  
domestic_locations <- merge(domestic_locations, unique(domestic_deals[,.(`Deal ID`,`Target country`)]), by="Deal ID")
domestic_locations[, c("Lat", "Long") := tstrsplit(Point, ",", fixed=TRUE)]
domestic_locations <- domestic_locations[!is.na(Lat) & !is.na(Long)]

br_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Brazil"], coords = c("Long", "Lat"), crs=4674)
pr_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Peru"], coords = c("Long", "Lat"), crs=4326)


transnational_locations <- merge(transnational_locations, unique(transnational_deals[,.(`Deal ID`,`Target country`)]), by="Deal ID")
transnational_locations[, c("Lat", "Long") := tstrsplit(Point, ",", fixed=TRUE)]

br_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Brazil"], coords = c("Long", "Lat"), crs=4674)
pr_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Peru"], coords = c("Long", "Lat"),crs=4326)



br_states <- st_read("raw/BR_UF_2021/BR_UF_2021.shp")
pr_states <- st_read("raw/per_adm_ign_20200714_shp/per_admbnda_adm1_ign_20200714.shp")


inf_br <- as.data.table(read_excel("raw/infrastructure_BR.xlsx"))
setnames(inf_br, c("Type of land registration system in the economy:"), c("land_registration_system"))
inf_br[Location=="Federal District", Location := "Distrito Federal"]


inf_pr <- as.data.table(read_excel("raw/infrastructure_PR.xlsx"))
setnames(inf_pr, c("Type of land registration system in the economy:"), c("land_registration_system"))

ldr_br <- as.data.table(read_excel("raw/ldr_BR.xlsx"))
setnames(ldr_br, c("Land dispute resolution index (0–8)"), c("land_dispute_resolution_index"))
ldr_br[Location=="Federal District", Location := "Distrito Federal"]
ldr_pr <- as.data.tabl(read_excel("raw/ldr_PR.xlsx"))
setnames(ldr_pr, c("Land dispute resolution index (0–8)"), c("land_dispute_resolution_index"))


br_states <- merge(br_states, inf_br[,.(Location, land_registration_system)], by.x="NM_UF", by.y="Location")
br_states <- merge(br_states, ldr_br[,.(Location, land_dispute_resolution_index)], by.x="NM_UF", by.y="Location")

# br_states <- st_transform(br_states, crs=4674)
# br_states <- st_set_crs(br_states, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")



ggplot() +
  geom_sf(data=br_states,aes(fill=land_dispute_resolution_index)) +
  geom_sf_label(data=br_states,aes(label = NM_UF)) +
  geom_sf(data=br_domestic_deals)




ggplot(pr_states) +
  geom_sf() +
  geom_sf_label(aes(label = ADM1_ES))


# Only run from here above

################
#################
#################


deals_tm <- rbind(transnational_tm, domestic_tm)
deals_tm <- deals_tm[,sum(N), by=c("year", "Deal scope")]



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
       x="\nYear", y="Number of Contracts Signed\n")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = c(as.Date("12-31-1990", "%Y-%m-%d"),as.Date("12-31-2021", "%Y-%m-%d"))
               ,expand=c(0,0))+
  scale_y_continuous(n.breaks = 10)+
  scale_color_manual(labels=c("Domestic", "Transnational"), values = c("#ff6666","#525266"))+
  theme(plot.title = element_text(hjust = 0.5))

# ,axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1,margin = margin(t = 0, r = 0, b = 0, l = 0))

# ggsave("lsla_timeline.jpg", width = 1200, height = 400, units="mm",scale = 0.3)
ggsave("lsla_timeline.pdf")

# lsla<-merge.data.table(deals, contracts, by="Deal ID",all.x = TRUE)

library(data.table)
library(ggplot2)
library(lubridate)
library(sf) 
library(readxl)
library(ggnewscale)

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
  
  df_tm[, `:=`("year"=as.Date(paste0(df_tm$year, "-01-01"), "%Y-%m-%d"))]
  
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
py_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Paraguay"], coords = c("Long", "Lat"), crs=4674)
pr_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Peru"], coords = c("Long", "Lat"), crs=4326)
bl_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Bolivia"], coords = c("Long", "Lat"), crs=4326)
ar_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Argentina"], coords = c("Long", "Lat"), crs=4326)
lb_domestic_deals <- st_as_sf(domestic_locations[`Target country`=="Liberia"], coords = c("Long", "Lat"), crs=4326)


transnational_locations <- merge(transnational_locations, unique(transnational_deals[,.(`Deal ID`,`Target country`)]), by="Deal ID")
transnational_locations[, c("Lat", "Long") := tstrsplit(Point, ",", fixed=TRUE)]

br_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Brazil"], coords = c("Long", "Lat"), crs=4674)
py_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Paraguay"], coords = c("Long", "Lat"), crs=4674)
pr_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Peru"], coords = c("Long", "Lat"),crs=4326)
bl_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Bolivia"], coords = c("Long", "Lat"),crs=4326)
ar_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Argentina"], coords = c("Long", "Lat"),crs=4326)
lb_transnational_deals <- st_as_sf(transnational_locations[`Target country`=="Liberia"], coords = c("Long", "Lat"),crs=4326)




br_states <- st_read("raw/BR_UF_2021/BR_UF_2021.shp")
br_municipalities <- st_read("raw/bra_adm_ibge_2020_shp/bra_admbnda_adm2_ibge_2020.shp")
pr_states <- st_read("raw/per_adm_ign_20200714_shp/per_admbnda_adm1_ign_20200714.shp")
py_states <- st_read("raw/pry_adm_dgeec_2020_shp/pry_admbnda_adm1_DGEEC_2020.shp")

bl_states <- st_read("raw/bol_adm_gb2014_shp/bol_admbnda_adm1_gov_2020514.shp")
ar_states <- st_read("raw/arg_adm_unhcr2017_shp/arg_admbnda_adm1_unhcr2017.shp")


geoepr <- st_read("raw/GeoEPR-2021/GeoEPR-2021.shp")

  
growup <-fread("raw/growup/data.csv")
# View(growup[,max(year), by=eval(colnames(growup)[colnames(growup)!="year"])])
growup <- growup[year==max(year)]
growup[,"groupid" := substr(gwgroupid, 5,8)]

growupBR2020 <- growup[year==max(year) & countryname=="Brazil"]
growupPY2020 <- growup[year==max(year) & countryname=="Paraguay"]



geoepr <- merge(geoepr, unique(growup[,.(gwgroupid, statusname)]), 
                by="gwgroupid")

br_greg <- greg[greg$FIPS_CNTRY=="BR",]
  
inf_br <- as.data.table(read_excel("raw/infrastructure_BR.xlsx"))
setnames(inf_br, c("Type of land registration system in the economy:"), c("land_registration_system"))
inf_br[Location=="Federal District", Location := "Distrito Federal"]


inf_pr <- as.data.table(read_excel("raw/infrastructure_PR.xlsx"))
setnames(inf_pr, c("Type of land registration system in the economy:"), c("land_registration_system"))

ldr_br <- as.data.table(read_excel("raw/ldr_BR.xlsx"))
setnames(ldr_br, c("Land dispute resolution index (0–8)"), c("land_dispute_resolution_index"))
ldr_br[Location=="Federal District", Location := "Distrito Federal"]

ldr_pr <- as.data.table(read_excel("raw/ldr_PR.xlsx"))
setnames(ldr_pr, c("Land dispute resolution index (0–8)"), c("land_dispute_resolution_index"))

# Merge geo data with World Bank indices
br_states <- merge(br_states, inf_br[,.(Location, land_registration_system)], by.x="NM_UF", by.y="Location")
br_states <- merge(br_states, ldr_br[,.(Location, land_dispute_resolution_index)], by.x="NM_UF", by.y="Location")

pr_states <- merge(pr_states, inf_pr[,.(Location, land_registration_system)], by.x="ADM1_ES", by.y="Location", all.x=T)
pr_states <- merge(pr_states, ldr_pr[,.(Location, land_dispute_resolution_index)], by.x="ADM1_ES", by.y="Location", all.x=T)


# Make sure CRS is properly specified
br_states <- st_set_crs(br_states, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")
br_states <- st_transform(br_states, crs=4674)


bl_states <- st_set_crs(bl_states, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")
bl_states <- st_transform(bl_states, crs=4674)

ar_states <- st_set_crs(ar_states, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")
ar_states <- st_transform(ar_states, crs=4674)

br_municipalities <- st_set_crs(br_municipalities, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")
br_municipalities <- st_transform(br_municipalities, crs=4674)


ggplot() +
  geom_sf(data=br_states) +
  geom_sf(data=geoepr[geoepr$statename=="Brazil",], aes(fill=statusname), alpha=0.2, lwd=0, show.legend = "polygon")+
  scale_fill_manual(name="Ethno-Ling. Group Status", labels=c("Monopoly", "Powerless"), values = c("blue","red"))+
  # geom_sf_label(data=br_states,aes(label = NM_UF)) +
  geom_sf(data=br_domestic_deals, aes(color="br_domestic_deals"), size=2.5, alpha=0.3, show.legend = "point") +
  geom_sf(data=br_transnational_deals, inherit.aes = FALSE, aes(color="br_transnational_deals"), size=2.5, alpha=0.3, show.legend = "point")+
  scale_color_manual(name="Deal Scope", labels=c("Domestic Deals","Transnational Deals"), values = c( "blue","green"))+
  theme_minimal()+
  guides(fill = guide_legend(override.aes = list(color = c(NA, NA) ) ) )


ggplot() +
  geom_sf(data=py_states) +
  geom_sf(data=geoepr[geoepr$statename=="Paraguay",], aes(fill=statusname), alpha=0.2, lwd=0, show.legend = "polygon")+
  scale_fill_manual(name="Ethno-Ling. Group Status", labels=c("Monopoly", "Powerless"), values = c("blue","red"))+
  # geom_sf_label(data=py_states,aes(label = NM_UF)) +
  geom_sf(data=py_domestic_deals, aes(color="py_domestic_deals"), size=2.5, alpha=0.3, show.legend = "point") +
  geom_sf(data=py_transnational_deals, inherit.aes = FALSE, aes(color="br_transnational_deals"), size=2.5, alpha=0.3, show.legend = "point")+
  scale_color_manual(name="Deal Scope", labels=c("Domestic Deals","Transnational Deals"), values = c( "blue","green"))+
  theme_minimal()+
  guides(fill = guide_legend(override.aes = list(color = c(NA, NA) ) ) )


ggplot() +
  geom_sf(data=pr_states) +
  geom_sf(data=geoepr[geoepr$statename=="Peru",], aes(fill=statusname), alpha=0.2, lwd=0, show.legend = "polygon")+
  scale_fill_manual(name="Ethno-Ling. Group Status", labels=c("Dominant", "Powerless"), values = c("blue","red"))+
  # geom_sf_label(data=py_states,aes(label = NM_UF)) +
  geom_sf(data=pr_domestic_deals, aes(color="pr_domestic_deals"), size=2.5, alpha=0.3, show.legend = "point") +
  geom_sf(data=pr_transnational_deals, inherit.aes = FALSE, aes(color="pr_transnational_deals"), size=2.5, alpha=0.3, show.legend = "point")+
  scale_color_manual(name="Deal Scope", labels=c("Domestic Deals","Transnational Deals"), values = c( "blue","green"))+
  theme_minimal()+
  guides(fill = guide_legend(override.aes = list(color = c(NA, NA) ) ) )


ggplot() +
  geom_sf(data=bl_states) +
  geom_sf(data=geoepr[geoepr$statename=="Bolivia",], aes(fill=statusname), alpha=0.2, lwd=0, show.legend = "polygon")+
  scale_fill_manual(name="Ethno-Ling. Group Status", labels=c("Dominant", "Powerless"), values = c("blue","red"))+
  # geom_sf_label(data=py_states,aes(label = NM_UF)) +
  geom_sf(data=bl_domestic_deals, aes(color="bl_domestic_deals"), size=2.5, alpha=0.3, show.legend = "point") +
  geom_sf(data=bl_transnational_deals, inherit.aes = FALSE, aes(color="bl_transnational_deals"), size=2.5, alpha=0.3, show.legend = "point")+
  scale_color_manual(name="Deal Scope", labels=c("Domestic Deals","Transnational Deals"), values = c( "blue","green"))+
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(color = c(NA, NA) ) ) )



ggplot() +
  geom_sf(data=ar_states) +
  geom_sf(data=geoepr[geoepr$statename=="Argentina",], aes(fill=statusname), alpha=0.2, lwd=0, show.legend = "polygon")+
  scale_fill_manual(name="Ethno-Ling. Group Status", labels=c("Monopoly", "Powerless"), values = c("blue","red"))+
  # geom_sf_label(data=py_states,aes(label = NM_UF)) +
  geom_sf(data=ar_domestic_deals, aes(color="ar_domestic_deals"), size=2.5, alpha=0.3, show.legend = "point") +
  geom_sf(data=ar_transnational_deals, inherit.aes = FALSE, aes(color="ar_transnational_deals"), size=2.5, alpha=0.3, show.legend = "point")+
  scale_color_manual(name="Deal Scope", labels=c("Domestic Deals","Transnational Deals"), values = c( "blue","green"))+
  theme_minimal()+
  guides(fill = guide_legend(override.aes = list(color = c(NA, NA) ) ) )



adm_liberia_rep <- readRDS("raw/adm-boundaries.rds")
inst_boundary_liberia_rep <- readRDS("raw/institutional-boundary.rds")

# names(adm_liberia_rep)

smallest_subnational <- adm_liberia_rep$adm3

ggplot()+
  geom_sf(data=smallest_subnational, lwd=0.15)+
  geom_sf(data=inst_boundary_liberia_rep$`40mi`, lwd=1)+
  geom_sf(data=lb_domestic_deals, aes(color="lb_domestic_deals"), size=2.5, alpha=0.6, show.legend = "point") +
  geom_sf(data=lb_transnational_deals, inherit.aes = FALSE, aes(color="lb_transnational_deals"), size=2.5, alpha=0.6, show.legend = "point")+
  scale_color_manual(name="Deal Scope", labels=c("Domestic Deals","Transnational Deals"), values = c( "blue","green"))+
  theme_minimal()







# ggplot() +
#   geom_sf(data=br_municipalities, lwd=0.06) +
#   # geom_sf_label(data=br_states,aes(label = NM_UF)) +
#   geom_sf(data=br_domestic_deals, size=2.5, alpha=0.3, color="blue") +
#   geom_sf(data=br_transnational_deals, size=2.5, alpha=0.1, color="green")+
#   geom_sf(data=geoepr[geoepr$statename=="Brazil" & geoepr$groupid==1000,], aes(fill=factor(groupid)), alpha=0.1)+
#   geom_sf(data=geoepr[geoepr$statename=="Brazil" & geoepr$groupid==2000,], aes(fill=factor(groupid)), alpha=0.1)+
#   geom_sf(data=geoepr[geoepr$statename=="Brazil" & geoepr$groupid==3000,], aes(fill=factor(groupid)), alpha=0.1)



ggplot() +
  geom_sf(data=py_states) +
  geom_sf_label(data=py_states,aes(label = ADM1_ES)) +
  geom_sf(data=py_domestic_deals, size=2.5, alpha=0.5, color="blue") +
  geom_sf(data=py_transnational_deals, size=2.5, alpha=0.5, color="green")+
  geom_sf(data=geoepr[geoepr$statename=="Paraguay",], aes(fill=factor(groupid)), alpha=0.2)+
  theme_minimal()

ggplot() +
  geom_sf(data=pr_states) +
  # geom_sf(data=pr_states,aes(fill=land_dispute_resolution_index)) +
  geom_sf_label(data=pr_states,aes(label = ADM1_ES)) +
  geom_sf(data=pr_domestic_deals, size=2.5, alpha=0.6, color="blue") +
  geom_sf(data=pr_transnational_deals, size=2.5, alpha=0.6, color="green")+
  geom_sf(data=geoepr[geoepr$statename=="Peru",], aes(fill=factor(groupid)), alpha=0.2)+
  theme_minimal()

ggplot(geoepr[geoepr$statename=="Paraguay",]) +
  geom_sf(aes(fill=groupid))


deals_tm <- rbind(countryDeals_domestic, countryDeals_transnational)
deals_tm <- deals_tm[,sum(N), by=c("year", "Deal scope")]


deals_tm[, `:=`("year"=as.Date(paste0(deals_tm$year, "-01-01"), "%Y-%m-%d"))]


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

# ggsave("lsla_timeline.pdf")


# Variables related to outcomes of interest, to explore later:
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




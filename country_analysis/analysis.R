library(data.table)
library(ggplot2)
library(kableExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Cleaned Country-Level Variables
load("country_data.Rdata")


## LSLA Plots and Tables
implemDeals <- implemDeals[,.(N=sum(N)),by=c("Current implementation status")]

kable_styling(kbl(implemDeals, booktabs = TRUE,format = "latex"),
              full_width = TRUE,
              position = "center",
              latex_options = c("striped", "scale_down"))


implemnegDeals <- multidimDeals[,.(N=sum(N)),
                                by=c("Current negotiation status",
                                     "Current implementation status")]

implemnegDeals <- implemnegDeals[order(-N,`Current negotiation status`)]

top_10_status_table <- kable_styling(kbl(implemnegDeals[1:10,], booktabs = TRUE,format = "latex"),
              full_width = TRUE,
              position = "center",
              latex_options = c("striped", "scale_down"))


# Count by year, scope
ggplot(Ndeals[(year(year_initiated) > 1990 & year(year_initiated) <= 2021)], aes(x=year_initiated, y=N, color =`Deal scope` ))+ 
  geom_col(data = Ndeals[(year(year_initiated) > 1990 & year(year_initiated) <= 2021),.(sum(N)), by=year_initiated], aes(x=year_initiated, y=V1), 
           color = "#cbbeb5", fill = "#cbbeb5",inherit.aes = TRUE, alpha=0.7)+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="\nYear", y="Number of Deals Initiated\n")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = c(as.Date("12-31-1990", "%Y-%m-%d"),as.Date("12-31-2021", "%Y-%m-%d"))
               ,expand=c(0,0))+
  scale_y_continuous(n.breaks = 10)+
  scale_color_manual(labels=c("Domestic", "Transnational"), values = c("#ff6666","#525266"))+
  theme(plot.title = element_text(hjust = 0.5))
# +
#   ggtitle("Large Scale Land Acquisitions\n")
ggsave("outputs/world_LSLAs.png",width=1300, height=800, scale=2, units = "px", bg="white")


# Count by region, year
ggplot(regionDeals[(year(year_initiated) > 1990 & year(year_initiated) <= 2021),.(sum(N)), by=c("year_initiated","Region")], 
       aes(x=year_initiated, y=V1, color =Region))+ 
  geom_line()+
  geom_point()+
  theme_minimal()+
  scale_color_brewer(palette = "Set2") +
  labs(x="\nYear", y="Number of Deals Initiated\n")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = c(as.Date("12-31-1990", "%Y-%m-%d"),as.Date("12-31-2021", "%Y-%m-%d")))+
  scale_y_continuous(n.breaks = 10)+
  theme(plot.title = element_text(hjust = 0.5))
# +
#   ggtitle("Large Scale Land Acquisitions\n")
ggsave("outputs/regional_LSLAs.png",width=1300, height=600, scale=2, units = "px", bg="white")




# Cumulative by region, year
ggplot(regionDeals[(year(year_initiated) > 1990 & year(year_initiated) <= 2021),.(year_initiated,Region, `Total Cumulative`),], 
       aes(x=year_initiated, y=`Total Cumulative`, color=Region, shape=Region))+ 
  # geom_line()+
  geom_point(size=3, alpha=0.6)+
  theme_minimal()+
  scale_color_brewer(palette = "Set2") +
  labs(x="\nYear", y="Number of Deals Initiated\n")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = c(as.Date("12-31-1990", "%Y-%m-%d"),as.Date("12-31-2021", "%Y-%m-%d")))+
  scale_y_continuous(n.breaks = 10)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("outputs/cumulative_world_LSLAs.png",width=1300, height=800, scale=2, units = "px", bg="white")


regionscopeDeals <- regionDeals[,.(N=sum(N)),
                                by=c("Region",
                                     "Deal scope")]

regionscopeDeals <- regionscopeDeals[order(- Region,`Deal scope`)]

regionscope_table <- kable_styling(kbl(regionscopeDeals, booktabs = TRUE,format = "latex"),
                                     full_width = TRUE,
                                     position = "center",
                                     latex_options = c("striped", "scale_down"))

countrytotals <- countryDeals[,.(N=sum(N)),
                                by=c("Target country")]

countrytotals <- countrytotals[order(-N)]

top_10_countries_table <- kable_styling(kbl(countrytotals[1:20,], booktabs = TRUE,format = "html"),
                                     full_width = TRUE,
                                     position = "center",
                                     latex_options = c("striped", "scale_down"))

dump(c("top_10_status_table", "regionscope_table", "top_10_countries_table"), file="outputs/all_tables.tex")

names(inv_protection)[3:ncol(inv_protection)] <- substr(names(inv_protection)[3:ncol(inv_protection)], 1, 4)

inv_protection <- melt(inv_protection, id.vars = c("Country ISO3", "Country Name"), 
                       variable.name = "Year", value.name = "Inv_Protection_Rank")

inv_protection[, `:=`("Year"=as.Date(paste0(inv_protection$Year, "-01-01"), "%Y-%m-%d"))]

inv_protection[, "in_LSLA" := (`Country Name` %in% countrytotals$`Target country`)]

ggplot()+
  geom_histogram(data = inv_protection, aes(x=Inv_Protection_Rank), binwidth = 5, fill="#2986cc", color="#144366", alpha=0.5)+
  geom_histogram(data = inv_protection[in_LSLA==TRUE], aes(x=Inv_Protection_Rank), binwidth = 5, fill="#a64d79", color="#53263c", alpha=0.5)+
  theme_minimal()


inv_protection_summary <- data.table("Sample"=c("World","LSLA Target Countries"),
                            "Min"=c(min(inv_protection[,Inv_Protection_Rank], na.rm = TRUE),
                                    min(inv_protection[in_LSLA==TRUE,Inv_Protection_Rank], na.rm = TRUE)),
                            "Mean"=c(mean(inv_protection[,Inv_Protection_Rank], na.rm = TRUE),
                                     mean(inv_protection[in_LSLA==TRUE,Inv_Protection_Rank], na.rm = TRUE)),
                            "Max"=c(max(inv_protection[,Inv_Protection_Rank], na.rm = TRUE),
                                    max(inv_protection[in_LSLA==TRUE,Inv_Protection_Rank], na.rm = TRUE)))




inv_protection_summary_table <- kable_styling(kbl(inv_protection_summary, booktabs = TRUE,format = "latex"),
                                   full_width = TRUE,
                                   position = "center",
                                   latex_options = c("striped", "scale_down"))



countryDeals <- countryDeals[(year(year) > 1990 & year(year) <= 2022),.(sum(N)), by=c("year","Target country")]
countryDeals <- merge.data.table(countryDeals, country_regions[,.(Country, Abbrev)], 
                 by.x = "Target country", by.y = "Country" )


countryDealsInv <- merge.data.table(countryDeals, inv_protection, 
                                    by.x = c("Target country","year"),
                                    by.y = c("Country Name", "Year"))

ggplot(countryDealsInv, aes(x=Inv_Protection_Rank, y=V1)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm") +
  theme_minimal()


countryDeals <- countryDeals[(year(year) > 1990 & year(year) <= 2022),.(sum(V1)), by=c("Target country")]

inv_protection <- inv_protection[, .("Avg_Rank" = mean(Inv_Protection_Rank, na.rm=T)), by=c("Country ISO3","Country Name")]
  
countryDealsInv <- merge.data.table(countryDeals, inv_protection, 
                                    by.x = c("Target country"),
                                    by.y = c("Country Name"))

ggplot(countryDealsInv, aes(x=Avg_Rank, y=V1)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm") +
  theme_minimal()

countryDealsInv <- merge.data.table(countryDeals, land_tenure_vars, 
                                    by.x = c("Target country"),
                                    by.y = c("Country"))


ggplot(countryDealsInv, aes(x=`B6071_public_policies_access_land:_disadvantaged_groups`, y=log(V1))) +
  # geom_violin() +
  geom_point(alpha=0.4, size=4) +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(countryDealsInv, aes(x=`B6083_land_tenure_rights:_rural_areas`, y=log(V1))) +
  geom_boxplot() +
  geom_point(alpha=0.4, size=2) +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(countryDealsInv, aes(x=`B6083_land_tenure_rights:_rural_areas`, y=`B6071_public_policies_access_land:_disadvantaged_groups`)) +
  geom_point(alpha=0.4, size=4) +
  geom_smooth(method = "lm") +
  theme_minimal()

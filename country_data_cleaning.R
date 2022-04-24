library(data.table)
library(ggplot2)
library(OECD)
library(countrycode)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Dependent Variables

## Land Grabs
domestic_deals <- fread("raw/domestic/deals.csv")
transnational_deals <- fread("raw/transnational/deals.csv")


# Get world regions
country_regions <- data.table(unique(rbind(cbind("Country"=unique(transnational_deals$`Target country`), 
                                                 "Region"=countrycode(unique(transnational_deals$`Target country`), 
                                                                      "country.name.en.regex", "region")),
                                           cbind("Country"=unique(domestic_deals$`Target country`), 
                                                 "Region"=countrycode(unique(domestic_deals$`Target country`), 
                                                                      "country.name.en.regex", "region")))))

country_regions[, "Abbrev" := countrycode(country_regions$Country, "country.name.en.regex", "genc3c") ]
country_regions[, "Abbrev_Iso" := countrycode(country_regions$Country, "country.name.en.regex", "iso3c") ]

clean_deals <- function(df, original, add_items=c()){
  # Make deal id integer
  df[,`:=`(`Deal ID`, as.integer(`Deal ID`))]

  # Create "date" column using size under contract information
  df[,`:=`("earliest_size_contract_date"=substr(sub("#.*", "",
                      `Size under contract (leased or purchased area, in ha)`),1,4))]
  
  df[earliest_size_contract_date=="", "earliest_size_contract_date" := substr(Sys.Date(), 1,4)]
  
  df[,`:=`("earliest_size_operation_date"=substr(sub("#.*", "",
                                             `Size in operation (production, in ha)`),1,4))]
  
  df[earliest_size_operation_date=="", "earliest_size_operation_date" := substr(Sys.Date(), 1,4)]
  
  
  df[,`:=`("earliest_intention_date"=substr(sub("#.*", "",
                       `Intention of investment`),1,4))]
  
  df[earliest_intention_date=="", "earliest_intention_date" := substr(Sys.Date(), 1,4)]
  
  
  df[,`:=`("earliest_negotiation_date"=substr(sub("#.*", "",
                                         `Negotiation status`),1,4))]
  
  df[earliest_negotiation_date=="", "earliest_negotiation_date" := substr(Sys.Date(), 1,4)]
  
  
  df[,`:=`("earliest_implementation_date"=substr(sub("#.*", "",
                                           `Implementation status`),1,4))]
  
  df[earliest_implementation_date=="", "earliest_implementation_date" := substr(Sys.Date(), 1,4)]
  
  
  df[,`:=`("year_initiated" = pmin(earliest_size_contract_date,
                                  earliest_size_operation_date,
                                  earliest_intention_date,
                                  earliest_negotiation_date,
                                  earliest_implementation_date, na.rm = TRUE))]
  
  if(!("Region" %in% names(df))){
    df <- merge.data.table(df, country_regions, by.x = "Target country", by.y = "Country", all.x = T)
  }
  
 
  # Prepare summaries by year_initiated
  df_tm <- df[year_initiated != ""]
  
  df_tm[, `:=`("year_initiated"=as.Date(paste0(df_tm$year_initiated, "-01-01"), "%Y-%m-%d"))]
  
  by_vec <- c(c("year_initiated", "Deal scope"), add_items)
  
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

# Count deals by year_initiated
Ndeals <- rbind(clean_deals(domestic_deals, FALSE), 
                clean_deals(transnational_deals, FALSE))


Ndeals <- Ndeals[order(as.numeric(substr(year_initiated,1,4))), "Cumulative" := cumsum(N), by=c("Deal scope")]

# Count deals by country year_initiated
countryDeals <- rbind(clean_deals(domestic_deals, FALSE, add_items = c("Target country")),
                      clean_deals(transnational_deals, FALSE, add_items = c("Target country")))


countryDeals <- countryDeals[order(as.numeric(substr(year_initiated,1,4))), "Total Cumulative" := cumsum(N), by=c("Target country")]
countryDeals <- countryDeals[order(as.numeric(substr(year_initiated,1,4))), "Country Scope Cumulative" := cumsum(N), by=c("Target country","Deal scope")]

# Count deals by negotiation status and year_initiated
negotiationDeals <- rbind(clean_deals(domestic_deals, FALSE, 
                                      add_items = c("Current negotiation status")),
                          clean_deals(transnational_deals, FALSE, 
                                      add_items = c("Current negotiation status")))

# Count deals by implementation status and year_initiated
implemDeals <- rbind(clean_deals(domestic_deals, FALSE, 
                                 add_items = c("Current implementation status")),
                     clean_deals(transnational_deals, FALSE, 
                                 add_items = c("Current implementation status")))


# Count deals by all dimensions above
multidimDeals <- rbind(clean_deals(domestic_deals, FALSE, 
                                   add_items = c("Target country",
                                                 "Current negotiation status",
                                                 "Current implementation status")),
                       clean_deals(transnational_deals, FALSE, 
                                   add_items = c("Target country",
                                                 "Current negotiation status",
                                                 "Current implementation status")))

# Count deals by region year_initiateds
regionDeals <- rbind(clean_deals(domestic_deals, FALSE, add_items = c("Region")),
                      clean_deals(transnational_deals, FALSE, add_items = c("Region")))

regionDeals <- regionDeals[order(as.numeric(substr(year_initiated,1,4))), "Total Cumulative" := cumsum(N), by=c("Region")]
regionDeals <- regionDeals[order(as.numeric(substr(year_initiated,1,4))), "Region Scope Cumulative" := cumsum(N), by=c("Region","Deal scope")]

# saveRDS(city, file="city.rds")

#### Independent Variables/Controls
## Land Tenure
land_tenure_vars <- data.table(read_excel("raw/countries_IPD_2016_EN.xlsx", sheet = 4))

# Rename first 3 cols
names(land_tenure_vars)[1:3] <- as.character(land_tenure_vars[3,1:3])

# Manipulate second row as part of col names
land_tenure_vars[2, names(land_tenure_vars) := as.list(tolower(land_tenure_vars[2]))]

# Remove parentheses
land_tenure_vars[2, names(land_tenure_vars) := as.list(gsub("(", "", land_tenure_vars[2], fixed=T))]
land_tenure_vars[2, names(land_tenure_vars) := as.list(gsub(")", "", land_tenure_vars[2], fixed=T))]

# Remove stopwords
land_tenure_vars[2, names(land_tenure_vars) := as.list(gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                                                                   collapse = '|'), "", land_tenure_vars[2], fixed=F))]

# Remove double spaces
land_tenure_vars[2, names(land_tenure_vars) := as.list(gsub("  ", " ", land_tenure_vars[2], fixed=T))]

# Insert underscores
land_tenure_vars[2, names(land_tenure_vars) := as.list(gsub(" ", "_", land_tenure_vars[2], fixed=T))]

# Change column names
names(land_tenure_vars)[4:ncol(land_tenure_vars)] <- paste(names(land_tenure_vars)[4:ncol(land_tenure_vars)],
                                                           as.character(land_tenure_vars[2,4:ncol(land_tenure_vars)]),
                                                           sep = "_")
# Remove extra rows
land_tenure_vars <- land_tenure_vars[4:nrow(land_tenure_vars)]


# Subset to land tenure vars
land_cols <- c(names(land_tenure_vars)[1:3] , names(land_tenure_vars)[grepl("land", names(land_tenure_vars), fixed = T)])
land_tenure_vars <- land_tenure_vars[,..land_cols]


# Reduced indicators
land_tenure_inds <- data.table(read_excel("raw/countries_IPD_2016_EN.xlsx", sheet = 5))

## Investor Protection
# unique(inv_protection$Indicator)

inv_protection <- fread("raw/countries_investment_protection.csv")

inv_protection <- inv_protection[Indicator=="Strength of investor protection" & `Subindicator Type`=="Rank"]
inv_protection[!rowSums(inv_protection[,6:ncol(inv_protection)], na.rm = T)]

inv_protection <- data.table(as.data.frame(inv_protection)[,colSums(is.na(inv_protection))<nrow(inv_protection)])
inv_protection[,3:5] <- NULL

invprot_doingbusiness <- read_excel("raw/countries_historical_doing_business.xlsx")
invprot_doingbusiness[2][grepl("invest", invprot_doingbusiness[2], fixed = T)]

# Other indicators of interest
# "Agricultural policy costs, 1-7 (best)"
# "Property rights, 1-7 (best)"  
# "Property rights (WEF)"
# "Business impact of rules on FDI, 1-7 (best)"
# "Favoritism in decisions of government officials, 1-7 (best)" 
# "Protection of minority shareholders interests, 1-7 (best)" 
# "Irregular payments and bribes, 1-7 (best)"
# "Ethical behavior of firms, 1-7 (best)"
# "Transparency of government policymaking, 1-7 (best)"
# "3. Undue influence" 
# "2. Ethics and corruption"
# "Diversion of public funds, 1-7 (best)"
# "Effectiveness of antimonopoly policy"
# "Transparency of government policymaking" 
# "Ethical behavior of firms" 
# "Irregular payments and bribes"
# "Business impact of rules on FDI"  
# "Prevalence of foreign ownership"
# "Strength of investor protection"
# "Total tax rate % profits"
# "Protection of minority shareholders interests" 
# "Strength of investor protection, 0-10 (best)*" 


## Land Cover & Soil Quality
# search_dataset("land")
landcov <- fread("raw/countries_LAND_USE.csv")

country_regions[!country_regions$Abbrev %in% unique(landcov$COU), Country]

landcov <- landcov[`Unit Code`=="PC"]


country_areas <- fread("raw/countries_land_cover_FAOSTAT.csv")


attainable_yield <- fread("https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Attainable%20yields%20(Mueller%20et%20al.%202012)/Attainable%20yields%20(Mueller%20et%20al.%202012).csv")
attainable_yield <- attainable_yield[Year==max(Year)]



## Agriculture FDI and Gov Expenditures
fdi <- fread("raw/countries_fdi.csv")
govexp <- fread("raw/countries_govexp.csv")
creditag <- fread("raw/countries_creditag.csv")


save(country_regions, countryDeals, implemDeals, 
     inv_protection, land_tenure_vars, multidimDeals,
     Ndeals, negotiationDeals, regionDeals, file="country_analysis/country_data.Rdata")

# https://data.ecb.europa.eu/
# docs: https://data.ecb.europa.eu/help/api/data

library(tidyverse)
library(ecb) # ECB package to ease work with endpoints

### set parameters (last 12 months)
filter <- list(lastNObservations = 13, detail = "full")

######################################################
########## HEADLINE inflation
#####################################################

# HICP - Overall index, Euro area (changing composition), Monthly
key <- "ICP.M.U2.N.000000.4.ANR"

# if wanting additional countries
#key <- "ICP.M.DE+FR+ES+IT+NL+U2.N.000000+XEF000+SERV00+GOODS0.4.ANR"

hicp <- get_data(key, filter)
hicp <- hicp %>% 
  select(date = obstime, value = obsvalue, title, icp_item) %>% 
  mutate(
    date = convert_dates(date)
  )

ggplot(hicp, aes(x = date, y = value, color = title)) +
  geom_line() +
  #facet_wrap(~ref_area, ncol = 3) +
  theme_minimal(18) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL, y = "Percent per annum\n", color = NULL,
       title = "HICP - headline and core\n") 


#####################################################
# decompose HICP by components: type of product
#####################################################

# release of different components
# https://www.ecb.europa.eu/stats/ecb_statistics/escb/html/table.en.html?id=JDF_ICP_ECONOMIC_ACTIVITIES_ANR


codes <- c(
    "XEF000", # core inflation, excl. food & energy
    "GOODS0", # goods
    "FOOD00", # food incl. alcohol & tobacco
    "FOODUN", # Unprocessed food
    "FOODPR", # processed food, incl. food and tobacco
    "IGOODS", # industrial goods
    "IGXE00", # industrial goods exvl energy
    "NRGY00", # energy 
    "SERV00" # services
  )

key <- paste0(
  "ICP.M.U2.N.",
  str_c(codes, collapse="+"),
  ".4.ANR"
)
components <- get_data(key, filter)
components <- components %>% 
  select(
    date = obstime, value = obsvalue, title, icp_item
  ) %>% 
  mutate(
    date = convert_dates(date),
    year = year(date)
  )

# get the weights (yearly)
key <- paste0(
  "ICP.A.U2.N.",
  str_c(codes, collapse="+"),
  ".4.INW"
)

weights <- get_data(key, filter)
weights <- weights %>% 
  select(
    year = obstime, weight = obsvalue, 
    icp_item
  ) %>% 
  mutate(
    weight = weight / 1000,
    year = as.numeric(year)
  )

# join in item weights
components <- components %>% 
  left_join(
    weights, by = c("year", "icp_item")
  ) 

components_product <- components %>% 
  mutate(
    value = value * weight,
    title = str_remove(title, "HICP - ")
  ) %>% 
  filter(
    title != "Goods", 
    !str_detect(title, "All-items|^(Food)|Industrial goods$")
  ) %>% 
  mutate(
    title = factor(title, levels = c(
      "Unprocessed food",
      "Processed food incl. alcohol and tobacco",
      "Industrial goods excluding energy",
      "Energy",
      "Services"
      )
    )
  )

# stacked bars plot
ggplot(
  components_product,
  aes(x = date, y = value, fill = title)
  ) + 
  geom_bar(position = "stack", stat = "identity") + 
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow=3, byrow = T)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_fill_viridis_d() +
  labs(
    x = NULL, 
    y = "Per cent annual\n", 
    fill = NULL,
    title = "Services are an increasingly large proportion of overall inflation in the eurozone",
    subtitle = "ECB Harmonised Index of Consumer Prices by type of product\n"
    ) 
# for stacked percent, set position = "fill"
ggsave("figures/ecb_hicp_by_product.jpg", dpi = 300, height = 8, width = 12)

# get flourish ready
tmp <- components_product %>% 
  select(date, title, value) %>% 
  pivot_wider(
    names_from = title, values_from = value
    ) %>% 
  mutate(
    date = format(date, "%b %Y")
  )

write_csv(tmp, file = "ecb_hcip_product.csv")


ggplot(
  components %>% 
    filter(str_detect(title, "All-items")) %>% 
    bind_rows(hicp), 
  aes(x = date, y = value, color = title)
  ) +
  geom_line() +
  #facet_wrap(~ref_area, ncol = 3) +
  theme_minimal(18) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL, y = "Percent per annum\n", color = NULL,
       title = "HICP - headline and core\n") 


######################################################
# decompose HICP by components: purpose of consumption
######################################################

items <- c(
  "FOOD AND NON-ALCOHOLIC BEVERAGES",
  "ALCOHOLIC BEVERAGES, TOBACCO",
  "CLOTHING AND FOOTWEAR",
  "HOUSING, WATER, ELECTRICITY, GAS AND OTHER FUELS",
  "FURNISHINGS, HOUSEHOLD EQUIPMENT AND ROUTINE HOUSE MAINTENANCE",
  "HEALTH",
  "TRANSPORT",
  "COMMUNICATION",
  "RECREATION AND CULTURE",
  "EDUCATION",
  "RESTAURANTS AND HOTELS",
  "MISCELLANEOUS GOODS AND SERVICES"
  )

component_lookup <- tibble(
  item = items,
  code = c(
    "010000","020000","030000","040000","050000",
    "060000","070000","080000","090000","100000",
    "110000","120000"
  )
)

key <- paste0(
  "ICP.M.U2.N.",
  str_c(component_lookup$code, collapse="+"),
  ".4.ANR"
  )

components <- get_data(key, filter)
components <- components %>% 
  select(
    date = obstime, value = obsvalue, title, icp_item
  ) %>% 
  mutate(
    date = convert_dates(date),
    title = str_remove(title, "HICP - "),
    year = year(date)
  )
  
# for weights
# https://data.ecb.europa.eu/data/datasets/ICP/structure
# https://data.ecb.europa.eu/data/datasets/ICP/ICP.A.U2.N.GOODS0.4.INW
key <- paste0(
  "ICP.A.U2.N.",
  str_c(component_lookup$code, collapse="+"),
  ".4.INW"
)

weights <- get_data(key, filter)

weights <- weights %>% 
  select(
    year = obstime, weight = obsvalue, 
    icp_item
  ) %>% 
  mutate(
    weight = weight / 1000,
    year = as.numeric(year)
  )

# join in item weights
components <- components %>% 
  left_join(
    weights, by = c("year", "icp_item")
  ) 

components_consumption <- components %>% 
  mutate(
    value = value * weight,
    title = str_remove(title, "HICP - ")
  ) %>% 
  filter(
    title != "Goods", 
    !str_detect(title, "All-items|^(Food)|Industrial goods$")
  ) %>% 
  mutate(
    title = factor(title, component_lookup$item)
  )

# stacked bars 
ggplot(
  components_consumption,
  aes(x = date, y = value, fill = title)
  ) + 
  geom_bar(position = "stack", stat = "identity") + 
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 6, byrow = T)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_fill_viridis_d() +
  labs(
    x = NULL, 
    y = "Per cent annual\n", 
    fill = NULL,
    title = "Restaurants & hotels are an increasingly large proportion of overall inflation in the eurozone",
    subtitle = "ECB Harmonised Index of Consumer Prices by type of consumption\n"
  ) 

####################################################################
# Gross domestic product at market prices, Euro area 20 (fixed composition) as of 1 January 2023, Quarterly
####################################################################

key <- "MNA.Q.Y.I9.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.GY"
gdp <- get_data(key, filter)

gdp <- gdp %>% 
  select(date = obstime, value = obsvalue, title, matches("ref_area")) %>% 
  mutate(
    date = convert_dates(date)
  )

ggplot(gdp, aes(x = date, y = value)) +
  geom_line() +
  theme_minimal() +
  #facet_wrap( ~ ref_area) +
  theme(legend.position = "bottom") +
  labs(x = NULL, 
       y = "Per cent per quarter\n",
       title = "GDP - headline\n"
       ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) 

# unemployment rate
key <- "LFSI.M.DE+FR+ES+IT+NL+I9.S.UNEHRT.TOTAL0.15_74.T"
unemp <- get_data(key, filter)
unemp <- unemp %>% 
  select(date = obstime, value = obsvalue, matches("ref_area")) %>% 
  mutate(
    date = convert_dates(date)
  )
    

ggplot(unemp, aes(x = date, y = value)) +
  geom_line() +
  #facet_wrap(~ref_area, ncol = 3, scale = "free_y") +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ref_area, ncol = 3, scale = "free_y") +
  guides(fill = guide_legend(nrow = 6, byrow = T)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_fill_viridis_d() +
  labs(
    x = NULL, 
    y = "Per cen\n", 
    fill = NULL,
    title = "Unemployment tickig up in Germany",
    subtitle = "Unemployment rate\n"
  ) 

# US dollar/Euro, daily
key <- "EXR.D.USD.EUR.SP00.A"
xr <- get_data(key, list(lastNObservations = 30*3, detail = "full"))

xr <- xr %>% 
  select(date = obstime, value = obsvalue, title) %>% 
  mutate(
    date = convert_dates(date)
  )
ggplot(xr, aes(x = date, y = value)) +
  geom_line() +
  #facet_wrap(~ref_area, ncol = 3, scale = "free_y") +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 6, byrow = T)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_fill_viridis_d() +
  labs(
    x = NULL, 
    y = "Per cent annual\n", 
    fill = NULL,
    title = "The euro continues to weaken against the dollar since the start of 2024",
    subtitle = "EUR/USD exchange rate\n"
  ) 


#########################
###### 10 yr bond yields
########################

key <- "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y"
bond_10yr <- get_data(key, list(lastNObservations = 30*3, detail = "full"))

bond_10yr <- bond_10yr %>% 
  select(date = obstime, value = obsvalue, title) %>% 
  mutate(
    date = convert_dates(date)
  )
fig_bonds <- ggplot(bond_10yr, aes(x = date, y = value)) +
  geom_line() +
  #facet_wrap(~ref_area, ncol = 3, scale = "free_y") +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill="#FFF1E0", color = "#FFF1E0"),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 6, byrow = T)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_fill_viridis_d() +
  labs(
    x = NULL, 
    y = "Spot rate\n", 
    fill = NULL,
    title = "Eurozone yields have risen substantially since the start of the year",
    subtitle = "10-year maturity - Government bond, nominal, all issuers whose rating is triple A - Euro area (changing composition)\n"
  ) 




# ##### DIRECT using API endpoints
# library(httr)
# 
# # direct using url
# url <- "https://data-api.ecb.europa.eu/service/data/EXR/M.USD.EUR.SP00.A?format=csvdata"
# download.file(url, destfile = "test.csv")
# 
# read_csv("test.csv")
# 
# # Gross domestic product at market prices, Euro area 20 (fixed composition) as of 1 January 2023, Quarterly
# fold <- "MNA"
# key <- "Q.Y.I9.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.GY"
# 
# 
# url <- paste0(
#   "https://data-api.ecb.europa.eu/service/data/",
#   fold, "/",
#   key,
#   "?format=csvdata"
# )
# download.file(url, destfile = "test.csv")
# 
# read_csv("test.csv")


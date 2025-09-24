library(tidyverse)
library(googledrive)
library(estatapi)
# https://github.com/yutannihilation/estatapi/blob/master/README.en.md

api_key_japan <- Sys.getenv("BOJ_API")

# meta info is useful
meta_info <- estat_getMetaInfo(
  appId = api_key_japan, 
  lang = "E", 
  statsDataId = "0003427113" #"0003036792"
)

# pull all items for all Japan
# TODO can make this faster, only pull latest 1-2 years of CPI data
cpi_raw <- estat_getStatsData(
  appId = api_key_japan,
  lang = "E",
  statsDataId = "0003427113", # cpi code
  cdArea = c("00000"), # all Japan code
  cdTimeFrom = "2000",
  cdTab = "1",
  lvCat01 = "1-3" # how many levels deep to go? There are 6 levels
)

labels <- cpi_raw %>% 
  distinct(code = cat01_code, label = `Items(2020-base)`)

# clean and calculate
cpi <- cpi_raw %>%
  filter(!str_detect(Time, "^\\d")) %>% 
  mutate(date = my(Time)) %>% 
  select(
    date, 
    index = value, 
    code = cat01_code, 
    label = `Items(2020-base)`
  ) %>%
  distinct(date, index, label, .keep_all = T) %>% # "All items" is doubled-up with different codes
  group_by(label, date) %>%
  distinct(label, .keep_all = T) %>% # some other labels doubled with different index valuues, e.g. "Meals outside the home" (for levels 1-3), "Services related to education"
  ungroup %>%
  arrange(desc(date)) 


# re-order cols to appear first
headlines <- c(
  "All items, seasonally adjusted",
  "Goods, seasonally adjusted",
  "Services, seasonally adjusted", 
  "All items, less fresh food, seasonally adjusted",
  "All items, less fresh food and energy, seasonally adjusted"
)

cpi <- cpi %>% 
  group_by(code) %>% 
  mutate_at(
    vars(index), 
    list(
      "One month" = ~ ((. / lead(., 1))**12 - 1)*100,
      "Three months" = ~ ((. / lead(., 3))**4 - 1)*100,
      "Six months" = ~ ((. / lead(., 6))**2 - 1)*100,
      "One year" = ~ ((. - lead(., 12))/ lead(., 12))*100
    )
  ) %>%
  ungroup


# save for calendar 
write_csv(cpi, file = "data/japan_cpi.csv")

repo <- git2r::repository(".")
git2r::pull(repo)
git2r::add(repo, path = "data/japan_cpi.csv", force = FALSE)
git2r::commit(repo, message = "Japan CPI for calendar")
git2r::push(credentials = git2r::cred_token())


write_csv(
  cpi %>% 
    filter(
      label %in% headlines,
      date >= "2014-01-01"
    ) %>% 
    select(date, index, label) %>% 
    pivot_wider(
      names_from = "label",
      values_from = "index"
    ),
  file = "Japan CPI seasonally adjusted.csv"
)



# weights post-2020
weights <- read_csv("data/japan_cpi_weights.csv")

weights <- weights %>% 
  filter(
    weight != "-"
  ) %>% 
  mutate(
    code = case_when(
      nchar(group_code) == 1 ~ paste0("000",group_code),
      nchar(group_code) == 2 ~ paste0("00",group_code),
      nchar(group_code) == 3 ~ paste0("0",group_code),
      TRUE ~ as.character(item_code)
    ),
    weight = str_remove(weight, " "),
    weight = as.numeric(weight),
    weight = weight / 10000
  ) %>% 
  select(
    label = item, code, weight
  )



# save data for non-persistent figures
save(cpi, cpi_raw, labels, weights, file = "data/japan_cpi.Rdata")


# pull level6 items for all Japan
cpi_level6 <- estat_getStatsData(
  appId = api_key_japan,
  lang = "E",
  statsDataId = "0003427113", # cpi code
  cdArea = c("00000"), # all Japan code
  cdTimeFrom = "2000",
  cdTab = "1",
  lvCat01 = "6" # how many levels deep to go? There are 6 levels
)

labels6 <- cpi_level6 %>% 
  distinct(code = cat01_code, label = `Items(2020-base)`)

# clean and calculate
cpi6 <- cpi_level6 %>%
  filter(!str_detect(Time, "^\\d")) %>% 
  mutate(date = my(Time)) %>% 
  select(
    date, 
    index = value, 
    code = cat01_code, 
    label = `Items(2020-base)`
  ) %>%
  distinct(date, index, label, .keep_all = T) %>% # "All items" is doubled-up with different codes
  group_by(label, date) %>%
  distinct(label, .keep_all = T) %>% # some other labels doubled with different index valuues, e.g. "Meals outside the home" (for levels 1-3), "Services related to education"
  ungroup %>%
  arrange(desc(date)) %>%
  group_by(code) %>% 
  mutate_at(
    vars(index), 
    list(
      "One month" = ~ ((. / lead(., 1))**12 - 1)*100,
      "Three months" = ~ ((. / lead(., 3))**4 - 1)*100,
      "Six months" = ~ ((. / lead(., 6))**2 - 1)*100,
      "One year" = ~ ((. - lead(., 12))/ lead(., 12))*100
    )
  ) %>%
  ungroup %>% 
  relocate(label, code, .before = index)

cpi6 <- cpi6 %>% 
  select(-label) %>% 
  left_join(
    weights, 
    by = "code"
  )

save(cpi6, file = "data/japan_cpi_level6.Rdata")

# calculate FT core
source("predict_ft_core.R")
ft_core <- predict_core(measure = "Japan CPI")

cpi <- cpi %>%
  bind_rows(
    ft_core %>%
      rename(label = id) %>%
      filter(
        label == "FT core",
        date >= min(cpi$date)
      )
  )

# save FT core
save(
  ft_core,
  file = "data/core-japan_cpi.Rdata"
)


# create table of current values + weights
current_table <- cpi %>%
  filter(date == max(date)) %>% 
  left_join(
    weights %>% select(-label), 
    by = "code"
  ) %>% 
  mutate(weight = weight * 100)

headlines <- c(
  "All items", 
  "All items, less fresh food",
  "All items, less fresh food and energy",
  "Services",
  "FT core"
)

current_table <- current_table %>% 
  filter(
    label %in% headlines
  ) %>% 
  mutate(
    label = factor(label, levels = headlines)
  ) %>% 
  arrange(label)

current_table <- current_table %>% 
  select(item = label, `One month`:weight) %>% 
  mutate(
    Excluded = 100 - weight
  ) %>% 
  rename(
    Item = item,
    Included = weight
  ) %>% 
  mutate_at(
    vars(Included, Excluded),
    ~ round(., digits = 1)
  )


write_csv(
  current_table %>% 
    mutate_at(
      vars(Included, Excluded),
      ~ if_else(is.na(.), "", as.character(.))
    ), 
  file = "data/japan-cpi_current_table.csv"
)


# save to github url for persistence chart
repo <- git2r::repository(".")
git2r::pull(repo)
git2r::add(repo, path = "data/japan-cpi_current_table.csv", force = FALSE)
git2r::commit(repo, message = "updated Japan CPI current month change")
git2r::push(credentials = git2r::cred_token())



###########################################
######### make flourish ready (long format)
###########################################

cpi <- cpi %>% 
  pivot_longer(
    cols = 5:ncol(.),
    names_to = "Period"
  ) %>% 
  select(-c(index, code)) %>% 
  pivot_wider(
    names_from = label, 
    values_from = value
  ) %>% 
  mutate(
    Period = factor(
      Period, 
      levels = c("One year", "Six months", "Three months", "One month")
    )
  ) %>% 
  arrange(Period,desc(date)) %>% 
  select(
    date, Period, any_of(headlines), everything()
  ) 


write_csv(cpi %>% 
            mutate_if(is.numeric, ~ ifelse(is.na(.),"",.)) %>% # NAs throw off flourish 
            filter(date >= "2019-01-01"), 
          file = "data/flourish-japan_cpi.csv"
)


save(cpi, file = "data/flourish-japan_cpi.Rdata")

# save to github url for persistence chart
repo <- git2r::repository(".")
git2r::pull(repo)
git2r::add(repo, path = "data/flourish-japan_cpi.csv", force = FALSE)
git2r::commit(repo, message = "updated Japan CPI data")
git2r::push(credentials = git2r::cred_token())


# save specific FT core figure
ft_core_flourish <- cpi %>% 
  select(
    date, Period, `All items`, `FT core`,
    `All items, less fresh food and energy`
  )
write_csv(ft_core_flourish, file = "data/japan-cpi_ft_core.csv")

# save to github url for persistence chart
repo <- git2r::repository(".")
git2r::pull(repo)
git2r::add(repo, path = "data/japan-cpi_ft_core.csv", force = FALSE)
git2r::commit(repo, message = "updated Japan CPI headline + FT core")
git2r::push(credentials = git2r::cred_token())



ft_core %>% 
  pivot_longer(
    cols = 3:ncol(.),
    names_to = "Period"
  ) %>% 
  pivot_wider(
    names_from = id, 
    values_from = value
  ) %>% 
  mutate(
    Period = factor(
      Period, 
      levels = c("One year", "Six months", "Three months", "One month")
    )
  ) %>% 
  arrange(Period,desc(date)) %>% 
  filter(date >= "2019-01-01")


# update headline inflation image & FT core panel
# TODO save time here by running only subset of below script
source("figs-headline_inflation.R")
source("figs-ft_core.R")


# and update wage-sensitive services
#source("wage_sensitive_services-japan.R")

load("data/japan_cpi.Rdata")
load("data/japan_cpi_level6.Rdata")

wage_sensitive_items <- read_csv("data/japan_list_of_high_labour_cost_ratio_CPIitems.csv")
non_wage_sensitive_items <- read_csv("data/japan_list_of_low_labour_cost_ratio_CPIitems.csv")

wage_sensitive_codes <- wage_sensitive_items$`Item code`
non_wage_sensitive_codes <- non_wage_sensitive_items$`Item code`


cpi_wage <- cpi6 %>% 
  filter(
    code %in% c(wage_sensitive_codes, non_wage_sensitive_codes)  
  )

## without unchaining
cpi_sensitive <- cpi_wage %>%
  #filter(date >= "2020-01-01") %>% 
  mutate(
    group = ifelse(code %in% wage_sensitive_codes, "wage_sensitive", "not_wage_sensitive")
  ) %>%
  group_by(date, group) %>%
  summarise(
    index_agg = sum(index * weight, na.rm = T) / sum(weight, na.rm = T)
  ) %>%
  arrange(desc(date)) %>%
  ungroup %>%
  pivot_wider(names_from = "group", values_from = "index_agg")



### Japanese CPI is chain linked every 5 years
## need to unchain and re-chain a bit differently as result
# https://www.stat.go.jp/english/data/cpi/1585.html#H1


# Re-aggregating: https://ec.europa.eu/eurostat/web/hicp/methodology#COVID-19%20and%20HICP
# unchain
# cpi_wage <- cpi_wage %>% 
#   select(
#     date, code, label, index, weight
#   ) %>% 
#   mutate(year = year(date))
# 
# 
# lookup <- cpi_wage %>% 
#   filter(
#     date %in% c("2004-12-01","2009-12-01","2014-12-01","2019-12-01")
#     ) %>% 
#   select(year, code, prev_dec = index) %>% 
#   mutate(year = year + 1)
# 
# cpi_wage <- cpi_wage %>% 
#   left_join(
#     lookup, by = c("year","code")
#   )
# 
# cpi_sensitive <- cpi_wage %>%
#   fill(prev_dec, .direction = "up") %>% 
#   mutate(index_unchained = (index / prev_dec)*100) %>% 
#   ungroup 
# 
# # aggregate
# cpi_sensitive <- cpi_sensitive %>% 
#   mutate(
#     group = ifelse(code %in% wage_sensitive_codes, "wage_sensitive", "not_wage_sensitive")
#   )
# 
# cpi_sensitive <- cpi_sensitive %>% 
#   group_by(date, group) %>% 
#   summarise(
#     index_agg = sum(index_unchained * weight, na.rm = T) / sum(weight, na.rm = T)  
#   ) %>% 
#   arrange(desc(date)) %>% 
#   ungroup
# 
# 
# cpi_sensitive <- cpi_sensitive %>% 
#   pivot_wider(names_from = "group", values_from = "index_agg")
# 
# # re-chain
# cpi_sensitive <- cpi_sensitive %>% 
#   arrange(date) %>% 
#   mutate(year = year(date))# %>% 
#   #filter(date >= "2001-12-01")
# 
# for (i in unique(cpi_sensitive$year)) {
#   
#   if (i <= 2001) {next}
#   
#   fact_not <- cpi_sensitive %>% 
#     filter(year == i-1) %>% 
#     filter(date == max(date)) %>% 
#     .$not_wage_sensitive
#   #.$index_rechain
#   
#   fact <- cpi_sensitive %>% 
#     filter(year == i-1) %>% 
#     filter(date == max(date)) %>% 
#     .$wage_sensitive
#   #.$index_rechain
#   
#   index_rechained <- cpi_sensitive %>% 
#     filter(year == i) %>% 
#     mutate(
#       wage_sensitive = wage_sensitive * (fact/100), 
#       not_wage_sensitive = not_wage_sensitive * (fact_not/100) 
#     )
#   #.$index_agg * (fact/100)
#   
#   cpi_sensitive[cpi_sensitive$year == i,] <- index_rechained
#   #hicp_sensitive[hicp_sensitive$year == i, "index_rechain"] <- index_rechained
#   
#   
# }


cpi_sensitive <- cpi_sensitive %>%
  pivot_longer(
    cols = c("not_wage_sensitive","wage_sensitive"),
    values_to = "index", 
    names_to = "label"
  ) %>% 
  arrange(desc(date)) %>% 
  group_by(label) %>% 
  mutate_at(
    vars(index), 
    list(
      "One month" = ~ ((. / lead(., 1))**12 - 1)*100,
      "Three months" = ~ ((. / lead(., 3))**4 - 1)*100,
      "Six months" = ~ ((. / lead(., 6))**2 - 1)*100,
      "One year" = ~ ((. - lead(., 12))/ lead(., 12))*100
    )
  ) %>%
  ungroup


write_csv(
  cpi_sensitive %>%
    # add services total
    bind_rows(
      cpi %>% 
        filter(code == c("0228")) # code for general services
    ) %>%
    select(date, label, `One year`) %>% 
    pivot_wider(
      names_from = label, 
      values_from = `One year`
    ) %>% 
    select(
      date,
      `General services`,
      `Services (wage-sensitive)` = wage_sensitive,
      `Services (excl. wage-sensitive)` = not_wage_sensitive
    ) %>% 
    filter(
      !is.na(`Services (wage-sensitive)`), 
      date >= "2018-01-01"
    ), 
  file = "data/wage_sensitive_services-japan.csv"
)


# save to github url for persistence chart
repo <- git2r::repository(".")
git2r::pull(repo)
git2r::add(repo, path = "data/wage_sensitive_services-japan.csv", force = FALSE)
git2r::commit(repo, message = "updated Japan wage-sensitive services")
git2r::push(credentials = git2r::cred_token())

### TODO the unchain / re-chain does not seem to work
## is Japan chained?
ggplot(
  cpi_sensitive %>% filter(date >= "2020-01-01"),
  aes(x=date, y=`One year`, color = label)
) +
  geom_line()



# update panel
source("wage_sensitive_panel.R")










#### decomposition ###
# join in item weights

# # this is hierarchy definition 
# meta_info$cat01 
# 
# 
# cpi_decomposition <- cpi_decomposition %>% 
#   mutate(year = year(date)) %>% 
#   left_join(
#     weights, by = c("year", "code")
#   ) %>% 
#   filter(code %in% codes)
# 
# 
# hicp_decomposition <- hicp_decomposition %>% 
#   mutate_at(
#     vars(index:`One year`),
#     ~ . * weight
#   ) 
# 
# components <- c(
#   "Food incl. alcohol and tobacco",
#   "Industrial goods excluding energy", 
#   "Energy",
#   "Services"
# )
# 
# 
# hicp_decomposition <- hicp_decomposition %>% 
#   filter(
#     label %in% c("Overall index",components)
#   ) %>% 
#   mutate(
#     label = factor(
#       label,
#       levels = c("Overall index", components)
#     ),
#     label = case_when(
#       label == "Food incl. alcohol and tobacco" ~ "Food",
#       str_detect(label, "Industrial goods") ~ "Goods",
#       TRUE ~ label
#     ),
#     
#   )
# 
# # get flourish ready
# tmp <- hicp_decomposition %>% 
#   select(date, label, `One year`) %>% 
#   pivot_wider(
#     names_from = label, values_from = `One year`
#   ) %>% 
#   mutate(
#     date = format(date, "%b %Y")
#   ) %>% 
#   filter(!is.na(`Overall index`)) 
# 
# 
# write_csv(tmp, file = "data/ecb_hcip_decomposed.csv")
# 
# 
# # save to github url for persistence chart
# repo <- git2r::repository(".")
# git2r::pull(repo)
# git2r::add(repo, path = "data/ecb_hcip_decomposed.csv", force = FALSE)
# git2r::commit(repo, message = "updated Eurozone HICP decomposition")
# git2r::push(credentials = git2r::cred_token())
# 

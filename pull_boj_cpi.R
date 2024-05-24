library(tidyverse)
library(estatapi)
# https://github.com/yutannihilation/estatapi/blob/master/README.en.md

api_key_japan <- Sys.getenv("BOJ_API")

# pull all items for all Japan
cpi_raw <- estat_getStatsData(
  appId = api_key_japan,
  lang = "E",
  statsDataId = "0003427113", # cpi code
  cdArea = c("00000"), # all Japan code
  cdTimeFrom = "2000",
  cdTab = "1"
)

labels <- cpi_raw %>% 
  distinct(code = cat01_code, label = `Items(2020-base)`)

# clean and calculate
cpi <- cpi_raw %>%
  filter(!str_detect(Time, "^\\d")) %>% 
  mutate(date = my(Time)) %>% 
  select(
    date, index = value, label = `Items(2020-base)`
  ) %>%
  distinct() %>% # "All items" is doubled-up with different codes
  group_by(label, date) %>% 
  distinct(label, .keep_all = T) %>% # some other labels doubled with different index valuues, e.g. "Meals outside the home", "Services related to education"
  ungroup %>%
  arrange(desc(date)) %>% 
  group_by(label) %>% 
  mutate_at(
    vars(index), 
    list(
      "One month" = ~ ((. - lead(., 1))/ lead(., 1))*100,
      "Three months" = ~ ((. - lead(., 3))/ lead(., 3))*100,
      "Six months" = ~ ((. - lead(., 6))/ lead(., 6))*100,
      "One year" = ~ ((. - lead(., 12))/ lead(., 12))*100
    )
  ) %>%
  ungroup

# make flourish ready (long format)
cols_pivot <- c("One month","Three months","Six months","One year")
cpi_list <- vector("list", length(cols_pivot))
for (i in seq_along(cpi_list)) {
  
  cpi_list[[i]] <- cpi %>% 
    select(date, label, matches(cols_pivot[[i]]) ) %>%
    distinct() %>% 
    pivot_wider(
      names_from = "label",
      values_from = cols_pivot[[i]]
    ) %>%
    mutate(
      Period = cols_pivot[[i]]
    )
}

# re-order cols to appear first
headlines <- c(
  "All items",
  "Goods",
  "Services", 
  "All items, less fresh food",
  "All items, less fresh food and energy"
)

# bind and save
cpi <- cpi_list %>% 
  reduce(bind_rows) %>%
  select(date, Period, any_of(headlines), everything()) %>%
  mutate(
    Period = factor(
      Period, 
      levels = c("One year", "Six months", 
                 "Three months", "One month")
    )
  ) %>%
  arrange(Period)  

write_csv(cpi %>% 
            mutate_if(is.numeric, ~ ifelse(is.na(.),"",.)) %>% # NAs throw off flourish 
            filter(date >= "2019-01-01"), 
          file = "data/flourish-boj_cpi.csv"
          )


save(cpi, file = "flourish-boj_cpi.Rdata")

# save to github url for persistence chart
repo <- git2r::repository(".")
git2r::pull(repo)
git2r::add(repo = ".", path = "data/flourish-boj_cpi.csv", force = FALSE)
git2r::commit(repo = ".", message = "updated Japan CPI data")
git2r::push(credentials = git2r::cred_token())


df.news <- read_csv('data/df_news.csv')
#df.news <- df.news %>% select(-c(X1,caseno))
df.emails <- read_csv('data/email headers.csv')
df.emails <- df.emails %>% 
  mutate(To = str_remove_all(To,"@gastech.com.kronos|@gastech.com.tethys")) %>%
  mutate(From = str_remove_all(From,"@gastech.com.kronos|@gastech.com.tethys")) %>%
  mutate(To = str_replace_all(To,"[.]"," ")) %>%
  mutate(From = str_replace_all(From,"[.]"," ")) %>% 
  mutate(Date = parse_date_time(x = Date, orders =c("%m%d%y %H%M","%m%d%y")))

df.emails <- df.emails %>% 
  mutate(Date.Date =  date(Date)) %>% 
  mutate(Date.Time =  hms::as_hms(Date))

df.emails <- df.emails %>% 
  mutate(nTo = lengths(str_split(To,pattern=","))) %>% 
  filter(nTo<=12)

df.emp <- readxl::read_xlsx('data/EmployeeRecords.xlsx')
#df.emp <- read_csv('data/EmployeeRecords.csv')


df.emp <- df.emp %>% 
  unite(FullName, 
        FirstName, 
        LastName, 
        sep=" ", 
        remove=FALSE)

x_full <- df.emails %>% 
  mutate(To = str_split(To,pattern=',')) %>% 
  unnest_longer(To) %>% 
  mutate(To = str_trim(To),
         From = str_trim(From)) %>%
  filter(!(From==To)) %>%
  mutate(Subject2 = str_replace_all(Subject, "[[:punct:]]", " ")) %>%
  left_join(select(df.emp, FullName, CurrentEmploymentType, CurrentEmploymentTitle), by=c("From"="FullName")) %>%
  replace_na(list(CurrentEmploymentType = "Executive", CurrentEmploymentTitle="CEO")) %>%
  rename(From_title = CurrentEmploymentTitle, From_dep =  CurrentEmploymentType) %>% 
  left_join(select(df.emp, FullName, CurrentEmploymentTitle, CurrentEmploymentType), by = c("To"="FullName")) %>%
  rename(To_title = CurrentEmploymentTitle, To_dep =  CurrentEmploymentType)

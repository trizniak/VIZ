
# ---- *** DATA *** ----

# ---- Deciles ----

data.x = get_eurostat_data("ilc_di01",
                           time_format="raw",
                           keepFlags=TRUE) %>%
  filter(indic_il=="TC",
         currency=="EUR",
         quantile %in% c("D1","D3","D5","D7","D9"),
         geo %in% names(country.list)) %>%
  mutate(YEAR=as.numeric(as.character(time))-1) %>%
  select(geo,YEAR,quantile,values) %>%
  rename(COUNTRY=geo,
         X=quantile,
         val=values) %>%
  mutate(X=factor(recode(X,"D5"="MEDIAN"),
                  levels=c("D1","D3","MEDIAN","D7","D9")),
         COUNTRY=factor(COUNTRY,
                        levels=names(country.list)))

data.x = data.x %>%
  left_join(data.x %>%
              filter(!is.na(val)) %>%
              group_by(COUNTRY) %>%
              summarize(ref.year=max(min.year,min(YEAR,na.rm=TRUE))))

data.x = data.x %>%
  left_join(data.x %>%
              filter(YEAR==ref.year) %>%
              select(COUNTRY,
                     X,
                     val) %>%
              rename(ref.val=val)) %>%
  left_join(data.x %>%
              filter(X=="MEDIAN") %>%
              select(COUNTRY,
                     YEAR,
                     val) %>%
              rename(MEDIAN=val)) %>%
  left_join(data.x %>%
              filter(X=="MEDIAN",
                     YEAR==ref.year) %>%
              select(COUNTRY,
                     val) %>%
              rename(ref.MEDIAN=val)) %>%
  mutate(val.m=val-MEDIAN,
         val.M=(val-MEDIAN)/ref.MEDIAN+1,
         val.c=val-ref.val,
         val.pc=100*(val/ref.val-1))

save(data.x,
     file="./DATA/data_x.Rdata")


# ---- Saving Rate by Income Quintile (SRIQ) ----

data.sriq = get_eurostat("icw_sr_03",
                         time_format="num",
                         keepFlags=FALSE) %>%
  rename(COUNTRY=geo,
         YEAR=time,
         val=values,
         INDICATOR=unit) %>%
  filter(COUNTRY %in% names(country.list),
         YEAR==2015,
         !is.na(val)) %>%
  select(c(COUNTRY,quantile,val))

save(data.sriq,
     file="./DATA/data_sriq.Rdata")

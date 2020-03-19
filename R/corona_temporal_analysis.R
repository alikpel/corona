require(pacman)
p_load(tidyverse)
p_load(magrittr)
p_load(fpp3)
p_load(plotly)
library(localizebase)



#"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

data_raw <- 
  localizebase::cache_eval(
    keys = Sys.Date(),
    expr = 
      local({
        list(
          confirmed = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
          ,deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
        ) %>% 
          map(read_csv)
      })
  )
    

MAX_DATE <- NULL

data_clean <- 
  local({
    data_tidy <- 
      data_raw$confirmed %>%
      select(-c("Lat", "Long")) %>%
      pivot_longer(-c("Province/State", "Country/Region"), "date") %>%
      set_colnames(c("state","country", "date", "confirmed")) %>%
      mutate(date=as.Date(date, "%m/%d/%y"))
    
    MAX_DATE <<- max(data_tidy$date)
    
    data_agg_and_enriched <- 
      data_tidy %>% 
      mutate(state = ifelse((country == "China" & state != "Hubei"),"Non-Hubei", state)) %>% 
      mutate(country_state = paste0(country, ifelse(is.na(state),"",paste0(": ",state)))) %>% # france is both country and state
      group_by(country,country_state,date) %>% 
      summarise(confirmed = sum(confirmed)) %>% 
      group_by(country) %>% 
      mutate(n_country_confirmed = sum(confirmed[date == MAX_DATE])) %>% 
      ungroup() %>%
      group_by(country_state) %>% 
      mutate(n_country_state_confirmed = sum(confirmed[date == MAX_DATE])) %>% 
      ungroup() %>% 
      group_by(country_state) %>%
      arrange(date) %>%
      mutate(
        confirmed_lag1 = lag(confirmed),
        r = confirmed/confirmed_lag1,
        r = ifelse(is.infinite(r), 0, r),
        r = ifelse(is.nan(r), NA_real_, r),
        r_lag1 = lag(r, 1)
        ) %>% 
      mutate(
        r_slope = r-r_lag1,
        r_slope2 = r_slope-lag(r_slope),
             ) %>% 
      ungroup() %>%
      group_by(country_state) %>% 
      mutate(date_peak = max(date[which.max(r)])) %>% 
      mutate(date_case100 = min(date[confirmed>=100])) %>% 
      mutate(date_r1.5_post_peak = min(date[date>=date_peak & r<=1.5])) %>% 
      ungroup() %>% 
      arrange(country_state, date) 
    
    # determine min state,country size
    # data_agg_and_enriched %>% 
    #   distinct(country_state, .keep_all = T) %>% 
    #   arrange(desc(n_country_state_confirmed)) %>% 
    #   view
    
    MIN_SAMPLE_SIZE = 14
    
    # get popular/interesting places + IL
    data_clean <- 
      data_agg_and_enriched %>% 
      filter_verbose(n_country_state_confirmed >= 1000 | country %in% "Israel") %>%  # country %in% c("Italy", "Israel", "Spain", "Korea, South", "United Kingdom") | 
      # mutate(r = ifelse(date<date_peak, NA_integer_, r)) %>%  # mask R's prior to peak
      filter_verbose(
        date >= date_peak &
        date >= date_case100 &
        date >= date_r1.5_post_peak) %>%
      filter_verbose(date>=date_peak) %>%
      group_by(country_state) %>% 
      mutate(sample_size = n()) %>%
      filter(sample_size>=MIN_SAMPLE_SIZE) %>% 
      arrange(country_state, date) %>% 
      mutate(n_day = row_number()) %>% 
      ungroup() %>% 
      select(country, country_state, n_day,n_country_state_confirmed, n_country_confirmed, date, everything())

         })



  
# data_clean %>% distinct(state)
# data_clean %>% distinct(country)

##### MODELING #####

DATE_INDEX_COL = "n_day" # "date
tsbl <- 
  as_tsibble(data_clean, 
           key = country_state, 
           index = DATE_INDEX_COL)


# Confirmed over time
(tsbl %>% 
    ggplot(aes(!!rlang::sym(DATE_INDEX_COL), y = confirmed, color = country_state)) + #  need to specify only the target var
    geom_point(alpha = 0.3)+
    geom_smooth(se=F)+
    ylab("confirmed cases") + xlab("date") +
    ggtitle("# confirmed by day")) %>% 
  ggplotly()

# log(confirmed) over time
(tsbl %>% 
    ggplot(aes(!!rlang::sym(DATE_INDEX_COL), y=log(confirmed), color = country_state)) + #  need to specify only the target var
    geom_point(alpha = 0.3)+
    geom_smooth(se=F)+
    ylab("confirmed cases") + xlab("date") +
    ggtitle("# confirmed by day")) %>% 
  ggplotly()

# R vs time ylim1,2   r - total confirmed cases till in day i/ day i-1
(
  tsbl %>% 
    ggplot(aes(!!rlang::sym(DATE_INDEX_COL), r, color = country_state)) + #  need to specify only the target var
    geom_point(alpha = 0.3) +
    geom_smooth(se=F) +
    # geom_smooth(se=F, method ="gam", formula = y ~ s(x, bs = "cs",k=4)) +
    facet_wrap(vars(country_state), scales = "free_x") +
    ylab("R") +
    ylim(1,1.75)+
    xlab("date") +
    ggtitle("R - total till day i / total till day i-1 ")
)
%>% 
  ggplotly()






(
  tsbl %>% 
    # filter(country %in% c("Italy","Israel")) %>% 
    ggplot(aes(!!rlang::sym(DATE_INDEX_COL), r, color = country_state)) + #  need to specify only the target var
    geom_point(alpha = 0.3) +
    geom_smooth(se=F)+
    ylab("R") + 
    xlab("day") +
    ggtitle("R by day")
)
%>% 
  ggplotly()




# R slope vs time
(
  tsbl %>% 
    ggplot(aes(date, y=r_slope)) + #  need to specify only the target var
    geom_point(alpha = 0.3) +
    geom_smooth(se=F, formula =  y ~ x) +
    facet_wrap(vars(country_state), scales = "free_y") +
    xlim(-1,1)+
    ylab("R slope") 
    # xlab("date") +
    # ggtitle(paste0(country_state,"R by day:"))
)
%>% 
  ggplotly()

# log R
(tsbl %>% 
    ggplot(aes(date, log(r), color = country_state)) + #  need to specify only the target var
    geom_point(alpha = 0.3) +
    geom_smooth(se = F)+
    ylab("log(R)") + 
    xlab("date") +
    ggtitle("log(R) by day"))%>% 
  ggplotly()




#### MODELING 

# fit simple models
TARGET_VAR = "r"
FORECASTING_DAYS = 7

# trainset - prior to 2007
ts_train <- 
  tsbl %>% 
  filter(date <= MAX_DATE - FORECASTING_DAYS)

fitted <- 
  ts_train %>% 
  #model(ARIMA(!!rlang::sym(TARGET_VAR) ~ trend() + pdq(d = 0)))
  model(ETS(!!rlang::sym(TARGET_VAR) ~  error("A") + trend("A") + season("N"), opt_crit="mae"))   # "Ad", phi = 0.9; trend("M", alpha_range=c(0.1,0.2), beta = .1)


  ts_train %>%  
  ggplot(aes(x=n_day,y=r)) +
  geom_point(alpha=0.2)+
  geom_line(aes(y=.fitted), data = augment(fitted))+
  geom_line(aes(y=r), data = forecast(fitted, h = 7) %>% mutate(r=pmax(r,1)), linetype = "dashed")+
  facet_wrap(vars(country_state), scales = "free_x") 

(
  ts_train %>% 
  # autoplot(r) +
  geom_line(data = augment(fited), linetype = "dashed")
) %>% ggplotly()


# for each <geo,time> calc 7 days fwd r coefs (based on fitted Rs)  
ts_forecast <- 
  ts_train %>% 
  full_join(augment(fitted) %>% select(country_state ,n_day, .fitted),
            by = c("country_state", "n_day")) %>% 
  mutate(r_lead7_prod = slide_dbl(lead(.fitted,1), prod, .size=7, .align="left"))
  




### FOCAST CASES FOR A GIVEN COUNTRY

#INPUT - 
R_FORECAST <- 1.1743536


r7coef_by_country_state <- 
  ts_forecast %>% 
  group_by(country_state) %>% 
  mutate(
    # r_closet_vs_fitted_abs_diff = abs(.fitted-R_FORECAST),
    r_closet_vs_fitted_prc_diff = 100*abs(.fitted-R_FORECAST)/R_FORECAST,
    is_r_closest = .fitted == min(.fitted[which.min(r_closet_vs_fitted_prc_diff)])
    ) %>% 
  ungroup() %>% 
  filter(is_r_closest, 
         r_closet_vs_fitted_prc_diff <= 5) %>% 
  arrange(r_lead7_prod) %>% 
  as_tibble() %>% 
  select(country_state, 
         r7coef = r_lead7_prod)
  
# QA
z <- 
  ts_forecast %>% 
  filter(country_state == "China: Hubei") %>% 
  left_join(r7coef_by_country_state,
            by = "country_state") %>% 
  mutate(confirmed_fc7 = confirmed*r7coef) %>% 
  select(country,country_state, date, n_day, confirmed, r, .fitted, r_lead7_prod, r7coef, confirmed_fc7)
  

  
  

forecast_by_country_state <- 
  unique(tsbl$country_state) %>% 
  purrr::set_names() %>% 
  map(function(country_state){
    data_ts <- tsbl %>% filter(country_state == !!country_state)
    # data_model <- fit %>% filter(country_state == !!country_state)
    
    data_ts %>% 
      autoplot(r) +
      geom_line(data = augment(fit),linetype = "dashed") +
      ggtitle(country_state)
  }
  )



# fitted_models <- 
#   list(
#     naive_models =
#       model(ts_train,
#             Naive = NAIVE(!!rlang::sym(TARGET_VAR)),
#             Drift = RW(!!rlang::sym(TARGET_VAR) ~ drift())
#       ),
#     advanced_models =
#       model(ts_train,
#             Seasonal_naive = SNAIVE(!!rlang::sym(TARGET_VAR)),
#             ETS = ETS(!!rlang::sym(TARGET_VAR)),
#             ARIMA = ARIMA(!!rlang::sym(TARGET_VAR)),
#       ))


forecasting <- 
  fitted_models %>% 
  map(
    
    
  )
  



# features: y, (y-1)/(lag(y)-1), 1+mean(x-1)
# 




library(tsibbledata)
z <- olympic_running %>%
  model(TSLM(Time ~ trend())) %>%
  forecast(h = FORECASTING_DAYS) 
  
  # interpolate(olympic_running) %>% 
  autoplot()





fitted_models$advanced_models %>% 
  forecast(h = FORECASTING_DAYS) %>% 
  autoplot(!!rlang::sym(TARGET_VAR))



# model with benchmark vs advanced models
fitted_models %>% 
  map(function(models){
    models %>% 
      forecast(.,h = FORECASTING_DAYS) %>% 
      autoplot(ts) 
      # xlab("Year") + ylab("Megalitres") +
      # ggtitle("Forecasts for quarterly beer production") +
      # guides(colour=guide_legend(title="Forecast"))
  })




# tsbl %>% 
#   # filter(country_state =="Austria") %>% 
#   autoplot(confirmed) +
#   geom_line(data = fit)



  # filter(country_state =="Austria") %>% 
  # tsbl %>% 
  # ggplot(aes(x= date, y= confirmed, color = country_state))+
  # geom_line() +
  # geom_line(data = fit)+
  #   facet_wrap(confirmed~.)



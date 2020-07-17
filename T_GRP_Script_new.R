#Importando as librarys
library(nycflights13)
library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)
library(sqldf)
library(ggplot2)
library(corrplot)
library(summarytools)

#Importando os dataframes
dfFlights <- flights
dfAirlines <- airlines
dfAirportsOrigin <- airports
dfAirportsDestination <- airports
dfPlanes <- planes
dfWeather <- weather

#Criando dataframe com os tipos de motor
dfEnginesTypes <- sqldf("select distinct engine from dfPlanes")
dfEnginesTypes$engines_id <- seq(1, 6)

#view(dfSummary(dfPlanes))

#filtrando o dataframe dos aeroportos origem, apenas com os que interessam
dfAirportsOrigin <- subset(dfAirportsOrigin, faa == 'EWR' | faa =='JFK' | faa =='LGA')

#Adicionando ID na cias aéreas
dfAirlines$airlines_id <- seq(1, 16)
#adicionando ID nos aeroportos origem
dfAirportsOrigin$airport_origin_id <- seq(1, 3)

#Definindo se houve atraso na decolagem do voo, de acordo com as normas da FAA
dfFlights <- dfFlights %>% mutate(delay_on_departure = if_else(dep_delay > 15,'Yes','No',missing = 'NA'))
dfFlights <- dfFlights %>% mutate(flag_delay_on_departure = if_else(dep_delay > 15,1,0))
#Definindo se houve atraso na aterissagem do voo, de acordo com as normas da FAA
dfFlights <- dfFlights %>% mutate(delay_on_arrive = if_else(arr_delay > 15,'Yes','No',missing = 'NA'))
dfFlights <- dfFlights %>% mutate(flag_delay_on_arrive = if_else(arr_delay > 15,1,0))
#Definindo se houve atraso, independente de qual momento
dfFlights <- dfFlights %>% mutate(delayed = if_else((delay_on_departure %in% 'Yes') | (delay_on_arrive %in% 'Yes'),'Yes','No',missing = 'NA'))
dfFlights <- dfFlights %>% mutate(flag_delayed = if_else((delay_on_departure %in% 'Yes') | (delay_on_arrive %in% 'Yes'),1,0))

#Unificando os campos dia, mês e ano numa data
dfFlights$flights_date <- as.Date(with(dfFlights, paste(year, month, day,sep="-")), "%Y-%m-%d")
dfFlights$flights_day_of_week <- weekdays(as.POSIXct(dfFlights$flights_date), abbreviate = T)
dfFlights$flights_nu_day_of_week <- wday(dfFlights$flights_date, label=F)

#Realizando o join entre todos os dataframes
dfComplete <- sqldf("select dfFlights.year as flights_year,
                            dfFlights.month as flights_month,
                            dfFlights.day as flights_day,
                            dfFlights.flights_date,
                            dfFlights.flights_day_of_week,
                            dfFlights.flights_nu_day_of_week,
                            dfFlights.dep_time as flights_dep_time,
                            dfFlights.sched_dep_time as flights_sched_dep_time,
                            dfFlights.dep_delay as flights_dep_delay,
                            dfFlights.arr_time as flights_arr_time,
                            dfFlights.sched_arr_time as flights_sched_arr_time,
                            dfFlights.arr_delay as flights_arr_delay,
                            dfFlights.carrier as flights_carrier,
                            dfFlights.flight as flights_flight,
                            dfFlights.tailnum as flights_tailnum,
                            dfFlights.origin as flights_origin,
                            dfFlights.dest as flights_dest,
                            dfFlights.air_time as flights_air_time,
                            dfFlights.distance as flights_distance,
                            dfFlights.hour as flights_hour,
                            dfFlights.minute as flights_minute,
                            dfFlights.time_hour as flights_time_hour,
                            dfFlights.delay_on_departure as flights_delay_on_departure,
                            dfFlights.delay_on_arrive as flights_delay_on_arrive,
                            dfFlights.delayed as flights_delayed,
                            dfFlights.flag_delay_on_departure as flights_flag_delay_on_departure,
                            dfFlights.flag_delay_on_arrive as flights_flag_delay_on_arrive,
                            dfFlights.flag_delayed as flights_flag_delayed,
                            dfPlanes.tailnum as planes_tailnum,
                            dfPlanes.year as planes_year,
                            dfPlanes.type as planes_type,
                            dfPlanes.manufacturer as planes_manufacturer,
                            dfPlanes.model as planes_moldel,
                            dfPlanes.engines as planes_engines,
                            dfPlanes.seats as planes_seats,
                            dfPlanes.speed as planes_speed,
                            dfPlanes.engine as planes_engine,
                            dfEnginesTypes.engines_id as planes_engine_id,
                            dfAirportsOrigin.faa as airport_origin_faa,
                            dfAirportsOrigin.name as airport_origin_name,
                            dfAirportsOrigin.airport_origin_id,
                            dfAirportsDestination.faa as airport_destination_faa,
                            dfAirportsDestination.name as airport_destination_name,
                            dfAirlines.carrier as airlines_carrier,
                            dfAirlines.name as airlines_name,
                            dfAirlines.airlines_id,
                            dfWeather.origin as weather_origin,
                            dfWeather.year as weather_year,
                            dfWeather.month as weather_month,
                            dfWeather.day as weather_day,
                            dfWeather.hour as weather_hour,
                            dfWeather.temp as weather_temp,
                            dfWeather.dewp as weather_dewp,
                            dfWeather.humid as weather_humid,
                            case dfWeather.humid
                            when 100 then 'Yes'
                            else 'No' end as weather_raining,
                            case dfWeather.humid
                            when 100 then 1
                            else 0 end as weather_flag_raining,
                            dfWeather.humid,
                            dfWeather.wind_dir as weather_wind_dir,
                            dfWeather.wind_speed as weather_wind_speed,
                            dfWeather.wind_gust as weather_wind_gust,
                            dfWeather.precip as weather_precip,
                            dfWeather.pressure as weather_pressure,
                            dfWeather.visib as weather_visib,
                            dfWeather.time_hour as weather_time_hour
                from dfFlights 
                  left join dfPlanes on dfFlights.tailnum = dfPlanes.tailnum
                  left join dfAirportsOrigin on dfFlights.origin = dfAirportsOrigin.faa
                  left join dfAirportsDestination on dfFlights.dest = dfAirportsDestination.faa
                  left join dfAirlines on dfFlights.carrier = dfAirlines.carrier
                  left join dfEnginesTypes on dfPlanes.engine = dfEnginesTypes.engine
                  left join dfWeather on dfFlights.year = dfWeather.year 
                                         and dfFlights.month = dfWeather.month 
                                         and dfFlights.day = dfWeather.day
                                         and dfFlights.hour = dfWeather.hour 
                                         and dfFlights.origin = dfWeather.origin")

#Descrição das variáveis do data frame
str(dfComplete)

# -xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
myAirports = c('EWR', 'JFK', 'LGA')

# ALL DEPARTURE CALCULATED DELAY FLIGHTS 
flights %>% 
  filter(!is.na(dep_time), 
         !is.na(arr_time),
         dep_time - sched_dep_time > 0 ,
         # (dep_time - sched_dep_time) != dep_delay,
         origin %in% myAirports 
  ) %>%
  select(dep_time, sched_dep_time, dep_delay)

View(head(flights, n = 50))

flights %>% 
  select(year, month, day, hour, minute)

flights %>% 
  select(time_hour)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

# -xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx






## ------------------------------------------------------------------


HHMM
1539

# Mutate date
getHour = function(hhmm) { return (hhmm %/% 100) }
getMinute = function(hhmm){ return (hhmm %% 100) }
make_dt <- function(year, month, day, hhmm) {
  make_datetime(year, month, day, getHour(hhmm), getMinute(hhmm))
}

# Filter no missing flights dos nossos aeroportos
my_flights = flights %>% 
  filter(!is.na(dep_time), 
         !is.na(arr_time),
         origin %in% c('EWR', 'JFK', 'LGA')
  ) %>% 
  # add lubrydate
  mutate(
    dep_time = make_dt(year, month, day, dep_time),
    arr_time = make_dt(year, month, day, arr_time),
    sched_dep_time = make_dt(year, month, day, sched_dep_time),
    sched_arr_time = make_dt(year, month, day, sched_arr_time),
  ) %>% 
  # somente informacoes que me interessa para data
  select(origin, dest, ends_with("delay"), ends_with("time"))

# adjust arrived date
my_flights <- my_flights %>% 
  mutate(
    virada = arr_time < dep_time,
    arr_time = arr_time + days(virada * 1),
    sched_arr_time = sched_arr_time + days(virada * 1)
  )

my_delayed_dep = my_flights %>% filter(dep_delay > 15)
my_delayed_arr = my_flights %>% filter(arr_delay > 15)

# departure times across the year
my_flights %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) + # 86400 seconds = 1 day
  facet_wrap(~month(dep_time))

# departure On Time arrive Delay
my_flights %>% 
  filter(dep_delay < 15, arr_delay > 15) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400 * 7) # 86400 seconds = 1 day

# 
my_flights %>% 
  filter(#dep_time < ymd(20130201),
    dep_delay > 15,arr_delay > 15
  ) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 3600) + 
  facet_wrap(~month(dep_time))

# distruição da quantidade de atrasos partida mensal
my_flights %>% 
  filter(dep_delay > 15,arr_delay > 15) %>%
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400 * 7) # 86400 seconds = 1 day

# distruição da quantidade de atrasos
my_flights %>% 
  filter(
    dep_time < ymd(20130201),
    dep_delay > 15,
    arr_delay > 15
  ) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 3600) # 600 s = 10 minutes

# distruição da quantidade de atrasos
my_flights %>% 
  filter(#dep_time < ymd(20130201),
    dep_delay > 15,arr_delay > 15
  ) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 3600) + 
  facet_wrap(~month(dep_time))



# flights depart during the week than on the weekend
my_flights %>% 
  mutate(wday = wday(dep_time, label = TRUE))%>% 
  ggplot(aes(x = wday)) +
  geom_bar()

# delays depart during the week than on the weekend
my_flights %>% 
  filter(dep_delay > 15,arr_delay > 15) %>%
  mutate(wday = wday(dep_time, label = TRUE))%>% 
  ggplot(aes(x = wday)) +
  geom_bar()

# delays horarios
View(my_flights)
my_flights %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

# voos horarios
sched_dep <- my_flights %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()


# atrasos no decorrer do dia
my_flights %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 1300)













my_flights %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()



ggplot(data = my_flight, aes(wday(dep)) + 
         my_flights %>% 
         mutate(wday = wday(dep_time, label = TRUE)) %>% 
         ggplot(aes(x = wday)) +
         
         
         
         
         str(my_flights)
       
       ggplot(data = my_flights) +
         geom_point(x = my_flights$month, y = mean(my_flights$dep_delay))
       
       
       
       
       
       View(mpg)
       View(head(my_flights))
       
       
       
       ggplot(data = mpg) + 
         geom_point(mapping = aes(x = displ, y = hwy, color = class, size=class))
       
       
       ggplot(data = my_flights) + 
         geom_point(mapping = aes(x = , y = hwy)) + 
         facet_wrap(~ class, nrow = 2)
       
       
       
       ggplot(data = mpg) + 
         geom_point(mapping = aes(x = drv, y = cyl)) +
         facet_grid(drv ~ cyl)
       
       
       
       ggplot(data = mpg) + 
         geom_point(mapping = aes(x = displ, y = hwy)) +
         facet_grid(drv ~ .)
       
       # left
       ggplot(data = mpg) + 
         geom_point(mapping = aes(x = displ, y = hwy)) +
         geom_smooth(mapping = aes(x = displ, y = hwy))
       
       # right
       ggplot(data = mpg) + 
         geom_smooth(mapping = aes(x = displ, y = hwy))
       
       ggplot(data = mpg, mapping = aes(hwy)) + 
         #geom_point() + 
         geom_histogram()
       
       
       
       
       ## ------------------------------------------------------------------
       
       str(dfComplete$flights_month)
       
       str(nycflights13::flights)
       
       str(flights)
       ggplot(flights, aes(month)) + 
         geom_histogram(aes(y =..density..),
                        breaks=seq(1, 12, by=1),
                        col = "black",
                        fill = "green",
                        alpha = .2
                        
         ) +
         geom_density(col=100)+
         scale_fill_gradient("Count", low="darkgreen", high="darkred") + 
         labs(title="Voos por mes", x="Mês", y="Quant")
       
       #Analisando a quantidade de voos que saem de cada aeroporto
       flights_from_origin_airports <- sqldf('select dfComplete.airport_origin_name, 
                               count(*) as qtd_flights 
                               from dfComplete 
                               group by dfComplete.airport_origin_name')
       
       bp_flights_origin_airports <- ggplot(data=flights_from_origin_airports, aes(x=qtd_flights, y=airport_origin_name,fill=airport_origin_name)) +
         geom_bar(stat="identity")+
         geom_text(aes(label=qtd_flights), vjust=-0.3, size=3.5)+
         theme_minimal()+ ggtitle("Flights by Origin Aiports") +
         xlab("Qtd") + ylab("Airport")
       
       bp_flights_origin_airports + theme(
         plot.title = element_text(color="black", size=14, face="bold.italic"),
         axis.title.x = element_text(color="black", size=12, face="bold"),
         axis.title.y = element_text(color="black", size=12, face="bold"),
         legend.position="none"
       )
       
       #Analisando a quantidade de voos com destino aos 10 principais aeroportos
       flights_from_dest_airports <- sqldf('select dfComplete.airport_destination_name, 
                               count(*) as qtd_flights 
                               from dfComplete 
                               group by dfComplete.airport_destination_name
                                    order by count(*) desc
                                    limit 10')
       
       bp_flights_dest_airports <- ggplot(data=flights_from_dest_airports, aes(x=reorder(airport_destination_name, -qtd_flights), y=qtd_flights,fill=airport_destination_name)) +
         geom_bar(stat="identity")+
         geom_text(aes(label=qtd_flights), vjust=-0.3, size=3.5)+
         theme_minimal()+ ggtitle("Flights by Top 10 Destination Aiports") +
         xlab("Airport") + ylab("Qtd")
       
       bp_flights_dest_airports + theme(
         plot.title = element_text(color="black", size=14, face="bold.italic"),
         axis.title.x = element_text(color="black", size=12, face="bold"),
         axis.title.y = element_text(color="black", size=12, face="bold"),
         legend.position="none",
         axis.text.x=element_text(angle=90, hjust=1)
       )
       
       #Distribuição do atraso
       flights_delayed <- sqldf("select * from dfComplete 
                               where dfComplete.flights_delayed = 'Yes'")
       flights_delayed$total_delay <- flights_delayed$flights_arr_delay + flights_delayed$flights_dep_delay
       
       ggplot(flights_delayed, aes(x=total_delay, fill=airport_origin_name)) +
         geom_histogram(binwidth=.5, alpha=.5, position="dodge") + coord_cartesian(xlim = c(-50, 300)) + ggtitle("Distribution of Delay")+  xlab("Delay") + ylab("Qtd") 
       
       
       #Analisando os voos com atraso, por aerporto
       flights_delayed <- sqldf("select dfComplete.airport_origin_name as Airport, 
                               sum(case flights_delay_on_departure
                                when 'Yes' then 1
                                else 0 end) as qtd_delay_on_departure,
                                sum(case flights_delay_on_arrive
                                when 'Yes' then 1
                                else 0 end) as qtd_delay_on_arrive,
                               count(*) as qtd_flights
                               from dfComplete 
                               where dfComplete.flights_delayed = 'Yes'
                               group by dfComplete.airport_origin_name")
       
       flights_delayed <- flights_delayed %>% 
         arrange(desc(Airport)) %>%
         mutate(prop = qtd_flights / sum(flights_delayed$qtd_flights) *100) %>%
         mutate(ypos = cumsum(prop)- 0.5*prop )
       
       ggplot(flights_delayed, aes(x="", y=prop, fill=Airport)) +
         geom_bar(stat="identity", width=1, color="white") +
         coord_polar("y", start=0) +
         theme_void() +
         ggtitle("Flights Delayeds by Airport")+
         
         geom_text(aes(y = ypos, label = paste(qtd_flights," - ",round(prop,2),"%")), color = "white", size=4) +
         scale_fill_brewer(palette="Set1")
       
       
       
       
       #A distribuição do tempo de atraso por aeroporto, na partida
       flight_delayed_time_dep <- sqldf("select * 
                                 from dfComplete 
                                 where flights_delay_on_departure ='Yes'")
       
       ggplot(flight_delayed_time_dep, aes(x=flights_dep_delay, y=airport_origin_name, fill=airport_origin_name)) + 
         geom_boxplot(outlier.shape=NA)+ ggtitle("Delay on Departure Distribution by Airport")+  xlab("Delay") + ylab("Airport") + coord_cartesian(xlim = c(-0, 200))
       
       delays_dep <- group_by(flight_delayed_time_dep, airport_origin_name)
       
       summarize(delays_dep, count = n(), mean_delay = mean(flights_dep_delay, na.rm = T), median_delay = median(flights_dep_delay,  na.rm = T))
       
       
       #A distribuição do tempo de atraso por aeroporto, na chegada
       flight_delayed_time_arr <- sqldf("select * 
                                 from dfComplete 
                                 where flights_delay_on_arrive ='Yes'")
       
       ggplot(flight_delayed_time_arr, aes(x=flights_arr_delay, y=airport_origin_name, fill=airport_origin_name)) + 
         geom_boxplot(outlier.shape=NA)+ ggtitle("Delay on Arrive Distribution by Airport")+  xlab("Delay") + ylab("Airport")  + coord_cartesian(xlim = c(-0, 200))
       
       delays_arr <- group_by(flight_delayed_time_arr, airport_origin_name)
       
       summarize(delays_arr, count = n(), mean_delay = mean(flights_arr_delay, na.rm = T), median_delay = median(flights_arr_delay,  na.rm = T))
       
       
       
       #A distribuição do tempo de atraso por cia aerea, na partida
       ggplot(flight_delayed_time_dep, aes(x=flights_dep_delay, y=airlines_name, fill=airlines_name)) + 
         geom_boxplot(outlier.shape=NA)+ ggtitle("Delay on Departure Distribution by Airline")+  xlab("Delay") + ylab("Airline")  + coord_cartesian(xlim = c(-0, 250))
       
       delays_dep_airline <- group_by(flight_delayed_time_dep, airlines_name)
       
       summarize(delays_dep_airline, count = n(), mean_delay = mean(flights_dep_delay, na.rm = T), median_delay = median(flights_dep_delay,  na.rm = T))
       
       
       #A distribuição do tempo de atraso por aeroporto, na chegada
       ggplot(flight_delayed_time_arr, aes(x=flights_arr_delay, y=airlines_name, fill=airlines_name)) + 
         geom_boxplot(outlier.shape=NA)+ ggtitle("Delay on Arrive Distribution by Airline")+  xlab("Delay") + ylab("Airline") + coord_cartesian(xlim = c(-0, 250))
       
       delays_arr_airline <- group_by(flight_delayed_time_arr, airlines_name)
       
       summarize(delays_arr_airline, count = n(), mean_delay = mean(flights_arr_delay, na.rm = T), median_delay = median(flights_arr_delay,  na.rm = T))
       
       #Sazonalidade dos atrasos
       
       #Por mes
       dfSazonalidade <- dfFlights %>% 
         select(origin, month, day , arr_delay, dep_delay) %>%
         filter(dep_delay >= 0) %>%
         group_by(origin, month, day) %>%
         summarise(avg_delay =  mean(arr_delay, na.rm = TRUE) + 
                     mean(dep_delay, na.rm = TRUE)) %>%
         ungroup() %>%
         arrange(-avg_delay)
       
       dfSazonalidade$date <- with(dfSazonalidade, ISOdate(year = 2013, month, day))
       
       #Gráfico da distribuição média do atraso
       g <- ggplot(dfSazonalidade, aes(x = dfSazonalidade$date, y = dfSazonalidade$avg_delay))
       g + geom_point(aes(color = dfSazonalidade$origin)) + xlab("Date") + ylab("Average Delay (mins)") + geom_smooth(color = "Black") + ggtitle("Seasonality Trends")
       #Tendência dos atrasos médios
       g + xlab("Date") + ylab("Average Delay (mins)") + geom_smooth(color = "Blue") + ggtitle("Seasonality Trends")
       
       #Por horario
       dep_delay_data <- dfFlights %>%  select(dep_delay, arr_delay, hour)  
       #Transformando a hora de 2400 para 0
       dep_delay_data$hour <- ifelse(dep_delay_data$hour == 2400, 0, 
                                     dep_delay_data$hour)
       
       dep_delay <- dep_delay_data %>%
         select(hour, dep_delay, arr_delay) %>%
         group_by(hour) %>%
         summarise(avg_delay = mean(arr_delay, na.rm = TRUE) +
                     mean(dep_delay, na.rm = TRUE)) %>%
         na.omit()
       
       g <- ggplot(dep_delay, aes(x = as.numeric(hour), y = avg_delay, 
                                  title = "Delay - hourly"))
       g + geom_point(color = "Black") + geom_smooth() + ylab("Average Delay (mins)") +
         xlab("Hour") + ggtitle("Delay by Hour")
       
       
       
       #Correlação com o clima
       dfComplete$hour <- ifelse(dfComplete$hour == 2400, 0, dfComplete$hour)
       dfComplete$total_delay <- dfComplete$flights_arr_delay + dfComplete$flights_dep_delay
       cor_data <- select(dfComplete, total_delay, weather_temp, weather_dewp, weather_humid,
                          weather_wind_dir, weather_wind_speed, weather_wind_gust, weather_precip, weather_pressure, weather_visib)
       
       corrplot(cor(na.omit(cor_data)), method = "pie", type = "upper",
                tl.srt = 25, tl.col = "Black", tl.cex = 1, title = "Correlation
         between all 'weather' variables & 'departure delay'", mar =c(0, 0, 4, 0) + 0.1)
       
       #Regressão linear das variaveis de clima X atraso
       summary(lm(total_delay ~., data = cor_data))
       
       g <- ggplot(cor_data, aes(y = weather_humid, x = total_delay))
       g + geom_smooth() + ylab("Relative Humidity") + 
         xlab("Total Delay (mins)") + ggtitle("Total Delay X Relative Humidity")
       #
       g <- ggplot(cor_data, aes(y = weather_temp, x = total_delay))
       g + geom_smooth() + ylab("Temperature") + 
         xlab("Total Delay (mins)") + ggtitle("Total Delay X Temperature")
       
       #Atrasos por aeronave
       dfComplete$planes_age <- dfComplete$flights_year - dfComplete$planes_year
       
       planes_age_delay <- dfComplete %>%
         select(planes_age, total_delay) %>%
         group_by(planes_age) %>%
         summarise(avg_delay = mean(total_delay, na.rm = TRUE)) %>%
         na.omit()
       
       g <- ggplot(planes_age_delay, aes(x = as.numeric(planes_age), y = avg_delay))
       g + geom_point(color = "Black") + geom_smooth() + ylab("Average Delay (mins)") +
         xlab("Age of Plane") + ggtitle("Delay by Planes Age")
       
       #Atrasos por fabricante
       manufacturer_delay <- dfComplete %>%
         select(planes_manufacturer, total_delay) %>%
         group_by(planes_manufacturer) %>%
         summarise(avg_delay = mean(total_delay, na.rm = TRUE)) %>%
         na.omit()
       
       ggplot(manufacturer_delay, aes(x = factor(planes_manufacturer) , y = avg_delay)) + 
         geom_bar(stat = "identity") + xlab("Average Delay (mins)") +
         ylab("Manufacturers") +
         geom_text(aes(label=round(avg_delay,0)), vjust=-0.3, size=3.5)+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
       
       #Atrasos por cia aerea
       airlines_delay <- dfComplete %>%
         select(airlines_name, total_delay) %>%
         group_by(airlines_name) %>%
         summarise(avg_delay = mean(total_delay, na.rm = TRUE)) %>%
         na.omit()
       
       ggplot(airlines_delay, aes(x = factor(airlines_name) , y = avg_delay)) + 
         geom_bar(stat = "identity") + xlab("Average Delay (mins)") +
         ylab("Airline") +
         geom_text(aes(label=round(avg_delay,0)), vjust=-0.3, size=3.5)+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
       
       #Atrasos por tipo de motor
       engine_type_avg_delay <- dfComplete %>%
         select(planes_engine, total_delay) %>%
         group_by(planes_engine) %>%
         summarise(avg_delay = mean(total_delay, na.rm = TRUE)) %>%
         na.omit()
       
       ggplot(engine_type_avg_delay, aes(x = factor(planes_engine) , y = avg_delay)) + 
         geom_bar(stat = "identity") + xlab("Average Delay (mins)") +
         ylab("Engine") +
         geom_text(aes(label=round(avg_delay,0)), vjust=-0.3, size=3.5)+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
       
       engine_type_delay <- dfComplete %>%
         select(planes_engine, total_delay) %>%
         na.omit()
       
       ggplot(engine_type_delay, aes(x=total_delay, y=planes_engine, fill=planes_engine)) + 
         geom_boxplot(outlier.shape=NA)+ ggtitle("Delay by Engine Airline")+  xlab("Delay") + ylab("Engine") + coord_cartesian(xlim = c(-100, 200))
       
       summarize(engine_type_delay, count = n(), mean_delay = mean(total_delay, na.rm = T), median_delay = median(total_delay,  na.rm = T))
       
       
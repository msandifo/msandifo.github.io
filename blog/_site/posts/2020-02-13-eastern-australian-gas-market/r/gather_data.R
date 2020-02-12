library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
source('/Volumes/data/Dropbox/msandifo/documents/programming/r/2020/gasbb/gasbb/r/funcs.R')

load("data/gladstoneLNG.Rdata")

archived.flows.csv <- "data/archived/ActualFlows.csv"

arch.flows <- read_csv(archived.flows.csv ) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate), lastchanged=lubridate::dmy_hm(lastchanged)) %>%
  dplyr::rename(facilityid=plantid, facilityname=plantname, locationid=zoneid, locationname=zonename, lastupdated=lastchanged) 
#tail(arch.flows ,10)  

#arch.flows$facilityname %>% unique()

#arch.flows %>% subset(locationname=="Curtis Island LNG Demand Zone" )
#arch.flows %>% subset(facilityname=="Wungoona"  )

flows.1819.csv <- c( "data/current/Actual Flow and Storage_2018_2019.csv", "data/current/Actual Flow and Storage_202001.csv" , "data/current/Actual Flow and Storage_202002.csv")
flows.1819 <- purrr::map_df(flows.1819.csv, data.table::fread) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate) )
#tail(flows.1819  )

 facility.types<-flows.1819[,c(3,4)] %>% group_by(facilityid) %>% dplyr::summarise(facilitytype=facilitytype[1])

 archived.flows <-  left_join(arch.flows,  facility.types )
 #archived.flows$state
 
 state.type<- flows.1819[,c(10,12)]%>% group_by(locationid) %>% dplyr::summarise(state=state[1])
 state.type.1<- flows.1819[,c(2,10)]%>% group_by(facilityname) %>% dplyr::summarise(state=state[1])
 
 #archived.flows <-  dplyr::left_join(archived.flows,   state.type )
 archived.flows <-  left_join(archived.flows,   state.type.1 )
 archived.flows <-  left_join(archived.flows,   state.type )
 #archived.flows$state
 
 
 af.reorder <-tidyr::pivot_wider(archived.flows,names_from =flowdirection,   values_from = actualquantity, values_fill=list(DELIVERY=0, RECEIPT=0) )%>% 
   dplyr::rename( supply=DELIVERY,demand=RECEIPT) %>%
   dplyr::select(gasdate,facilityname ,      facilityid ,  facilitytype, demand, supply , locationname  , locationid  , state )
 cf.reorder <-flows.1819 %>%
   dplyr::select(gasdate,facilityname ,      facilityid , facilitytype, demand, supply , locationname  , locationid  , state )

 actual.flows <- bind_rows(af.reorder, cf.reorder) %>% subset(  !is.na(gasdate)) %>% subset(state %ni% c("NT" , "TAS") )
# tail(actual.flows)
# actual.flows$state %>% unique()
# actual.flows$facilityname[is.na(actual.flows $facilitytype)] %>% unique()

# actual.flows$facilityname[!is.na(actual.flows $facilitytype)] %>% unique()
actual.flows$facilitytype[actual.flows$facilityname %in% c("Ballera Gas Plant" ,"Wungoona" )] <-"PROD"
actual.flows$facilitytype[actual.flows$facilityname %in% c("Chookoo Storage Ballera" ) ] <-"STOR"
actual.flows$facilitytype[actual.flows$facilityname %in% c("Longford to Melbourne" ,    "NSW-Victoria Interconnect", "South West Pipeline","Spring Gully Pipeline" )  ] <-"PIPE"


actual.flows$state[actual.flows$facilityname %in% c("Ballera Gas Plant", "Berwyndale South" )  ] <-"QLD"

#actual.flows[actual.flows$state=="NSW" & !is.na(actual.flows$state),]
# ggplot(actual.flows %>% subset(locationid==540027 & facilitytype=="PIPE")  , aes(gasdate, demand, col=factor(facilityid)))+geom_line() 
# ggplot(actual.flows %>% subset( supply<5000 & supply>200 &facilityid %in% c( 530040 ))  , aes(gasdate, supply, col=factor(facilityid)))+geom_line() 
# 
# ggplot(actual.flows %>% subset( supply<50000  &facilityid %in% c( 530040, 530048 ))  , aes(gasdate, supply, col=factor(facilityid)))+geom_line() 
# ggplot(actual.flows %>% subset(   facilityid %in% c(  540091, 540092,540093,540058,540060 ) & supply!=0)  , aes(gasdate, supply, col=factor(facilityid)))+geom_line() 
# ggplot(actual.flows %>% subset(   facilityid %in% c(  540091, 540092,540093) & locationid==540030)  , aes(gasdate, demand, col=factor(facilityname)))+geom_line() 
# ggplot(actual.flows %>% subset(   facilityid %in% c(  540091, 540092,540093) & locationid==540030) %>% group_by(gasdate) %>% dplyr::summarise(demand= sum(demand, na.rm=T), supply=sum(supply, na.rm=T))  , aes(gasdate, demand))+geom_line() +geom_line(aes(y=supply),col="red2") 
# 
# ggplot(actual.flows %>% subset(      locationid==540030) %>% group_by(gasdate) %>% dplyr::summarise(demand= sum(demand))  , aes(gasdate, demand ))+geom_line() 
# 

# (actual.flows   %>%      subset(facilitytype=="PIPE" & state=="QLD") )$locationname  %>% unique()
# (actual.flows   %>%      subset(facilitytype=="PIPE" & state=="QLD") )$facilityname  %>% unique()

 
  
 curtis.day <-actual.flows   %>%  
  subset(facilitytype=="PIPE" & state=="QLD" & 
           (str_detect(locationname, "Curtis Island" ) 
             # str_detect(facilityname, "Wallumbilla to Gladstone Pipeline" )|
             #   str_detect(facilityname, "GLNG Gas Transmission Pipeline" )
             # str_detect(facilityname,"APLNG Pipeline")
               )) %>% 
  group_by(gasdate, locationname ) %>% 
  dplyr::summarise(state="QLD", demand= sum(demand), supply=sum(supply ), adj.supply =max(demand,supply, na.rm=T), facilityname=facilityname[1]) %>%
    dplyr::arrange(gasdate)  %>% subset(adj.supply>0)
 
 curtis.month <- curtis.day %>% 
   mutate(year=lubridate::year(gasdate), month=lubridate::month(gasdate)) %>%
   group_by(year, month, state) %>%
   dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), adj.supply=mean(adj.supply, na.rm=T), gasdate=mean(gasdate)) 
 
 curtis.month$gasdate = lubridate::ymd(paste0( curtis.month$year,"-", curtis.month$month,"-15"))
 #names( curtis.month)
 parasitic <-mean(head(curtis.month[curtis.month$gasdate > ymd("2016-01-01"),]$adj.supply, -1)/glad.lng[glad.lng$date> ymd("2016-01-01"),]$TJ)
 glad.lng.ym <- glad.lng %>%
   dplyr::mutate(month=month(date), year=year(date))  %>%
   select(year, month, TJ, date)
 #smoother
 les <-(length(glad.lng.ym$TJ)-1)
 glad.lng.ym$TJ[2:les] = glad.lng.ym$TJ[2:les -1]/6 +glad.lng.ym$TJ[2:les]*4/6 +glad.lng.ym$TJ[2:les +1]/6
 glad.lng.ym$TJ[les+1] = glad.lng.ym$TJ[les]*3/4 +glad.lng.ym$TJ[les+1]*1/4
 
 
 curtis.month.2015 <- tibble( year= glad.lng.ym$year[2:10], 
                                  month= glad.lng.ym$month[2:10], 
                                  state="QLD", 
                                  demand=NA, 
                                  supply=glad.lng.ym$TJ[2:10], 
                                  adj.supply=glad.lng.ym$TJ[2:10]*parasitic,
                                  gasdate=glad.lng.ym$date[2:10] )
 
 curtis.month<- bind_rows(curtis.month.2015, curtis.month)

 
 # actual.flows   %>%  
 #       subset(facilitytype=="PROD") %>% 
 #         group_by(gasdate) %>% 
 #         dplyr::summarise(demand= sum(demand ), supply=sum(supply )) %>% 
 #         subset(supply<15000) %>% 
 #  ggplot(aes(gasdate, supply ))+
 #  geom_line()  

total.prod.day <-actual.flows   %>% 
  subset(facilitytype=="PROD" & state!="NT")%>%   subset(supply<15000) %>% 
  
  group_by(gasdate, state)  %>%
  dplyr::summarise(demand= sum(demand, na.rm=T),supply=sum(supply, na.rm=T))


total.prod.month <- total.prod.day %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::week(gasdate)) %>%
  group_by(year, month, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), gasdate=mean(gasdate)) 

 ind <-  which( total.prod.month$state=="NSW" &  total.prod.month$supply >100) 
total.prod.month$supply[ind] <-1
g.date <- c(total.prod.month$gasdate[ind] , total.prod.month$gasdate[1066]) 

g.date <- c(total.prod.month$gasdate[ind] , total.prod.month$gasdate[311], total.prod.month$gasdate[1071]) 
#g.date <- as.Date(c("2010-06-14", "2010-06-07", "2014-01-05"))
total.prod.month$supply[total.prod.month$supply>100 & total.prod.month$state=="NSW"] <-0




#---------

total.prod.month <- total.prod.day %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::month(gasdate)) %>%
  group_by(year, month, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), gasdate=mean(ymd(paste(year,"-",month,"-15")))) 

# from ratio of infliws and exports
#parasitic <-mean(head(curtis.month[curtis.month$gasdate > ymd("2016-01-01"),]$adj.supply, -1)/glad.lng[glad.lng$date> ymd("2016-01-01"),]$TJ)

#total.prod.month.join <-left_join(total.prod.month , glad.lng.ym )  
total.prod.month.join <-left_join(total.prod.month , curtis.month[, c("gasdate","adj.supply")]    %>% dplyr::rename(TJ= adj.supply))
# tail(curtis.month)
# tail(total.prod.month.join %>% subset(state=="QLD"),20)
total.prod.month.join$TJ[is.na(total.prod.month.join$TJ)] <-0
total.prod.month.join$adj.supply <- total.prod.month.join$supply
# total.prod.month.join$adj.supply[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
#   total.prod.month.join$TJ[total.prod.month.join$state=="QLD"]* parasitic
total.prod.month.join$adj.supply[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
  total.prod.month.join$TJ[total.prod.month.join$state=="QLD"] #* parasitic

#total.prod.month.join$adj.supply[total.prod.month.join$adj.supply<=0] =1


dd= total.prod.month.join %>% subset(state=="NSW") #%>% arrange((adj.supply)) 
total.prod.month.join$adj.supply[total.prod.month.join$adj.supply>100 & total.prod.month.join$state=="NSW"] <-0
 total.prod.month.join$adj.supply[total.prod.month.join$adj.supply<1 & total.prod.month.join$state=="QLD"] <-1
 total.prod.month.join$adj.supply[is.na(total.prod.month.join$adj.supply)& total.prod.month.join$state=="NSW"] <-1
total.prod.month.join$date <- ymd(paste0(total.prod.month.join$year,"-", total.prod.month.join$month, "-", 15))


reservation <- .08
total.prod.month.join$adj.supply.force.maj <- total.prod.month.join$supply
total.prod.month.join$adj.supply.force.maj[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
  total.prod.month.join$TJ[total.prod.month.join$state=="QLD"]*(1-reservation ) 



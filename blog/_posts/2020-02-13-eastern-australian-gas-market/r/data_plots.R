library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

load("data/gladstoneLNG.Rdata")
`%ni%` = Negate(`%in%`) 


tik_do <-function( my.p, file="test", width=8, height=5, standalone=T, close=T, bg="white" , sanitize=T, console=F){
  
  tikzDevice::tikz(paste0(file,'.tex'), standAlone = standalone,
                   width=width, height=height, bg=bg, 
                   sanitize = sanitize,
                   verbose=T, console=console)
  print(my.p)
  if (close) dev.off()
}


pdf2png <- function(file, open=TRUE, clean=TRUE){
  if ( tools::file_ext(file) =="tex") {
    message(paste("processing",file,"file using tools::texi2dvi" ))
    tools::texi2dvi(file,pdf=T, clean = clean)  
    file.remove( basename(stringr::str_replace(file, ".tex", ".aux") ) )
    file.remove( basename(stringr::str_replace(file, ".tex", ".log")))
    outfile = stringr::str_replace(file, ".tex", ".pdf") %>% str_split("/")  
    file = outfile[[1]][length(outfile[[1]])] 
  }
  outfile = stringr::str_replace(file, ".pdf", ".png")
  system(paste("sips -s format png --padColor FFFFFF --optimizeColorForSharing -s formatOptions 300 ", file,"  --out ", outfile))
  system(paste("convert", outfile," -fill white -opaque none ", outfile))
  if (open==T) system(paste("open ", outfile ))
}

archived.flows.csv <- "data/archived/ActualFlows.csv"
#archived.flows <- data.table::fread(archived.flows.csv ) %>% dplyr::rename_all(tolower)
arch.flows <- read_csv(archived.flows.csv ) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate), lastchanged=lubridate::dmy_hm(lastchanged)) %>%
  dplyr::rename(facilityid=plantid, facilityname=plantname, locationid=zoneid, locationname=zonename, lastupdated=lastchanged) 
tail(arch.flows ,10)  

arch.flows$facilityname %>% unique()

arch.flows %>% subset(locationname=="Curtis Island LNG Demand Zone" )
arch.flows %>% subset(facilityname=="Wungoona"  )

flows.1819.csv <- c( "data/current/Actual Flow and Storage_2018_2019.csv", "data/current/Actual Flow and Storage_202001.csv" , "data/current/Actual Flow and Storage_202002.csv")
flows.1819 <- purrr::map_df(flows.1819.csv, data.table::fread) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate) )
tail(flows.1819  )

 facility.types<-flows.1819[,c(3,4)] %>% group_by(facilityid) %>% dplyr::summarise(facilitytype=facilitytype[1])

 archived.flows <-  left_join(arch.flows,  facility.types )
 #archived.flows$state
 
 state.type<- flows.1819[,c(10,12)]%>% group_by(locationid) %>% dplyr::summarise(state=state[1])
 state.type.1<- flows.1819[,c(2,10)]%>% group_by(facilityname) %>% dplyr::summarise(state=state[1])
 
 #archived.flows <-  dplyr::left_join(archived.flows,   state.type )
 archived.flows <-  left_join(archived.flows,   state.type.1 )
 archived.flows <-  left_join(archived.flows,   state.type )
 archived.flows$state
 
 
 af.reorder <-tidyr::pivot_wider(archived.flows,names_from =flowdirection,   values_from = actualquantity, values_fill=list(DELIVERY=0, RECEIPT=0) )%>% 
   dplyr::rename( supply=DELIVERY,demand=RECEIPT) %>%
   dplyr::select(gasdate,facilityname ,      facilityid ,  facilitytype, demand, supply , locationname  , locationid  , state )
 cf.reorder <-flows.1819 %>%
   dplyr::select(gasdate,facilityname ,      facilityid , facilitytype, demand, supply , locationname  , locationid  , state )

 actual.flows <- bind_rows(af.reorder, cf.reorder) %>% subset(  !is.na(gasdate)) %>% subset(state %ni% c("NT" , "TAS") )
tail(actual.flows)
actual.flows$state %>% unique()
actual.flows$facilityname[is.na(actual.flows $facilitytype)] %>% unique()

actual.flows$facilityname[!is.na(actual.flows $facilitytype)] %>% unique()
actual.flows$facilitytype[actual.flows$facilityname %in% c("Ballera Gas Plant" ,"Wungoona" )] <-"PROD"
actual.flows$facilitytype[actual.flows$facilityname %in% c("Chookoo Storage Ballera" ) ] <-"STOR"
actual.flows$facilitytype[actual.flows$facilityname %in% c("Longford to Melbourne" ,    "NSW-Victoria Interconnect", "South West Pipeline","Spring Gully Pipeline" )  ] <-"PIPE"

actual.flows$facilityname[is.na(actual.flows$state)] %>% unique()
actual.flows$state[actual.flows$facilityname %in% c("Ballera Gas Plant", "Berwyndale South" )  ] <-"QLD"
actual.flows[actual.flows$state=="NSW" & !is.na(actual.flows$state),]
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
 names( curtis.month)
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

 
 actual.flows   %>%  
       subset(facilitytype=="PROD") %>% 
         group_by(gasdate) %>% 
         dplyr::summarise(demand= sum(demand ), supply=sum(supply )) %>% 
         subset(supply<15000) %>% 
  ggplot(aes(gasdate, supply ))+
  geom_line()  

total.prod.day <-actual.flows   %>% 
  subset(facilitytype=="PROD" & state!="NT")%>%   subset(supply<15000) %>% 
  
  group_by(gasdate, state)  %>%
  dplyr::summarise(demand= sum(demand, na.rm=T),supply=sum(supply, na.rm=T))

#ggplot(total.prod.day  , aes(gasdate, supply , fill=state))+geom_area() 

total.prod.month <- total.prod.day %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::week(gasdate)) %>%
  group_by(year, month, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), gasdate=mean(gasdate)) 

# ind <-  which( total.prod.month$state=="VIC" &  total.prod.month$supply >1150) 
# 
# total.prod.month[total.prod.month$state=="NSW",]%>% dplyr::arrange( desc(supply))
ind <-  which( total.prod.month$state=="NSW" &  total.prod.month$supply >100) 
total.prod.month$supply[ind] <-16
g.date <- c(total.prod.month$gasdate[ind] , total.prod.month$gasdate[1066]) 

# n.ind <-  which( total.prod.month$state=="NSW" &  total.prod.month$supply <1) 
# total.prod.month$supply[n.ind] <-1

# total.prod.month[ total.prod.month$state=="SA" ,]%>% dplyr::arrange(  desc(supply))
# 
#  total.prod.month[which( total.prod.month$supply<200 & total.prod.month$state != "NSW"),]
# total.prod.month[ind,]  
# 
# #ind <-  which( total.prod.month$state=="NSW" &  total.prod.month$supply >100) 
# total.prod.month [ind,1] 


#mutate(state= factor(state, levels=c("NSW", "VIC", "SA", "QLD"))) 
  
  
  
(p1 <-ggplot(total.prod.month %>% subset(state!="NSW") %>%
         mutate(state = factor(state, levels=c(#"NSW",
                                               "SA", "VIC", "QLD"))) , # %>%
#subset (gasdate != g.date) , 
aes(gasdate, supply , fill=state))+
  geom_area(position="stack",col=c("grey80" ), size=.1) +
  geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
  geom_line(data=glad.lng.ym  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
  
  # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
  #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
  annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
  annotate("text", x=as.Date("2018-01-01"), y=4000. ,label= "Gladstone\ndemand", size=2.6, colour="white") +
  annotate("text", x=as.Date("2019-01-01"), y=2700. ,label= "Gladstone\nLNG exports", size=2.6, colour="yellow") +
  scale_fill_manual(
    values =  c(
    #  "lightblue1" ,
      "red2",
      "darkblue",    
      "green4") )+
   scale_y_continuous(  expand = c(0,0))+
  scale_x_date(  expand = c(0,0) )+
   hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
  
  theme(legend.position = c(.144,.8),
        legend.background = element_rect(fill=  "white" , size=0),
        #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
        #panel.ontop = T,
        legend.direction = "vertical", 
        legend.title = element_blank())+
  
  labs(title= "Australian east coast gas market - production", 
      # subtitle="production - TJ/day",
       y= "TJ/day",
       x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
  geom_hline(yintercept = 1:5*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
  geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
             size=.15, col= mmt::add.alpha("grey20",.5))
)


ggsave(paste0('figs/auseast_coast.png'),  width=8.5, height=4.8)
#legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
# tik_do(p1, file= "figs/auseast_coast_tz")
# pdf2png("figs/auseast_coast_tz.tex")
# 
# list.files()
# pdf2png("./figs/MaunaLoa_trendn.tex")

g.date <- c(total.prod.month$gasdate[ind] , total.prod.month$gasdate[311], total.prod.month$gasdate[1071]) 
#g.date <- as.Date(c("2010-06-14", "2010-06-07", "2014-01-05"))
total.prod.month$supply[total.prod.month$supply>100 & total.prod.month$state=="NSW"] <-0

(p2 <-ggplot( total.prod.month %>% subset( state !="NSW") %>%
          mutate(state = factor(state, levels=c(#"NSW", 
                                                "SA", "VIC", "QLD"))) %>%
          subset (gasdate %ni% g.date) , aes(gasdate, supply , fill=state))+
  geom_area(position="fill",col=c("grey80" ), size=.1) +
  scale_fill_manual(
    values = c(
   #   "lightblue1" ,
      "red2",
      "darkblue",    
      "green4" ))+
  scale_y_continuous(  expand = c(0,0), labels = scales::percent, breaks= (1:4)*2/10)+
  scale_x_date(  expand = c(0,0) )+
  hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
  
  theme(legend.position = c(.85,.3),
        legend.background = element_rect(fill= mmt::add.alpha("white", .5), size=0),
        legend.direction = "vertical", legend.title = element_blank())+
  labs(title= "Australian east coast gas market - production", 
    #   subtitle="production - TJ/day",
       y= " ",
       x=NULL, caption="data source from AEMO")+
 # geom_hline(yintercept= c(.3, .7), col="white", linetype=2, size=.1)+
  geom_hline(yintercept = (1:4)*2/10, size=.15, col= mmt::add.alpha("grey80",.5))+
  geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
             size=.15, col= mmt::add.alpha("grey80",.5))
)

ggsave(paste0('figs/auseast_coast_per.png'),  width=8.5, height=4.8)



#ggplot(actual.flows %>% subset(      locationid==530013) %>% group_by(gasdate) %>% dplyr::summarise(supply= sum( supply))  , aes(gasdate, supply ))+geom_line() 
# 
# actual.flows$locationid %>% unique
# actual.flows$locationid[actual.flows$facilityname   %>% stringr::str_detect("Longford Ga")]
# actual.flows$facilityname[actual.flows$facilityname   %>% stringr::str_detect("Longford")]
# actual.flows$facilityid[which(stringr::str_detect(actual.flows$facilityname,  "GLNG Gas Transmission Pipeline" ))[1]]
# actual.flows$facilityid[which(stringr::str_detect(actual.flows$facilityname,  "APLNG Pipeline" ))[1]]
# actual.flows$facilityid[which(stringr::str_detect(actual.flows$facilityname,  "Queensland Gas Pipeline" ))[1]]
# actual.flows$facilityid[which(stringr::str_detect(actual.flows$facilityname,"South West Queensland Pipeline"))[1]]
# actual.flows$facilityid[which(stringr::str_detect(actual.flows$facilityname,"Wallumbilla to Gladstone Pipeline"))[1]]
# 
# actual.flows$facilityid[which(stringr::str_detect(actual.flows$facilityname,"Longford to"))[1]]
# actual.flows$facilityid[which(stringr::str_detect(actual.flows$facilityname,"Mel"))[1]]
# 
# 
# ggplot(actual.flows %>% subset(locationid==540027 & facilitytype=="PROD") %>% dplyr::group_by(gasdate) %>% dplyr::summarise(supply=sum(supply)), aes(gasdate, supply))+geom_line() 
# flows.1819
# 
# flows.1819$locationname %>% unique()
# 
# archived.flows$zonename  %>% unique()
# archived.flows$locationname  %>% unique()
# 
# 
# flows.1819 %>% subset(gasdate==lubridate::ymd("2019-12-31") & state=="QLD" & facilitytype=="PIPE" & facilityid %in% c(  540091, 540092,540093))
# 
# af.reorder%>% subset(gasdate==lubridate::ymd("2017-12-31")   & facilitytype=="PIPE" & facilityid %in% c(  540091, 540092,540093))
# cf.reorder%>% subset(gasdate==lubridate::ymd("2019-12-31")   & facilitytype=="PIPE" & facilityid %in% c(  540091, 540092,540093))
# 
# af.reorder%>% subset(gasdate==lubridate::ymd("2017-12-31")      & locationid==530013 )
# cf.reorder%>% subset(gasdate==lubridate::ymd("2019-12-31")      & locationid ==c(  530013))


#---------

total.prod.month <- total.prod.day %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::month(gasdate)) %>%
  group_by(year, month, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), gasdate=mean(ymd(paste(year,"-",month,"-15")))) 

library(lubridate)
# from ratio of infliws and exports
parasitic <-mean(head(curtis.month[curtis.month$gasdate > ymd("2016-01-01"),]$adj.supply, -1)/glad.lng[glad.lng$date> ymd("2016-01-01"),]$TJ)

#total.prod.month.join <-left_join(total.prod.month , glad.lng.ym )  
total.prod.month.join <-left_join(total.prod.month , curtis.month[, c("gasdate","adj.supply")]    %>% dplyr::rename(TJ= adj.supply))
tail(curtis.month)
tail(total.prod.month.join %>% subset(state=="QLD"),20)
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

 
(p3 <-ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20) & state!="NSW")%>%
               mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
    geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>%
                subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ), size=.1) +
    # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # 
    # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
      annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("Gladstone demand\n(~LNG exports x ", ( round(parasitic, 2) ) ,")") , size=2.6, colour="white") +
     annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
     scale_fill_manual(
      values =  c(
    #    "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0), limits=c(-4300,2300))+
    scale_x_date(  expand = c(0,0) )+
    hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.144,.2),
        # legend.background = element_rect(fill=  "white" , size=0),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= "Australian east coast gas market - production balance", 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = -4:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
    geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))
)

ggsave(paste0('figs/auseast_coast_exports.png'),  width=8.5, height=4.8)


(p3a <-ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20)  &  state!="NSW")%>%
               mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
 #   geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # 
    # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
    scale_fill_manual(
      values =  c(
       # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0), limits=c(0,2500))+
    scale_x_date(  expand = c(0,0) )+
    hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.144,.2),
        #  legend.background = element_rect(fill=  "white" , size=0),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= "Australian east coast gas market - domestic balance", 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = -0:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
    geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))
)


ggsave(paste0('figs/auseast_coast_exports_domestic.png'),  width=8.5, height=4.8)

reservation <- .08
total.prod.month.join$adj.supply.force.maj <- total.prod.month.join$supply
total.prod.month.join$adj.supply.force.maj[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
  total.prod.month.join$TJ[total.prod.month.join$state=="QLD"]*(1-reservation ) 


(p3a <-ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20)  &  state!="NSW")%>%
                mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
              #subset (gasdate != g.date) , 
              aes(date, adj.supply.force.maj , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
    #   geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # 
    # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    annotate("text", x=as.Date("2014-01-01"), y=970. ,label= "domestic supply", size=2.6, colour="white") +
    scale_fill_manual(
      values =  c(
        # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0), limits=c(0,2500))+
    scale_x_date(  expand = c(0,0) )+
    hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.144,.2),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #legend.background = element_rect(fill=  "white" , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= paste0("Australian east coast gas market - domestic balance with ",round(( reservation)*100,0),"% reservation"), 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = -0:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
    geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))
)


ggsave(paste0('figs/auseast_coast_exports_domestic_fm.png'),  width=8.5, height=4.8)

(p4 <-ggplot(total.prod.month.join %>% subset( state !="NSW" &gasdate< Sys.Date()-days(20))%>%
               mutate(state = factor(state, levels=c(#"NSW", 
                                                     "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply , fill=state))+
     geom_area(position="fill",col=c("grey80" ), size=.1) +
     #geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # # 
    # # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    # annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    # annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
     scale_fill_manual(
      values =  c(
       # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0),  labels = scales::percent, breaks= (1:4)*2/10)+
    scale_x_date(  expand = c(0,0) )+
    hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.16,.59),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
 
    labs(title= "Australian east coast gas market - domestic supply balance", 
         # subtitle="production - TJ/day",
         y= " ",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = (1:4)*2/10, size=.15, col= mmt::add.alpha("grey20",.5))+
    #geom_hline(yintercept = 1:4, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))
)

ggsave(paste0('figs/auseast_coast_exports_per.png'),  width=8.5, height=4.8)



(p3a <-ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20)  &  state!="NSW")%>%
                mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
              #subset (gasdate != g.date) , 
              aes(date, adj.supply.force.maj , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
    #   geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # 
    # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    annotate("text", x=as.Date("2014-01-01"), y=970. ,label= "domestic supply", size=2.6, colour="white") +
    scale_fill_manual(
      values =  c(
        # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0), limits=c(0,2500))+
    scale_x_date(  expand = c(0,0) )+
    hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.144,.2),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #legend.background = element_rect(fill=  "white" , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= paste0("Australian east coast gas market - domestic balance with ",round(( reservation)*100,0),"% reservation"), 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = -0:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
    geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))
)


ggsave(paste0('figs/auseast_coast_exports_domestic_fm.png'),  width=8.5, height=4.8)

(p4 <-ggplot(total.prod.month.join %>% subset( state !="NSW" &gasdate< Sys.Date()-days(20))%>%
               mutate(state = factor(state, levels=c(#"NSW", 
                 "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply.force.maj , fill=state))+
    geom_area(position="fill",col=c("grey80" ), size=.1) +
    #geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # # 
    # # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    # annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    # annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
    scale_fill_manual(
      values =  c(
        # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0),  labels = scales::percent, breaks= (1:4)*2/10)+
    scale_x_date(  expand = c(0,0) )+
    hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.16,.59),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= paste0("Australian east coast gas market - domestic balance with ",round(( reservation)*100,0),"% reservation"), 
         # subtitle="production - TJ/day",
         y= " ",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = (1:4)*2/10, size=.15, col= mmt::add.alpha("grey20",.5))+
    #geom_hline(yintercept = 1:4, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))
)

ggsave(paste0('figs/auseast_coast_exports_per_fm.png'),  width=8.5, height=4.8)


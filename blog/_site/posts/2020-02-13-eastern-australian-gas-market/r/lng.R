#add to reproscir::download_gasbb()
gasbb_archived_data_repo<- "https://aemo.com.au/Gas/Gas-Bulletin-Board/Other-Information/Archived-Reports"

do_update=F
if (do_update){
glad.lng <-rbind(purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2015 ),
                 purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2016 ),
                 purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2017 ),
                 purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2018 ),
                 purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2019 ),
                 purrr::map_df(1,    reproscir::read_gladstone_ports, year=2020 )
)

glad.lng$TJ <-glad.lng$tonnes *12* 8.975/172/365
 

glad.lng <- rbind(glad.lng[1,] , glad.lng)
glad.lng$date[1] <-glad.lng$date[1] - months(1)
glad.lng$tonnes[1] <-0
glad.lng$TJ[1] <-0
glad.lng$year[1] <-2014
glad.lng$month[1] <-12
save(glad.lng, file="data/gladstoneLNG.Rdata")
} else load("data/gladstoneLNG.Rdata")



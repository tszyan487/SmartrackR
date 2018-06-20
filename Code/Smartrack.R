rm(list=ls(all=TRUE))

library(dplyr)
library(magrittr)
library(lubridate)

work_dir <- "C://Data//Smartrack//"
setwd(paste0(work_dir, ".//Data//"))


# Load the data
tfv_file <- list.files()
buses <- data.frame()

for(i in 1:length(tfv_file))
{
  buses <- rbind(buses,
                     read.csv(tfv_file[i], header = TRUE,stringsAsFactors = F,skip = 1))
}

setwd(work_dir)
# buses <- read.csv("C://Data/Smartrack/Data/TFV - CTD_13062018.csv", header = T, stringsAsFactors = F) %>% as.tbl()
# validLegs <- read.csv("C://Data/Smartrack/ValidLegs.csv", stringsAsFactors = F)

buses <- buses %>% filter(Geofence.Name != "")

# format arrival time and separate geofence name into columns, then order by bus and timestamp
buses <- buses %>% mutate(arrival = as.POSIXct(strptime(gsub('[\\.]','',Enter.Time), format = '%d/%m/%Y %I:%M:%S %p')) ,
                         project = unlist(lapply(strsplit(Geofence.Name," - "),'[[',2)),
                         stop.order = as.numeric(unlist(lapply(strsplit(Geofence.Name," - "),'[[',3))),
                         destination = unlist(lapply(strsplit(Geofence.Name," - "),'[[',4))) 
# %>%
# arrange(Resource.Name, seconds(Enter.Time))



#format dwell time into seconds
tmp <- strsplit(buses$Time.Inside.Geofence..hh.mm.ss., split=":") %>% lapply(as.numeric,1)
dwellTime <- do.call(rbind, lapply(tmp, rbind))
dwellTime <- dwellTime[,1]*60*60 + dwellTime[,2]*60 + dwellTime[,3]
buses$dwellTime <- dwellTime %>% seconds()


#calculate departure time (arrival+dwell) and get origin from preceding row
buses <- buses %>% mutate(departure = lag(dwellTime,1)+lag(arrival,1),
                          origin = ifelse(lag(Resource.Name,1)==Resource.Name,lag(destination,1),"0"))
buses$origin[1] <- "0"


#VALID TRIPS:
# Caulfield - Carnegie - LTD EXP Flag - Full Exp Flag - Westall
# Malvern - Carnegie - LTD EXP Flag - Oakleigh - Huntingdale - Clayton - Westall
# Caulfield - Carnegie - Murrumbeena - Hughesdale - Oakleigh - Huntingdale - Clayton - Westall
express <- c("Caulfield","Carnegie","LTD EXP Flag","Full Exp Flag","Westall")
ltd_express <- c("Malvern","Carnegie","LTD EXP Flag","Oakleigh","Huntingdale","Clayton","Westall")
sas <- c("Caulfield","Carnegie","Murrumbeena","Hughesdale","Oakleigh","Huntingdale","Clayton","Westall")


#Variables for while loop
stops <- 1
buses$type <- c(rep(0,length(buses$Resource.Name)))


#Iterate to assign bus type based on stopping pattern
while(stops <= length(buses$origin)) {
  org = buses$origin
  dest = buses$destination
  
  #EXPRESS BUS
  if ((
      (org[stops] == first(express) && dest[stops] == express[2]) &&
      (lead(org,1)[stops] == express[2] && lead(dest,1)[stops] == express[3]) &&
      (lead(org,2)[stops] == express[3] && lead(dest,2)[stops] == express[4]) &&
      (lead(org,3)[stops] == express[4] && lead(dest,3)[stops] == last(express))
     )
      ||
    (
      (org[stops] == last(express) && dest[stops] == express[4]) &&
      (lead(org,1)[stops] == express[4] && lead(dest,1)[stops] == express[3]) &&
      (lead(org,2)[stops] == express[3] && lead(dest,2)[stops] == express[2]) &&
      (lead(org,3)[stops] == express[2] && lead(dest,3)[stops] == first(express))
    ))
    {
      buses$type[stops:(stops+length(express)-1)] <- "Express"
      stops <- stops+length(express)-1
  }
  
  #LTD EXPRESS BUS
  else if ((
    (org[stops] == first(ltd_express) && dest[stops] == ltd_express[2]) &&
    (lead(org,1)[stops] == ltd_express[2] && lead(dest,1)[stops] == ltd_express[3]) &&
    (lead(org,2)[stops] == ltd_express[3] && lead(dest,2)[stops] == ltd_express[4]) &&
    (lead(org,3)[stops] == ltd_express[4] && lead(dest,3)[stops] == ltd_express[5]) &&
    (lead(org,4)[stops] == ltd_express[5] && lead(dest,4)[stops] == ltd_express[6]) &&
    (lead(org,5)[stops] == ltd_express[6] && lead(dest,5)[stops] == last(ltd_express))
  )
  ||
  ( #lasst = 7
    (org[stops] == last(ltd_express) && dest[stops] == ltd_express[6]) &&
    (lead(org,1)[stops] == ltd_express[6] && lead(dest,1)[stops] == ltd_express[5]) &&
    (lead(org,2)[stops] == ltd_express[5] && lead(dest,2)[stops] == ltd_express[4]) &&
    (lead(org,3)[stops] == ltd_express[4] && lead(dest,3)[stops] == ltd_express[3]) &&
    (lead(org,4)[stops] == ltd_express[3] && lead(dest,4)[stops] == ltd_express[2]) &&
    (lead(org,5)[stops] == ltd_express[2] && lead(dest,5)[stops] == first(ltd_express))
  ))
  {
    buses$type[stops:(stops+length(ltd_express)-1)] <- "LTD ltd_express"
    stops <- stops+length(ltd_express)-1
  }
  
  #SAS BUS
  else if ((
    (org[stops] == first(sas) && dest[stops] == sas[2]) &&
    (lead(org,1)[stops] == sas[2] && lead(dest,1)[stops] == sas[3]) &&
    (lead(org,2)[stops] == sas[3] && lead(dest,2)[stops] == sas[4]) &&
    (lead(org,3)[stops] == sas[4] && lead(dest,3)[stops] == sas[5]) &&
    (lead(org,4)[stops] == sas[5] && lead(dest,4)[stops] == sas[6]) &&
    (lead(org,5)[stops] == sas[6] && lead(dest,5)[stops] == sas[7]) &&
    (lead(org,6)[stops] == sas[7] && lead(dest,6)[stops] == last(sas))
  )
  ||
  ( #lasst = 7
    (org[stops] == last(sas) && dest[stops] == sas[7]) &&
    (lead(org,1)[stops] == sas[7] && lead(dest,1)[stops] == sas[6]) &&
    (lead(org,2)[stops] == sas[6] && lead(dest,2)[stops] == sas[5]) &&
    (lead(org,3)[stops] == sas[5] && lead(dest,3)[stops] == sas[4]) &&
    (lead(org,4)[stops] == sas[4] && lead(dest,4)[stops] == sas[3]) &&
    (lead(org,5)[stops] == sas[3] && lead(dest,5)[stops] == sas[2]) &&
    (lead(org,6)[stops] == sas[2] && lead(dest,6)[stops] == first(sas))
  ))
  {
    buses$type[stops:(stops+length(sas)-1)] <- "SAS"
    stops <- stops+length(sas)-1
  }
  else {
    buses$type[stops] <- "None"
    stops <- stops+1
    }
}

#remove non RRP Buses and unnecessary columns
railRep <- buses %>% filter(type != "None") %>% 
  select(project,Resource.Name,Registration,type,origin,destination,departure,arrival,dwellTime)


#variables needed to work out trip ID
startpoints <- c("Caulfield","Westall","Malvern")
new_trips <- railRep$origin %in% startpoints
railRep$tripId <- c(rep(0,length(railRep$Resource.Name)))
id = 0

# logic: if there is no origin (i.e. this is the first data point for the bus) OR 
# if the origin is a startpoint, then the leg is part of a new trip
for(leg in seq(1,length(railRep$origin))) {
  railRep$tripId[leg] = ifelse(new_trips[leg],id+1,id)
  id = ifelse(new_trips[leg],id+1,id)
}

# Determine the peak time
tripDeparture <- railRep %>% filter(!duplicated(tripId)) %>%
  select(tripId,departure) %>% mutate(tripDeparture = hour(departure)) %>%
  select(tripId,tripDeparture)

railRep <- railRep %>% left_join(tripDeparture,"tripId")

railRep <- railRep %>% mutate(peak = sapply(tripDeparture,function(x){
  if(x>6 & x<10){"AM Peak"}
  else if (x>9 & x<16){"Intra Peak"}
  else if (x>15 & x<19){"PM Peak"}
  else {"Off Peak"}
}))

# if the destinationi is Caulfield, Westall or Malvern, the dwelltime = 0, then dwelltime/60
railRep$dwellAdj <- ifelse(railRep$destination %in% startpoints, 0, railRep$dwellTime/60)
                                
# travel_times <- railRep %>% 
#   filter(difftime(arrival,departure, tz = "AEST", units = "mins") < 50,
#          !(tripId %in% c(77,127))) %>% 
#   group_by(tripId, type, peak) %>%
#   summarise(TripTime = sum(difftime(arrival,departure, tz = "AEST", units = "mins")+dwellAdj))

# travel_times <- travel_times %>% group_by(type, peak) %>%
#   summarise(TravelTimes = mean(TripTime))

# write.csv(travel_times,paste0("test2.csv"))


# write.csv(travel_times,paste0("CTD Bus Travel Times - ",date(Sys.time()-days(1)),".csv"))


###TO DO:
#Add dwell times to total JT - Done
#Calculate time for each leg of journey

leg_times <- railRep %>%
  filter(difftime(arrival,departure, tz = "AEST", units = "mins") < 50,
         !(tripId %in% c(77,127))) %>%
  group_by(tripId, origin, destination, peak, type, departure, arrival) %>%
  summarise(TripTime = sum(difftime(arrival,departure, tz = "AEST", units = "mins")+dwellAdj))

# leg_times <- leg_times %>% group_by(origin,destination, peak, type) %>%
#   summarise(TravelTimes = mean(TripTime))

# write.csv(leg_times,paste0("CTD Bus Leg Times - ",date(Sys.time()-days(1)),".csv"))

merge_times <- merge(railRep, leg_times, by = c("tripId","type","peak","departure","arrival","origin","destination"), all.x = T)

merge_times <- cbind("CTD_ID" = sprintf("CTD_ID_%06d", 1:nrow(merge_times)), merge_times)

merge_times$tripId <- as.character(merge_times$tripId)

# Validation 

tripTime_check <- subset(merge_times, merge_times$TripTime >= 30)

merge_times <- merge_times[c( "CTD_ID"
                             ,"tripId"
                             ,"project"
                             ,"Resource.Name"
                             ,"Registration"
                             ,"departure"
                             ,"arrival"
                             ,"type"
                             ,"peak"
                             ,"origin"
                             ,"destination" 
                             ,"dwellAdj"
                             ,"TripTime"
)]

write.csv(merge_times,paste0("CTD Bus Leg Time.csv"))

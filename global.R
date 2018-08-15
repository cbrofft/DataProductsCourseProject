library(dplyr)
library(plyr)
library(geosphere)


shipments <- read.csv("data/shipments.csv", header = TRUE, sep = ",")
site_locations <- read.csv("~/cold_chain/country_lookup_updated.csv")
shipments2 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,"shipmentcarrierdesc"=shipments$shipmentcarrierdesc,"shipmentcontainerdesc"=shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,"shipmentdestinationdesc"=shipments$shipmentdestinationdesc, "shipmentitemproductdesc"=shipments$shipmentitemproductdesc,"route"=shipments$route,"Alarm.Triggered"=shipments$Alarm.Triggered,"Count.of.alarms"=shipments$Count.of.alarms, "Excursion" = shipments$shipmentrootcauseoftempexcursiondesc)
shipments3 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,"shipmentcarrierdesc"=shipments$shipmentcarrierdesc,"shipmentcontainerdesc"=shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,"shipmentdestinationdesc"=shipments$shipmentdestinationdesc, "shipmentitemproductdesc"=shipments$shipmentitemproductdesc, "tempminvalue"=shipments$tempminvalue, "tempmaxvalue"=shipments$tempmaxvalue,"tempmeanvalue"=shipments$tempmeanvalue,"route"=shipments$route,"Alarm.Triggered"=shipments$Alarm.Triggered,"Count.of.alarms"=shipments$Count.of.alarms, "Excursion" = shipments$shipmentrootcauseoftempexcursiondesc)
shipments4 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,"shipmentcarrierdesc"=shipments$shipmentcarrierdesc,"shipmentcontainerdesc"=shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,"shipmentdestinationdesc"=shipments$shipmentdestinationdesc,"route"=shipments$route,"Alarm.Triggered"=shipments$Alarm.Triggered,"Count.of.alarms"=shipments$Count.of.alarms, "Excursion" = shipments$shipmentrootcauseoftempexcursiondesc)

shipments5 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,"shipmentcarrierdesc"=shipments$shipmentcarrierdesc,"shipmentcontainerdesc"=shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,"shipmentdestinationdesc"=shipments$shipmentdestinationdesc, "shipmentitemproductdesc"=shipments$shipmentitemproductdesc,"route"=shipments$route,"Alarm.Triggered"=shipments$Alarm.Triggered,"Count.of.alarms"=shipments$Count.of.alarms, "Excursion"= shipments$shipmentrootcauseoftempexcursiondesc)


uniqshipments <- unique(shipments2)
uniqshipments2 <-unique(shipments3)
uniqshipments3 <- unique(shipments4)


#False Alarms
falsealarms <- data.frame(unique(shipments$shipmentrootcauseoftempexcursiondesc))
falselarms2 <-data.frame(falsealarms[grepl("^False Alarm", falsealarms$unique.shipments.shipmentrootcauseoftempexcursiondesc.),])
falselarms2 <- data.frame("excursion"=falselarms2$falsealarms.grepl...False.Alarm...falsealarms.unique.shipments.shipmentrootcauseoftempexcursiondesc....)

#REMOVE RECORDS WITH A TRUE & FALSE
n<-data.frame(table(uniqshipments3$shipmentid))
n <- as.data.frame(n)
uniqshipments3 <- merge(x= uniqshipments3, y= n, by.x = "shipmentid", by.y = "Var1", all.x=TRUE)
uniqshipments3<-uniqshipments3[!(uniqshipments3$Alarm.Triggered==FALSE & uniqshipments3$Freq==2),]



#REMOVE RECORDS WITH TRUE AND FALSE FOR PRODUCT
n2<-data.frame(table(uniqshipments$shipmentid, uniqshipments$shipmentitemproductdesc))
n2<-n2[!(n2$Freq==0),]
uniqshipments <- merge(x= uniqshipments, y= n2, by.x = c("shipmentid","shipmentitemproductdesc"), by.y = c("Var1","Var2"), all.x=TRUE)
uniqshipments<-uniqshipments[!(uniqshipments$Alarm.Triggered==FALSE & uniqshipments$Freq==2),]

#FALSE ALARMS
uniqshipments5 <- within(uniqshipments, Alarm.Triggered[Alarm.Triggered == TRUE & Excursion %in% falselarms2$excursion] <- 'FALSE')
#REMOVE RECORDS WITH TRUE AND FALSE FOR EXCLUSIONS
# n3<-data.frame(table(uniqshipments5$shipmentid, uniqshipments5$shipmentitemproductdesc))
# n3<-n3[!(n3$Freq==0),]
# uniqshipments5 <- merge(x= uniqshipments5, y= n3, by.x = c("shipmentid","shipmentitemproductdesc"), by.y = c("Var1","Var2"), all.x=TRUE)
# uniqshipments5<-uniqshipments5[!(uniqshipments5$Alarm.Triggered==FALSE & uniqshipments5$Freq==2),]

#ALL NEW
routes9 <- uniqshipments3 %>% group_by(shipmentorigindesc,shipmentdestinationdesc, shipmentcarrierdesc, shipmentcontainerdesc, Alarm.Triggered) %>% dplyr::summarise(cnt = n_distinct(shipmentid))
routes29 <- merge(x= routes9, y= site_locations, by.x = "shipmentorigindesc", by.y = "country_Desc", all.x=TRUE)
routes39 <- merge(x= routes29, y= site_locations, by.x = "shipmentdestinationdesc", by.y = "country_Desc", all.x=TRUE)
routes49 <- data.frame(routes39$shipmentorigindesc, routes39$Latitude.x, routes39$Longitude.x, routes39$shipmentdestinationdesc, routes39$Latitude.y, routes39$Longitude.y)
uniqueroutes2 <- unique(routes49)
cleanroutes3 <-uniqueroutes2[complete.cases(uniqueroutes2),]
cleanroutes4 <- data.frame("route"= paste(as.character(cleanroutes3$routes39.shipmentorigindesc), as.character(cleanroutes3$routes39.shipmentdestinationdesc),sep=" -> "),"origin" = as.character(cleanroutes3$routes39.shipmentorigindesc), "destination" = as.character(cleanroutes3$routes39.shipmentdestinationdesc), "origin_long"=cleanroutes3$routes39.Longitude.x, "origin_lat"=cleanroutes3$routes39.Latitude.x, "dest_long"= cleanroutes3$routes39.Longitude.y, "dest_lat"=cleanroutes3$routes39.Latitude.y)

#TEMPERATURE SET UP
maxtemp <- aggregate(uniqshipments2$tempmaxvalue, by=list(uniqshipments2$shipmentid,uniqshipments2$shipmentitemproductdesc), FUN=max)[]
colnames(maxtemp) <- c("Group.1", "Group.2", "tempmaxvalue")
meantemp <- aggregate(uniqshipments2$tempmeanvalue, by=list(uniqshipments2$shipmentid,uniqshipments2$shipmentitemproductdesc), FUN=mean)[]
colnames(meantemp) <- c("Group.1", "Group.2", "tempmeanvalue")
mintemp <- aggregate(uniqshipments2$tempminvalue, by=list(uniqshipments2$shipmentid,uniqshipments2$shipmentitemproductdesc), FUN=min)[]
colnames(mintemp) <- c("Group.1", "Group.2", "tempminvalue")
uniqshipments4 <- merge(x= uniqshipments, y= meantemp, by.x = c("shipmentid","shipmentitemproductdesc"), by.y = c("Group.1","Group.2"), all.x=TRUE)
uniqshipments4 <- merge(x= uniqshipments4, y= maxtemp, by.x = c("shipmentid","shipmentitemproductdesc"), by.y = c("Group.1","Group.2"), all.x=TRUE)
uniqshipments4 <- merge(x= uniqshipments4, y= mintemp, by.x = c("shipmentid","shipmentitemproductdesc"), by.y = c("Group.1","Group.2"), all.x=TRUE)

#PRODUCT
routes <- uniqshipments %>% group_by(shipmentorigindesc,shipmentdestinationdesc, shipmentcarrierdesc, shipmentcontainerdesc, shipmentitemproductdesc, Alarm.Triggered) %>% dplyr::summarise(cnt = n_distinct(shipmentid))
routes2 <- merge(x= routes, y= site_locations, by.x = "shipmentorigindesc", by.y = "country_Desc", all.x=TRUE)
routes3 <- merge(x= routes2, y= site_locations, by.x = "shipmentdestinationdesc", by.y = "country_Desc", all.x=TRUE)
routes4 <- data.frame(routes3$shipmentorigindesc, routes3$Latitude.x, routes3$Longitude.x, routes3$shipmentdestinationdesc, routes3$Latitude.y, routes3$Longitude.y)
uniqueroutes <- unique(routes4)
cleanroutes <-uniqueroutes[complete.cases(uniqueroutes),]
cleanroutes2 <- data.frame("route"= paste(as.character(cleanroutes$routes3.shipmentorigindesc), as.character(cleanroutes$routes3.shipmentdestinationdesc),sep=" -> "),"origin" = as.character(cleanroutes$routes3.shipmentorigindesc), "destination" = as.character(cleanroutes$routes3.shipmentdestinationdesc), "origin_long"=cleanroutes$routes3.Longitude.x, "origin_lat"=cleanroutes$routes3.Latitude.x, "dest_long"= cleanroutes$routes3.Longitude.y, "dest_lat"=cleanroutes$routes3.Latitude.y)
#PRODUCT
#routemap2 <- data.frame(routes3$shipmentitemproductdesc,routes3$shipmentorigindesc, routes3$Latitude.x, routes3$Longitude.x, routes3$shipmentdestinationdesc, routes3$Latitude.y, routes3$Longitude.y, routes3$cnt)
routes5 <- data.frame(routes3$shipmentitemproductdesc,routes3$shipmentorigindesc, routes3$Latitude.x, routes3$Longitude.x, routes3$shipmentdestinationdesc, routes3$Latitude.y, routes3$Longitude.y)
uniqueproductroutes <- unique(routes5)
cleanproductroutes <-uniqueproductroutes[complete.cases(uniqueproductroutes),]
cleanproductroutes2 <- data.frame("product"= as.character(cleanproductroutes$routes3.shipmentitemproductdesc),"route"= paste(as.character(cleanproductroutes$routes3.shipmentorigindesc), as.character(cleanproductroutes$routes3.shipmentdestinationdesc),sep=" -> "),"origin" = as.character(cleanproductroutes$routes3.shipmentorigindesc), "destination" = as.character(cleanproductroutes$routes3.shipmentdestinationdesc), "origin_long"=cleanproductroutes$routes3.Longitude.x, "origin_lat"=cleanproductroutes$routes3.Latitude.x, "dest_long"= cleanproductroutes$routes3.Longitude.y, "dest_lat"=cleanproductroutes$routes3.Latitude.y)

#CARRIER
routes6 <- data.frame(routes3$shipmentcarrierdesc,routes3$shipmentorigindesc, routes3$Latitude.x, routes3$Longitude.x, routes3$shipmentdestinationdesc, routes3$Latitude.y, routes3$Longitude.y)
uniquecarrierroutes <- unique(routes6)
cleancarrierroutes <-uniquecarrierroutes[complete.cases(uniquecarrierroutes),]
cleancarrierroutes2 <- data.frame("carrier"= as.character(cleancarrierroutes$routes3.shipmentcarrierdesc),"route"= paste(as.character(cleancarrierroutes$routes3.shipmentorigindesc), as.character(cleancarrierroutes$routes3.shipmentdestinationdesc),sep=" -> "),"origin" = as.character(cleancarrierroutes$routes3.shipmentorigindesc), "destination" = as.character(cleancarrierroutes$routes3.shipmentdestinationdesc), "origin_long"=cleancarrierroutes$routes3.Longitude.x, "origin_lat"=cleancarrierroutes$routes3.Latitude.x, "dest_long"= cleancarrierroutes$routes3.Longitude.y, "dest_lat"=cleancarrierroutes$routes3.Latitude.y)

#CONTAINER
routes7 <- data.frame(routes3$shipmentcontainerdesc,routes3$shipmentorigindesc, routes3$Latitude.x, routes3$Longitude.x, routes3$shipmentdestinationdesc, routes3$Latitude.y, routes3$Longitude.y)
uniquecontainerroutes <- unique(routes7)
cleancontainerroutes <-uniquecontainerroutes[complete.cases(uniquecontainerroutes),]
cleancontainerroutes2 <- data.frame("container"= as.character(cleancontainerroutes$routes3.shipmentcontainerdesc),"route"= paste(as.character(cleancontainerroutes$routes3.shipmentorigindesc), as.character(cleancontainerroutes$routes3.shipmentdestinationdesc),sep=" -> "),"origin" = as.character(cleancontainerroutes$routes3.shipmentorigindesc), "destination" = as.character(cleancontainerroutes$routes3.shipmentdestinationdesc), "origin_long"=cleancontainerroutes$routes3.Longitude.x, "origin_lat"=cleancontainerroutes$routes3.Latitude.x, "dest_long"= cleancontainerroutes$routes3.Longitude.y, "dest_lat"=cleancontainerroutes$routes3.Latitude.y)






site_locations2 <- data.frame("id" = site_locations$country_Desc, "lon" = site_locations$Longitude, "lat" = site_locations$Latitude)
site_locations2 <- as.matrix(site_locations2)
#COUNTS BY ALL
alarm_cnt <- aggregate(Count.of.alarms ~ route, data = uniqshipments3, sum)
alarm_cntex <- aggregate(Count.of.alarms ~ route, data = uniqshipments5, sum)
cnt_routes <- count(uniqshipments3$route)
cnt_routesex <- count(uniqshipments5$route)
alarm_rate_Pct1 <- merge(x = cnt_routes, y = alarm_cnt, by.x = "x",by.y="route", all.x = TRUE)
alarm_rate_Pct1$alarm_pct <- alarm_rate_Pct1$Count.of.alarms / alarm_rate_Pct1$freq
alarm_rate_Pct1$bins <- alarm_rate_Pct1$alarm_pct * 100
#exclusion rates
alarm_rate_Pct1ex <- merge(x = cnt_routesex, y = alarm_cntex, by.x = "x",by.y="route", all.x = TRUE)
alarm_rate_Pct1ex$alarm_pcte <- alarm_rate_Pct1ex$Count.of.alarms / alarm_rate_Pct1ex$freq
alarm_rate_Pct1ex$bins <- alarm_rate_Pct1ex$alarm_pct * 100
cleanroutesalarms<-merge(x = cleanroutes4, y = alarm_rate_Pct1, by.x = "route",by.y="x", all.x = TRUE)

cleanroutesalarmsex<-merge(x = cleanroutes4, y = alarm_rate_Pct1ex, by.x = "route",by.y="x", all.x = TRUE)

#TEMP
tmpby_route <- aggregate(cbind(tempmeanvalue,tempminvalue,tempmaxvalue) ~ route, data = uniqshipments4, mean)
temp1_route <- merge(x = cnt_routes, y = tmpby_route, by.x = "x",by.y="route", all.x = TRUE)



#COUNTS BY PRODUCT
alarm_cnt_product <- aggregate(Count.of.alarms ~ route + shipmentitemproductdesc, data = uniqshipments, sum)
cnt_productroutes <- count(cbind(as.character(uniqshipments$route), as.character(uniqshipments$shipmentitemproductdesc)))
alarm_rate_Pct1_prod <- merge(x = cnt_productroutes, y = alarm_cnt_product, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentitemproductdesc"), all.x = TRUE)
alarm_rate_Pct1_prod$alarm_pct <- alarm_rate_Pct1_prod$Count.of.alarms / alarm_rate_Pct1_prod$freq
alarm_rate_Pct1_prod$bins <- alarm_rate_Pct1_prod$alarm_pct * 100
cleanroutesalarms_prod<-merge(x = cleanproductroutes2, y = alarm_rate_Pct1_prod, by.x = cbind("route","product"),by.y=cbind("x.1","x.2"), all.x = TRUE)
#Counts by product exclusion
alarm_cnt_productex <- aggregate(Count.of.alarms ~ route + shipmentitemproductdesc, data = uniqshipments5, sum)
cnt_productroutesex <- count(cbind(as.character(uniqshipments5$route), as.character(uniqshipments5$shipmentitemproductdesc)))
alarm_rate_Pct1_prodex <- merge(x = cnt_productroutesex, y = alarm_cnt_productex, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentitemproductdesc"), all.x = TRUE)
alarm_rate_Pct1_prodex$alarm_pct <- alarm_rate_Pct1_prodex$Count.of.alarms / alarm_rate_Pct1_prodex$freq
alarm_rate_Pct1_prodex$bins <- alarm_rate_Pct1_prodex$alarm_pct * 100
cleanroutesalarms_prodex<-merge(x = cleanproductroutes2, y = alarm_rate_Pct1_prodex, by.x = cbind("route","product"),by.y=cbind("x.1","x.2"), all.x = TRUE)

#TEMPS by PRODUCT
tmpby_product <- aggregate(cbind(tempmeanvalue,tempminvalue,tempmaxvalue) ~ route + shipmentitemproductdesc, data = uniqshipments4, mean)
temp1_prod <- merge(x = cnt_productroutes, y = tmpby_product, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentitemproductdesc"), all.x = TRUE)
temp2_prod <- merge(x = cleanroutesalarms_prod, y=temp1_prod, by.x = cbind("route","product","freq"), by.y = cbind("x.1", "x.2", "freq"), all.x= TRUE)

#Counts by product/temp exclusion
tmpby_productex <- aggregate(cbind(tempmeanvalue,tempminvalue,tempmaxvalue) ~ route + shipmentitemproductdesc, data = uniqshipments4, mean)
temp1_prodex <- merge(x = cnt_productroutesex, y = tmpby_productex, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentitemproductdesc"), all.x = TRUE)
temp2_prodex <- merge(x = cleanroutesalarms_prodex, y=temp1_prodex, by.x = cbind("route","product","freq"), by.y = cbind("x.1", "x.2", "freq"), all.x= TRUE)
#COUNTS BY CARRIER
alarm_cnt_carrier <- aggregate(Count.of.alarms ~ route + shipmentcarrierdesc, data = uniqshipments, sum)
cnt_carrierroutes <- count(cbind(as.character(uniqshipments$route), as.character(uniqshipments$shipmentcarrierdesc)))
alarm_rate_Pct1_car <- merge(x = cnt_carrierroutes, y = alarm_cnt_carrier, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentcarrierdesc"), all.x = TRUE)
alarm_rate_Pct1_car$alarm_pct <- alarm_rate_Pct1_car$Count.of.alarms / alarm_rate_Pct1_car$freq
alarm_rate_Pct1_car$bins <- alarm_rate_Pct1_car$alarm_pct * 100
cleanroutesalarms_car<-merge(x = cleancarrierroutes2, y = alarm_rate_Pct1_car, by.x = cbind("route","carrier"),by.y=cbind("x.1","x.2"), all.x = TRUE)

#Counts by Carrier with exclusion
alarm_cnt_carrierex <- aggregate(Count.of.alarms ~ route + shipmentcarrierdesc, data = uniqshipments5, sum)
cnt_carrierroutesex <- count(cbind(as.character(uniqshipments5$route), as.character(uniqshipments5$shipmentcarrierdesc)))
alarm_rate_Pct1_carex <- merge(x = cnt_carrierroutesex, y = alarm_cnt_carrierex, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentcarrierdesc"), all.x = TRUE)
alarm_rate_Pct1_carex$alarm_pct <- alarm_rate_Pct1_carex$Count.of.alarms / alarm_rate_Pct1_carex$freq
alarm_rate_Pct1_carex$bins <- alarm_rate_Pct1_carex$alarm_pct * 100
cleanroutesalarms_carex<-merge(x = cleancarrierroutes2, y = alarm_rate_Pct1_carex, by.x = cbind("route","carrier"),by.y=cbind("x.1","x.2"), all.x = TRUE)

#COUNTS BY CONTAINER
alarm_cnt_container <- aggregate(Count.of.alarms ~ route + shipmentcontainerdesc, data = uniqshipments, sum)
cnt_containerroutes <- count(cbind(as.character(uniqshipments$route), as.character(uniqshipments$shipmentcontainerdesc)))
alarm_rate_Pct1_con <- merge(x = cnt_containerroutes, y = alarm_cnt_container, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentcontainerdesc"), all.x = TRUE)
alarm_rate_Pct1_con$alarm_pct <- alarm_rate_Pct1_con$Count.of.alarms / alarm_rate_Pct1_con$freq
alarm_rate_Pct1_con$bins <- alarm_rate_Pct1_con$alarm_pct * 100
cleanroutesalarms_con<-merge(x = cleancontainerroutes2, y = alarm_rate_Pct1_con, by.x = cbind("route","container"),by.y=cbind("x.1","x.2"), all.x = TRUE)

#Counts by Container with exclusion
alarm_cnt_containerex <- aggregate(Count.of.alarms ~ route + shipmentcontainerdesc, data = uniqshipments5, sum)
cnt_containerroutesex <- count(cbind(as.character(uniqshipments5$route), as.character(uniqshipments5$shipmentcontainerdesc)))
alarm_rate_Pct1_conex <- merge(x = cnt_containerroutesex, y = alarm_cnt_containerex, by.x = cbind("x.1","x.2"),by.y=cbind("route","shipmentcontainerdesc"), all.x = TRUE)
alarm_rate_Pct1_conex$alarm_pct <- alarm_rate_Pct1_conex$Count.of.alarms / alarm_rate_Pct1_conex$freq
alarm_rate_Pct1_conex$bins <- alarm_rate_Pct1_conex$alarm_pct * 100
cleanroutesalarms_conex<-merge(x = cleancontainerroutes2, y = alarm_rate_Pct1_conex, by.x = cbind("route","container"),by.y=cbind("x.1","x.2"), all.x = TRUE)

#CREATE POLYLINES FOR ALL
outlist2 <-list()
for (i in 1:nrow(cleanroutesalarms)) { 

 
  
  routedesc <- cbind("route"= as.character(cleanroutesalarms$route[i]),"origin" = as.character(cleanroutesalarms$origin[i]), "destination" = as.character(cleanroutesalarms$destination[i]), "count"= cleanroutesalarms$freq[i], "alarms"= cleanroutesalarms$Count.of.alarms[i], "percentage"= cleanroutesalarms$alarm_pct[i], "bin"= cleanroutesalarms$bins[i], gcIntermediate(c(cleanroutesalarms$origin_long[i], cleanroutesalarms$origin_lat[i]), c(cleanroutesalarms$dest_long[i], cleanroutesalarms$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                      sp=FALSE))
 
  
  outlist2[[i]] <- routedesc
  
 
  
  
  }


gg3<-unlist(lapply(outlist2, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)


#CREATE POLYLINES FOR ALL w/ exclusion
outlist21 <-list()
for (i in 1:nrow(cleanroutesalarmsex)) { 
  
  
  
  routedesc2 <- cbind("route"= as.character(cleanroutesalarmsex$route[i]),"origin" = as.character(cleanroutesalarmsex$origin[i]), "destination" = as.character(cleanroutesalarmsex$destination[i]), "count"= cleanroutesalarmsex$freq[i], "alarms"= cleanroutesalarmsex$Count.of.alarms[i], "percentage"= cleanroutesalarmsex$alarm_pct[i], "bin"= cleanroutesalarmsex$bins[i], gcIntermediate(c(cleanroutesalarmsex$origin_long[i], cleanroutesalarmsex$origin_lat[i]), c(cleanroutesalarmsex$dest_long[i], cleanroutesalarmsex$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                sp=FALSE))
  
  
  outlist21[[i]] <- routedesc2
  
  
  
  
}


gg31<-unlist(lapply(outlist21, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)






################PRODUCT POLYLINES####################################



prodlist <- list()
for (i in 1:nrow(cleanproductroutes2)) { 
  
  routeproddesc <- cbind("route"= as.character(cleanroutesalarms_prod$route[i]), "product" = as.character(cleanroutesalarms_prod$product[i]), "origin" = as.character(cleanroutesalarms_prod$origin[i]), "destination" = as.character(cleanroutesalarms_prod$destination[i]), "count"= cleanroutesalarms_prod$freq[i], "alarms"= cleanroutesalarms_prod$Count.of.alarms[i], "percentage"= cleanroutesalarms_prod$alarm_pct[i], "bin"= cleanroutesalarms_prod$bins[i], gcIntermediate(c(cleanroutesalarms_prod$origin_long[i], cleanroutesalarms_prod$origin_lat[i]), c(cleanroutesalarms_prod$dest_long[i], cleanroutesalarms_prod$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                sp=FALSE))
  
  
  prodlist[[i]] <- routeproddesc
  
  
}
ggprod<-unlist(lapply(prodlist, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)


################PRODUCT POLYLINES with exclusion####################################



prodlistex <- list()
for (i in 1:nrow(cleanproductroutes2)) { 
  
  routeproddesc2 <- cbind("route"= as.character(cleanroutesalarms_prodex$route[i]), "product" = as.character(cleanroutesalarms_prodex$product[i]), "origin" = as.character(cleanroutesalarms_prodex$origin[i]), "destination" = as.character(cleanroutesalarms_prodex$destination[i]), "count"= cleanroutesalarms_prodex$freq[i], "alarms"= cleanroutesalarms_prodex$Count.of.alarms[i], "percentage"= cleanroutesalarms_prodex$alarm_pct[i], "bin"= cleanroutesalarms_prodex$bins[i], gcIntermediate(c(cleanroutesalarms_prodex$origin_long[i], cleanroutesalarms_prodex$origin_lat[i]), c(cleanroutesalarms_prodex$dest_long[i], cleanroutesalarms_prodex$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     sp=FALSE))
  
  
  prodlistex[[i]] <- routeproddesc2
  
  
}
ggprodex<-unlist(lapply(prodlistex, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)


################PRODUCT and TEMP POLYLINES####################################



prodtemplist <- list()
for (i in 1:nrow(temp2_prod)) { 
  
  routeprodtempdesc <- cbind("route"= as.character(temp2_prod$route[i]), "product" = as.character(temp2_prod$product[i]), "origin" = as.character(temp2_prod$origin[i]), "destination" = as.character(temp2_prod$destination[i]), "count"= temp2_prod$freq[i], "alarms"= temp2_prod$Count.of.alarms[i], "percentage"= temp2_prod$alarm_pct[i], "bin"= temp2_prod$bins[i], "AvgMeanTemp"=temp2_prod$tempmeanvalue[i], "AvgMaxTemp"=temp2_prod$tempmaxvalue[i], "AvgMinTemp"=temp2_prod$tempminvalue[i], gcIntermediate(c(temp2_prod$origin_long[i], temp2_prod$origin_lat[i]), c(temp2_prod$dest_long[i], temp2_prod$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     sp=FALSE))
  
  
  prodtemplist[[i]] <- routeprodtempdesc
  
  
}
ggprodtemp<-unlist(lapply(prodtemplist, function(x)
if (class(x) == "list") x else list(x)), recursive=FALSE)

################PRODUCT and TEMP POLYLINES with exclusion####################################



prodtemplistex <- list()
for (i in 1:nrow(temp2_prod)) { 
  
  routeprodtempdesc2 <- cbind("route"= as.character(temp2_prodex$route[i]), "product" = as.character(temp2_prodex$product[i]), "origin" = as.character(temp2_prodex$origin[i]), "destination" = as.character(temp2_prodex$destination[i]), "count"= temp2_prodex$freq[i], "alarms"= temp2_prodex$Count.of.alarms[i], "percentage"= temp2_prodex$alarm_pct[i], "bin"= temp2_prodex$bins[i], "AvgMeanTemp"=temp2_prodex$tempmeanvalue[i], "AvgMaxTemp"=temp2_prodex$tempmaxvalue[i], "AvgMinTemp"=temp2_prodex$tempminvalue[i], gcIntermediate(c(temp2_prodex$origin_long[i], temp2_prodex$origin_lat[i]), c(temp2_prodex$dest_long[i], temp2_prodex$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      sp=FALSE))
  
  
  prodtemplistex[[i]] <- routeprodtempdesc2
  
  
}
ggprodtempex<-unlist(lapply(prodtemplistex, function(x)
  if (class(x) == "list") x else list(x)), recursive=FALSE)
 
################CARRIER POLYLINES####################################



carlist <- list()
for (i in 1:nrow(cleancarrierroutes2)) { 
  
  routecardesc <- cbind("route"= as.character(cleanroutesalarms_car$route[i]), "carrier" = as.character(cleanroutesalarms_car$carrier[i]), "origin" = as.character(cleanroutesalarms_car$origin[i]), "destination" = as.character(cleanroutesalarms_car$destination[i]), "count"= cleanroutesalarms_car$freq[i], "alarms"= cleanroutesalarms_car$Count.of.alarms[i], "percentage"= cleanroutesalarms_car$alarm_pct[i], "bin"= cleanroutesalarms_car$bins[i], gcIntermediate(c(cleanroutesalarms_car$origin_long[i], cleanroutesalarms_car$origin_lat[i]), c(cleanroutesalarms_car$dest_long[i], cleanroutesalarms_car$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     sp=FALSE))
  
  
  carlist[[i]] <- routecardesc
  
  
}
ggcar<-unlist(lapply(carlist, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)


################CARRIER POLYLINES with exclusion####################################



carlistex <- list()
for (i in 1:nrow(cleancarrierroutes2)) { 
  
  routecardesc2 <- cbind("route"= as.character(cleanroutesalarms_carex$route[i]), "carrier" = as.character(cleanroutesalarms_carex$carrier[i]), "origin" = as.character(cleanroutesalarms_carex$origin[i]), "destination" = as.character(cleanroutesalarms_carex$destination[i]), "count"= cleanroutesalarms_carex$freq[i], "alarms"= cleanroutesalarms_carex$Count.of.alarms[i], "percentage"= cleanroutesalarms_carex$alarm_pct[i], "bin"= cleanroutesalarms_carex$bins[i], gcIntermediate(c(cleanroutesalarms_carex$origin_long[i], cleanroutesalarms_carex$origin_lat[i]), c(cleanroutesalarms_carex$dest_long[i], cleanroutesalarms_carex$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            sp=FALSE))
  
  
  carlistex[[i]] <- routecardesc2
  
  
}
ggcarex<-unlist(lapply(carlistex, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)

################CONTAINER POLYLINES####################################



conlist <- list()
for (i in 1:nrow(cleancontainerroutes2)) { 
  
  routecondesc <- cbind("route"= as.character(cleanroutesalarms_con$route[i]), "container" = as.character(cleanroutesalarms_con$container[i]), "origin" = as.character(cleanroutesalarms_con$origin[i]), "destination" = as.character(cleanroutesalarms_con$destination[i]), "count"= cleanroutesalarms_con$freq[i], "alarms"= cleanroutesalarms_con$Count.of.alarms[i], "percentage"= cleanroutesalarms_con$alarm_pct[i], "bin"= cleanroutesalarms_con$bins[i], gcIntermediate(c(cleanroutesalarms_con$origin_long[i], cleanroutesalarms_con$origin_lat[i]), c(cleanroutesalarms_con$dest_long[i], cleanroutesalarms_con$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            sp=FALSE))
  
  
  conlist[[i]] <- routecondesc
  
  
}
ggcon<-unlist(lapply(conlist, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)


################CONTAINER POLYLINES with exclusion####################################



conlistex <- list()
for (i in 1:nrow(cleancontainerroutes2)) { 
  
  routecondesc2 <- cbind("route"= as.character(cleanroutesalarms_conex$route[i]), "container" = as.character(cleanroutesalarms_conex$container[i]), "origin" = as.character(cleanroutesalarms_conex$origin[i]), "destination" = as.character(cleanroutesalarms_conex$destination[i]), "count"= cleanroutesalarms_conex$freq[i], "alarms"= cleanroutesalarms_conex$Count.of.alarms[i], "percentage"= cleanroutesalarms_conex$alarm_pct[i], "bin"= cleanroutesalarms_conex$bins[i], gcIntermediate(c(cleanroutesalarms_conex$origin_long[i], cleanroutesalarms_conex$origin_lat[i]), c(cleanroutesalarms_conex$dest_long[i], cleanroutesalarms_conex$dest_lat[i]), n=20, breakAtDateLine = TRUE, addStartEnd=TRUE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                sp=FALSE))
  
  
  conlistex[[i]] <- routecondesc2
  
  
}
ggconex<-unlist(lapply(conlistex, function(x) 
  if (class(x) == "list") x else list(x)), recursive=FALSE)





cleantable <- uniqshipments4 %>%
  select(
    ShipmentID = shipmentid,
    ShipDate = shipmentshippeddate,
    Carrier = shipmentcarrierdesc,
    ContainerType = shipmentcontainerdesc,
    Origin = shipmentorigindesc,
    Destination = shipmentdestinationdesc,
    Product = shipmentitemproductdesc,
    TempMinValue = tempminvalue,
    TempMaxValue = tempmaxvalue,
    TempMeanValue = tempmeanvalue,
    Route = route,
    Alarm = Alarm.Triggered,
    CountofAlarms = Count.of.alarms
  )


# alarm_counts <- nrow(uniqshipments[uniqshipments$shipmentitemproductdesc %in% x & uniqshipments$shipmentorigindesc %in% y & uniqshipments$shipmentdestinationdesc %in% z & uniqshipments$Alarm.Triggered == TRUE,])
# percentage_alarm <- (alarm_counts/ship_counts)*100
# percentage_alarm <- round(percentage_alarm, digits = 2)


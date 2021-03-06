Shipping Department Strategy
========================================================
author: Chris Brofft
date: 8/15/2018
autosize: true

Executive Summary
========================================================

At the ABC Company we have been looking for ways to monitor our shipping routes and ensuring that our product deliveries are reaching our customers without encountering temperture excursions.  Since most of our product line is tempature sensitive, a solution has been developed to monitor our shipping routes, carriers, containers and product types.  Included in our visual map is
- Map of alarm percentages (temperature excursions)
- Map of Avg Min Temperature
- Map of Avg Max Temperature
- Map of Avg Mean Temperature

Features
========================================================
- Filter by temperature ranges
- Filter by product
- Filter by origin location
- Filter by destination location
- Filter by container type
- Filter by carrier type
- Map of Avg Max Temperature
- Map of Avg Mean Temperature

Slide With Code 
========================================================


```r
library(dplyr)
library(plyr)
library(geosphere)


shipments <- read.csv("data/shipments.csv", header = TRUE, sep = ",")
site_locations <- read.csv("~/cold_chain/country_lookup_updated.csv")
shipments2 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,
                         "shipmentcarrierdesc"=shipments$shipmentcarrierdesc,"shipmentcontainerdesc"
                         =shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,"shipmentdestinationdesc"=
                           shipments$shipmentdestinationdesc, "shipmentitemproductdesc"=shipments$shipmentitemproductdesc,
                         "route"=shipments$route,"Alarm.Triggered"=shipments$Alarm.Triggered,
                         "Count.of.alarms"=shipments$Count.of.alarms, "Excursion" = shipments$shipmentrootcauseoftempexcursiondesc)
shipments3 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,
                         "shipmentcarrierdesc"=shipments$shipmentcarrierdesc,
                         "shipmentcontainerdesc"=shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,
                         "shipmentdestinationdesc"=shipments$shipmentdestinationdesc, "shipmentitemproductdesc"=shipments$shipmentitemproductdesc, "tempminvalue"
                         =shipments$tempminvalue, "tempmaxvalue"=shipments$tempmaxvalue,
                         "tempmeanvalue"=shipments$tempmeanvalue,
                         "route"=shipments$route,"Alarm.Triggered"=shipments$Alarm.Triggered,
                         "Count.of.alarms"=shipments$Count.of.alarms, "Excursion" = shipments$shipmentrootcauseoftempexcursiondesc)
shipments4 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,
                         "shipmentcarrierdesc"=shipments$shipmentcarrierdesc,
                         "shipmentcontainerdesc"=shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,
                         "shipmentdestinationdesc"=shipments$shipmentdestinationdesc,
                         "route"=shipments$route,"Alarm.Triggered"=shipments$Alarm.Triggered,
                         "Count.of.alarms"=shipments$Count.of.alarms, "Excursion" = shipments$shipmentrootcauseoftempexcursiondesc)
```
Slide With Code 
========================================================

```r
shipments5 <- data.frame("shipmentid" = shipments$shipmentid,"shipmentshippeddate"=shipments$shipmentshippeddate,
                         "shipmentcarrierdesc"=shipments$shipmentcarrierdesc,
                         "shipmentcontainerdesc"=shipments$shipmentcontainerdesc,
                         "shipmentorigindesc"=shipments$shipmentorigindesc,
                         "shipmentdestinationdesc"=shipments$shipmentdestinationdesc, "shipmentitemproductdesc"=shipments$shipmentitemproductdesc,"route"=shipments$route,
                         "Alarm.Triggered"=shipments$Alarm.Triggered,
                         "Count.of.alarms"=shipments$Count.of.alarms, "Excursion"= shipments$shipmentrootcauseoftempexcursiondesc)


uniqshipments <- unique(shipments2)
uniqshipments2 <-unique(shipments3)
uniqshipments3 <- unique(shipments4)
```
Landing Map
========================================================

![plot of chunk unnamed-chunk-3](ShippingDepartmentStrategy-figure/unnamed-chunk-3-1.png)

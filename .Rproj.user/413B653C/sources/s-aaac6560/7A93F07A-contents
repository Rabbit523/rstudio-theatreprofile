allCompanies <- function(admin, password, host, tickettable, venuetable, eventtable, productiontable, companytable, dbname)
{
  library(RMySQL)
  con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
  companies <- dbReadTable(conn = con, name = companytable)
  companies[3] <- list(NULL)
  companies <- companies[order(companies$companyName, decreasing = FALSE, na.last = TRUE),]
  data.table::setDT(companies)
  companies <- data.frame(companies)
  companies$index <- as.numeric(row.names(companies)) - 1
  dbDisconnect(conn = con)
  print(companies)
}

userValidCheck <- function(admin,password,host,table,dbname,userID,ipAddress,activeKey)
{
  library(RMySQL)
  userID <- as.numeric(userID)
  time <- Sys.time()
  con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
  statement <- paste("SELECT * FROM ",table," WHERE activeKey = '",activeKey,"';", sep = "")
  logDetails <- dbGetQuery(conn = con, statement = statement)
  numLogDetails <- nrow(logDetails)
  if (numLogDetails == 0)
  {
    numDBRows <- nrow(dbReadTable(conn = con,name = table))
    numDBRows = numDBRows + 1
    statement = paste("INSERT INTO ",table," (row_names, userID, ipAddress, activeKey, time) VALUES (",numDBRows,",",userID,",","'",ipAddress,"',","'",activeKey,"',","'",time,"');",sep = "")
    dbGetQuery(conn = con, statement = statement)
    userValid <- "TRUE"
  } else if (numLogDetails == 1)
  {
    if (logDetails$ipAddress[1] == ipAddress & logDetails$userID[1] == userID)
    {
      userValid <- "TRUE"
    } else
      userValid <- "FALSE"
  }
  dbDisconnect(conn = con)
  print(userValid)
}

companiesLinkedUserID <- function(admin,password,host,companytable,ownershiptable,tpcompanytable,dbname,userID)
{
  library(RMySQL)
  userID <- as.numeric(userID)
  # Test if SuperUser, currently UserID #5
  if (userID == 5 | userID == 974 | userID == 107 | userID == 3104)
  {
    con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
    companies <- dbReadTable(conn = con, name = companytable)
    companies[3] <- list(NULL)
    companies <- companies[order(companies$companyName, decreasing = FALSE, na.last = TRUE),]
    data.table::setDT(companies)
    companies <- data.frame(companies)
    companies$index <- as.numeric(row.names(companies)) - 1
  } else if (userID != 5 | userID != 974 | userID != 107 | userID != 3104)
  {
    con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
    statement <- paste("SELECT * FROM ",ownershiptable," WHERE userID = ",userID,";",sep = "")
    companyOwnership <- dbGetQuery(conn = con, statement = statement)
    numCompanies <- nrow(companyOwnership)
    companyIDList <- ""
    for (i in 1:numCompanies)
    {
      companyIDList <- paste(companyIDList,",",companyOwnership$companyID[i],sep = "")
    }
    companyIDList <- substr(companyIDList,2,nchar(companyIDList))

    #Grab Company Names and tttcCompanyIDs from tbl_tttcompany table
    statement <- paste("SELECT id, companyName FROM ",companytable," WHERE tpCompanyID IN (",companyIDList,");", sep = "")
    companies <- dbGetQuery(conn = con, statement = statement)
    data.table::setDT(companies)
    companies <- data.frame(companies)
    companies$index <- as.numeric(row.names(companies)) - 1
  }
  dbDisconnect(conn = con)
  print(companies)

}

companyShowsPerformance <- function(admin,password,host,tickettable,venuetable,eventtable,productiontable,dbname,organizationID,startDate,endDate)
{
  library(RMySQL)
  con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
  statement <- paste("SELECT* FROM ",venuetable," WHERE tttcCompanyID = ",organizationID,";", sep = "")

  # Grab all venues tied to Company ID
  tttcVenueID = dbGetQuery(conn = con, statement = statement)
  numTTTCVenues <- nrow(tttcVenueID)
  venueIDList <- ""
  for (i in 1:numTTTCVenues)
  {
    venueIDList <- paste(venueIDList,",",tttcVenueID$id[i],sep = "")
  }
  venueIDList <- substr(venueIDList,2,nchar(venueIDList))

  # Grab all productions tied to Venue ID
  statement <- paste("SELECT * FROM ",productiontable," WHERE tttcVenueID IN (",venueIDList,");", sep = "")
  tttcProductionID <- dbGetQuery(conn = con, statement = statement)

  #Grab all events tied to Production ID
  numTTTCProductions <- nrow(tttcProductionID)
  productionIDList <- ""
  for (i in 1:numTTTCProductions)
  {
    productionIDList <- paste(productionIDList,",",tttcProductionID$id[i],sep = "")
  }
  productionIDList <- substr(productionIDList,2,nchar(productionIDList))
  statement <- paste("SELECT * FROM ",eventtable," WHERE tttcProductionID IN (",productionIDList,");", sep = "")
  tttcEventID <- dbGetQuery(conn = con, statement = statement)

  #Filter out date range to only include productions from that time period
  startDate <- substr(startDate,1,10)
  endDate <- substr(endDate,1,10)
  tttcEventID <- tttcEventID[which(as.Date(tttcEventID$eventDate) >= as.Date(startDate) & as.Date(tttcEventID$eventDate) <= as.Date(endDate)),]
  uniqueProds <- unique(tttcEventID$tttcProductionID)
  numUniqueProds <- length(uniqueProds)
  filteredTTTCProductionID <- data.frame()
  for (i in 1:numUniqueProds)
  {
    singleTTTCProduction <- tttcProductionID[which(tttcProductionID$id == uniqueProds[i]),]
    filteredTTTCProductionID <- rbind(filteredTTTCProductionID,singleTTTCProduction)
  }
  tttcProductionID <- filteredTTTCProductionID
  numTTTCProductions <- nrow(tttcProductionID)

  #Grab tickets tied to Event IDs
  numTTTCEvents <- nrow(tttcEventID)
  eventIDList <- ""
  for (i in 1:numTTTCEvents)
  {
    eventIDList <- paste(eventIDList,",",tttcEventID$id[i],sep = "")
  }
  eventIDList <- substr(eventIDList,2,nchar(eventIDList))
  statement <- paste("SELECT * FROM ",tickettable," WHERE tttcEventID IN (",eventIDList,");", sep = "")
  ticketsCompany <- dbGetQuery(conn = con, statement = statement)

  # Tickets By Show
  totalTicketsSold <- sum(ticketsCompany$quantity)
  ticketsByShow <- data.frame(tttcProductionID$id,tttcProductionID$title,netSales = 0,netAttendedTickets = 0,netPurchasedTickets = 0,totalCompedProdTkts = 0,totalRefundedProdTkts = 0,pctCapacity = 0,numEvents = 0)
  for (i in 1:numTTTCProductions)
  {
    eventsTied2Prod <- tttcEventID$id[which(tttcEventID$tttcProductionID == tttcProductionID$id[i])]
    numEventsTied2Prod <- length(eventsTied2Prod)
    totalProductionTickets <- 0
    netPurchasedTickets <- 0
    totalCompedProdTkts <- 0
    totalRefundedProdTkts <- 0
    netSalesProduction <- 0
    k = 1
    pctCapacityEvent <- 0
    for (j in 1:numEventsTied2Prod)
    {
      ticketsSoldAtEvent <- sum(ticketsCompany$quantity[which(ticketsCompany$tttcEventID == eventsTied2Prod[j])])
      totalProductionTickets <- totalProductionTickets + ticketsSoldAtEvent
      ticketsCompedAtEvent <-  sum(ticketsCompany$quantity[which(ticketsCompany$tttcEventID == eventsTied2Prod[j] & ticketsCompany$boxOfficeComp == 1)])
      totalCompedProdTkts <- totalCompedProdTkts + ticketsCompedAtEvent
      ticketsRefundedAtEvent <- sum(ticketsCompany$quantity[which(ticketsCompany$tttcEventID == eventsTied2Prod[j] & ticketsCompany$refundSale == 1)])
      totalRefundedProdTkts <- totalRefundedProdTkts + ticketsRefundedAtEvent
      ticketSalesAtEvent <-  sum(ticketsCompany$netTotal[which(ticketsCompany$tttcEventID == eventsTied2Prod[j] & ticketsCompany$refundSale == 0 & ticketsCompany$boxOfficeComp == 0)])
      netSalesProduction <- netSalesProduction + ticketSalesAtEvent

      #Capacity grabbed and calculated from each event
      productionIDFromEvent <- tttcEventID$tttcProductionID[which(tttcEventID$id == eventsTied2Prod[j])]
      venueIDFromProduction <- tttcProductionID$tttcVenueID[which(tttcProductionID$id == productionIDFromEvent)]
      capacityVenue <- tttcVenueID$capacity[which(tttcVenueID$id == venueIDFromProduction)]
      pctCapacityEvent[k] <- round(((ticketsSoldAtEvent - ticketsRefundedAtEvent) / capacityVenue) * 100, 2)
      k = k + 1
    }
    netPurchasedTickets <- totalProductionTickets - totalCompedProdTkts - totalRefundedProdTkts
    netAttendedTickets <- totalProductionTickets - totalRefundedProdTkts

    #Add all ticket sales for production
    ticketsByShow$netSales[i] = netSalesProduction
    ticketsByShow$netAttendedTickets[i] = netAttendedTickets
    ticketsByShow$netPurchasedTickets[i] = netPurchasedTickets
    ticketsByShow$totalCompedProdTkts[i] = totalCompedProdTkts
    ticketsByShow$totalRefundedProdTkts[i] = totalRefundedProdTkts
    ticketsByShow$pctCapacity[i] = round(mean(pctCapacityEvent),2)
    ticketsByShow$numEvents[i] = numEventsTied2Prod
  }
  ticketsByShow$pctTotalPurchasedTkts <- round((ticketsByShow$netPurchasedTickets / sum(ticketsByShow$netPurchasedTickets)) * 100,2)
  ticketsByShow <- ticketsByShow[order(ticketsByShow$netSales,ticketsByShow$pctTotalPurchasedTkts,decreasing = TRUE,na.last = TRUE),]
  ticketsByShow$pctPaidAttendees <- round((ticketsByShow$netPurchasedTickets / ticketsByShow$netAttendedTickets) * 100, 2)
  ticketsByShow$avgTktPrice <- round((ticketsByShow$netSales / (ticketsByShow$netPurchasedTickets)),2)
  dbDisconnect(conn = con)
  data.table::setDT(ticketsByShow)
  ticketsByShow <- data.frame(ticketsByShow)
  ticketsByShow$index <- as.numeric(row.names(ticketsByShow)) - 1

  #Calculate Totals for All Shows
  netSales <- sum(ticketsByShow$netSales)
  netAttendedTickets <- sum(ticketsByShow$netAttendedTickets)
  netPurchasedTickets <- sum(ticketsByShow$netPurchasedTickets)
  totalCompedProdTkts <- sum(ticketsByShow$totalCompedProdTkts)
  totalRefundedProdTkts <- sum(ticketsByShow$totalRefundedProdTkts)
  pctCapacity <- round(mean(ticketsByShow$pctCapacity),2)
  numEvents <- sum(ticketsByShow$numEvents)
  pctTotalPurchasedTkts <- sum(ticketsByShow$pctTotalPurchasedTkts)
  pctPaidAttendees <- round(mean(ticketsByShow$pctPaidAttendees),2)
  avgTktPrice <- round(mean(ticketsByShow$avgTktPrice),2)
  totalsTBS <- data.frame(tttcProductionID.id = 0, tttcProductionID.title = "All Shows",netSales,netAttendedTickets,netPurchasedTickets,totalCompedProdTkts,totalRefundedProdTkts,pctCapacity,numEvents,pctTotalPurchasedTkts,pctPaidAttendees,avgTktPrice,index = (nrow(ticketsByShow)))
  ticketsByShow <- rbind(ticketsByShow,totalsTBS)
  print(ticketsByShow)
}

showEventDetails <- function(admin,password,host,tickettable,eventtable,dbname,productionID,startDate,endDate)
{
  library(RMySQL)
  library(lubridate)
  con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
  statement <- paste("SELECT * FROM ",eventtable," WHERE tttcProductionID = ",productionID,";",sep = "")
  tttcEventID <- dbGetQuery(conn = con, statement = statement)

  #Filter out date range to only include productions from that time period
  startDate <- substr(startDate,1,10)
  endDate <- substr(endDate,1,10)
  tttcEventID <- tttcEventID[which(as.Date(tttcEventID$eventDate) >= as.Date(startDate) & as.Date(tttcEventID$eventDate) <= as.Date(endDate)),]

  numEvents <- nrow(tttcEventID)
  tttcEventID$numAttended <- 0
  tttcEventID$wdayNum <- wday(as.Date(tttcEventID$eventDate))
  tttcEventID$weekday <- ""
  tttcEventID$time <- substr(tttcEventID$eventDate,nchar(tttcEventID$eventDate)-7,nchar(tttcEventID$eventDate)-3)
  for (i in 1:numEvents)
  {
    statement <- paste("SELECT * FROM ",tickettable," WHERE tttcEventID = ",tttcEventID$id[i],";", sep = "")
    eventTickets <- dbGetQuery(conn = con, statement = statement)
    eventTickets <- eventTickets[which(eventTickets$refundSale == 0),]
    eventTickets <- eventTickets[which(eventTickets$refundSale == 0),]
    tttcEventID$numAttended[i] <- sum(eventTickets$quantity)
    if (tttcEventID$wdayNum[i] == 1)
    {
      tttcEventID$weekday[i] <- 'Sunday'
    } else if (tttcEventID$wdayNum[i] == 5)
    {
      tttcEventID$weekday[i] <- 'Thursday'
    } else if (tttcEventID$wdayNum[i] == 6)
    {
      tttcEventID$weekday[i] <- 'Friday'
    } else if (tttcEventID$wdayNum[i] == 7)
    {
      tttcEventID$weekday[i] <- 'Saturday'
    } else if (tttcEventID$wdayNum[i] == 2)
    {
      tttcEventID$weekday[i] <- 'Monday'
    } else if (tttcEventID$wdayNum[i] == 3)
    {
      tttcEventID$weekday[i] <- 'Tuesday'
    } else if (tttcEventID$wdayNum[i] == 4)
    {
      tttcEventID$weekday[i] <- 'Wednesday'
    }
  }

  #Calculate Avg Attendance by Day
  avgAttdnByDay <- data.frame()
  days <- unique(tttcEventID$weekday)
  numDays <- length(days)
  for (i in 1:numDays)
  {
    avgAttendance <- round(mean(tttcEventID$numAttended[which(tttcEventID$weekday == days[i])]),2)
    avgAttdnByDay <- rbind(avgAttdnByDay,data.frame(days[i],avgAttendance))
  }

  #Calculate Avg Attendance by Time
  avgAttdnByTime <- data.frame()
  times <- unique(tttcEventID$time)
  numTimes <- length(times)
  for (i in 1:numTimes)
  {
    avgAttendance <- round(mean(tttcEventID$numAttended[which(tttcEventID$time == times[i])]),2)
    avgAttdnByTime <- rbind(avgAttdnByTime,data.frame(times[i],avgAttendance))
  }
  tttcEventID$eventDate <- substr(tttcEventID$eventDate,1,10)
  tttcEventID$eventDate <- format(as.Date(tttcEventID$eventDate), format = "%b-%d-%y")
  for (i in 1:numEvents)
  {
    if (as.numeric(substr(tttcEventID$time[i],1,2)) <= 13)
    {
      tttcEventID$eventDate[i] <- paste(tttcEventID$eventDate[i]," M",sep = "")
    } else if (as.numeric(substr(tttcEventID$time[i],1,2)) <= 17)
    {
      tttcEventID$eventDate[i] <- paste(tttcEventID$eventDate[i]," A",sep = "")
    } else if (as.numeric(substr(tttcEventID$time[i],1,2)) <= 20)
    {
      tttcEventID$eventDate[i] <- paste(tttcEventID$eventDate[i]," E",sep = "")
    } else
      tttcEventID$eventDate[i] <- paste(tttcEventID$eventDate[i]," N",sep = "")
  }
  avgAttdnByDay <- avgAttdnByDay[order(avgAttdnByDay$avgAttendance, decreasing = TRUE, na.last = TRUE),]
  avgAttdnByTime <- avgAttdnByTime[order(avgAttdnByTime$avgAttendance, decreasing = TRUE, na.last = TRUE),]
  showDetails <- list(tttcEventID,avgAttdnByDay,avgAttdnByTime)
  dbDisconnect(conn = con)
  print(showDetails)
}

showEventGeography <- function(admin,password,host,otheradmin,otherpassword,otherhost,tickettable,usertable,eventtable,dbname,geotable,tatheatredb,productionID,startDate,endDate)
{
  library(RMySQL)
  library(zipcode)
  library(data.table)
  con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
  statement <- paste("SELECT * FROM ",eventtable," WHERE tttcProductionID = ",productionID,";",sep = "")
  tttcEventID <- dbGetQuery(conn = con, statement = statement)

  #Filter out date range to only include productions from that time period
  startDate <- substr(startDate,1,10)
  endDate <- substr(endDate,1,10)
  tttcEventID <- tttcEventID[which(as.Date(tttcEventID$eventDate) >= as.Date(startDate) & as.Date(tttcEventID$eventDate) <= as.Date(endDate)),]

  numEvents <- nrow(tttcEventID)
  queryEventIDs <- ""
  for (i in 1:numEvents)
  {
    queryEventIDs <- paste(queryEventIDs,",",tttcEventID$id[i],sep = "")
  }
  queryEventIDs <- substr(queryEventIDs,2,nchar(queryEventIDs))
  statement <- paste("SELECT * FROM ",tickettable," WHERE tttcEventID IN (",queryEventIDs,");", sep = "")
  eventTickets <- dbGetQuery(conn = con, statement = statement)
  eventTickets <- eventTickets[which(eventTickets$refundSale == 0),]
  eventTickets <- eventTickets[which(eventTickets$refundSale == 0),]
  numTickets <- nrow(eventTickets)
  queryUserIDs <- ""
  for (i in 1:numTickets)
  {
    queryUserIDs <- paste(queryUserIDs,",",eventTickets$tttcUserID[i],sep = "")
  }
  queryUserIDs <- substr(queryUserIDs,2,nchar(queryUserIDs))
  statement <- paste("SELECT * FROM ",usertable," WHERE id IN (",queryUserIDs,");", sep = "")
  usersFromEvents <- dbGetQuery(conn = con, statement = statement)
  zipCodesEvents <- unique(usersFromEvents$zip)
  numZips <- length(zipCodesEvents)
  ticketsByZip <- data.frame(zipCodesEvents)
  ticketsByZip$attendance <- 0
  for (i in 1:numZips)
  {
    totalTickets <- 0
    userIDZipMatch <- usersFromEvents$id[which(usersFromEvents$zip == zipCodesEvents[i])]
    numUserIDZipMatch <- length(userIDZipMatch)
    for (j in 1:numUserIDZipMatch)
    {
      singleCount <- length(which(userIDZipMatch[j] == eventTickets$tttcUserID))
      totalTickets <- totalTickets + singleCount
    }
    ticketsByZip$attendance[i] = totalTickets
  }
  ticketsByZip <- ticketsByZip[order(ticketsByZip$attendance, decreasing = TRUE, na.last = TRUE),]
  ticketsByZip$zipCodesEvents <- as.character(ticketsByZip$zipCodesEvents)
  ticketsByZip$zipCodesEvents[which(ticketsByZip$zipCodesEvents == "")] = "Box Office"
  ticketsByZip <- ticketsByZip[1:10,]
  dbDisconnect(conn = con)
  con <- dbConnect(MySQL(),user=otheradmin, password=otherpassword, host=otherhost, dbname=tatheatredb)
  zipList <- dbReadTable(conn = con, name = geotable)
  dbDisconnect(conn = con)
  ticketsByZip$city <- ""
  for (i in 1:10)
  {
    if(ticketsByZip$zipCodesEvents[i] != "Box Office")
    {
      ticketsByZip$city[i] <- zipList$V2[which(substr(ticketsByZip$zipCodesEvents[i],1,3) == zipList$V3)]
    }
  }
  print(ticketsByZip)
}

showPeerComparison <- function(admin,password,host,tickettable,eventtable,venuetable,tpshowtable,showtable,productiontable,dbname,productionID)
{
  library(RMySQL)
  con <- dbConnect(MySQL(),user=admin, password=password, host=host, dbname=dbname)
  statement <- paste("SELECT tttcShowID FROM ",productiontable," WHERE id = ",productionID,";",sep = "")
  tttcShowID <- as.numeric(dbGetQuery(conn = con, statement = statement))
  statement <- paste("SELECT tpShowID, title FROM ",showtable," WHERE id = ",tttcShowID,";",sep = "")
  tpShowID <- dbGetQuery(conn = con, statement = statement)
  if (tpShowID$tpShowID[1] != 0)
  {
    statement <- paste("SELECT id FROM ",showtable," WHERE tpShowID = ",tpShowID$tpShowID,";",sep = "")
    peerShowIDs <- dbGetQuery(conn = con, statement = statement)
    peerShowIDs <- peerShowIDs[which(peerShowIDs$id != tttcShowID),]
    numPeerShows <- length(peerShowIDs)
    queryPeerShowIDs <- ""
    is.integer0 <- function(x)
    {
      is.integer(x) && !length(x)
    }
    if (!(is.integer0(peerShowIDs)))
    {
      for (i in 1:numPeerShows)
      {
        queryPeerShowIDs <- paste(queryPeerShowIDs,",",peerShowIDs[i],sep = "")
      }
      queryPeerShowIDs <- substr(queryPeerShowIDs,2,nchar(queryPeerShowIDs))
      statement <- paste("SELECT id, tttcVenueID FROM ",productiontable," WHERE tttcshowID IN (",queryPeerShowIDs,");",sep = "")
      peerProductionIDs <- dbGetQuery(conn = con, statement = statement)
      numPeerProductions <- nrow(peerProductionIDs)
      queryPeerProductionIDs <- ""
      queryPeerVenueIDs <- ""
      for (i in 1:numPeerProductions)
      {
        queryPeerProductionIDs <- paste(queryPeerProductionIDs,",",peerProductionIDs$id[i],sep = "")
        queryPeerVenueIDs <- paste(queryPeerVenueIDs,",",peerProductionIDs$tttcVenueID[i],sep = "")
      }
      queryPeerProductionIDs <- substr(queryPeerProductionIDs,2,nchar(queryPeerProductionIDs))
      queryPeerVenueIDs <- substr(queryPeerVenueIDs,2,nchar(queryPeerVenueIDs))
      statement <- paste("SELECT * FROM ",eventtable," WHERE tttcProductionID IN (",queryPeerProductionIDs,");",sep = "")
      tttcEventID <- dbGetQuery(conn = con, statement = statement)

      #Grab tickets tied to Event IDs
      numTTTCEvents <- nrow(tttcEventID)
      eventIDList <- ""
      for (i in 1:numTTTCEvents)
      {
        eventIDList <- paste(eventIDList,",",tttcEventID$id[i],sep = "")
      }
      eventIDList <- substr(eventIDList,2,nchar(eventIDList))
      statement <- paste("SELECT * FROM ",tickettable," WHERE tttcEventID IN (",eventIDList,");", sep = "")
      ticketsCompany <- dbGetQuery(conn = con, statement = statement)

      # Peer Ticket Data
      totalTicketsSold <- sum(ticketsCompany$quantity)
      ticketsByShow <- data.frame(tpShowID$tpShowID,tpShowID$title,netSales = 0,netAttendedTickets = 0,netPurchasedTickets = 0,totalCompedProdTkts = 0,totalRefundedProdTkts = 0, pctCapacity = 0, numEvents = 0)
      ticketsByShow$netSales[1] <-  sum(ticketsCompany$netTotal[which(ticketsCompany$refundSale == 0 & ticketsCompany$boxOfficeComp == 0)])
      ticketsByShow$netAttendedTickets[1] <- sum(ticketsCompany$quantity[which(ticketsCompany$refundSale == 0)])
      ticketsByShow$totalCompedProdTkts[1] <- sum(ticketsCompany$quantity[which(ticketsCompany$refundSale == 0 & ticketsCompany$boxOfficeComp == 1)])
      ticketsByShow$netPurchasedTickets[1] <- sum(ticketsCompany$quantity[which(ticketsCompany$refundSale == 0 & ticketsCompany$boxOfficeComp == 0)])
      ticketsByShow$totalRefundedProdTkts[1] <- sum(ticketsCompany$quantity[which(ticketsCompany$refundSale == 1 & ticketsCompany$boxOfficeComp == 0)])
      ticketsByShow$numEvents[1] <- length(unique(ticketsCompany$tttcEventID))
      ticketsByShow$avgTktPrice[1] <- round((ticketsByShow$netSales[1] / ticketsByShow$netPurchasedTickets[1]), 2)
      ticketsByShow$pctPaidAttendees[1] <- round((ticketsByShow$netPurchasedTickets[1] / ticketsByShow$netAttendedTickets[1])*100, 2)

      #Grab capacity from Venues table
      statement <- paste("SELECT capacity FROM ",venuetable," WHERE id IN (",queryPeerVenueIDs,");",sep = "")
      venueCapacities <- dbGetQuery(conn = con, statement = statement)
      avgCapacity <- mean(venueCapacities$capacity)
      avgTktsPerShow <- ticketsByShow$netAttendedTickets[1] / ticketsByShow$numEvents[1]
      ticketsByShow$pctCapacity[1] <- round((avgTktsPerShow / avgCapacity) * 100, 2)
      ticketsByShow$totalEvents <- 0
      ticketsByShow$totalEvents[1] <- round(ticketsByShow$numEvents[1] / nrow(peerProductionIDs), 2)
    } else if (is.integer0(peerShowIDs))
    {
      ticketsByShow <- data.frame(tpShowID$tpShowID,tpShowID$title,netSales = 0,netAttendedTickets = 0,netPurchasedTickets = 0,totalCompedProdTkts = 0,totalRefundedProdTkts = 0, pctCapacity = 0, numEvents = 0, totalEvents = 0, avgTktPrice = 0, pctPaidAttendees = 0)
    }
  } else if (tpShowID$tpShowID[1] == 0)
  {
    ticketsByShow <- data.frame(tpShowID$tpShowID,tpShowID$title,netSales = 0,netAttendedTickets = 0,netPurchasedTickets = 0,totalCompedProdTkts = 0,totalRefundedProdTkts = 0, pctCapacity = 0, numEvents = 0, totalEvents = 0, avgTktPrice = 0, pctPaidAttendees = 0)
  }
  dbDisconnect(conn = con)
  print(ticketsByShow)
}

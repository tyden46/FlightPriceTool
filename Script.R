library(httr)
library(rjson)
#depart=c("HOU","LRD","IAH","DFW")
#destination=c("HND","NRT","KIX","ITM")
depart="LRD"
destination=c("ATL"
,"DFW"
,"DEN"
,"ORD"
,"LAX"
,"CLT"
,"LAS"
,"MCO"
,"SEA"
,"MIA"
,"IAH"
,"FLL"
,"SFO"
,"JFK"
,"MSP"
,"EWR"
,"DTW"
,"BOS"
,"SLC"
,"PHL"
,"BWI"
,"TPA"
,"SAN"
,"MDW"
,"IAD"
,"BNA"
,"LGA"
,"DAL"
,"DCA"
,"PDX"
,"HNL"
,"HOU"
,"AUS"
,"STL"
,"RSW"
,"SMF"
,"MSY"
,"RDU"
,"SJU"
,"SJC"
,"OAK"
,"MCI"
,"CLE"
,"IND"
,"SAT"
,"SNA"
,"PIT"
,"CVG"
,"CMH"
,"PBI"
,"JAX"
,"MKE"
,"ONT"
,"BDL"
,"OGG"
,"ANC"
,"OMA"
,"MEM"
,"RNO")
counter=1
tableExists=FALSE
for(x in depart){
  for (y in destination){
    print(paste("Testing",x,"to",y))
    url <- paste("https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/browseroutes/v1.0/US/USD/en-US/",x,"/",y,"/anytime/anytime",sep="")
    API_KEY <- "###"
    response <- VERB("GET",
                     url,
                     add_headers("x-rapidapi-key" = API_KEY,
                                 "x-rapidapi-host" = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com"),
                     content_type("application/octet-stream"))
    Robject <- httr::content(response,"text")
    if(Robject!=""){
      result <- fromJSON(Robject)
      if(length(result$Quotes)>0){
        depDate=result$Quotes[[1]]$OutboundLeg$DepartureDate
        retDate=result$Quotes[[1]]$InboundLeg$DepartureDate
        price=as.numeric(result$Routes[[1]]$Price)
        for(v in 1:length(result$Carriers)){
          if(result$Carriers[[v]]$CarrierId==result$Quotes[[1]]$OutboundLeg$CarrierIds){
            outboundCarrier=result$Carriers[[v]]$Name
          }
        }
        for(b in 1:length(result$Carriers)){
          if(result$Carriers[[b]]$CarrierId==result$Quotes[[1]]$InboundLeg$CarrierIds){
            inboundCarrier=result$Carriers[[b]]$Name
          }
        }
        quoteTime=result$Quotes[[1]]$QuoteDateTime
        row=c(x,y,depDate,retDate,price,outboundCarrier,inboundCarrier,quoteTime)
        if(tableExists==FALSE){
          myTable=as.data.frame(t(row))
          tableExists=TRUE
        }else{
          myTable=as.data.frame(rbind(myTable,t(row)))
        }
        if(counter==1){
          best=result
        }else{
          bestPrice=min(best$Routes[[1]]$Price,result$Routes[[1]]$Price)
          if(bestPrice!=best$Routes[[1]]$Price){
            best=result
          }
        }
        counter=counter+1
      }
    }
  }
}
myTable$V5=as.numeric(myTable$V5)
colnames(myTable)=c("Departure", "Destination", "Departure Date", "Return Date", "Price (USD)",
                    "Departing airline", "Returning Airline", "Date of Quote")
library(twilio)
library(ggplot2)
library(gridExtra)
library(RCurl)
library(ssh)
png("June17Flights.png", height=180, width=900)
p<-tableGrob(myTable[order(myTable$`Price (USD)`)[1:5],], rows=NULL)
grid.arrange(p)
dev.off()
Sys.setenv(TWILIO_SID = "###")
Sys.setenv(TWILIO_TOKEN = "###")
session=ssh_connect("###@###", keyfile = NULL, passwd = "###", verbose = FALSE)
scp_upload(session, "June17Flights.png", to = "/var/www/html", verbose = TRUE)

tw_send_message("5555555555", "5555555555", "Here are the Cheapest flights from Texas to Japan",
                media_url = "http://www.tysondawson.com/June17Flights.png")

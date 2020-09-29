#Importing Dataset
dataset <- read.csv(file.choose())

#Finding Not Available / Missing Values in Dataset
is.na(dataset) -> MisData
length(MisData[MisData==T])

#Processing Date
install.packages('lubridate')
library(lubridate)
dmy(dataset$Date) ->dataset$Date

#Extracting Monthly and Daily Ticket Count
install.packages('dplyr')
library(dplyr)
monthly_count<- summarise(group_by(dataset,Month =as.integer(month(Date))),
                          Count = n())
daily_count<- summarise(group_by(dataset,Date),Count =n())
monthly_count<-arrange(monthly_count,Month)

#Comparing Monthly and Daily Complaints
#_____________
library(ggplot2)
#1-Monthly
ggplot(data = monthly_count,aes(x=Month,y=Count,label = Count))+
  geom_point(size = 1)+
  geom_line()+
  geom_text()+
  scale_x_continuous(breaks = monthly_count$Month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))
#2-Daily
ggplot(data = daily_count,aes(x=as.POSIXct(Date),y=Count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))

#Complaint Type Processing
network_tickets<- contains(dataset$Customer.Complaint,match = 'network',ignore.case = T)
payment_tickets<- contains(dataset$Customer.Complaint,match = 'payment',ignore.case = T)
billing_tickets<- contains(dataset$Customer.Complaint,match = 'bill',ignore.case = T)
email_tickets<- contains(dataset$Customer.Complaint,match = 'email',ignore.case = T)
charges_ticket<- contains(dataset$Customer.Complaint,match = 'charge',ignore.case = T)
speed_ticket<- contains(dataset$Customer.Complaint,match = 'speed',ignore.case = T)
service_ticket<- contains(dataset$Customer.Complaint,match = 'service',ignore.case = T)

dataset$ComplaintType[payment_tickets]<- "payment"
dataset$ComplaintType[network_tickets]<- "Network"
dataset$ComplaintType[service_ticket]<- "service"
dataset$ComplaintType[billing_tickets]<- "Billing"
dataset$ComplaintType[email_tickets]<- "Email"
dataset$ComplaintType[charges_ticket]<- "Charges"
dataset$ComplaintType[speed_ticket]<- "speed"

dataset$ComplaintType[-c(payment_tickets,network_tickets,
                         billing_tickets,charges_ticket,
                         email_tickets,speed_ticket,
                         service_ticket)]<- "Others"

table(dataset$ComplaintType)

#Creating new Variable ComplaintStatus with values Open and Closed
open_complaints<- (dataset$Status == "Open"| dataset$Status =="Pending")
closed_complaints<-(dataset$Status == "Closed"| dataset$Status =="Solved")
dataset$ComplaintStatus[ open_complaints]<-"Open" 
dataset$ComplaintStatus[closed_complaints]<- "Closed" 

#Creating Stacked barchart for complaints based on State and Status
dataset<- group_by(dataset,State,ComplaintStatus)
chart_data<- summarise(dataset,Count = n())
ggplot(as.data.frame(chart_data) ,mapping = aes(x=State,y=Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets",
       fill= "Status")

#Finding State which has Highest number of Unresolved Tickets
chart_data%>%
  filter(ComplaintStatus == "Open")->
  open_complaints
open_complaints[open_complaints$Count == max(open_complaints$Count),c(1,3)]

#Calculating Resolution Percentage based on Total and Catagory
resolved_data <- group_by(dataset,ComplaintStatus)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 
resolved_data <- group_by(dataset,Received.Via,ComplaintStatus)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data)))

#Ploting Pie Chart for Total Resolved Vs Category Resolved
install.packages("ggpubr")
library(ggpubr)
par(mfrow = c(1,2))
total<-ggplot(total_resloved,aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

# Pie Chart for Category wise Ticket Status
category<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(Received.Via,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
ggarrange(total,category,nrow = 1, ncol = 2)







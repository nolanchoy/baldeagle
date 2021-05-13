data_alt<-expenditures_FWS_ts%>%
  distinct(Scientific.Name, Species_total, Delisting.Date,.keep_all = TRUE)%>%
  ungroup()%>%
  group_by(Scientific.Name)%>%
  #sum of species
  summarize(Species_sum=sum(Species_total),
            FWS_sum=sum(FWS_total),
            Other_fed_sum=sum(Other_fed),
            States_sum=sum(States_total))%>%
  #other variables
  left_join(expenditures_FWS_ts%>%
              select(Class,
                     Genus,
                     Species,
                     Scientific.Name,
                     ESA.Listing.Status,
                     ESA.Listing.Date,
                     Delisting.Date,
                     Conservation.Plan.Type,
                     Conservation.Plan.Title,
                     Recovery.Document.Date),
            by="Scientific.Name")%>%
  #binary Recovery
  mutate(Recovered=ifelse(ESA.Listing.Status=="Recovery","1","0"))%>%
  distinct(Scientific.Name,Conservation.Plan.Title,.keep_all = T)%>%
  ungroup()%>%
  #plan numbers
  group_by(Scientific.Name)%>%
  mutate(Plan_num=n())%>%
  #time since listing
  mutate(ESA.Listing.Date=as.Date(ESA.Listing.Date,"%m-%d-%Y"),
         Time_since_listing=as.numeric(difftime(as.Date('05-01-2021',"%m-%d-%Y"),ESA.Listing.Date,units='days')))%>%
  ungroup()%>%
  group_by(Scientific.Name,Conservation.Plan.Type)%>%
  mutate(Plan_type_count=n())%>%
  mutate(Conservation.Plan.Type=gsub("^$|^ $", "N/A", Conservation.Plan.Type))%>%
  pivot_wider(names_from=Conservation.Plan.Type, values_from=Plan_type_count, values_fill = 0)%>%
  select(-Conservation.Plan.Title)%>%
  distinct(.keep_all = T)%>%
  ungroup()%>%
  #deal with logging 0's
  mutate(FWS_sum=FWS_sum+1,
         Other_fed_sum=Other_fed_sum+1,
         States_sum=States_sum+1,
         Species_sum=Species_sum+1)

data_alt$Recovered = data_alt$Recovered=="1"

# First Model
logitmodel<-glm(Recovered~
      log(Species_sum)+
      Class+
      Time_since_listing
      -1,
    data=data_alt,
    family=binomial)
logitmodel%>%summary()
predict(logitmodel,data.frame(Class="actinopteri",Species_sum=200000000,Time_since_listing=18000),type="response")

#Recovered is a bad way of measuring species health (but it is one way). For example, since there are no recovered amphibians, the model assumes all amphibians are doomed to die.


#Model 2
model2_data<-inner_join(data_alt,redlist_expen%>%select(Class,Genus,Species,populationTrend),by=c("Class","Genus","Species"))%>%
  distinct(.keep_all =)%>%
  filter(nchar(populationTrend)>1,!populationTrend=="Unknown")%>%
  mutate(Okay=ifelse(populationTrend %in% c("Increasing","Stable"),1,0))

model2_data$Okay = model2_data$Okay=="1"

logitmodel<-glm(Okay~
                  log(FWS_sum)+
                  log(Other_fed_sum)+
                  log(States_sum)+
                  Class+
                  Time_since_listing
                -1,
                data=model2_data,
                family=binomial)
logitmodel%>%summary()

logitmodel<-glm(Okay~
                  log(Species_sum)+
                  Class+
                  Time_since_listing
                -1,
                data=model2_data,
                family=binomial)
logitmodel%>%summary()

predict(logitmodel,data.frame(Class="actinopteri",Species_sum=200000000, Time_since_listing=7300),type="response")

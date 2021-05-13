Core Datasets
================
Nolan Choy
4/25/2021

# Final Datasets

Here are the data files we’ve compiled so far: (Key/Final datasets are
**bolded**)

**LPI\_data:** This is the original dataset from Living Planet

LPI\_only\_obs: This is the Living Planet data set in time series format
(each observation is a Year with a Count)

**redlist\_data:** This is the original dataset from IUCN Redlist

merged\_data: This is the merged dataset of LPI\_data and redlist\_data
(not time series)

merged\_timeseries: This is merged\_data in time series format (each
observation is a Year with a Count)

expen\_X: Expenditure from FWS for Year X

expenditures\_X: Cleaned expenditure data for Year X

**expenditures\_ts:** Time series expenditure data (expenditures\_200X
compressed into one set)

**LPI\_red\_expen\_ts:** Time series dataset of LPI, IUCN Redlist, and
Expenditures

FWS: FWS dataset downloaded from FWS Endangered Species Act site

FWS\_recovery: FWS dataset with recovery plan data

**expenditures\_FWS\_ts:** Joined dataset of FWS, FWS\_recovery, and
expenditures\_ts

# Generate Sets

## LPI\_data

``` r
LPI_data<-read.csv('../dataset/LPR2020data_public.csv')%>%
   mutate(Subspecies=ifelse(Subspecies=="NULL"|nchar(Subspecies)==0,NA,Subspecies))
```

## LPI\_only\_obs

``` r
LPI_only_obs<-LPI_data%>%
  pivot_longer(starts_with("X"),names_to="Year", values_to="Count")%>%
  group_by(ID)%>%
  filter(!Count=='NULL')%>%
  mutate(Year=sub('.', '', Year))

LPI_only_obs$ID<-as.factor(LPI_only_obs$ID)
LPI_only_obs$Count<-as.numeric(LPI_only_obs$Count)
LPI_only_obs$Year<-as.double(LPI_only_obs$Year)
```

## redlist\_data

``` r
redlist_data<-read.csv('../dataset/redlistspecies.csv')
```

## merged\_data

``` r
redlist_data$scientificName <- gsub(" ", "_", redlist_data$scientificName)

#join livingplanet with redlist
merged_data<-left_join(LPI_data,redlist_data, by=c('Binomial'='scientificName'))
```

``` r
#pivot
merged_timeseries<-merged_data%>%
  pivot_longer(starts_with("X"),names_to="Year", values_to="Count")%>%
  group_by(ID)%>%
  filter(!Count=='NULL')%>%
  mutate(Year=sub('.', '', Year))

#set classes
merged_timeseries$ID<-as.factor(merged_timeseries$ID)
merged_timeseries$Year<-as.double(merged_timeseries$Year)
merged_timeseries$Count<-as.numeric(merged_timeseries$Count)
merged_timeseries$redlistCategory <- factor(merged_timeseries$redlistCategory, levels = c(NA, 'Data Deficient', 'Lower Risk/least concern', 'Least Concern', 'Lower Risk/conservation dependent', 'Lower Risk/near threatened', 'Near Threatened', 'Vulnerable', 'Endangered', 'Critically Endangered', 'Extinct in the Wild', 'Extinct'))
merged_data$redlistCategory <- factor(merged_data$redlistCategory, levels = c(NA, 'Data Deficient', 'Lower Risk/least concern', 'Least Concern', 'Lower Risk/conservation dependent', 'Lower Risk/near threatened', 'Near Threatened', 'Vulnerable', 'Endangered', 'Critically Endangered', 'Extinct in the Wild', 'Extinct'))
```

## brRead

### These functions help read nested brackets for expenditure data

``` r
# These functions help read nested brackets

lastBrRead<-function(string){
   #lastBrRead reads the last outermost bracket
   #Example: "welcome (home) (my(lovely) gator)" %>% lastbrRead() returns "my(lovely) gator"
   openBr=0
   startpos=0
   endpos=0
   string_list<-unlist(strsplit(string, split = ""))
   for(pos in 1:nchar(string)){
      if(string_list[pos]=="("){
         if (openBr==0){
            startpos=pos
         }
         openBr<-openBr+1
      } else if(string_list[pos]==")"){
         if (openBr==1){
            endpos=pos
         }
         openBr<-openBr-1
      }
   }
   string<-substr(string,startpos+1,endpos-1)
   return(string)
}

rmBrRead<-function(string){
   if (nchar(string)==0) {return(string)}
   #rnBrRead removes any inner brackets
   #Example: "my(lovely) gator" %>% rmBrRead() returns "my gator"
   string_list<-unlist(strsplit(string, split = ""))
   openBr=0
   cutstartpos=0
   cutendpos=0
   for(pos2 in 1:nchar(string)){
      if(string_list[pos2]=="("){
         if (openBr==0){
            cutstartpos=pos2
         }
         openBr<-openBr+1
      } else if(string_list[pos2]==")"){
         if (openBr==1){
            cutendpos=pos2
         }
         openBr<-openBr-1
      }
   }
   string<-paste(substr(string, 1,cutstartpos-1), substr(string, cutendpos+1,nchar(string)), sep="")
   return(string)
}

brRead<-function(string){
   #"welcome (home) (my(lovely) gator)" %>% brRead() returns "my gator" 
   return(rmBrRead(lastBrRead(string)))
}
```

# expenditures\_200X

``` r
#2000
expen_2000<-read_excel('../dataset/expen/expenditure_2000.xlsx',3)
expenditures_2000<-expen_2000%>%
   rename(Common_name="SPECIES NAME (50 CFR PART 17)", Scientific="SCIENTIFIC NAME", Class="RANK", FWS_total="FWS TOTAL", Other_fed="OTHER FED", Fed_total="FED TOTAL", States_total="STATES TOTAL", Species_total="SPECIES TOTAL ($000)")%>% #rename variables
   filter(!is.na(Common_name))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)%>%
   filter(!is.na(Scientific))

expenditures_2000<-expenditures_2000%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>%
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA))%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   mutate(Year=2000)%>%
   mutate(FWS_total=FWS_total*1000, Other_fed=Other_fed*1000, Fed_total=Fed_total*1000, States_total=States_total*1000, Species_total=Species_total*1000)%>%
   data.frame()


#2001
expen_2001<-read_excel('../dataset/expen/expenditure_2001.xlsx',5)

expenditures_2001<-expen_2001%>%
   rename(Species_sci="Species (50 CFR Part 17)", Class="Class",FWS_total="FWS\r\nTotal ($)", Other_fed="Other Fed ($)", Fed_total="Fed Total ($)", States_total="States Total ($)", Species_total="Species Total ($)")%>% #rename variables
   filter(!is.na(Species_sci))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

expenditures_2001<-expenditures_2001%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2001)%>%
   data.frame()


#2002
expen_2002<-read_excel('../dataset/expen/expenditure_2002.xlsx',5)

expenditures_2002<-expen_2002%>%
   rename(Species_sci="Species (50 CFR Part 17)", FWS_total="FWS\r\nTotal ($)", Other_fed="Other Fed ($)", Fed_total="Fed Total ($)", States_total="States Total ($)", Species_total="Species Total ($)")%>% #rename variables
   filter(!is.na(Species_sci))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Clams"
   select(-temp)


expenditures_2002<-expenditures_2002%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2002)%>%
   data.frame()


#2003
expen_2003<-read_excel('../dataset/expen/expenditure_2003.xlsx',5)

expenditures_2003<-expen_2003%>%
   rename(Species_sci="Species (50 CFR Part 17)", Status="Stat- us", FWS_total="FWS\r\nTotal ($)", Other_fed="Other Fed ($)", Fed_total="Fed Total ($)", States_total="States Total ($)", Species_total="Species Total ($)")%>% #rename variables
   filter(!is.na(Species_sci))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=="Clams"))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

expenditures_2003<-expenditures_2003%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2003)%>%
   data.frame()


#2004
expen_2004<-read_excel('../dataset/expen/expenditure_2004.xlsx',5)

expenditures_2004<-expen_2004%>%
   rename(Species_sci="Species (50 CFR Part 17)", FWS_total="FWS\r\nTotal", Other_fed="Other Fed", Fed_total="Fed Total", States_total="States Total", Species_total="Species Total")%>% #rename variables
   filter(!is.na(Species_sci))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

expenditures_2004<-expenditures_2004%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2004)%>%
   data.frame()


#2005
expen_2005<-read_excel('../dataset/expen/expenditure_2005_06.xlsx',1)

expenditures_2005<-expen_2005%>%
   rename(Species_sci="Species (50 CFR Part 17)", Class="Rank", FWS_total="FWS\r\nTotal", Other_fed="Other Fed", Fed_total="Fed Total", States_total="States Total", Species_total="Species Total")%>% #rename variables
   filter(!is.na(Species_sci))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=="Clams"))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

expenditures_2005<-expenditures_2005%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2005)%>%
   data.frame()


#2006
expen_2006<-read_excel('../dataset/expen/expenditure_2005_06.xlsx',2)

expenditures_2006<-expen_2006%>%
   rename(Species_sci="Species (50 CFR Part 17)", FWS_total="FWS\r\nTotal", Other_fed="Other Fed", Fed_total="Fed Total", States_total="States Total", Species_total="Species Total")%>% #rename variables
   filter(!is.na(Species_sci))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

expenditures_2006<-expenditures_2006%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2006)%>%
   data.frame()

#2007
expen_2007<-read_excel('../dataset/expen/expenditure_2007.xlsx',1)

expenditures_2007<-expen_2007%>%
   rename(Species_sci="Species (50 CFR Part 17)", FWS_total="FWS\r\nTotal", Other_fed="Other Fed", Fed_total="Fed Total", States_total="States Total", Species_total="Species Total")%>% #rename variables
   filter(!is.na(Species_sci))%>%
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=="Clams"))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

expenditures_2007<-expenditures_2007%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2007)%>%
   data.frame()

#2008
expen_2008<-read_excel('../dataset/expen/expenditure_2008.xlsx',2)%>%
   select(-starts_with("..."))
```

    ## New names:
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...7
    ## * `` -> ...9
    ## * ...

``` r
expenditures_2008<-expen_2008%>%
   rename(Species_sci="Species\r\n(50 CFR Part 17)",Class="Group\r\nName", FWS_total="FWS Total", Other_fed="Other Fed", Fed_total="Fed Total", States_total="States\r\nTotal", Species_total="Species\r\nTotal")%>% #rename variables
   filter(!is.na(Class))%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)
expenditures_2008<-expenditures_2008%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2008)%>%
   data.frame()

#Clean up data
expenditures_2008<-expenditures_2008%>%
   filter(!Species=="17") #indicating rows with headers


#2009
expen_2009<-read_excel('../dataset/expen/expenditure_2009.xlsx',1)%>%
   select(-starts_with("..."))
expenditures_2009<-expen_2009%>%
   rename(Class = "Group",Species_sci="Species (50 CFR Part 17)", FWS_total="FWS Total", Other_fed="Other Fed", Fed_total="Fed Total", States_total="States Total", Species_total="Species Total")%>% #rename variables
   filter(!is.na(Class))%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)
expenditures_2009<-expenditures_2009%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2009)%>%
   data.frame()
#Clean up data
expenditures_2009<-expenditures_2009%>%
   filter(!Species=="17") #indicating rows with headers


#2010
expenditures_2010<-read_excel('../dataset/expen/expenditure_2010.xlsx',2)%>%
   select(-starts_with("...")) %>% rename(FWS_total="FWS_Total", Other_fed="Other_Fed", Fed_total="Fed_Total", States_total="States_Total", Species_total="Species_Total")%>%
   filter(!is.na(Class))%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)
expenditures_2010<-expenditures_2010%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2010)%>%
   data.frame()
#Clean up data
expenditures_2010<-expenditures_2010%>%
   filter(!Species=="17") #indicating rows with headers


#2011
file <- '../dataset/expen/expenditure_2011.xlsx'
expen_2011 <- excel_sheets(file)[1:30]%>%map_df(., ~ read_excel(file, sheet = .x))
expen_2011<-expen_2011%>%
   #species is stored in a different column
   mutate(Species_2_filled=!is.na(`Species (50 CFR\r\nPart 17)`)&is.na(`Species (50 CFR`))%>% 
   mutate(`Species (50 CFR`=ifelse(Species_2_filled==TRUE,`Species (50 CFR\r\nPart 17)`,`Species (50 CFR`))%>%
   select(-Species_2_filled)%>%
   #concatenate descriptions
   mutate(temp=cumsum(!is.na(Rank)|is.na(`Species (50 CFR`)))%>%
   group_by(temp)%>%
   mutate(`Species (50 CFR`=str_c(`Species (50 CFR`, collapse=" "))%>%
   filter(!is.na(Rank)&!is.na(`Species Total`))%>%
   ungroup()%>%
   select(-`Species (50 CFR\r\nPart 17)`,-temp)

expen_2011$`Group Name`[is.na(expen_2011$`Group Name`)]<-0 #set NA's as 0

expenditures_2011<-expen_2011%>%
   rename(Class=`Group Name`, Species=`Species (50 CFR`, FWS_total="FWS Total", Other_fed="Other Fed", Fed_total="Fed Total", States_total="States Total", Species_total="Species Total")%>% #rename variables
   filter(!is.na(Class))%>%
   mutate(temp=cumsum(Class=='Clams'))%>% #set rows before "Fishes subtotal" to 0
   filter(temp==0)%>% #select rows before "Fishes subtotal"
   select(-temp)%>% 
   mutate(temp=cumsum(!Class=="Group Name" & !sapply(Class,nchar)<=3))%>% #set Classes
   group_by(temp)%>%
   mutate(Class=dplyr::first(Class))%>%
   ungroup()%>%
   select(-temp)

expenditures_2011$Class<-as.factor(expenditures_2011$Class)
expenditures_2011$Class<-recode(expenditures_2011$Class, "Mammals"="Mammalia","Birds"="Aves","Reptiles"="Reptilia","Amphibians"="Amphibia", "Fishes"="Actinopteri")

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)
expenditures_2011<-expenditures_2011%>%
   mutate(Species_loc=str_split(Species," - "))%>% #split by "-" into c(species,location)
   mutate(Species_sci=sapply(Species_loc, "[", 1), #get Species/Location cols
          Location=sapply(Species_loc, "[", 2))%>%
   select(-Species_loc)%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   mutate(Scientific=gsub(" - |- |-\\r\n| -|", "",Scientific))%>%
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2011)


#2012
expenditures_2012<-read_excel('../dataset/expen/expenditure_2012.xlsx')%>%
   select(-starts_with("...")) %>% rename(Other_fed="Other_Fed", Fed_total="Fed_Total", States_total="States_Total", Species_total="Species_Total")%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Clams"
   select(-temp)%>%
   filter(!is.na(Class))

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)

expenditures_2012<-expenditures_2012%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2012)%>%
   data.frame()
#Clean up data
expenditures_2010<-expenditures_2010%>%
   filter(!Species=="17") #indicating rows with headers

expenditures_2013<-read_excel('../dataset/expen/expenditure_2013.xlsx')%>%
   select(-starts_with("...")) %>% rename(Other_fed="Other_Fed", Fed_total="Fed_Total", States_total="States_Total", Species_total="Species_Total")%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Clams"
   select(-temp)%>%
   filter(!is.na(Class))


#2013
expenditures_2013<-expenditures_2013%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2013)%>%
   data.frame()
#Clean up data
expenditures_2013<-expenditures_2013%>%
   filter(!Species=="17") #indicating rows with headers


#2014
expenditures_2014<-read_excel('../dataset/expen/expenditure_2014.xlsx')%>%
   select(-starts_with("...")) %>% rename(Other_fed="Other_Fed", Fed_total="Fed_Total", States_total="States_Total", Species_total="Species_Total")%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Clams"
   select(-temp)%>%
   filter(!is.na(Class))

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)
expenditures_2014<-expenditures_2014%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2014)%>%
   data.frame()
#Clean up data
expenditures_2014<-expenditures_2014%>%
   filter(!Species=="17") #indicating rows with headers


#2015
expenditures_2015<-read_excel('../dataset/expen/expenditure_2015.xlsx')%>%
   select(-starts_with("..."))%>% rename(Other_fed="Other_Fed", Fed_total="Fed_Total", States_total="States_Total", Species_total="Species_Total")%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Clams"
   select(-temp)%>%
   filter(!is.na(Class))

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)
expenditures_2015<-expenditures_2015%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2015)%>%
   data.frame()
#Clean up data
expenditures_2015<-expenditures_2015%>%
   filter(!Species=="17") #indicating rows with headers


#2016
expenditures_2016<-read_excel('../dataset/expen/expenditure_2016.xlsx')%>%
   select(-starts_with("..."))%>% rename(Other_fed="Other_Fed", Fed_total="Fed_Total", States_total="States_Total", Species_total="Species_Total")%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Clams"
   select(-temp)%>%
   filter(!is.na(Class))

expenditures_2016<-expenditures_2016%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2016)%>%
   data.frame()
#Clean up data
expenditures_2016<-expenditures_2016%>%
   filter(!Species=="17") #indicating rows with headers


#2017
expenditures_2017<-read_excel('../dataset/expen/expenditure_2017.xlsx')%>%
   select(-starts_with("...")) %>% filter(!is.na(Species_sci))%>% rename(Other_fed="Other_Fed", Fed_total="Fed_Total", States_total="States_Total", Species_total="Species_Total")%>%
   filter(!is.na(Species_sci))%>%
   mutate(temp=cumsum(Class=='Clams'))%>%
   filter(temp==0)%>% #select rows before "Clams"
   select(-temp)%>%
   filter(!is.na(Class))

#Set Species/Subspecies/Location/Year (because bless them for putting 3 variables in one column)
expenditures_2017<-expenditures_2017%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),brRead(Species_sci),Species_sci))%>% #if there is no parentheses at all
   mutate(Scientific=ifelse(grepl("\\(",Species_sci),rmBrRead(Scientific),Scientific))%>% #if there is still parentheses after brRead
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1))%>%
   relocate(Species, .after = Scientific)%>%
   mutate(Year=2017)%>%
   data.frame()
#Clean up data
expenditures_2017<-expenditures_2017%>%
   filter(!Species=="17") #indicating rows with headers
```

## expnditures\_ts

``` r
expen_selection<-function(data){
   data<-data%>%select(Class, Genus, Species, Subspecies, FWS_total, Other_fed, Fed_total, States_total, Species_total, Year)
   data$FWS_total%<>%as.numeric(unlist(data$FWS_total))
   data$Other_fed%<>%as.numeric(unlist(data$Other_fed))
   data$Fed_total%<>%as.numeric(unlist(data$Fed_total))
   data$States_total%<>%as.numeric(unlist(data$States_total))
   data$Species_total%<>%as.numeric(unlist(data$Species_total))
   data$Year%<>%as.numeric(unlist(data$Year))
   data$Class%<>%as.character(unlist(data$Class))
   data$Genus%<>%as.character(unlist(data$Genus))
   data$Species%<>%as.character(unlist(data$Species))
   data$Subspecies%<>%as.character(unlist(data$Subspecies))
   return(data)
}

expenditures_list<-list(
   expenditures_2000,
   expenditures_2001,
   expenditures_2002,
   expenditures_2003,
   expenditures_2004,
   expenditures_2005,
   expenditures_2006,
   expenditures_2007,
   expenditures_2008,
   expenditures_2009,
   expenditures_2010,
   expenditures_2011,
   expenditures_2012,
   expenditures_2013,
   expenditures_2014,
   expenditures_2015,
   expenditures_2016,
   expenditures_2017
   )%>%lapply(expen_selection)

#Vertical join by columns (time series format)
expenditures_ts<-map_df(expenditures_list, rbind)%>%filter(!nchar(Year)==1)
```

## LPI\_red\_expen\_ts

``` r
#Coerce uniformity along join keys by setting to lowercase
merged_timeseries<-merged_timeseries %>%
   mutate(Class = tolower(Class),
          Species = tolower(Species),
          Subspecies = tolower(Subspecies),
          Genus = tolower(Genus))

expenditures_ts<-expenditures_ts%>%
   mutate(Class = tolower(Class),
          Species = tolower(Species),
          Subspecies = tolower(Subspecies),
          Genus = tolower(Genus))

LPI_red_expen_ts<-merged_timeseries%>%left_join(.,expenditures_ts,by=c("Year","Genus", "Species", "Class"))
LPI_red_expen_ts$Class<-as.factor(LPI_red_expen_ts$Class)

LPI_red_expen_ts<-LPI_red_expen_ts%>%group_by(ID)%>%mutate(Countz=(Count-mean(Count))/sd(Count))
```

## expenditures\_FWS\_ts

``` r
expenditures_ts<-left_join(expenditures_ts,LPI_data%>%mutate(Genus = tolower(Genus))%>%dplyr::select(Common_name, Genus, Species),by=c("Genus","Species"))

FWS<-read.csv('../dataset/FWS_full.csv')%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Scientific.Name),rmBrRead(Scientific.Name),Scientific.Name))%>% #if there is no parentheses at all
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=tolower(sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1)))

FWS_recovery<-read.csv('../dataset/FWS_recovery.csv')%>%
   rowwise%>% #subset by row
   mutate(Scientific=ifelse(grepl("\\(",Species.Scientific.Name),rmBrRead(Species.Scientific.Name),Species.Scientific.Name))%>% #if there is no parentheses at all
   ungroup()%>%
   mutate(Species=ifelse( #create Species variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3, #if Species is format: (Family Species Subspecies)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",2), #then return 2nd to last element (Species)
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),tail,1)))%>% #else return last element (Species)
   mutate(Subspecies=ifelse( #create Subspecies variable
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),length)==3,
      sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),"[",3),
      NA)
          )%>%
   mutate(Genus=tolower(sapply(strsplit(Scientific, "\\s{2,}| |\\)|\\(|\r\n"),head,1)))%>%
   select(-Scientific,-Species.Scientific.Name, -Species.Common.Name, -Recovery.Document.Title_url,-Species.Population, -Species.Scientific.Name_url, ECOS.Species.Group)

expenditures_FWS_ts<-left_join(expenditures_ts,FWS, by=c("Genus","Species","Subspecies"))%>%distinct(.keep_all = TRUE)%>%filter(!is.na(Scientific.Name))%>%left_join(FWS_recovery,by=c("Genus","Species","Subspecies"))
```

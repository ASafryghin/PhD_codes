#Question 1 ----
#optimality values for the three communities
AS_MainData<-read.csv( '/Users/alexsafry/Desktop/PhD_Thesis/Data/AS_MainData_FINAL.csv')

sdata<-Waibira
sdata<-sdata[sdata$Duration_analysis_include!="Exclude",]
sdata<-sdata[sdata$Duration_analysis_include!="Exclude_start",]
#exclude rows without known GA end time
sdata<-sdata[sdata$Ga_value!="GA_stop_ex",]
sdata<-sdata[sdata$Ga_value!="MAU_GA_ex",]
#remove unclear sequence parts
##sdata<-sdata[sdata$Sequence_part!="Unclear",] - don't really need this
#Remove gestures produced by unk individuals
sdata<-sdata[sdata$Signaller!="Unk",]
sdata<-sdata[sdata$Signaller!="A_F",]
sdata<-sdata[sdata$Signaller!="A_M",]
sdata<-sdata[sdata$Signaller!="SA_F",]
sdata<-sdata[sdata$Signaller!="SA_M",]
sdata<-sdata[sdata$Signaller!="J",]
sdata<-sdata[sdata$Signaller!="I",]

sdata$morph<-as.factor(sdata$morph) 
droplevels(sdata$morph)
sdata$Gesture_record<-as.factor(sdata$Gesture_record) 
droplevels(sdata$Gesture_record)

Waibira<-AS_MainData%>%
  subset(Social_unit=="Waibira")

Bossou<-AS_MainData%>%
  subset(Social_unit=="Bossou")


bootpsiSonso_morph_GA<-sapply(1:1000, function(x){
  sdata.boot <- Sonso[sample(1:nrow(Sonso), replace = T),]
  return(optimality_score_function(sdata.boot,type= 'morph', duration = "GA"))})


bootpsiWaibira_morph_GA<-sapply(1:1000, function(x){
  sdata.boot <- Waibira[sample(1:nrow(Waibira), replace = T),]
  return(optimality_score_function(sdata.boot,type= 'morph', duration = "GA"))})


bootpsiBossou_morph_GA<-sapply(1:1000, function(x){
  sdata.boot <- Bossou[sample(1:nrow(Bossou), replace = T),]
  return(optimality_score_function(sdata.boot,type= 'morph', duration = "GA"))})

bootpsiSonso_GA_GA<-sapply(1:1000, function(x){
  sdata.boot <- Sonso[sample(1:nrow(Sonso), replace = T),]
  return(optimality_score_function(sdata.boot,type= 'Gesture_record', duration = "GA"))})


bootpsiWaibira_GA_GA<-sapply(1:1000, function(x){
  sdata.boot <- Waibira[sample(1:nrow(Waibira), replace = T),]
  return(optimality_score_function(sdata.boot,type= 'Gesture_record', duration = "GA"))})


bootpsiBossou_GA_GA<-sapply(1:1000, function(x){
  sdata.boot <- Bossou[sample(1:nrow(Bossou), replace = T),]
  return(optimality_score_function(sdata.boot,type= 'Gesture_record', duration = "GA"))})

Waibira_morph_GA<-data.frame(bootpsiWaibira_morph_GA)
Waibira_morph_GA$Group<-"Waibira"
colnames(Waibira_morph_GA)<-c("Psi", "Group")#rename cols

Sonso_morph_GA<-data.frame(bootpsiSonso_morph_GA)
Sonso_morph_GA$Group<-"Sonso"
colnames(Sonso_morph_GA)<-c("Psi", "Group")#rename cols

Bossou_morph_GA<-data.frame(bootpsiBossou_morph_GA)
Bossou_morph_GA$Group<-"Bossou"
colnames(Bossou_morph_GA)<-c("Psi", "Group")#rename cols

Waibira_GA_GA<-data.frame(bootpsiWaibira_GA_GA)
Waibira_GA_GA$Group<-"Waibira"
colnames(Waibira_GA_GA)<-c("Psi", "Group")#rename cols

Sonso_GA_GA<-data.frame(bootpsiSonso_GA_GA)
Sonso_GA_GA$Group<-"Sonso"
colnames(Sonso_GA_GA)<-c("Psi", "Group")#rename cols

Bossou_GA_GA<-data.frame(bootpsiBossou_GA_GA)
Bossou_GA_GA$Group<-"Bossou"
colnames(Bossou_GA_GA)<-c("Psi", "Group")#rename cols

MorphData<-bind_rows(Sonso_morph_GA,Bossou_morph_GA,Waibira_morph_GA)
GAData<-bind_rows(Sonso_GA_GA,Bossou_GA_GA,Waibira_GA_GA)
MorphData$Category<-"Morph"
GAData$Category<-"Gesture action"
GraphData<-bind_rows(MorphData,GAData)

##Graph and post-hoc
library(ggplot2)
install.packages('ggridges')
library(ggridges)
install.packages('ggpubr')
library(ggpubr)
graph<-ggplot(MorphData, aes(x = Psi, y = Group, fill=stat(x))) + geom_density_ridges_gradient(scale=0.9, rel_min_height =0.01) +
  scale_fill_viridis_c(name = "Psi", option = "H") +  theme_ridges() + scale_y_discrete(expand = c(0.05,0)) +
  labs(y= "Group", x = "Psi") +theme(axis.title.y = element_text(angle=0, vjust = 0.5))
graph1<-ggplot(GAData, aes(x = Psi, y = Group, fill=stat(x))) + geom_density_ridges_gradient(scale=0.9, rel_min_height =0.01) +
  scale_fill_viridis_c(name = "Psi", option = "H") +  theme_ridges() + scale_y_discrete(expand = c(0.05,0)) +
  labs(y= "Group", x = "Psi") +theme(axis.title.y = element_text(angle=0, vjust = 0.5))
GraphData<-GraphData%>%group_by(Category)
graph2<-ggplot(GraphData, aes(x = Psi, y = Group, fill=stat(x))) + geom_density_ridges_gradient(scale=0.9, rel_min_height =0.01) +
  scale_fill_viridis_c(name = "Psi", option = "H") +  theme_ridges() + scale_y_discrete(expand = c(0.05,0)) +
  labs(y= "Group", x = "Psi") +theme(axis.title.y = element_text(angle=0, vjust = 0.5))

GraphData$Mix<-paste(GraphData$Category, GraphData$Group)
levels(as.factor(GraphData$Mix))

GraphData$Mix <- factor(GraphData$Mix, levels = c("Morph Bossou","Gesture action Bossou", "Morph Sonso","Gesture action Sonso", "Morph Waibira","Gesture action Waibira"))

graph2 <- ggplot(GraphData, aes(x = Psi, y = Mix, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 0.9, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Psi", option = "H") +
  theme_ridges() +
  scale_y_discrete(expand = c(0.05, 0)) +
  labs( x = "Psi", y="") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))+
  theme(axis.title.x = element_text(hjust= 0.5)) 

graph2 
#Then I test whether the distributions are statistically different or not
res<-GraphData%>%group_by(Mix)%>%
  summarise(mean=mean(Psi))
GraphData<-GraphData%>%left_join(res)

GraphData$res<-GraphData$Psi-GraphData$mean

library(car)

result <- ks.test(GraphData$res, "pnorm")
print(result)

citation("stats")

library(FSA)
# Post-hoc Dunn's test
posthoc_dunn <- dunnTest(Psi ~ Mix, data = GraphData, method = "bonferroni") #all significant 
print(posthoc_dunn)
citation(FSA)

#Additional analysis for Optimality significance
Bossou<-AS_MainData%>%
  subset(Social_unit=="Sonso")
sdata<-Bossou
sdata<-sdata[sdata$Duration_analysis_include!="Exclude",]
sdata<-sdata[sdata$Duration_analysis_include!="Exclude_start",]
#exclude rows without known GA end time
sdata<-sdata[sdata$Ga_value!="GA_stop_ex",]
sdata<-sdata[sdata$Ga_value!="MAU_GA_ex",]
#remove unclear sequence parts
##sdata<-sdata[sdata$Sequence_part!="Unclear",] - don't really need this
#Remove gestures produced by unk individuals
sdata<-sdata[sdata$Signaller!="Unk",]
sdata<-sdata[sdata$Signaller!="A_F",]
sdata<-sdata[sdata$Signaller!="A_M",]
sdata<-sdata[sdata$Signaller!="SA_F",]
sdata<-sdata[sdata$Signaller!="SA_M",]
sdata<-sdata[sdata$Signaller!="J",]
sdata<-sdata[sdata$Signaller!="I",]
Bossou.sum<-sdata%>%group_by(Gesture_record)%>%summarise(p=n(), d=median(Ga_duration))

reps <- 100000
results <- rep(0, reps)
x <- c(Bossou.sum$p)
y <- c(Bossou.sum$d)
L <- sum(x*y)
print (c("real L is", L))
sortvector <- 1:length(x)
for (i in 1:reps)
{
  sortvector <- sample(sortvector, replace = F)
  xtemp <- x[sortvector]
  L_temp <- sum(xtemp *y)
  results[i] <- L_temp
}
is_small <- sum(results <L) #changed check if it is significiantly big swap '<' with '>'
print(c("P of being so small is estimated as ", is_small/reps)) 

optimality_score_function(sdata, "Gesture_record", "GA")


library(dplyr)
Bossou<-AS_MainData%>%filter(Social_unit %in% "Bossou")
HighBossou<-Bossou%>%filter(Sri_Z>1.96)
HighBossou_sum<-HighBossou%>%group_by(Signaller, Recipient,RankRcp)%>%summarise(median=median(Ga_duration), F=n())

Bossou<-Bossou%>%mutate(rel= case_when(Signaller == "FANA" & Recipient%in%c("FOAF", "FANLE", "FLANLE","FANWA","FOTAIU") ~"MO",
                                       Signaller %in% c("FOAF", "FANLE", "FLANLE","FANWA","FOTAIU") & Recipient == "FANA" ~ "OM",
                                       Signaller == "FOKAIYE" & Recipient%in% "FOTAIU" ~ "OM",
                                       Signaller == "FOTAIU" & Recipient%in% "FOKAIYE" ~"MO",
                                       Signaller == "JIRE" & Recipient%in%c("JEJE", "JOKURO", "JA","JURU","JOYA", "JODOAMON", "JIMATO") ~"MO",
                                       Recipient == "JIRE" & Signaller%in%c("JEJE", "JOKURO", "JA","JURU","JOYA", "JODOAMON", "JIMATO") ~"OM",
                                       Signaller == "NINA" & Recipient%in%c("NA_chimp", "NTO") ~"MO",
                                       Recipient == "NINA" & Signaller%in%c("NA_chimp", "NTO") ~"OM",
                                       Signaller == "PAMA" & Recipient%in%c("PILI", "PONI", "PELEY") ~"MO",
                                       Recipient == "PAMA" & Signaller%in%c("PILI", "PONI", "PELEY") ~"OM",
                                       Signaller == "PILI" & Recipient =="POKURU" ~ "MO",
                                       Recipient == "PILI" & Signaller =="POKURU" ~ "OM",
                                       Signaller == "VELU" & Recipient%in%c("VUI", "VUAVUA") ~ "MO",
                                       Recipient == "VELU" & Signaller%in%c("VUI", "VUAVUA") ~ "OM",
                                       Signaller == "VUAVUA" & Recipient =="VEVE" ~ "MO",
                                       Recipient == "VUAVUA" & Signaller =="VEVE" ~ "OM",
                                       Signaller == "YO" & Recipient =="YOLO" ~ "MO",
                                       Recipient == "YO" & Signaller =="YOLO" ~ "OM",
                                       Signaller == "YO" & Recipient =="YUNRO" ~ "MO",
                                       Recipient == "YO" & Signaller =="YUNRO" ~ "OM"))
library(ggplot2)
ggplot(Bossou, aes( x=rel, y=Ga_duration)) + geom_boxplot()

Bossou$rel<-as.character(Bossou$rel)

Bossou.H<-Bossou%>%filter(Sri_Z>1.96)%>%group_by(rel)%>%summarise(F=n())
Bossou.an<-Bossou%>%select(rel, Ga_duration)%>%na.omit()
res<-aov(Ga_duration ~ rel, data = Bossou.an)
summary(res)

#Question 2 ----
library(lubridate)
library(dplyr)
SonsoPresence<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Social indices data/Sonso_pres_day.csv")
SonsoPresence<-tidyr::gather(SonsoPresence,key="individual", value="value",2:153)
SonsoPresence$Date<-as.Date(SonsoPresence$Date, format="%d/%m/%Y")
SonsoPresence<-SonsoPresence%>%
  filter(Date<=as.Date("2012-06-30") & Date>=as.Date("2007-09-01")) %>%
  mutate(date = ymd(Date),
         year = year(date)) %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ paste0("DecFeb_", ifelse(month(date) == 12, year + 1, year)),
    month(date) %in% c(3, 4, 5) ~ paste0("MarMay_", year),
    month(date) %in% c(6, 7, 8) ~ paste0("JunAug_", year),
    month(date) %in% c(9, 10, 11) ~ paste0("SepNov_", year)
  ))%>%select(-Date, -date)%>%unique()

SonsoDep<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Sonso Community independence individuals.csv")
SonsoDep<-tidyr::gather(SonsoDep,key="SeasonCode", value="Status", -individual)

Sonso<-AS_MainData%>%filter(Social_unit %in% "Sonso")

SonsoPresence<-SonsoPresence%>%
  left_join(SonsoDep, by=c('individual', 'season'='SeasonCode'))%>%
  filter(season %in% Sonso$season)%>%
  filter(Status %in% c("Ind"))%>%
  select(-value, -year)%>%
  unique()

ComSizeSonso<-SonsoPresence%>%
  group_by(season)%>%
  summarise(F=n())

SonsoPartyScans<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Social indices data/Sonso_PartyCom.csv")
SonsoPartyScans$Date<-as.Date(SonsoPartyScans$Date)

u <- SonsoPartyScans %>%
  mutate(date = ymd(Date),
         year = year(date)) %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ paste0("DecFeb_", ifelse(month(date) == 12, year + 1, year)),
    month(date) %in% c(3, 4, 5) ~ paste0("MarMay_", year),
    month(date) %in% c(6, 7, 8) ~ paste0("JunAug_", year),
    month(date) %in% c(9, 10, 11) ~ paste0("SepNov_", year)
  ))%>%
  mutate(across(where(is.numeric), function(x) ifelse(x >= 1, 1, x)))%>%
  select(-X, -year,-date)%>%
  mutate(party_size=rowSums(.[4:107]))%>%
  group_by(season)%>%
  summarise(av_ps=mean(party_size))

ComSizeSonso<-ComSizeSonso%>%
  left_join(u)

ComSizeSonso$R_ps<-(ComSizeSonso$av_ps/ComSizeSonso$F)*100
ComSizeSonso$Group<-"Sonso"
AvSonsoRP<-mean(ComSizeSonso$R_ps)



#Repeat for wabira -
WaibiraPresence<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Social indices data/Waibira_pres_day.csv")
WaibiraPresence<-tidyr::gather(WaibiraPresence,key="individual", value="value",2:122)
WaibiraPresence$Date<-as.Date(WaibiraPresence$Date, format="%d/%m/%Y")

WaibiraPresence<-WaibiraPresence%>% #Select only relevant dates
  filter(Date<=as.Date("2022-05-31") & Date>=as.Date("2016-12-01")) %>% #assign code to dates
  mutate(date = ymd(Date),
         year = year(date)) %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ paste0("DecFeb_", ifelse(month(date) == 12, year + 1, year)),
    month(date) %in% c(3, 4, 5) ~ paste0("MarMay_", year),
    month(date) %in% c(6, 7, 8) ~ paste0("JunAug_", year),
    month(date) %in% c(9, 10, 11) ~ paste0("SepNov_", year)
  ))%>%select(-Date, -date)%>%unique()

WaibiraDep<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Waibira Community independence individuals.csv")#upload independence 
WaibiraDep<-tidyr::gather(WaibiraDep,key="season", value="Status", -Individual)

Waibira<-AS_MainData%>%filter(Social_unit %in% "Waibira")
WaibiraPresence<-WaibiraPresence%>%
  left_join(WaibiraDep, by=c('individual'='Individual', 'season'))%>%
  filter(season %in% Waibira$season)%>% #assign to each individual if they were independent or not in that saeson
  #filter only independent individuals
  filter(Status %in% c("Ind"))%>%
  select(-value, -year)%>%
  unique()

ComSizeWaibira<-WaibiraPresence%>% #here you have the community size of waibira per each season 
  group_by(season)%>%
  summarise(F=n())


WaibiraPartyScans<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Social indices data/Waibira_PartyCom.csv")
WaibiraPartyScans$Date<-as.Date(WaibiraPartyScans$Date, format = "%d/%m/%Y")
#OR USE THIS:

u <- WaibiraPartyScans %>%
  mutate(date = ymd(Date),
         year = year(date)) %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ paste0("DecFeb_", ifelse(month(date) == 12, year + 1, year)),
    month(date) %in% c(3, 4, 5) ~ paste0("MarMay_", year),
    month(date) %in% c(6, 7, 8) ~ paste0("JunAug_", year),
    month(date) %in% c(9, 10, 11) ~ paste0("SepNov_", year)
  ))%>%
  mutate(across(where(is.numeric), function(x) ifelse(x >= 1, 1, x)))%>%
  select(-X, -year,-date)%>%
  mutate(party_size=rowSums(.[4:79]))%>%
  group_by(season)%>%
  summarise(av_ps=mean(party_size))

ComSizeWaibira<-ComSizeWaibira%>%
  left_join(u)

ComSizeWaibira$R_ps<-(ComSizeWaibira$av_ps/ComSizeWaibira$F)*100
ComSizeWaibira$Group<-"Waibira"


#Bossou
BossouInd<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Bossou independence.csv")
BossouInd$Initials <- BossouInd$Initials  %>% replace_na("NA_chimp")

# get the column names of the data frame
colnames(BossouInd)[2:ncol(BossouInd)] <- substring(colnames(BossouInd)[2:ncol(BossouInd)], 2)
col_names <- names(BossouInd)

# print updated column titles
col_names[2:length(col_names)] <- paste0("01/12/", col_names[2:length(col_names)])

# set the modified column names to the data frame
names(BossouInd) <- col_names


BossouID<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Bossou ID list.csv")
BossouID<-mutate_all(BossouID,toupper)
BossouID$ID <- BossouID$ID %>% replace_na("NA_chimp")
BossouID$Full_name <- BossouID$Full_name %>% replace_na("NA_chimp")
BossouID<-BossouID%>%
  mutate(ID=ifelse(ID %in% "JÁ", "JA", ID))


# Join Bossou.ranks with BossouID data by 'Initials' and 'ID' columns
df<-BossouInd
df <- df %>%
  left_join(BossouID, by = c("Initials" = "ID")) %>%
  select(-Full_name)%>%  mutate(Initials=ifelse(Initials %in% "JÁ", "JA", Initials))%>%
  rename(ID=Initials)%>%
  pivot_longer(cols = -ID, names_to = "Date", values_to = "Presence")

df$Date<-as.Date(df$Date, format = "%d/%m/%Y")

df <- df %>%
  mutate(date = ymd(Date),
         year = year(date)) %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ paste0("DecFeb_", ifelse(month(date) == 12, year + 1, year)),
    month(date) %in% c(3, 4, 5) ~ paste0("MarMay_", year),
    month(date) %in% c(6, 7, 8) ~ paste0("JunAug_", year),
    month(date) %in% c(9, 10, 11) ~ paste0("SepNov_", year)
  ))%>%select(-date,-year)

ComSizeBossou<-df%>%#Bossou community size per season
  group_by(season)%>%
  summarise(F=sum(Presence))


#Find bossou average party size per season 
BossouPartyScans<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Social indices data/Bossou FINAL IND PARTY SCANS.csv")

u<-BossouPartyScans%>%mutate(party_size=rowSums(.[3:33]))%>%
  group_by(season)%>%
  summarise(av_ps=mean(party_size))

ComSizeBossou<-ComSizeBossou%>%
  left_join(u)

ComSizeBossou$R_ps<-(ComSizeBossou$av_ps/ComSizeBossou$F)*100
ComSizeBossou$Group<-"Bossou"

ComSize<-bind_rows(ComSizeSonso, ComSizeWaibira,ComSizeBossou)

write.csv(ComSize, "/Users/as493/Documents/GitHub/PhD_Thesis/Data/Community relative party sizes per period.csv")

ComSize.summary<-ComSize%>%
  group_by(Group)%>%
  summarise(AvRP=mean(R_ps, na.rm=T))

write.csv(ComSize.summary, "/Users/as493/Documents/GitHub/PhD_Thesis/Data/Community relative party sizes.csv")
# Assuming you have a data frame called 'data' with columns 'season', 'community', and other relevant variable

unique_communities <- unique(AS_MainData$Social_unit)
unique_seasons <- unique(AS_MainData$season)

str(AS_MainData)


# List to store the subset results
subset_results <- list()

# Form subsets based on 'community' and 'season', apply the function, and store results
for (Social_unit in unique_communities) {
  community_data <- AS_MainData[AS_MainData$Social_unit == Social_unit, ]
  
  for (season in unique_seasons) {
    subset_data <- community_data[community_data$season == season, ]
    
    # Check if the subset is empty
    if (nrow(subset_data) > 50) {
      # Apply the optimality_score_function on the subset_data
      optimality_score <- optimality_score_function(subset_data, "morph", "GA")
      
      # Store the results in a data frame with additional columns for 'community' and 'season'
      result <- data.frame(community = Social_unit, season = season, optimality_score = optimality_score)
      
      # Add the result to the subset_results list
      subset_results[[length(subset_results) + 1]] <- result
      
      # You can perform further analysis or processing on the subset_data here
    }
  }
}

# Combine all the subset results into a single data frame
combined_results <- do.call(rbind, subset_results)


#Now check how these vary with relative party sizes
ComSize<-ComSize%>%select(R_ps, Group, season)

ComSize<-ComSize%>%left_join(combined_results, by = c("season", "Group"="community"))

ComSize<-ComSize%>%unique()

library(ggplot2)
ggplot(data=ComSize, aes(x=R_ps, y=optimality_score)) + 
  geom_point(aes(color=Group) ) +geom_smooth(aes(color=Group),method = 'lm')


write.csv(ComSize, "/Users/as493/Documents/GitHub/PhD_Thesis/Data/Otpimality_party size dataset.csv")
ComSize<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Otpimality_party size dataset.csv")





#CHECK what happens if Psi values are assigned the previous R_ps
ComSize<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Otpimality_party size dataset.csv")

combined_results <- separate(combined_results, season, into = c("Months", "year"), sep = "_")

combined_results<-combined_results%>%mutate(
  new_months=case_when(community %in% c("Sonso","Waibira") & Months %in% "DecFeb"~"SepNov",
                       community %in% c("Sonso","Waibira") & Months %in% "MarMay"~"DecFeb",
                       community %in% c("Sonso","Waibira") & Months %in% "JunAug"~"MarMay",
                       community %in% c("Sonso","Waibira") & Months %in% "SepNov"~"JunAug",
                       community %in% "Bossou" ~ Months)
)

combined_results <- combined_results %>% mutate(
  new_year = case_when(
    community %in% c("Sonso", "Waibira") & new_months == "SepNov" ~ as.numeric(year) - 1,
    community %in% "Bossou" ~ as.numeric(year) - 1,
    TRUE ~ as.numeric(year)  # Default case if no conditions match
  )
)

combined_results <- unite(combined_results, new_season, new_months, new_year, sep = "_")

ComSize<-ComSize%>%
  left_join(combined_results, by = c("season"="new_season", "Group"="community"))%>%
  rename(Psi_future=optimality_score.y, Psi=optimality_score.x)

ComSize<-ComSize%>%select(-year,-Months)

plot2data<-ComSize%>%filter(Group%in%c("Sonso", "Waibira"))#For the second plot I only include Sonso and Waibira as I can get the immediately previous relative party size, 
#which is not really the case for Bossou - as Bossou gets the R_ps from the previous year

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
write.csv(ComSize,"/Users/as493/Documents/GitHub/PhD_Thesis/Data/Graphs/Chapter 2/Data Graph RPS-Psi plot1.csv")

my_x_title1<- expression(paste("Psi"))
my_y_title1<- expression(paste("Relative party size (%)"))

plot1<-ggplot(data=ComSize, aes(x=Psi, y=R_ps)) + 
  geom_point(aes(color=Group, shape = Group),size=4 ) +
  geom_smooth(aes(color=Group),method = 'lm', se=F)+
  theme_classic()+
  labs(x = my_x_title1, y = my_y_title1) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.position = 'none', plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"))+
  ylim(0,60)+xlim(-1,0.7)+
  scale_color_manual(values =colorBlindBlack8[c(3,6,2)] )+  ggtitle("Period subsets")
plot1
write.csv(plot2data,"/Users/as493/Documents/GitHub/PhD_Thesis/Data/Graphs/Chapter 2/Data Graph RPS-Psi plot2.csv")

my_x_title2<- expression(paste("Next period Psi"))
my_y_title2<- expression(paste("Relative party size (%)"))
plot2<-ggplot(data=plot2data, aes(x=Psi_future, y=R_ps)) +  
  geom_point(aes(color=Group, shape = Group), size=4) + scale_shape_manual(values = c(17,15))+
  geom_smooth(aes(color=Group),method = 'lm', se=F)+
  theme_classic()+
  labs(x = my_x_title2, y = my_y_title2) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.position = 'none', plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"))+
  ylim(0,60)+xlim(-1,0.7)+
  scale_color_manual(values =colorBlindBlack8[c(6,2)] )+  ggtitle("Following period subsets")
plot2


#Try combinidn the datasets and making one single grpah
str(plot2data)
str(ComSize)
Plo.comparison.data<-ComSize%>%filter(complete.cases(.))
Plo.comparison.data$Psidifference<-Plo.comparison.data$Psi_future-Plo.comparison.data$Psi
Plo.comparison.data<-gather(Plo.comparison.data, "Time", "Psi", 5:6)
Plo.comparison.data$season2<-paste(Plo.comparison.data$Group, Plo.comparison.data$season)
Plo.comparison.data$Psidifference.levels<-ifelse(Plo.comparison.data$Psidifference>0, "Higher", "Lower")

install.packages("ggnewscale")
library(ggnewscale)
my_x_title2<- expression(paste("Psi"))


plot2.extra1<-Plo.comparison.data%>%filter(Group %in% c("Sonso", "Waibira"))%>% 
  arrange(Psidifference) %>%
  ggplot( aes(x=Psi, y=R_ps, group=season2)) +   scale_shape_manual(values = c(17,15))+  scale_color_manual(values =colorBlindBlack8[c(6,2)] )+
  geom_point(aes(color=Group, shape = Group), size=6) +  theme_classic()+ 
  labs(x = my_x_title2, y = my_y_title2) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12)) +xlim(-1,0.7)+
  # start a new scale
  geom_path(aes(x = Psi, y = R_ps, group = season2), color="black", size=1, 
            arrow = arrow(length = unit(0.3, "cm"), type='closed')) + ylim(5,30)

plot2.extra1



# Arrange the plots side by side
grid.arrange(plot1, plot2, ncol = 2)

#Now do it for every year --- 
YearComSize<- separate(AS_MainData, season, into=c("months", "year"), sep = "_")

unique_communities <- unique(YearComSize$Social_unit)
unique_year <- unique(YearComSize$year)

# List to store the subset results
subset_results <- list()

# Form subsets based on 'community' and 'season', apply the function, and store results
for (Social_unit in unique_communities) {
  community_data <- YearComSize[YearComSize$Social_unit == Social_unit, ]
  
  for (year in unique_year) {
    subset_data <- community_data[community_data$year == year, ]
    
    # Check if the subset is empty
    if (nrow(subset_data) > 50) {
      # Apply the optimality_score_function on the subset_data
      optimality_score <- optimality_score_function(subset_data, "morph", "GA")
      
      # Store the results in a data frame with additional columns for 'community' and 'season'
      result <- data.frame(community = Social_unit, year = year, optimality_score = optimality_score)
      
      # Add the result to the subset_results list
      subset_results[[length(subset_results) + 1]] <- result
      
      # You can perform further analysis or processing on the subset_data here
    }
  }
}

# Combine all the subset results into a single data frame
combined_results_year <- do.call(rbind, subset_results)

YearRPS<- ComSize
YearRPS<- separate(YearRPS, season, into=c("months", "year"), sep = "_")
YearRPS<-YearRPS%>%group_by(Group, year)%>%summarise(R_ps=mean(R_ps))

YearRps<-YearRPS%>%left_join(combined_results_year, by = c("Group"="community", "year"))       
my_x_title3<- expression(paste("Psi"))
my_y_title3<- expression(paste("Relative party size (%)"))
plot3<-ggplot(data=YearRps, aes(x=optimality_score, y=R_ps)) +  
  geom_point(aes(color=Group, shape = Group), size=4) +
  geom_smooth(aes(color=Group),method = 'lm', se=F)+
  theme_classic()+
  labs(x = my_x_title3, y = my_y_title3) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.position = 'none', plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"))+
  ylim(0,60)+xlim(-1,0.7)+
  scale_color_manual(values =colorBlindBlack8[c(3,6,2)] )+  ggtitle("Yearly subsets")

library(gridExtra)
grid.arrange(plot3, plot1, ncol = 2)

#One value per community 
subset_results <- list()

# Form subsets based on 'community' and 'season', apply the function, and store results
for (Social_unit in unique_communities) {
  subset_data <- AS_MainData[AS_MainData$Social_unit == Social_unit, ]
  
  # Check if the subset is empty
  if (nrow(subset_data) > 50) {
    # Apply the optimality_score_function on the subset_data
    optimality_score <- optimality_score_function(subset_data, "morph", "GA")
    
    # Store the results in a data frame with additional columns for 'community' and 'season'
    result <- data.frame(community = Social_unit, optimality_score = optimality_score)
    
    # Add the result to the subset_results list
    subset_results[[length(subset_results) + 1]] <- result
    
    # You can perform further analysis or processing on the subset_data here
  }
}

# Combine all the subset results into a single data frame
combined_results_all <- do.call(rbind, subset_results)

ComSize.summary<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Community relative party sizes.csv")

my_x_title4<- expression(paste("Psi"))
my_y_title4<- expression(paste("Relative party size (%)"))
plot4<-ggplot(data=ComSize.summary, aes(x=optimality_score, y=AvRP)) +  
  geom_point(aes(color=Group, shape = Group), size=4) +
  theme_classic()+
  labs(x = my_x_title4, y = my_y_title4) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.position = 'none',    plot.title = element_text(hjust = 0, vjust = 1, size = 14, face = "bold"))+
  ylim(0,60)+xlim(-1,0.7)+
  scale_color_manual(values =colorBlindBlack8[c(3,6,2)] )+  
  geom_text(aes(label = c("Bossou", "Sonso","Waibira")), hjust = 0.5, vjust = -1.5, size = 4) +  ggtitle("All data")  # Add the title

#Graph all together
grid.arrange(plot4, plot3, plot1, plot2,ncol = 2)

#Question 3 ----
sdata<-AS_MainData
sdata<-sdata[sdata$Duration_analysis_include!="Exclude",]
sdata<-sdata[sdata$Duration_analysis_include!="Exclude_start",]
#exclude rows without known GA end time
sdata<-sdata[sdata$Ga_value!="GA_stop_ex",]
sdata<-sdata[sdata$Ga_value!="MAU_GA_ex",]
Above<-sdata%>% #create datasets
  filter(Sri_Z>=1.96)
EveryoneElse<-sdata%>%
  filter(Sri_Z<1.96)

Sonso<-AS_MainData%>%filter(Social_unit=="Sonso")
Waibira<-AS_MainData%>%filter(Social_unit=="Waibira")
Bossou<-AS_MainData%>%filter(Social_unit=="Bossou")

SonsoAb<-Above%>%filter(Social_unit =="Sonso") #split per community
WaibiraAb<-Above%>%filter(Social_unit == "Waibira")
BossouAb<-Above%>%filter(Social_unit == "Bossou")


bootpsiWaibiraAb<-sapply(1:1000, function(x){
  sdata.boot <- WaibiraAb[sample(1:nrow(WaibiraAb), replace = T),]
  return(optimality_score_function(sdata.boot, "morph", "GA"))})

bootpsiSonsoAb<-sapply(1:1000, function(x){
  sdata.boot <- SonsoAb[sample(1:nrow(SonsoAb), replace = T),]
  return(optimality_score_function(sdata.boot, "morph", "GA"))})

bootpsiBossouAb<-sapply(1:1000, function(x){
  sdata.boot <- BossouAb[sample(1:nrow(BossouAb), replace = T),]
  return(optimality_score_function(sdata.boot, "morph", "GA"))})

bootpsiWaibiraAb<-data.frame(bootpsiWaibiraAb)
bootpsiWaibiraAb$Group<-"Waibira High SRI"
colnames(bootpsiWaibiraAb)<-c("Psi", "Group")#rename cols

bootpsiSonsoAb<-data.frame(bootpsiSonsoAb)
bootpsiSonsoAb$Group<-"Sonso High SRI"
colnames(bootpsiSonsoAb)<-c("Psi", "Group")#rename cols

bootpsiBossouAb<-data.frame(bootpsiBossouAb)
bootpsiBossouAb$Group<-"Bossou High SRI"
colnames(bootpsiBossouAb)<-c("Psi", "Group")#rename cols

#check how it is for data<1

SonsoOth<-EveryoneElse%>%filter(Social_unit =="Sonso")
WaibiraOth<-EveryoneElse%>%filter(Social_unit == "Waibira")
BossouOth<-EveryoneElse%>%filter(Social_unit == "Bossou")


bootpsiWaibiraOth<-sapply(1:1000, function(x){
  sdata.boot <- WaibiraOth[sample(1:nrow(WaibiraOth), replace = T),]
  return(optimality_score_function(sdata.boot, "morph", "GA"))})

bootpsiSonsoOth<-sapply(1:1000, function(x){
  sdata.boot <- SonsoOth[sample(1:nrow(SonsoOth), replace = T),]
  return(optimality_score_function(sdata.boot, "morph", "GA"))})

bootpsiBossouOth<-sapply(1:1000, function(x){
  sdata.boot <- BossouOth[sample(1:nrow(BossouOth), replace = T),]
  return(optimality_score_function(sdata.boot, "morph", "GA"))})

bootpsiWaibiraOth<-data.frame(bootpsiWaibiraOth)
bootpsiWaibiraOth$Group<-"Waibira Typical SRI"
colnames(bootpsiWaibiraOth)<-c("Psi", "Group")#rename cols

bootpsiSonsoOth<-data.frame(bootpsiSonsoOth)
bootpsiSonsoOth$Group<-"Sonso Typical SRI"
colnames(bootpsiSonsoOth)<-c("Psi", "Group")#rename cols

bootpsiBossouOth<-data.frame(bootpsiBossouOth)
bootpsiBossouOth$Group<-"Bossou Typical SRI"
colnames(bootpsiBossouOth)<-c("Psi", "Group")#rename cols


Graph<-bind_rows(bootpsiSonsoAb, bootpsiWaibiraAb,bootpsiSonsoOth,bootpsiWaibiraOth,bootpsiBossouOth,bootpsiBossouAb)

#Graph
library(ggplot2)
library(ggridges)
ggplot(Graph, aes(x = Psi, y = Group, fill=stat(x))) + geom_density_ridges_gradient(scale=0.9, rel_min_height =0.01) + #Not enough data
  scale_fill_viridis_c(name = "Psi", option = "H") +    theme_ridges() +
  scale_y_discrete(expand = c(0.05, 0)) +
  labs( x = "Psi", y="") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))+
  theme(axis.title.x = element_text(hjust= 0.5)) 

#Then I test whether the distributions are statistically different or not
res<-Graph%>%group_by(Group)%>%
  summarise(mean=mean(Psi))
Graph<-Graph%>%left_join(res)
Graph$res<-Graph$Psi-Graph$mean

library(car)
result <- ks.test(Graph$res, "pnorm")
print(result)


# Post-hoc Dunn's test
posthoc_dunn <- dunnTest(Psi ~ Group, data = Graph, method = "bonferroni") #all significant 
print(posthoc_dunn)

#Question 4 ----
library(lme4)
library(car)
library(ggplot2)
library(sjPlot)
library(parameters)
library(minqa)
AS_MainData<-read.csv( '/Users/as493/Documents/GitHub/PhD_Thesis/Data/AS_MainData_FINAL.csv')
library(dplyr)
subset.GA<-AS_MainData%>%filter(Ga_value %in% c("GA_stop_in", "MAU_GA_in" ) & Duration_analysis_include %in% c("ExcludeEnd", "Include"))%>%select(Ga_duration, Sri_Z, Social_unit, morph, Goal, Signaller)
subset.GA<-na.omit(subset.GA)
communitysplit<-subset.GA%>%group_by(Social_unit)%>%summarise(F=n())

library(lme4)
model<-lmer(log(Ga_duration)~Sri_Z*Social_unit + (1|morph) + (1|Goal) + (1|Signaller), data=subset.GA)

null.model<-lmer(log(Ga_duration) ~ 1 + (1|morph) + (1|Goal) + (1|Signaller), data=subset.GA)

vif_model<-lmer(log(Ga_duration) ~ Sri_Z + Social_unit + (1|morph) + (1|Goal) + (1|Signaller), data=subset.GA)
car::vif(vif_model)

par(mfrow = c(2,2))
qqnorm(resid(model)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(model))
hist(resid(model)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(model), resid(model)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 

anova(model, null.model) 

library(sjPlot)
modified_tab_model(model)
plot_model(model, type="int")



#Change reference point
subset.GA$Social_unit<-as.factor(subset.GA$Social_unit)
subset.GA$Social_unit <- relevel(subset.GA$Social_unit, ref = "Sonso")
model.ref<-lmer(log(Ga_duration)~Sri_Z*Social_unit + (1|morph) + (1|Goal) + (1|Signaller), data=subset.GA)

modified_tab_model(model.ref)

#Check within community
Bossou<-subset.GA%>%filter(Social_unit %in%'Bossou')
model.bossou<-lmer(log(Ga_duration)~Sri_Z + (1|morph) + (1|Goal) + (1|Signaller), data=Bossou)
par(mfrow = c(2,2))
qqnorm(resid(model.bossou)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(model.bossou))
hist(resid(model.bossou)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(model.bossou), resid(model.bossou)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 
model.bossou.null<-lmer(log(Ga_duration)~1 + (1|morph) + (1|Goal) + (1|Signaller), data=Bossou)
anova(model.bossou,model.bossou.null)
modified_tab_model(model.bossou)

Sonso<-subset.GA%>%filter(Social_unit %in%'Sonso')

model.Sonso<-lmer(log(Ga_duration)~Sri_Z + (1|morph) + (1|Goal) + (1|Signaller), data=Sonso)
par(mfrow = c(2,2))
qqnorm(resid(model.Sonso)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(model.Sonso))
hist(resid(model.Sonso)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(model.Sonso), resid(model.Sonso)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 
model.sonso.null<-lmer(log(Ga_duration)~1 + (1|morph) + (1|Goal) + (1|Signaller), data=Sonso)
anova(model.Sonso,model.sonso.null)
modified_tab_model(model.Sonso)


Waibira<-subset.GA%>%filter(Social_unit %in%'Waibira')
model.Waibira<-lmer(log(Ga_duration)~Sri_Z + (1|morph) + (1|Goal) + (1|Signaller), data=Waibira)
model.Waibira.null<-lmer(log(Ga_duration)~1 + (1|morph) + (1|Goal) + (1|Signaller), data=Waibira)
anova(model.Waibira,model.Waibira.null)
tab_model(model.Waibira)

subset.MAU<-AS_MainData%>%filter(Mau_value %in% c("MAU_in", "MAU_GA_in" ) & Duration_analysis_include %in% c("ExcludeEnd", "Include"))%>%select(Mau_duration, Sri_Z, Social_unit, morph, Goal, Signaller)

Mau_values<-AS_MainData%>%group_by(Social_unit, Mau_value)%>%summarise(f=n())

subset.MAU<-na.omit(subset.MAU)
communitysplit<-subset.MAU%>%group_by(Social_unit)%>%summarise(F=n())
model.mau<-lmer(log(Mau_duration)~Sri_Z*Social_unit + (1|morph) + (1|Goal) + (1|Signaller), data=subset.MAU)

null.model.mau<-lmer(log(Mau_duration) ~ 1 + (1|morph) + (1|Goal) + (1|Signaller), data=subset.MAU)

vif_model.mau<-lmer(log(Mau_duration) ~ Sri_Z + Social_unit + (1|morph) + (1|Goal) + (1|Signaller), data=subset.MAU)
car::vif(vif_model.mau)

par(mfrow = c(2,2))
qqnorm(resid(model.mau)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(model.mau))
hist(resid(model.mau)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(model.mau), resid(model.mau)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 

anova(model.mau, null.model.mau) 

library(sjPlot)
modified_tab_model(model.mau)
tab_model(model.mau)

library(sjPlot)
plot_model(model.mau, type="int")

install.packages("ggblend")
library(ggblend)

ggplot(subset, aes(x=Sri_Z, y=log(Mau_duration), color = Social_unit)) +
  scale_color_manual(values =colorBlindBlack8[c(3,2,6)] )+ stat_smooth(method = "lm") 

#Change reference point
subset.MAU$Social_unit<-as.factor(subset.MAU$Social_unit)
subset.MAU$Social_unit <- relevel(subset.MAU$Social_unit, ref = "Sonso")
model.ref<-lmer(log(Mau_duration)~Sri_Z*Social_unit + (1|morph) + (1|Goal) + (1|Signaller), data=subset.MAU)

modified_tab_model(model.ref)

#bossou
Bossou<-subset.MAU%>%filter(Social_unit %in% "Bossou")
model.bossou<-lmer(log(Mau_duration)~Sri_Z + (1|morph) + (1|Goal) + (1|Signaller), data=Bossou)
model.bossou.null<-lmer(log(Mau_duration)~1+ (1|morph) + (1|Goal) + (1|Signaller), data=Bossou)
anova(model.bossou, model.bossou.null)
modified_tab_model(model.bossou)

Sonso<-subset.MAU%>%filter(Social_unit %in% "Sonso")
model.Sonso<-lmer(log(Mau_duration)~Sri_Z + (1|morph) + (1|Goal) + (1|Signaller), data=Sonso)
model.Sonso.null<-lmer(log(Mau_duration)~1+ (1|morph) + (1|Goal) + (1|Signaller), data=Sonso)
anova(model.Sonso, model.Sonso.null)
tab_model(model.Sonso)

Waibira<-subset.MAU%>%filter(Social_unit %in% "Waibira")
model.Waibira<-lmer(log(Mau_duration)~Sri_Z + (1|morph) + (1|Goal) + (1|Signaller), data=Waibira)
model.Waibira.null<-lmer(log(Mau_duration)~1+ (1|morph) + (1|Goal) + (1|Signaller), data=Waibira)
anova(model.Waibira, model.Waibira.null)
tab_model(model.Waibira)

#Question 5----
library(dplyr)
library(lubridate)

Goal<-AS_MainData%>%
  group_by(Social_unit,Goal)%>%
  summarise(F=n())


sdata<-AS_MainData%>%filter(Goal=="Play")
sdata<-sdata[sdata$Duration_analysis_include!="Exclude",]
sdata<-sdata[sdata$Duration_analysis_include!="Exclude_start",]
#exclude rows without known MAU end time
sdata<-sdata[sdata$Mau_value!="MAU_ex",]
sdata<-sdata[sdata$Mau_value!="MAU_GA_ex",]
#remove unclear sequence parts
##sdata<-sdata[sdata$Sequence_part!="Unclear",] - don't really need this
#Remove gestures produced by unk individuals
sdata<-sdata[sdata$Signaller!="Unk",]
sdata<-sdata[sdata$Signaller!="A_F",]
sdata<-sdata[sdata$Signaller!="A_M",]
sdata<-sdata[sdata$Signaller!="SA_F",]
sdata<-sdata[sdata$Signaller!="SA_M",]
sdata<-sdata[sdata$Signaller!="J",]
sdata<-sdata[sdata$Signaller!="I",]

data<-sdata %>%
  group_by(Social_unit,morph) %>%
  summarize(median=median(Mau_duration),
            freq=n())%>%na.omit()

Sonso.data<-data%>%filter(Social_unit %in% "Sonso")
Waibira.data<-data%>%filter(Social_unit %in% "Waibira")
Bossou.data<-data%>%filter(Social_unit %in% "Bossou")
#sort them by duration
Sonso.data$duration_rank <- dense_rank(Sonso.data$median)
Sonso.data$duration_rrank<-Sonso.data$duration_rank/max(Sonso.data$duration_rank)

Waibira.data$duration_rank<-dense_rank(Waibira.data$median)
Waibira.data$duration_rrank<-Waibira.data$duration_rank/max(Waibira.data$duration_rank)

Bossou.data$duration_rank<-dense_rank(Bossou.data$median)
Bossou.data$duration_rrank<-Bossou.data$duration_rank/max(Bossou.data$duration_rank)
#Assign rank to each morph
AS_data_ranks_play<-bind_rows(Sonso.data, Waibira.data,Bossou.data)

AS_data_ranks_play<-AS_MainData%>%filter(Goal=="Play")%>%
  left_join(AS_data_ranks_play, by=c("morph", "Social_unit"))

Dyad_morph_play<-AS_data_ranks_play%>%
  select(Ga_duration, Mau_duration, median, duration_rrank, Signaller, Recipient, Social_unit, SRI, Sri_Z, morph, season)%>%
  na.omit()%>%
  group_by(Signaller, Recipient, season)%>%
  summarise(value=sum(duration_rrank),
            f=n(),
            relative_value=value/f,
            Social_unit=Social_unit,
            Sri_Z=Sri_Z)%>%unique()

Group_summaries<-Dyad_morph_play%>%
  group_by(Social_unit)%>%
  summarise(f=n())

sum(Group_summaries$f)

library(car)
hist(log(Dyad_morph_play$value))
morph.model.play<-lmer(relative_value ~ Sri_Z*Social_unit + (1|Signaller), data=Dyad_morph_play)
model.vif<-lmer(relative_value ~ Sri_Z+Social_unit + (1|Signaller), data=Dyad_morph_play) 
vif(model.vif)#Check for multicollinearity - anything below 2 is acceptable

par(mfrow = c(2,2))
qqnorm(resid(morph.model.play)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(morph.model.play))
hist(resid(morph.model.play)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(morph.model.play), resid(morph.model.play)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 
library(fitdistrplus)
descdist(Dyad_morph_play$value, boot=1000)


model.null<-lmer(relative_value ~ 1 + (1|Signaller), data=Dyad_morph_play)
anova(morph.model.play, model.null)

modified_tab_model(morph.model.play)
Dyad_morph_play$Social_unit<-as.factor(Dyad_morph_play$Social_unit)
Dyad_morph_play$Social_unit <- relevel(Dyad_morph_play$Social_unit, ref = "Sonso")
model4.ref<-lmer(relative_value ~ Sri_Z*Social_unit + (1|Signaller), data=Dyad_morph_play)

modified_tab_model(model4.ref)


bossou.q5<-Dyad_morph_play%>%filter(Social_unit%in%"Bossou")
model.bossou<-lmer(relative_value ~ Sri_Z + (1|Signaller), data=bossou.q5)
model.bossou.null<-lmer(relative_value ~ 1 + (1|Signaller), data=bossou.q5)
anova(model.bossou, model.bossou.null)
modified_tab_model(model.bossou)

Sonso<-Dyad_morph_play%>%filter(Social_unit%in%"Sonso")
model.sonso<-lmer(relative_value ~ Sri_Z + (1|Signaller), data=Sonso)
model.sonso.null<-lmer(relative_value ~ 1 + (1|Signaller), data=Sonso)
anova(model.sonso, model.sonso.null)
tab_model(model.Sonso)

Waibira<-Dyad_morph_play%>%filter(Social_unit%in%"Waibira")
model.Waibira<-lmer(relative_value ~ Sri_Z + (1|Signaller), data=Waibira)
model.Waibira.null<-lmer(relative_value ~ 1 + (1|Signaller), data=Waibira)
anova(model.Waibira, model.Waibira.null)
tab_model(model.Waibira)

#Goal

sdata<-AS_MainData%>%filter(Goal=="Groom")
sdata<-sdata[sdata$Duration_analysis_include!="Exclude",]
sdata<-sdata[sdata$Duration_analysis_include!="Exclude_start",]#exclude rows without known MAU end time
sdata<-sdata[sdata$Mau_value!="MAU_ex",]
sdata<-sdata[sdata$Mau_value!="MAU_GA_ex",]
#remove unclear sequence parts
##sdata<-sdata[sdata$Sequence_part!="Unclear",] - don't really need this
#Remove gestures produced by unk individuals
sdata<-sdata[sdata$Signaller!="Unk",]
sdata<-sdata[sdata$Signaller!="A_F",]
sdata<-sdata[sdata$Signaller!="A_M",]
sdata<-sdata[sdata$Signaller!="SA_F",]
sdata<-sdata[sdata$Signaller!="SA_M",]
sdata<-sdata[sdata$Signaller!="J",]
sdata<-sdata[sdata$Signaller!="I",]

data<-sdata %>%
  group_by(Social_unit,morph) %>%
  summarize(median=median(Mau_duration),
            freq=n())%>%
  na.omit()

Sonso.data<-data%>%filter(Social_unit %in% "Sonso")
Waibira.data<-data%>%filter(Social_unit %in% "Waibira")
Bossou.data<-data%>%filter(Social_unit %in% "Bossou")
#sort them by duration
Sonso.data$duration_rank <- dense_rank(Sonso.data$median)
Sonso.data$duration_rrank<-Sonso.data$duration_rank/max(Sonso.data$duration_rank)

Waibira.data$duration_rank<-dense_rank(Waibira.data$median)
Waibira.data$duration_rrank<-Waibira.data$duration_rank/max(Waibira.data$duration_rank)

Bossou.data$duration_rank<-dense_rank(Bossou.data$median)
Bossou.data$duration_rrank<-Bossou.data$duration_rank/max(Bossou.data$duration_rank)
#Assign rank to each morph
AS_data_ranks<-bind_rows(Sonso.data, Waibira.data,Bossou.data)    
n<-AS_MainData%>%filter(Goal=="Groom")%>%
  left_join(AS_data_ranks, by=c("morph", "Social_unit"))

str(n)

Dyad_morph_groom<-n %>%
  group_by(Signaller, Recipient, season)%>%
  summarise(relative_value=mean(duration_rrank),
            Social_unit=Social_unit,
            Sri_Z=Sri_Z)%>%
  unique()

Group_summaries<-Dyad_morph_groom%>%
  group_by(Social_unit)%>%
  summarise(f=n())
sum(Group_summaries$f)

library(ggplot2)
ggplot(Dyad_morph_groom, aes(x=Sri_Z, y=relative_value, color=Social_unit)) + geom_smooth(method = 'lm') + geom_point()
Dyad_morph_groom<-Dyad_morph_groom%>%na.omit()
hist(Dyad_morph_groom$relative_value)

Dyad_morph_groom$Social_unit<-as.factor(Dyad_morph_groom$Social_unit)
Dyad_morph_groom$Social_unit <- relevel(Dyad_morph_groom$Social_unit, ref = "Sonso")

morph.model.groom<-lmer(relative_value ~ Sri_Z*Social_unit +(1|Signaller), data=Dyad_morph_groom)

#Check for collinearity
model1<-lmer(relative_value ~ Sri_Z+Social_unit + (1|Signaller), data=Dyad_morph_groom)
library(car)
vif(model1)#Check for multicollinearity - anything below 2 is acceptable

par(mfrow = c(2,2))
qqnorm(resid(morph.model.groom)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(morph.model.groom))
hist(resid(morph.model.groom)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(morph.model.groom), resid(morph.model.groom)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 

model.null<-lmer(relative_value ~ 1 + (1|Signaller), data=Dyad_morph_groom)
anova(morph.model.groom, model.null)

modified_tab_model(model)

Dyad_morph_groom$Social_unit <- relevel(Dyad_morph_groom$Social_unit, ref = "Sonso")
model1<-lmer(relative_value ~ Sri_Z*Social_unit + (1|Signaller), data=Dyad_morph_groom)
modified_tab_model(model1)

#Community specific 

bossou.q5<-Dyad_morph_groom%>%filter(Social_unit%in%"Bossou")
model.bossou<-lmer(relative_value ~ Sri_Z + (1|Signaller), data=bossou.q5)
model.bossou.null<-lmer(relative_value ~ 1 + (1|Signaller), data=bossou.q5)
anova(model.bossou, model.bossou.null)
modified_tab_model(model.bossou)

Sonso<-Dyad_morph_groom%>%filter(Social_unit%in%"Sonso")
model.sonso<-lmer(relative_value ~ Sri_Z + (1|Signaller), data=Sonso)
model.sonso.null<-lmer(relative_value ~ 1 + (1|Signaller), data=Sonso)
anova(model.sonso, model.sonso.null)

Waibira<-Dyad_morph_groom%>%filter(Social_unit%in%"Waibira")
model.Waibira<-lmer(relative_value ~ Sri_Z + (1|Signaller), data=Waibira)
model.Waibira.null<-lmer(relative_value ~ 1 + (1|Signaller), data=Waibira)
anova(model.Waibira, model.Waibira.null)
tab_model(model.Waibira)

#Question 6----

library(dplyr)
Sonso<-AS_MainData%>%filter(Social_unit %in% "Sonso")
SonsoCommTime<-read.csv('/Users/as493/Documents/GitHub/PhD_Thesis/Data/SonsoIDSex2.csv')%>%dplyr::select(ID, First.seen)
str(SonsoCommTime)
SonsoCommTime$First.seen <- as.Date(SonsoCommTime$First.seen, format = "%d/%m/%Y") # create date for presence file
Sonso.sub<-Sonso%>%left_join(SonsoCommTime, by=c("Signaller"="ID"))%>%
  rename(First.seen.Sig=First.seen)
Sonso.sub<-Sonso.sub%>%left_join(SonsoCommTime, by=c("Recipient"="ID"))%>%
  rename(First.seen.Rcp=First.seen)

SonsoDep<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Sonso community independence individuals.csv")
SonsoDep<-tidyr::gather(SonsoDep,key="SeasonCode", value="Status", -individual)

t<-Sonso.sub%>%
  left_join(SonsoDep, by=c("Signaller"="individual", "season"="SeasonCode"))
names(t)[names(t) == 'Status'] <- 'StatusSig'

t<-t%>%
  left_join(SonsoDep, by=c("Recipient"="individual", "season"="SeasonCode"))
names(t)[names(t) == 'Status'] <- 'StatusRecip'

t<-t%>%unique()

t<-t%>%mutate(StatusSig = ifelse(StatusSig == "PREV", "Ind", StatusSig))
t<-t%>%mutate(StatusRecip = ifelse(StatusRecip == "PREV", "Ind", StatusRecip))

str(t)
Sonso.sub1<-t%>%dplyr::select(Signaller, Recipient, Date, Communication_number, Recording_number,Gesture_record, 
                              morph,Gesture_duration, Mau_duration, Ga_duration, SRI, First.seen.Sig, First.seen.Rcp, Goal, Sri_Z, StatusSig, StatusRecip, Sgn_age, Social_unit, Ga_value, Duration_analysis_include)


Sonso.sub1<-Sonso.sub1%>%filter(StatusSig %in% "Ind" & StatusRecip %in% "Ind")
# Create two variables with timestamps
Sonso.sub1$DateCom <- as.POSIXct(Sonso.sub1$Date)
Sonso.sub1$First.seen.Sig.Com <- as.POSIXct(Sonso.sub1$First.seen.Sig)
Sonso.sub1$First.seen.Rcp.Com <- as.POSIXct(Sonso.sub1$First.seen.Rcp)

Sonso.sub1$TimeComSig<-difftime( Sonso.sub1$DateCom,Sonso.sub1$First.seen.Sig.Com,  units = "weeks")
Sonso.sub1$TimeComRcp<-difftime( Sonso.sub1$DateCom,Sonso.sub1$First.seen.Rcp.Com, units = "weeks")
# Calculate the time difference in years
Sonso.sub1$TimeComSig<-Sonso.sub1$TimeComSig/52
Sonso.sub1$TimeComSig<-round(Sonso.sub1$TimeComSig)
Sonso.sub1$TimeComRcp<-Sonso.sub1$TimeComRcp/52
Sonso.sub1$TimeComRcp<-round(Sonso.sub1$TimeComRcp)

Sonso.sub1<-Sonso.sub1%>%mutate(Overlap = ifelse(TimeComSig>=TimeComRcp, TimeComRcp, TimeComSig))


Sonso.sub1$TimeComSig<-as.numeric(Sonso.sub1$TimeComSig)
Sonso.sub1$TimeComRcp<-as.numeric(Sonso.sub1$TimeComRcp)
Sonso.sub1$Overlap<-as.numeric(Sonso.sub1$Overlap)

#Repeat for Bossou
library(dplyr)
BossouAge<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Bossou ID list_Sex.csv")%>%select(ID,Birth.year)%>%na.omit()%>%mutate_all(toupper)
BossouAge<-read.csv("/Users/alexsafry/Desktop/PhD_Thesis/Data/Bossou ID list_Sex.csv")%>%dplyr::select(ID,Birth.year)%>%na.omit()%>%mutate_all(toupper)
BossouAge$ID<-ifelse(BossouAge$ID == "NA_CHIMP", "NA_chimp", BossouAge$ID)

t<-AS_MainData%>%filter(Social_unit %in% "Bossou")%>%left_join(BossouAge, by = c("Signaller" = "ID"))%>%rename(Sig_birth=Birth.year)
t<-t%>%left_join(BossouAge, by = c("Recipient" = "ID"))%>%rename(Rcp_birth=Birth.year)
t$Rcp_birth<-as.Date(t$Rcp_birth, format = "%Y")
t$Sig_birth<-as.Date(t$Sig_birth, format = "%Y")

t$Sig_birth <- as.POSIXct(t$Sig_birth)
t$Rcp_birth <- as.POSIXct(t$Rcp_birth)

t$DateCom<-as.POSIXct(t$Date)

t$SigComTime<-difftime( t$DateCom,t$Sig_birth,  units = "weeks")
t$RcpComTime<-difftime( t$DateCom,t$Rcp_birth, units = "weeks")

t$SigComTime<-t$SigComTime/52
t$SigComTime<-round(t$SigComTime)
t$RcpComTime<-t$RcpComTime/52
t$RcpComTime<-round(t$RcpComTime)

t<-t%>%mutate(Overlap = ifelse(SigComTime>=RcpComTime, RcpComTime, SigComTime))


t$SigComTime<-as.numeric(t$SigComTime)
t$RcpComTime<-as.numeric(t$RcpComTime)
t$Overlap<-as.numeric(t$Overlap)

t<-t%>%rename(TimeComRcp = RcpComTime, 
              TimeComSig = SigComTime)

#Add independence to dataset
library(tidyr)
BossouInd<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Bossou independence.csv")
BossouInd<-read.csv("/Users/alexsafry/Desktop/PhD_Thesis/Data/Bossou independence.csv")
BossouInd$Initials <- BossouInd$Initials  %>% replace_na("NA_chimp")
# get the column names of the data frame
colnames(BossouInd)[2:ncol(BossouInd)] <- substring(colnames(BossouInd)[2:ncol(BossouInd)], 2)
col_names <- names(BossouInd)

# print updated column titles
col_names[2:length(col_names)] <- paste0("01/12/", col_names[2:length(col_names)])

# set the modified column names to the data frame
names(BossouInd) <- col_names

BossouID<-read.csv("/Users/as493/Documents/GitHub/PhD_Thesis/Data/Bossou ID list.csv")
BossouID<-read.csv("/Users/alexsafry/Desktop/PhD_Thesis/Data/Bossou ID list.csv")
BossouID<-mutate_all(BossouID,toupper)
BossouID$ID <- BossouID$ID %>% replace_na("NA_chimp")
BossouID$Full_name <- BossouID$Full_name %>% replace_na("NA_chimp")

BossouID$ID<-toupper(BossouID$ID)
BossouID<-BossouID%>%mutate(ID=ifelse(ID=="NA_CHIMP", "NA_chimp", ID))
# Join Bossou.ranks with BossouID data by 'Initials' and 'ID' columns
df<-BossouInd
str(df)
df <- df %>%left_join(BossouID, by = c("Initials" = "ID")) %>%
  dplyr::select(-Initials)%>%  mutate(Full_name=ifelse(Full_name %in% "JÁ", "JA", Full_name))%>%
  rename(ID=Full_name)

str(df)
# Gather 'Year' and 'Rank' columns
df <- df %>%
  pivot_longer(cols = -ID, names_to = "Date", values_to = "Independece")

df$Date<-as.Date(df$Date, format = "%d/%m/%Y")

library(lubridate)
df <- df %>%
  mutate(date = ymd(Date),
         year = year(date)) %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ paste0("DecFeb_", ifelse(month(date) == 12, year + 1, year)),
    month(date) %in% c(3, 4, 5) ~ paste0("MarMay_", year),
    month(date) %in% c(6, 7, 8) ~ paste0("JunAug_", year),
    month(date) %in% c(9, 10, 11) ~ paste0("SepNov_", year)
  ))%>%dplyr::select(-date,-year)
df$season<-replace(df$season, df$season == "SepNov_2016", "DecFeb_2017")
df$season<-replace(df$season, df$season == "MarMay_1994", "DecFeb_1994")
df<-df%>%dplyr::select(-Date)


t<-t%>%
  left_join(df, by=c("Signaller"="ID", "season"="season"))
names(t)[names(t) == 'Independece'] <- 'StatusSig'

t<-t%>%
  left_join(df, by=c("Recipient"="ID", "season"="season"))
names(t)[names(t) == 'Independece'] <- 'StatusRecip'

BossouSubset<-t%>%filter(StatusSig ==1 & StatusRecip ==1)

BossouSubset<-BossouSubset%>%dplyr::select(Signaller, Recipient, Date, Communication_number, Recording_number,morph, Gesture_record, Gesture_duration, Mau_duration, Ga_duration, SRI, Goal, Sri_Z, Sgn_age, Overlap, TimeComSig, TimeComRcp, Social_unit, Recipient, Ga_value, Duration_analysis_include)

Sonso.sub1<-Sonso.sub1%>%dplyr::select(Signaller, Recipient, Date, Communication_number, Recording_number,Gesture_record,morph, Gesture_duration, Mau_duration, Ga_duration, SRI, Goal, Sri_Z, Sgn_age, Overlap, TimeComSig, TimeComRcp, Social_unit, Recipient, Duration_analysis_include, Ga_value)


Q6subset<-bind_rows(BossouSubset, Sonso.sub1)


GoalSummary<-Q6subset%>%group_by(Goal, Social_unit)%>%summarise(F=n())

Q6subset<-Q6subset%>%left_join(GoalSummary, by =c("Goal", "Social_unit"))
Q6subset.graph<-Q6subset%>%filter(F>=20)
Q6subset.graph<-Q6subset.graph%>%filter(Goal %in% c("AffilationContact","Groom", "DirectAttention", "StopBehaviour"))
levels(as.factor(Q6subset.graph$Goal))
ggplot(Q6subset.graph, aes(x=Overlap, y=log(Ga_duration), color=Social_unit)) + geom_smooth(method = "lm") + geom_point()+ facet_wrap(~Goal)

goal.summary<-Sonso%>%group_by(Goal)%>%summarise(F=n())

library(lme4)
library(sjPlot)
library(car)
Q6subset$Social_unit<-as.factor(Q6subset$Social_unit)
levels(Q6subset$Social_unit)
Q6subset<-Q6subset%>%dplyr::select(Ga_duration, Mau_duration, Overlap, Social_unit, TimeComSig, Signaller, Gesture_record, Goal, morph, Sri_Z, Recipient, Ga_value, Duration_analysis_include)%>%na.omit()
levels(as.factor(Q6subset$Ga_value))
Q6subset<-Q6subset%>%filter(Ga_value %in% c("MAU_GA_in", "GA_stop_in") & Duration_analysis_include %in% c("ExcludeEnd", "Include"))


Q6subset.graph<-Q6subset.graph%>%dplyr::select(Ga_duration, Mau_duration, Overlap, Social_unit, TimeComSig, Signaller, Gesture_record, Goal, morph, Sri_Z, Recipient)%>%na.omit()

Q6subset.sub.groom<-Q6subset%>%filter(Goal %in% "Groom")


Q6subset.sub.groom<-Q6subset.sub.groom%>%dplyr::select(Ga_duration, Mau_duration, Overlap, Social_unit, TimeComSig, Signaller, Gesture_record, Goal, morph, Sri_Z, Recipient)%>%na.omit()

Q6subset.sub.groom$Social_unit<-as.factor(Q6subset.sub.groom$Social_unit)
Q6subset.sub.groom$Social_unit<- relevel(Q6subset.sub.groom$Social_unit, ref = "Sonso")
library(car)
overlap.model<-lmer(log(Ga_duration)~ Overlap*Social_unit+Sri_Z+ (1|morph), data=Q6subset.sub.groom)
overlap.model.null<-lmer(log(Ga_duration)~ 1 +(1| morph), data=Q6subset.sub.groom)
anova(overlap.model,overlap.model.null)
modelx.vif<-lmer(log(Ga_duration)~ Overlap+Social_unit+Sri_Z+(1|Signaller) + (1|morph), data=Q6subset.sub.groom)
vif(modelx.vif)
par(mfrow = c(2,2))
qqnorm(resid(overlap.model)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(overlap.model))
hist(resid(overlap.model)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(overlap.model), resid(overlap.model)) #he plo

modified_tab_model(overlap.model)





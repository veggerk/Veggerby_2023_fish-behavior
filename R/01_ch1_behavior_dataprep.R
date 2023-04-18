
#data prep
# required packages
library(tidyverse)
library(here)

# load raw data
data1<-"master_behavior.csv"
data1 <- here("data/raw_data", data1)
data1 <- read_csv(data1,
                na = c("", "NA"))



# removed unused and/or duplicate columns
data1<-data1 %>%
  select(-EdgeOtherSide,-HabitatDetReg,-RefTreSiteNo,-ObservationDate,
         -Media_file,-Total_media_length, -FPS,-Modifiers,-Behavior_type,-Start,-Stop,-NewFG2,-NewFG3,
         -VideoDate_M,-VideoDate_D,-VideoDate_Y,-Date_eel,-Month_eel,-Day_eel,-Year_eel,-Region_eel,
         -Location_eel,-HabitatType_eel,-ReferenceFor_eel,-O_NumShootsDiagonal_eel,-O_MaxSheathWidth_eel,
         -O_MaxBladeLength_cm_eel,-O_SheathLength_cm_eel,-O_DryEpiphyteMass__eel,-O_DryAboveGroundEelgrassBiomass__eel)


# remove unwanted species and species with less than 3 observations (just midshipman)
data2<-data1[data1$Subject!="unknown non flatfish",]
data2<-data2[data2$Subject!="dogfish shark",]
data2<-data2[data2$Subject!="jelly fish",]
data2<-data2[data2$Subject!="harbor seal",]
data2<-data2[data2$Subject!="diving duck",]
data2<-data2[data2$Subject!="midshipman",]


#change names to broader groups
levels(data2$Subject) <- c(levels(data2$Subject), "crab", "flatfish","C.crab", "UnID.crab","UnID.flatfish","greenling") #can't assign non-pre-existing value without ading levels

data2$Subject[which(data2$Subject=="shiner perch")]="surf perch"
data2$Subject[which(data2$Subject=="cancer crab")]="crab"
data2$Subject[which(data2$Subject=="dungeness crab")]="crab"
data2$Subject[which(data2$Subject=="graceful crab")]="crab"
data2$Subject[which(data2$Subject=="unknown crab")]="UnID.crab"
data2$Subject[which(data2$Subject=="spider crab")]="crab"
data2$Subject[which(data2$Subject=="staghorn sculpin")]="sculpin"
data2$Subject[which(data2$Subject=="starry flounder")]="flatfish"
data2$Subject[which(data2$Subject=="unknown flatfish")]="flatfish"
data2$Subject[which(data2$Subject=="speckled sanddab")]="flatfish"
data2$Subject[which(data2$Subject=="surf smelt")]="forage fish"
data2$Subject[which(data2$Subject=="pacific sand lance")]="forage fish"
data2$Subject[which(data2$Subject=="white spotted greenling")]="greenling"


####### Remove Select Videos & Obs
#Remove edge videos [only keep center habitat measurements, edge only recorded during initial trial study]
unique(data2$CenterEdge)
data2=data2[data2$CenterEdge=="Center",]
unique(data2$CenterEdge) #double check no edge left in data

#HabitatDet column (main data)Remove all gear/grow out methods/species except oyster flipbag, clam, oyster on bottom
# some oyster long line and geoduck recorded, but long lines are becoming rare and were only at 1 site. 
# geoducks are beyond the scope of this study so these removed as well.
unique(data2$HabitatDet)
data2=data2[data2$HabitatDet=="Flipbag" | data2$HabitatDet=="Clam" | data2$HabitatDet=="Oyster OnBottom"| data2$HabitatDet=="eelgrass"| data2$HabitatDet=="sediment" |data2$HabitatDet=="filamentous algae",]
unique(data2$HabitatDet)

#habitat column (for reference-treatment analysis) Remove all gear/grow out methods/species except oyster flipbag, clam, oyster on bottom
unique(data2$Habitat)
data2=data2[data2$Habitat=="Flipbag" | data2$Habitat=="Clam" | data2$Habitat=="Oyster OnBottom"| data2$Habitat=="Clam/Flipbag"| data2$Habitat=="Flipbag/Oyster OnBottom" |data2$Habitat=="Clam/Oyster OnBottom",]
unique(data2$Habitat)

#Remove Samish West Fisk Bar as that site only had geoducks, which we are not using for this particular paper
unique(data2$Site)
data2=data2[data2$Site!="Samish West-Fisk Bar",]
unique(data2$Site)

#Remove Juan de Fuca site -  only a few videos every recorded, and not recorded properly so site not usable
data2=data2[data2$PugetSoundRegion!="JuanFuca",]
unique(data2$PugetSoundRegion)


# remove rivera sites, they were not recorded properly and shouldn't be used
data2=data2[data2$Site!="Rivera Eld",]
data2=data2[data2$Site!="Rivera Hammersley",]

# combine dosewallips sites to be one, which they are
data2$Site[which(data2$Site=="Taylor at Dosewallips")]="Dosewallips"

#Remove schools (as coded in BORIS) - Note there are still big groups of perch not considered school (just milling)
data2=data2[data2$Behavior!="s",]
unique(data2$Behavior)

# Summer 2017 & 2018 (no spring, no fall) 
unique(data2$Month)
data2=data2[data2$Month>5 & data2$Month<9 ,] #Only June, July, Aug
unique(data2$Month)




#add 3 level structure complexity column
data2$structure=NA
data2$structure[which(data2$HabitatDet=="Flipbag")]="HStr"
data2$structure[which(data2$HabitatDet=="eelgrass")]="HStr"
data2$structure[which(data2$HabitatDet=="filamentous algae")]="LStr"
data2$structure[which(data2$HabitatDet=="Clam")]="LStr"
data2$structure[which(data2$HabitatDet=="Oyster OnBottom")]="LStr"
data2$structure[which(data2$HabitatDet=="sediment")]="NoStr"

#add Aquaculture yes/no column
data2$Aq=NA
data2$Aq[which(data2$HabitatDet=="Flipbag")]="AqYes"
data2$Aq[which(data2$HabitatDet=="eelgrass")]="AqNo"
data2$Aq[which(data2$HabitatDet=="filamentous algae")]="AqNo"
data2$Aq[which(data2$HabitatDet=="Clam")]="AqYes"
data2$Aq[which(data2$HabitatDet=="Oyster OnBottom")]="AqYes"
data2$Aq[which(data2$HabitatDet=="sediment")]="AqNo"




# change behavior to numeric binary feeding or not feeding so R can correctly interpret response variable
data2$Behavior[which(data2$Behavior=="f")]="1"
data2$Behavior[which(data2$Behavior=="o")]="0"
data2$Behavior[which(data2$Behavior=="r")]="0"
data2$Behavior[which(data2$Behavior=="t")]="0"
unique(data2$Behavior)

colnames(data2)[colnames(data2) == "Behavior"] <- "feeding_behavior"
colnames(data2)[colnames(data2) == "HabitatDet"] <- "habitat_type"

# rename substrate type row values to more informative names
unique(data2$eelgrass_algae_bare)

data2$eelgrass_algae_bare[which(data2$eelgrass_algae_bare=="a")]="algae"
data2$eelgrass_algae_bare[which(data2$eelgrass_algae_bare=="ec")]="eelgrass"
data2$eelgrass_algae_bare[which(data2$eelgrass_algae_bare=="b")]="bare"
data2$eelgrass_algae_bare[which(data2$eelgrass_algae_bare=="ba")]="bare/algae"
data2$eelgrass_algae_bare[which(data2$eelgrass_algae_bare=="e")]="eelgrass"
data2$eelgrass_algae_bare[which(data2$eelgrass_algae_bare=="ab")]="bare/algae"
data2$eelgrass_algae_bare[which(data2$eelgrass_algae_bare=="ae")]="eelgrass"
unique(data2$eelgrass_algae_bare)


#final sanity check for if the data is cleaned up properly
unique(data2$feeding_behavior)
unique(data2$Site)
unique(data2$habitat_type)
unique(data2$PugetSoundRegion)
unique(data2$Subject)
unique(data2$CenterEdge)
unique(data2$eelgrass_algae_bare)

# write clean data file for analysis
clean_file_name_data <- "behavior_data.csv"
clean_data_loc <- here("data/clean_data", clean_file_name_data)
data2 %>% write_csv(clean_data_loc)



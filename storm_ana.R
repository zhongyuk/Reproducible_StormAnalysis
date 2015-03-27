
# Load the data
setwd('..')
setwd('./Desktop')
#storm <- read.csv('repdata-data-StormData.csv', header=TRUE)

storm <- read.csv(bzfile('repdata-data-StormData.csv.bz2', open='r'), header=TRUE)

# Select variables
varnames <- c('EVTYPE', 'FATALITIES', 'INJURIES', 'PROPDMG','PROPDMGEXP','CROPDMG','CROPDMGEXP')

stormsel <- subset(storm,select=varnames)
colnames(stormsel) <- tolower(colnames(stormsel))

# Consolidate/cleaning eventype
library(dplyr)
stormsolid <- mutate(stormsel, evtypesolid = as.character(tolower(evtype)))

evtable <- c('Astronomical Low Tide', 'Avalanche|avalance', 'Blizzard', 'Coastal Flood|coastal surge',
             'Wind Chill|Cold Chill|cool|low temp|record low',
             'Debris Flow','Dense Fog|fog|vog', 'Dense Smoke|smoke', 'Drought', 'Dust Devil|dust',
             'Dust Storm', 'Excessive Heat',
             'Extreme Cold|Wind Chill|cold', 'Flash Flood|flash floo', 'Flood|stream',
             'Frost|Freeze|glaze|ice|icy', 'Funnel Cloud|Funne', 'Freezing Fog|freezing spray','Hail',
             'Heat|hot|high temperature|record high|warm|dry',
             'Heavy Rain|wet|heavy shower|excessive precipitation|heavy precip|rain',
             'Heavy Snow|snow',
             'High Surf|surf','High Wind|wind|wnd','Hurricane|Typhoon', 'Ice Storm', 'Lake Effect Snow',
             'Lakeshore Flood', 'Lightning|lighting|ligntning|lights','Marine Hail', 'Marine High Wind',
             'Marine Strong Wind', 'Marine TSTM Wind|coastal storm','Rip Current', 'Seiche',
             'Sleet|wintry mix|mix precip|freezing drizz|wintery mix|mixed precip|winter mix|heavy mix',
             'Storm Surge|high sea|high swell|high water|rough seas|wave|marine mishap|heavy sea|rising water|tide',
             'Strong Wind',
             'thunderstorm|TSTM Wind|tstm','Tornado|gustnado|wall cloud|torndao','Tropical Depression',
             'Tropical Storm','Tsunami','Volcanic Ash|volcanic','Waterspout|water spout|wayterspout',
             'Wildfire|fire|red flag critera',
             'Winter Storm|microburst|downburst','Winter Weather', 'landslide|mudslide|rock slide|mud slide',
             'other|summary|none|temperature record|record temperature|monthly')
numbs <- seq(1,50,1)
i <- 1

for (event in evtable){
    indTemp <- grep(event, stormsolid$evtype, ignore.case=TRUE)
    stormsolid$evtypesolid[indTemp] <- i
    i <- i+1
}

markind <- stormsolid$evtypesolid %in% numbs
nonmarkind <- which(!markind)
others  <- stormsolid$evtypesolid[nonmarkind]


## Question 1: most harmful type of events with respect to health
indTemp <- grep('snow', stormsolid$evtypesolid, ignore.case=TRUE)
stormsolid$evtypesolid[indTemp] <- 22


markind <- stormsolid$evtypesolid %in% numbs
nonmarkind <- which(!markind)
stormsolid$evtypesolid[nonmarkind] <- 50


# group variables
health <- group_by(stormsolid, evtypesolid) %>% summarise(sum(fatalities), sum(injuries))
colnames(health) <- c('evetype', 'fatality', 'injury')

health <- mutate(health, fat_plus_inj = fatality + injury)
healthsort <- arrange(health,  desc(fat_plus_inj), desc(fatality), desc(injury))



# For plotting
healthplot <- healthsort[1:3,]
healthplot$evetype <- c("Tornado","Heat", "Thunderstorm")
library(lattice)
barchart(fatality+injury~evetype, data=healthplot, auto.key=TRUE,xlab="Severe Weather Event Type",
         ylab="Fatalities or Injuries", main="Top 3 Harmful Severe Weather Events to Population Health")





## Question 2: most harmful type of events with respect to economics
stormclean <- stormsolid
expnote <- c('0','1','h|H|2','K|3','4','5','K|6','7','8','M','B')
expnum <- c('1','10','100','1000','10000','100000','1000000','10000000','100000000','1000000000','1000000000')
i <- 1
stormclean$propdmgexp <- as.character(stormclean$propdmgexp)
stormclean$cropdmgexp <- as.character(stormclean$cropdmgexp)

for (exps in expnote){
    indTemp <- grep(exps, stormclean$propdmgexp, ignore.case=TRUE)
    stormclean$propdmgexp[indTemp] <- expnum[i]
    i = i+1
}

markind <- stormclean$propdmgexp %in% expnum
nonmarkind <- which(!markind)
stormclean$propdmgexp[nonmarkind] <- 1
stormclean$propdmgexp <- as.numeric(stormclean$propdmgexp)




cropexpnote <- c('0','1','h|H|2','K|3','4','5','K|k|6','7','8','M|m','B')
i <- 1
for (exps in cropexpnote){
    indTemp <- grep(exps, stormclean$cropdmgexp, ignore.case=TRUE)
    stormclean$cropdmgexp[indTemp] <- expnum[i]
    i=i+1
}

markind <- stormclean$cropdmgexp %in% expnum
nonmarkind <- which(!markind)
stormclean$cropdmgexp[nonmarkind] <- 1
stormclean$cropdmgexp <- as.numeric(stormclean$cropdmgexp)

stromclean <- mutate(stormclean, propdmgv = propdmg*propdmgexp, cropdmgv=cropdmg*cropdmgexp)
economy <- group_by(stromclean, evtypesolid) %>% summarise(sum(propdmgv,na.rm=TRUE),sum(cropdmgv,na.rm=TRUE))
colnames(economy) <- c('evetype','propdmg', 'cropdmg')
economy <- mutate(economy, totaldmg=propdmg+cropdmg)
economysort <- arrange(economy, desc(totaldmg), desc(propdmg),desc(cropdmg))


# For plotting
economyplot <- economysort[1:3,]
economyplot$evetype <- c("Tornado","Flood", "Hail")
barchart(propdmg+cropdmg~evetype, data=economyplot, auto.key=TRUE,xlab="Severe Weather Event Type",
         ylab="Property Damage or Crop Damage", main="Top 3 Economic Consequencial Severe Weather Events")

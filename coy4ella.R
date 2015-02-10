install.packages("RODBC")
install.packages("dplyr")

library(RODBC)
library(dplyr)

dir       <- "K:/Physiology/2014_15 data/TRC NC1502 Orchard database 2014.accdb" #establish file path
channel   <- odbcConnectAccess2007(dir)                                          #create ODBC connection channel to database
tabs      <- sqlTables(channel)                                                  #get dataframe of tables in the database connected via variable 'channel'
psaRaw    <- sqlFetch(channel, "Psa monitoring")                                 #fetch data from 'Psa monitoring' table
coyRaw    <- sqlFetch(channel, "TRC_COY converted")                              #fetch data from 'TRC_COY converted' table
frtcntRaw <- sqlFetch(channel, "TRC_FruitThin")                                  #fetch data from 'TRC_FruitThin' table
deetsRaw  <- sqlFetch(channel, "TRC_aDetails")                                   #fetch data from 'TRC_aDetails' table

x <- left_join(frtcntRaw, coyRaw, by="CaneID") #merge frtcnt & coy tables
x <- left_join(x, deetsRaw, by="CaneID")       #merge merged table & details table

names(x) <- gsub(" ",".",names(x)) #remove spaces from column names
names(x) <- gsub("#","",names(x))  #remove #tags from column names
names(x) <- gsub("-","",names(x))  #remove hyphens from column names

#create tidy dataframe
y <- data.frame(
  CaneID     =x$CaneID,
  UniqueFPI  =x$Unique.FPI,
  UniqueFPIx =x$Unique.FPI.x,
  UniqueFPIy =x$Unique.FPI.y,
  Sex        =x$Sex.y,
  Sel_yr     =x$Selyr,
  CaneInUse  =x$CaneInUse, #0=Not in use(FALSE)   1=In use(TRUE)
  King.flowers.per.bud=x$King.flowers.per.bud,
  King_Flowers=x$King_Flowers,
  FruitPreThin=x$FruitPreThin
)

y <- y[which(y$CaneInUse==1),]                #subset dataset where CaneInUse==1
y <- y[grep("[F]",y$Sex),]                    #subset dataset where sex=='F'
t <- y[(which(y$King.flowers.per.bud > 3)),]  #subset dataset where King flowers per windter bud are greater than 3

length(unique(t$Sel_yr)) #get number of Sel_yr

t$frtLossesPerc <- ((t$King_Flowers - t$FruitPreThin)/t$King_Flowers)*100

o <- order(t$frtLossesPerc)
t <- t[o,]

meltT <- melt(t, id.vars = c("Sel_yr"), measure.vars = c("King.flowers.per.bud", "King_Flowers", "FruitPreThin", "frtLossesPerc"))

reshapeT <- dcast(meltT, Sel_yr ~ variable, mean)

goldOut  <- reshapeT[grep("^Y",reshapeT$Sel_yr),]
greenOut <- reshapeT[grep("^G|^H|^Tm",reshapeT$Sel_yr),]
redOut   <- reshapeT[grep("^R",reshapeT$Sel_yr),]

write.csv(reshapeT, file="K:/Physiology/2014_15 data/rSummary/output.csv")

write.csv(goldOut, file="K:/Physiology/2014_15 data/rSummary/Goldoutput.csv")
write.csv(greenOut, file="K:/Physiology/2014_15 data/rSummary/Greenoutput.csv")
write.csv(redOut, file="K:/Physiology/2014_15 data/rSummary/Redoutput.csv")



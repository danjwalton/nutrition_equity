required.packages <- c("reshape2","ggplot2","WDI","data.table")
lapply(required.packages, require, character.only=T)

wd <- "G:/My Drive/Work/GitHub/nutrition_equity"
wd2 <- "G:/My Drive/Work/GitHub/p20_indicator_time_trends/data/DHSauto"

###Define functions
lorenzdist <- function(data, weights=1){
  df <- data.table(z=data, p=weights)
  df <- df[order(z)]
  if(df$p==1){
    df$P <- seq(0+1/nrow(df),1,1/nrow(df))
  } else {
    df$P <- df$p/sum(df$p)
    df$P <- cumsum(df$P)
  }
  df$pos.z <- df$z + 600 #abs(min(df$z))
  df$L <- df$pos.z/sum(df$pos.z)
  df$L <- cumsum(df$L)
  return(df)
}

ginicalc <- function(P,L){
  dP <- diff(P)
  dL <- diff(L)
  fL <- head(L,-1)
  dPL <- dP*dL
  dPfL <- dP*fL
  area <- sum(dPL) + sum(dPfL)
  gini <- 1-2*area
  return(gini)
}

###Metadata
setwd(wd)
dhsmeta<- fread("project_data/dhs_meta_data20190524.csv")
dhsmeta<- subset(dhsmeta, Recode.Structure.!="DHS-I")

dhsmeta$Country.[which(dhsmeta$Country.=="Cape Verde")]<-"Cabo Verde"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo")]<-"Congo, Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo Democratic Republic")]<-"Congo, Democratic Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Egypt")]<-"Egypt, Arab Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Gambia")]<-"Gambia, The"
dhsmeta$Country.[which(dhsmeta$Country.=="Yemen")]<-"Yemen, Republic of"
names(dhsmeta)[which(names(dhsmeta)=="Country.")] <- "CountryName"

dhsmeta$filename <- paste0(dhsmeta$dhs_cc,"HR",dhsmeta$dhs_recode_code,"DT")
dhsmeta <- dhsmeta[which(!is.na(dhsmeta$dhs_cc)),]

dhsmeta2 <- unique(dhsmeta[,c("CountryName","surveyyr","filename")])

#Generate single datafile with all height-for-age data together
if(!("stuntingrawDHS.RData" %in% list.files("project_data"))){
  setwd(wd2)
  #Surveys missing the Birth Recode section
  missing.br <- c(
    "aobr51fl.RData",
    "bfbr70fl.RData",
    "bubr6hfl.RData",
    "kebr7afl.RData",
    "lbbr61fl.RData",
    "lbbr71fl.RData",
    "mdbr71fl.RData",
    "mlbr70fl.RData",
    "mwbr6hfl.RData",
    "mwbr7ifl.RData",
    "rwbr6qfl.RData",
    "rwbr7afl.RData",
    "slbr71fl.RData",
    "snbr50fl.RData",
    "tgbr71fl.RData",
    "tzbr7ifl.RData", 
    "ugbr72fl.RData"
  )
  
  rdatas <- list.files(pattern="*.RData",ignore.case=T)
  rdatas <- substr(rdatas,1,nchar(rdatas)-6)
  dataList <- list()
  dataIndex <- 1
  #Iterate through all surveys identified by the DHSmeta scrape
  pb <- txtProgressBar(max=nrow(dhsmeta),style=3)
  for(i in 1:nrow(dhsmeta2)){
    dhsmetasubset <- dhsmeta2[i]
    country <- tolower(substr(dhsmetasubset$filename,1,2))
    recode <- tolower(substr(dhsmetasubset$filename,3,4))
    phase <- tolower(substr(dhsmetasubset$filename,5,6))
    subphase <- substr(dhsmeta2$filename,5,5)
    rdata_name <- paste0(country,recode,phase,"fl")
    #Find matching survey in DHSauto folder; otherwise, skip
    if(!(rdata_name %in% rdatas)){ next; }
    #Load individual recode
    if(exists("data")){rm(data)}
    pr_patha <- paste0(country,"pr",phase)
    pr_path <- paste0(tolower(pr_patha),"fl.RData")
    load(pr_path)
    #If individual recode includes height-for-age, keep this. Otherwise, load birth recode and look for height-for-age there.
    if(length(names(data)[which(names(data)=="hc70")])==1){
      dat <- data.table(V1=data$hc70, V2=data$hv005)
    } else {
      rm(data)
      br_patha <- paste0(country,"br",phase)
      br_path <- paste0(tolower(br_patha),"fl.RData")
      if(!(br_path %in% missing.br)){
        load(br_path)
        if(length(names(data)[which(names(data)=="hw5")])){
          dat <- data.table(V1=data$hw5, V2=data$v005)
        } else { next; }
      } else { next; }
    }
    rm(data)
    dat <- dat[complete.cases(dat)]
    if(nrow(dat)==0){next;}
    dat$V3 <- dhsmetasubset$filename
    dataList[[i]] <- dat
    setTxtProgressBar(pb,i)
  }
  data <- rbindlist(dataList)
  setwd(wd)
  save(data, file="project_data/stuntingrawDHS.RData")
} else {
  load("project_data/stuntingrawDHS.RData")
}

data <- data[V1 < 9900]
gini <- data[, .(Gini = ginicalc(lorenzdist(V1,V2)$P, lorenzdist(V1,V2)$L)), by=V3]

fwrite(gini, "project_data/Stunting Gini stats.csv")

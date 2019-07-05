required.packages <- c("reshape2","ggplot2","WDI","data.table")
lapply(required.packages, require, character.only=T)

wd <- "G:/My Drive/Work/GitHub/nutrition_equity"
wd2 <- "G:/My Drive/Work/GitHub/p20_indicator_time_trends/data/DHSauto"

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

dhsmeta$filename=paste0(dhsmeta$dhs_cc,"HR",dhsmeta$dhs_recode_code,"DT")
dhsmeta=dhsmeta[which(!is.na(dhsmeta$dhs_cc)),]

dhsmeta2 <- unique(dhsmeta[,c("CountryName","surveyyr","filename")])

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

####Run function####
setwd(wd2)

missing.br = c(
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

rdatas = list.files(pattern="*.RData",ignore.case=T)
rdatas = substr(rdatas,1,nchar(rdatas)-6)
dataList <- list()
dataIndex <- 1
pb = txtProgressBar(max=nrow(dhsmeta),style=3)
for(i in 1:nrow(dhsmeta2)){
  dhsmetasubset <- dhsmeta2[i]
  country <- tolower(substr(dhsmetasubset$filename,1,2))
  recode <- tolower(substr(dhsmetasubset$filename,3,4))
  phase <- tolower(substr(dhsmetasubset$filename,5,6))
  subphase <- substr(dhsmeta2$filename,5,5)
  rdata_name = paste0(country,recode,phase,"fl")
  if(!(rdata_name %in% rdatas)){ next; }
  if(exists("pr")){ rm(pr)}
  pr_patha <- paste0(country,"pr",phase)
  pr_path <- paste0(tolower(pr_patha),"fl.RData")
  load(pr_path)
  pr <- as.data.table(data)
  remove(data)
  if(length(names(pr)[which(names(pr)=="hc70")])){
    dat <- as.data.table(pr$hc70/100)
  } else {
    rm(pr)
    if(exists("br")){rm(br)}
    br_patha <- paste0(country,"br",phase)
    br_path <- paste0(tolower(br_patha),"fl.RData")
    if(!(br_path %in% missing.br)){
      load(br_path)
      br <- as.data.table(data)
      remove(data)
      dat <- as.data.table(br$hw5/100)
    } else { next; }
  }
  dat <- dat[complete.cases(dat)]
  if(nrow(dat)==0){next;}
  dat <- dat[V1 < 99]
  dat <- dat[order(V1)]
  dat$P <- seq(0+1/nrow(dat),1,1/nrow(dat))
  dat$z <- dat$V1 + abs(min(dat$V1))
  dat$L <- dat$z/sum(dat$z)
  dat$L <- cumsum(dat$L)
  dataList[[i]] <- as.data.table(cbind(country=dhsmetasubset$CountryName, year=dhsmetasubset$surveyyr, file=dhsmetasubset$filename, P=dat$P, L=dat$L))
  setTxtProgressBar(pb,i)
}

alldist <- rbindlist(dataList)
alldist.stats <- alldist[, .(Gini = ginicalc(as.numeric(P),as.numeric(L))), by=.(country,year,file)]

setwd(wd)
fwrite(alldist.stats, "project_data/Stunting gini stats.csv")

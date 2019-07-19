required.packages <- c("reshape2","ggplot2","WDI","data.table")
lapply(required.packages, require, character.only=T)

wd <- "G:/My Drive/Work/GitHub/nutrition_equity"
wd2 <- "G:/My Drive/Work/GitHub/p20_indicator_time_trends/data/DHSauto"

###Define functions
lorenzdist <- function(input,weights=1,offset=0){
  suppressWarnings({
    df <- data.table(z=input, p=weights)
    df <- df[order(z)]
    if(df$p==1){
      df$P <- seq(0+1/nrow(df),1,1/nrow(df))
    } else {
      df$p <- df$p/sum(df$p)
      df$P <- cumsum(df$p)
    }
    df$z.off <- df$z + offset
    df$z.weight <- df$z.off * df$p
    df$l <- df$z.weight/sum(df$z.weight)
    df$L <- cumsum(df$l)
    return(df)
  })
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

polarisationcalc <- function(input,weights=1,z,offset=0){
  dist <- lorenzdist(input,weights,offset)
  input.off <- input + offset
  g <- ginicalc(dist$P,dist$L)
  z.off <- z + offset
  dist$qdif <- abs(z.off - dist$z.off)
  dist$`dL/dP` <- c(0,diff(dist$L))/c(0,diff(dist$P))
  thresholdset <- dist[dist[, .I[which(dist$qdif==min(dist$qdif))]]]
  #grad <- coef(lm(thresholdset$L~thresholdset$P))[2]
  grad <- min(thresholdset$`dL/dP`)
  q <- min(thresholdset$P)
  lq <- min(thresholdset$L)
  b <- q - lq
  t <- b + (0.5-q)*(1-grad)
  gp <- (2/grad)*(2*t-g)
  return(gp)
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
equity <- data[, .(Gini=ginicalc(lorenzdist(V1,V2,600)$P, lorenzdist(V1,V2,600)$L), Polarisation_0=polarisationcalc(V1,V2,0,600), `Polarisation_-2`=polarisationcalc(V1,V2,-200,600),Mean=weighted.mean(V1,V2)/100), by=.(V3)]
equity <- merge(dhsmeta2, equity, by.x="filename", by.y="V3")

fwrite(equity, "project_data/Stunting equity stats.csv")

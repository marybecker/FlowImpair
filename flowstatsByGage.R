library(ggplot2)
library(lubridate)
library(reshape2)

setwd("P:/Projects/GitHub_Prj/FlowImpair")
indexgage<-read.csv("usgsindexgage.csv",header=TRUE)
indexgage$SiteNumber<-paste("0",indexgage$SiteNumber,sep="")

######Function to read in data from USGS data service correctly###############################
######assume one header line and tab delimited structure, with # as a comment out to skip#####
parse_fstat<-function(fstat_lines,skip='#',delim='\t'){
  x<-1;
  while(x<length(fstat_lines) && startsWith(fstat_lines[x],skip)){
    x<-x+1;
  }
  header<-strsplit(fstat_lines[x],delim)[[1]];
  D<-as.data.frame(matrix('',ncol=length(header),nrow=length(fstat_lines)-x),stringsAsFactors=F);
  colnames(D)<-header;
  for(i in x+2:length(fstat_lines)){
    r<-strsplit(fstat_lines[i],delim)[[1]];
    D[i-x-1,1:length(r)]<-r;
  }
  D
}

#character string from right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#########Get Flow Stat by Day for Each Gage#######################
############################################################################
  
  n<- dim(indexgage)[1]
  for (i in 1:n){  
  
  base_url    <-'https://waterservices.usgs.gov/nwis';
  start_date  <- '2017-01-01';
  end_date    <- '2018-01-01';
  parameterCd <-'00060';
  statCd      <-'00003';
  site        <- indexgage$SiteNumber[i] 
  siteName    <- indexgage$SiteName
  
  
  flowparts <- c(base_url,'/dv/?format=rdb',
                 '&sites=',       site,
                 '&startDT=',     start_date,
                 '&endDT=',       end_date,
                 '&statCd=', statCd,
                 '&parameterCd=', parameterCd,
                 '&siteType=',    'ST',
                 '&siteStatus=',   'all');
  
  flow_url<-paste(flowparts,sep='',collapse='');
  flow<-parse_fstat(readLines(flow_url))
  flow<-flow[which(flow$agency_cd!="NA"),]
  colnames(flow)[4]<-"q"
  
  #EXAMPLE URL FOR TEST
  #flow_url<-'https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=01187300&
  #startDT=1997-01-01&endDT=2017-05-01&statCd=00003&parameterCd=00060&siteType=ST&siteStatus=all'
  
  
  fstat_parts <- c(base_url,'/stat/?format=rdb,1.0',
                   '&sites=',      site,
                   '&statReportType=','daily',
                   '&statTypeCd=',      'all',
                   '&parameterCd=',   parameterCd);
  #EXAMPLE URL FOR TEST
  #fstat_url<- 'https://waterservices.usgs.gov/nwis/stat/?format=rdb,1.0&sites=01118300
  #&statReportType=daily&statTypeCd=all&parameterCd=00060';
  fstat_url<-paste(fstat_parts,sep='',collapse='');
  fstat<-parse_fstat(readLines(fstat_url)); 
  
  fstat$m<- paste("0",fstat$month_nu,sep="")
  fstat$m<- substrRight(fstat$m,2)
  fstat$d<- paste("0",fstat$day_nu,sep="")
  fstat$d<- substrRight(fstat$d,2)
  fstat$md<- paste(fstat$m,"-",fstat$d,sep="")
  flow$md<- substrRight(flow$datetime,5)
  fstat<-fstat[which(fstat$parameter_cd==parameterCd),]
  
  flowstat<- merge(flow,fstat,by="md",all.x=TRUE)
  flowstat<- transform(flowstat,q=as.numeric(q),max_va=as.numeric(max_va),min_va=as.numeric(min_va),
                       p10_va=as.numeric(p10_va),p25_va=as.numeric(p25_va),p75_va=as.numeric(p75_va),
                       p90_va=as.numeric(p90_va))
  
  # The seven percentile classes are defined as follows:
  #   
  # 1 = Lowest ever for the date at a streamgage
  # 2 = < 10th percentile
  # 3 = 10th to 24th percentile
  # 4 = 25th to 75th percentile
  # 5 = 76th to 90th percentile
  # 6 = > 90th percentile
  # 7 = Highest ever for the date at a streamgage
  
  flowstat$index<- ifelse(flowstat$q==flowstat$min_va,1,
                          ifelse(flowstat$q<=flowstat$p10_va &
                                   flowstat$q>flowstat$min_va,2,
                                 ifelse(flowstat$q>flowstat$p10_va & 
                                          flowstat$q<=flowstat$p25_va,3,
                                        ifelse(flowstat$q>flowstat$p25_va &
                                                 flowstat$q<=flowstat$p75,4,
                                               ifelse(flowstat$q>flowstat$p75 &
                                                        flowstat$q<=flowstat$p90,5,
                                                      ifelse(flowstat$q>flowstat$p90 &
                                                               flowstat$q<flowstat$max_va,6,
                                                             ifelse(flowstat$q==flowstat$max_va,7,NA)))))))
  
  write.csv(flowstat,paste("data/",site,".csv",sep=""),row.names=FALSE)
  
  }

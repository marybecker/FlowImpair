library(data.table)
library(ggplot2)
library(lubridate)

setwd("/Users/tbecker/Documents/Projects/2017/FlowImpairment")
indexgage<-read.csv("usgsindexgage.csv",header=TRUE)
indexgage$SiteNumber<-paste("0",indexgage$SiteNumber,sep="")

#assume one header line and tab delimited structure, with # as a comment out to skip
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

#build one site flow fstats index
get_sfindex<-function(site,base_url,start_date,end_date,parameterCd){
  flow_parts <- c(base_url,'/dv/?format=rdb',
                  '&sites=',       site,
                  '&startDT=',     start_date,
                  '&endDT=',       end_date,
                  '&statCd=',      '00003',
                  '&parameterCd=', parameterCd,
                  '&siteType=',    'ST',
                  '&siteStatus=',   'all');
  #flow_url<-'https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=01187300&startDT=1997-01-01&endDT=2017-05-01&statCd=00003&parameterCd=00060&siteType=ST&siteStatus=all'
  flow_url<-paste(flow_parts,sep='',collapse='');
  flow <- fread(flow_url);
  
  fstat_parts <- c(base_url,'/stat/?format=rdb,1.0',
                   '&sites=',      site,
                   '&statReportType=','daily',
                   '&statTypeCd=',      'all',
                   '&parameterCd=',   parameterCd);
  #fstat_url<- 'https://waterservices.usgs.gov/nwis/stat/?format=rdb,1.0&sites=01118300&statReportType=daily&statTypeCd=all&parameterCd=00060';
  fstat_url<-paste(fstat_parts,sep='',collapse='');
  fstat<-parse_fstat(readLines(fstat_url)); 
  
  fstat$m<- paste("0",fstat$month_nu,sep="")
  fstat$m<- substrRight(fstat$m,2)
  fstat$d<- paste("0",fstat$day_nu,sep="")
  fstat$d<- substrRight(fstat$d,2)
  fstat$md<- paste(fstat$m,"-",fstat$d,sep="")
  flow$md<- substrRight(flow$datetime,5)
  fstat<-fstat[which(fstat$parameter_cd==parameterCd),]
  
  n<-dim(flow)[1]
  flow<- flow[2:n,]
  colnames(flow)[4]<-"q"
  
  flowstat<- merge(flow,fstat,by="md")
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
  
  flowstat$index<- ifelse(flowstat[,5]==flowstat$min_va,1,
                          ifelse(flowstat[,5]<=flowstat$p10_va &
                                   flowstat[,5]>flowstat$min_va,2,
                                 ifelse(flowstat[,5]>flowstat$p10_va & 
                                          flowstat[,5]<=flowstat$p25_va,3,
                                        ifelse(flowstat[,5]>flowstat$p25_va &
                                                 flowstat[,5]<=flowstat$p75,4,
                                               ifelse(flowstat[,5]>flowstat$p75 &
                                                        flowstat[,5]<=flowstat$p90,5,
                                                      ifelse(flowstat[,5]>flowstat$p90 &
                                                               flowstat[,5]<flowstat$max_va,6,
                                                             ifelse(flowstat[,5]==flowstat$max_va,7,NA)))))))
  
  sfindex<-data.frame(flowstat$datetime,flowstat$index)
  colnames(sfindex)<-c("datetime",site)
  sfindex #return the sfindex
}



#character string from right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#reused parameters here
base_url    <-'https://waterservices.usgs.gov/nwis';
start_date  <-'1997-01-01';
end_date    <-'2017-05-01';
parameterCd <-'00060';
D<-list(); #our empty list of list data sructure
n<-2; #used for limiting test range in a functional loop
for(i in 1:dim(indexgage)[1]){#read out all the individual data tables
  site<-indexgage[i,1]
  sfindex<-get_sfindex(site,base_url,start_date,end_date,parameterCd);
  for(j in 1:dim(sfindex)[1]){ #read through the rows and load data into D
    d<-as.character(sfindex[j,'datetime']);
    v<-sfindex[j,site];
    if(is.null(D[[d]])){ #d is not in D =>(add a new datetime key and a new key site and index value v)
      D[[d]]<-list();
      D[[d]][[site]]<-v;
    }else{               #d is in D =>(add a new key site and index value v)
      D[[d]][[site]]<-v;
    }
  }
}

#[1]build a new empty dataframe E
n<-length(D);
m<- dim(indexgage)[1];
E<-matrix(NA,nrow=n,ncol=m);
dates<-sort(names(D))
colnames(E)<-indexgage[,1];
rownames(E)<-dates;
#[2]read from the D and write into E
for(i in 1:n){#for each datetime
  sites <- names(D[[dates[i]]]); #sites in that datetime
  for(j in 1:length(D[[dates[i]]])){#for each site
    E[i,sites[j]]<-D[[dates[i]]][[j]];
  }
}

##############################################################################################
E$year<- substr(row.names(E),1,4)

flowstat2016<-flowstat[which(flowstat$year=="2016"),]
flowstat2016summer<-flowstat2016[which(flowstat2016$month_nu>5&flowstat2016$month_nu<9),]
flowstat2016summer$datetime<- ymd(flowstat2016summer$datetime)

ggplot(flowstat2016summer,aes(datetime,index))+
  geom_line()+
  labs(y="streamflow index",x="date",title="Pendelton Hill Summer 2016")+
  ylim(1,7)

                

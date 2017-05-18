library(data.table)
library(ggplot2)
library(lubridate)
library(reshape2)

setwd("P:/Projects/GitHub_Prj/FlowImpair")
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

#character string from right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
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
  
  sfindex<-data.frame(flowstat$datetime,flowstat$index)
  colnames(sfindex)<-c("datetime",site)
  sfindex #return the sfindex
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

findex<-as.data.frame(E)
findex$index<-rowMeans(findex,na.rm=TRUE)     #only run if first time run, if index exists will include in avg
findex$gagecnt<-(rowSums(!is.na(findex)))-1  #only run -1 if first time run, if gagecnt already exists than -2

##############################################################################################
findex$sdate<- row.names(findex)
findex$sdate<- ymd(findex$sdate)

findexsummer2016<-findex[which(findex$sdate>='2016-06-01'&findex$sdate<'2016-09-01'),]
findexsummer2016lg<-findexsummer2016[,c(1:13,15)]
findexsummer2016lg<-melt(findexsummer2016lg,id=c("sdate","index"))
findexsummeravggage<-colMeans(findexsummer2016[,1:12],na.rm=TRUE)
findexsummeravggage<-melt(findexsummeravggage)
findexsummeravggage$gage<-row.names(findexsummeravggage)


findex$syear<- substr(findex$sdate,1,4)
findex$smonth<- substr(findex$sdate,6,7)
findexsummer<-findex[which(findex$smonth=='06'|findex$smonth=='07'|findex$smonth=='08'),]
findexsummeravg<-aggregate(findexsummer$index,list(findexsummer$syear),mean)
colnames(findexsummeravg)<-c("Year","Index")


ggplot(findexsummeravggage,aes(gage,value))+
  geom_bar(stat="identity")+
  labs(y="Average streamflow Index",title="Average summer (June - August) least disturbed streamflow index 2016")+
  theme_light()

ggplot(findexsummeravg,aes(Year,Index))+
  geom_bar(stat="identity")+
  labs(y="Average streamflow Index",title="Average summer (June - August) least disturbed streamflow index")+
  theme_light()
  

ggplot(findexsummer2016,aes(sdate,index))+
  geom_line(colour="red",size=1.5)+
  labs(y="Average streamflow index",x="2016",title="Least Disturbed Flow Index Summer 2016")+
  scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7),labels=c("Dry  1",2,3,"Normal  4",5,6,"Wet  7"))+
  theme_light()

ggplot(findexsummer2016lg, aes(sdate,value,colour=variable))+
  geom_line()+
  labs(y="Streamflow Index",x="2016",title="Least Disturbed Flow Index by Gage Summer 2016")+
  theme(legend.title=element_blank())



                

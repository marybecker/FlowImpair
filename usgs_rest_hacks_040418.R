library(ggplot2)
library(lubridate)
library(reshape2)

setwd("/home/mkozlak/Projects/GitHub/FlowImpair")
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


#########Function to Build flow tables and index stat#######################
############################################################################
get_sfindex<-function(site,base_url,start_date,end_date,parameterCd,statCd){
  
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
  
  sfindex<-data.frame(flowstat$datetime,flowstat$index)
  colnames(sfindex)<-c("datetime",site)
  sfindex #return the sfindex
  
}



##############Specify data and dates for flow builder#####################
##############Create empty list to combine gages into one table###########
base_url    <-'https://waterservices.usgs.gov/nwis';
start_date  <- '2017-01-01';
end_date    <- '2018-01-01';
parameterCd<-'00060';
statCd      <-'00003';

D<-list(); #our empty list of list data sructure
for(i in 1:dim(indexgage)[1]){#read out all the individual data tables
  site<-indexgage[i,1]
  sfindex<-get_sfindex(site,base_url,start_date,end_date,parameterCd,statCd);
  write.csv(sfindex,paste("data/",site,".csv",sep=""),row.names=FALSE)
  
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

findexlng<-melt(findex,id.vars=c("sdate","gagecnt"))##Convert findex from wide to long
write.csv(findexlng,"findexlng2017.csv")

#######Average SF Index Least Disturbed#####################
rects <- data.frame(ystart = c(-Inf,3,5), 
                    yend = c(3,5,Inf), 
                    cat = c("low flow","normal","high"),
                    col=c("white","gray30","gray85"))

xstart <- min(findex$sdate)-30  ##Specified for plot to ensure rects coverage
xend <- max(findex$sdate)+30

p<- ggplot()+
      geom_line(data=findex, aes(sdate,index))+
      geom_line(data=findexRG, aes(sdate,index,size=1))+
      labs(y="",x="")+
      scale_y_continuous(limits=c(1,7),breaks=c(2,4,6),labels=c("Low","Normal","High"))+
      theme(legend.position="none",panel.background=element_rect(fill="white",colour="black"))

p + geom_rect(data=rects,aes(xmin=xstart,xmax=xend,
                           ymin = ystart, ymax = yend),alpha = 0.3,fill=rects$col)+
    coord_cartesian(xlim=with(findex,range(sdate)))

#######Average SF Index Least Disturbed RG BioP#####################
findexRG<- findex
findexRG$month<- substr(findexRG$sdate,6,7)
findexRG<- findexRG[which(findexRG$month=="07"|findexRG$month=="08"|
                            findexRG$month=="09"|findexRG$month=="10"),]

rects <- data.frame(ystart = c(-Inf,3,5), 
                    yend = c(3,5,Inf), 
                    cat = c("low flow","normal","high"),
                    col=c("white","gray30","gray85"))

xstart <- min(findexRG$sdate)-30  ##Specified for plot to ensure rects coverage
xend <- max(findexRG$sdate)+30

p<- ggplot()+
  geom_line(data=findexRG, aes(sdate,index))+
  geom_line(data=gfindex, aes(sdate,gage,colour="red"))+##Added Bunnell to Compare
  labs(y="Stream Flow Index",x=NULL,title="Flow Conditions At 12 Least Disturbed Gages during 
       Rearing and Growth Bioperiod 2017 (Bunnell - Red Line)")+
  scale_y_continuous(limits=c(1,7),breaks=c(2,4,6),labels=c("Low","Normal","High"))

p + geom_rect(data=rects,aes(xmin=xstart,xmax=xend,
                             ymin = ystart, ymax = yend),alpha = 0.3,fill=rects$col)+
  coord_cartesian(xlim=with(findexRG,range(sdate)))+
  theme(legend.position="none",panel.background=element_rect(fill="white",colour="black"))

#######SF Index Individual Gage#####################
gage<-'01188000'
gfindex<- findex[,c(gage,'sdate')]
gfindex<-gfindex[which(gfindex[1]!="NA"),]
colnames(gfindex)[1]<-"gage"

rects <- data.frame(ystart = c(-Inf,3,5), 
                    yend = c(3,5,Inf), 
                    cat = c("low flow","normal","high"),
                    col=c("blue","purple","green"))

xstart <- min(gfindex$sdate)-30
xend <- max(gfindex$sdate)+30

ggplot()+
  geom_line(data=gfindex, aes(sdate,gage))+
  labs(y="",x="",title="Flow Conditions At Bunnell Brook 2017")+
  scale_y_continuous(limits=c(1,7),breaks=c(1,4,7),labels=c("Low","Normal","High"))+
  geom_rect(data=rects,aes(xmin=xstart,xmax=xend,
                             ymin = ystart, ymax = yend),alpha = 0.3,fill=rects$col)+
  coord_cartesian(xlim=with(gfindex,range(sdate)))

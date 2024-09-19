#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: KISDI Megatrend Project - Data Prep 03. EDA
##  goal : Megatrend Tech Analysis
##  Data set: KIPRIS
##  Time Span: 
##  Variables
##      Input: 
##      Output:  
##  Methodology: 
##  Time-stamp: :  
##  Notice :
#######################################################################

library('data.table')
library('dplyr')
library('readr')
library('magrittr')
library('ggplot2')
library('EconGeo')
library('igraph')
library('splitstackshape')
library('tidyr')

`%ni%` <- Negate(`%in%`)

country.code <- "KR"

load(file=paste0("R file/",country.code,"/CPC.RData"))

trend.1 <- c('G06Q 30/06','H04L 29/08','G06F 17/30','G06Q 20/40','G06Q 10/00',
             'G06Q 50/00','G06N 3/00','H04W 4/00','B60W 60/00','G06T 19/00')
trend.2 <- c('G06N 3/00','B25J 9/00','G06F 9/46','B25J 19/02','G05B 19/418',
             'G06Q 50/28','G16H 10/60','G06Q 50/24','G06F 3/01','G16Y 20/00')
trend.3 <- c('G06Q 30/02','G06F 17/30','G06N 3/08','G06Q 50/22','G06Q 50/24',
             'G06Q 40/00','G16H 10/60','H04L 29/08','G06Q 20/40','G06Q 10/00')
trend.4 <- c('G06T 19/00','G06F 3/01','G06N 3/08','G06F 17/30','H04W 4/00',
             'G06F 19/00','H04L 67/42','G16Y 20/00','G06Q 50/00','G06Q 40/06')
   
cpc.trend.1 <- CPC %>% filter(cpc코드 %in% trend.1) %>% mutate(type="플랫폼의 전방위적 확산")         
cpc.trend.2 <- CPC %>% filter(cpc코드 %in% trend.2) %>% mutate(type="자동화: 노동 형태 다변화")          
cpc.trend.3 <- CPC %>% filter(cpc코드 %in% trend.3) %>% mutate(type="초개인화-맞춤화")           
cpc.trend.4 <- CPC %>% filter(cpc코드 %in% trend.4) %>% mutate(type="가상화-융합화")           

cpc.trend.all <- rbind(cpc.trend.1, cpc.trend.2, cpc.trend.3, cpc.trend.4)
save(cpc.trend.all, file="R file/cpc.trend.all.RData")

cpc.trend.all %>% mutate(cpc4=substr(cpc코드,1,4)) %>%
  group_by(type,cpc4) %>% summarize(patent=length(unique(출원번호))) %>%
  ggplot(aes(x=type, y=patent, fill=cpc4)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw()

### Load Data
load(file="R file/cpc.trend.all.RData")

head(cpc.trend.all)
rm(CPC, cpc.trend.1, cpc.trend.2, cpc.trend.3, cpc.trend.4)

### Bibliographic
load(file=paste0("R file/",country.code,"/Bibliographic.RData"))

# remove NA columns
Bibliographic <- Bibliographic[,is.na(names(Bibliographic))==FALSE]

biblio.trend <- cpc.trend.all %>% select(출원번호,type) %>% unique %>%
  left_join(Bibliographic)
save(biblio.trend, file="R file/biblio.trend.RData")
rm(Bibliographic)

load(file="R file/biblio.trend.RData")

biblio.trend %>% mutate(year=substr(공개일자,1,4)) %>%
  group_by(year) %>% summarize(patent=length(unique(출원번호))) %>%
  filter(year!=" " & year != 2023) %>%
  ggplot(aes(x=year,y=patent,group="all")) + geom_line() +
  theme_bw()

biblio.trend %>% mutate(year=substr(공개일자,1,4)) %>%
  group_by(type,year) %>% summarize(patent=length(unique(출원번호))) %>%
  filter(year!=" " & year != 2023) %>%
  ggplot(aes(x=year,y=patent,group=type,color=type)) + 
  geom_line() +
  theme_bw()

biblio.trend %>% mutate(year=substr(공개일자,1,4)) %>%
  group_by(type) %>%
  summarize(pat=length(unique(출원번호)))

biblio.trend %>% mutate(year=substr(공개일자,1,4)) %>%
  filter(year>=2019) %>%
  group_by(type) %>%
  summarize(pat=length(unique(출원번호)))

biblio.trend %>% head(1)

### Rnd
load(file=paste0("R file/",country.code,"/Rnd.RData"))
Rnd %>% head(1)

# remove NA columns
Rnd <- Rnd[,is.na(names(Rnd))==FALSE]

rnd.trend <- cpc.trend.all %>% select(출원번호,type) %>% unique %>% 
  left_join(Rnd)
save(rnd.trend, file="R file/rnd.trend.RData")
write.csv(rnd.trend, file="Megatrend_data/rnd.trend.csv")
rm(Rnd)

rnd.trend %>% head(1)

rnd.trend %>% mutate(funding=ifelse(is.na(연구개발사업일련번호),"NoFunding","Funding")) %>%
  group_by(type,funding) %>%
  summarize(patent=length(unique(출원번호))) %>%
  ggplot(aes(x=type, y=patent, fill=funding)) +
  geom_bar(stat="identity", position="fill") + theme_bw()

rnd.trend %>% 
  filter(is.na(연구개발사업일련번호)==FALSE) %>%
  group_by(연구부처명) %>%
  summarize(patent=length(unique(출원번호))) %>%
  arrange(desc(patent)) %>% head(20) %>%
  ggplot(aes(x=연구부처명,y=patent)) +
  geom_bar(stat='identity') + theme_bw() +
  coord_flip()

rnd.trend %>% 
  filter(is.na(연구개발사업일련번호)==FALSE) %>%
  group_by(type,연구부처명) %>%
  summarize(patent=length(unique(출원번호))) %>%
  arrange(desc(patent)) %>% slice(1)

rnd.trend %>% 
  filter(is.na(연구개발사업일련번호)==FALSE) %>%
  group_by(주관기관명) %>%
  summarize(patent=length(unique(출원번호))) %>%
  arrange(desc(patent)) %>% head(20) %>%
  ggplot(aes(x=주관기관명,y=patent)) +
  geom_bar(stat='identity') + theme_bw() +
  coord_flip()

rnd.trend %>% 
  filter(is.na(연구개발사업일련번호)==FALSE) %>%
  group_by(type,주관기관명) %>%
  summarize(patent=length(unique(출원번호))) %>%
  arrange(desc(patent)) %>% slice(1)

rnd.trend %>% 
  filter(is.na(연구개발사업일련번호)==FALSE) %>%
  group_by(연구사업명) %>%
  summarize(patent=length(unique(출원번호))) %>%
  arrange(desc(patent)) %>% head(20) %>%
  ggplot(aes(x=연구사업명,y=patent)) +
  geom_bar(stat='identity') + theme_bw() +
  coord_flip()

rnd.trend %>% 
  filter(is.na(연구개발사업일련번호)==FALSE) %>%
  group_by(type,연구사업명) %>%
  summarize(patent=length(unique(출원번호))) %>%
  arrange(desc(patent)) %>% slice(1)

rnd.buis.fig <- list()
for (i in 1:length(unique(rnd.trend$type))){
  rnd.buis.fig[[i]] <- rnd.trend %>% 
    filter(is.na(연구개발사업일련번호)==FALSE) %>%
    filter(type==unique(rnd.trend$type)[i]) %>%
    group_by(연구사업명) %>%
    summarize(patent=length(unique(출원번호))) %>%
    arrange(desc(patent)) %>% head(10) %>%
    ggplot(aes(x=연구사업명,y=patent)) +
    geom_bar(stat='identity') + theme_bw() +
    coord_flip() + ggtitle(unique(rnd.trend$type)[i])
}

load(file=paste0("R file/",country.code,"/RelatedPerson.RData"))

# remove NA columns
RelatedPerson <- RelatedPerson[,is.na(names(RelatedPerson))==FALSE]

relatedperson.trend <- cpc.trend.all %>% select(출원번호,type) %>% unique %>%
  left_join(RelatedPerson)
rm(RelatedPerson)

save(relatedperson.trend, file="R file/relatedperson.trend.RData")

load(file="R file/relatedperson.trend.RData")

applicant.bar <- list()
for(i in 1:length(unique(relatedperson.trend$type))){
  applicant.bar[[i]] <- relatedperson.trend %>% 
    filter(관련인구분=="출원인(Applicant)") %>%
    filter(type==unique(relatedperson.trend$type)[i]) %>%
    mutate(country.type=ifelse(국가명=="대한민국(Republic of Korea)","KR","Others")) %>%
    group_by(type,성명,country.type) %>% summarize(patent=length(unique(출원번호))) %>% 
    ungroup %>% group_by(type) %>%
    arrange(desc(patent)) %>% slice(1:20) %>%
    ggplot(aes(x=성명, y=patent, fill=country.type)) + geom_bar(stat="identity") + theme_bw() +
    coord_flip() + ggtitle(unique(relatedperson.trend$type)[i])
}
applicant.bar[[4]]

i<-1  
inventor.bar <- list()
for(i in 1:length(unique(relatedperson.trend$type))){
  inventor.bar[[i]] <- relatedperson.trend %>% 
    filter(관련인구분=="발명자(Inventor)") %>%
    filter(type==unique(relatedperson.trend$type)[i]) %>%
    group_by(type,성명) %>% summarize(patent=length(unique(출원번호))) %>%
    arrange(desc(patent)) %>% slice(1:10) %>%
    ggplot(aes(x=성명, y=patent)) + geom_bar(stat="identity") + theme_bw() +
    coord_flip() + ggtitle(unique(relatedperson.trend$type)[i])
}

### CPC
load(file=paste0("R file/",country.code,"/CPC.RData"))
CPC %>% head(1)

cpc.trend <- cpc.trend.all %>% select(출원번호,type) %>% unique %>%
  left_join(CPC)
rm(CPC)

save(cpc.trend, file="R file/cpc.trend.RData")

cpc.trend %>% head

cpc.bar <- list()
for (i in 1:length(unique(cpc.trend$type))){
  cpc.bar[[i]] <- cpc.trend %>% 
    filter(type==unique(cpc.trend$type)[i]) %>%
    filter(cpc코드 %ni% (cpc.trend.all %>% filter(type==unique(cpc.trend$type)[i]) %>%
             select(cpc코드) %>% unique %>% unlist %>% as.vector())) %>%
    mutate(cpc=substr(cpc코드,1,1)) %>%
    group_by(type,cpc) %>% summarize(patent=length(unique(출원번호))) %>%
    ggplot(aes(x=type, y=patent, fill=cpc)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw() + ggtitle(unique(cpc.trend$type)[i])
}
cpc.bar[[4]]

i <- 1
cpc.trend %>% 
  filter(type==unique(cpc.trend$type)[i]) %>%
  filter(cpc코드 %ni% (cpc.trend.all %>% filter(type==unique(cpc.trend$type)[i]) %>%
                       select(cpc코드) %>% unique %>% unlist %>% as.vector())) %>%
  group_by(type) %>% summarize(cpc=length(unique(cpc코드)))

i <- 4
cpc.trend %>% 
  filter(type==unique(cpc.trend$type)[i]) %>%
  filter(cpc코드 %ni% (cpc.trend.all %>% filter(type==unique(cpc.trend$type)[i]) %>%
                       select(cpc코드) %>% unique %>% unlist %>% as.vector())) %>%
  mutate(cpc4=substr(cpc코드,1,4)) %>%
  group_by(cpc4) %>% summarize(patent=length(unique(출원번호))) %>% 
  arrange(desc(patent)) %>% head(1)

i<-1
cpc.trend %>% 
  filter(type==unique(cpc.trend$type)[i]) %>%
  filter(cpc코드 %ni% (cpc.trend.all %>% filter(type==unique(cpc.trend$type)[i]) %>%
                       select(cpc코드) %>% unique %>% unlist %>% as.vector())) %>%
  mutate(cpc=substr(cpc코드,1,1)) %>%
  group_by(type,cpc) %>% summarize(patent=length(unique(출원번호))) %>% head(1)



###########################################################################
### User-defined functions
###########################################################################

### [FUNCTION] GETTING EDGELIST
# Simpler version: available for big data set

getting.edgelist.local <- function(InputDF){
  
  # InputDF <- subset(app.inv.ipc, period==unique(app.inv.ipc$period)[[t]])
  # InputDF <- subset(pat.reg.cpc, US_CA_metro_name==unique(pat.reg.cpc$US_CA_metro_name)[[i]])
  # InputDF <- ctr.set.w
  
  #1, Making list (Extracting unique cpc code for each observation to each list)
  from = which(colnames(InputDF)=="1")
  to = which(colnames(InputDF)==colnames(InputDF)[length(InputDF)])
  tech.list <- apply(InputDF,1,function(y) unique(y[c(from:to)]))
  
  #2. Excluding (NA code in cpc code)
  tech.list <- lapply(tech.list,function(y) y[!is.na(y)])  
  
  max.len <- max(sapply(tech.list, length))
  corrected.list <- lapply(tech.list, function(x) {c(x, rep(NA, max.len - length(x)))})
  M1 <- do.call(rbind, corrected.list) 
  rm(corrected.list, tech.list)
  
  idx <- t(combn(max.len, 2))
  
  # Add row_names and remove NAs
  edgelist <- lapply(1:nrow(idx), function(ib) M1[, c(idx[ib, 1], idx[ib, 2])] %>% 
                       data.frame %>% # mutate(row_names=row.names(.)) %>% 
                       drop_na() # filter(is.na(X1)==FALSE)
  )
  # rm(M1)
  
  edgelist <- do.call(rbind, edgelist) 
  
  # Add firm_name by merging with row_names
  edgelist %<>% mutate(share=1) %>% filter(is.na(X2)==FALSE) %>%
    #   left_join(InputDF %>% mutate(row_names=row.names(.)) %>%
    #               select(pubid,row_names,share) %>% unique, by=c("row_names")) %>% unique %>% select(-c("row_names","APPLN_ID")) %>%
    #   # mutate(X2= case_when(is.na(X2)==TRUE ~ X1, is.na(X2)==FALSE ~ X2)) %>%
    dplyr::rename(Source=X1, Target=X2, weight=share)  # Firm=FULLNAMESTD,
  
  edgelist <- edgelist[rowSums(is.na(edgelist[1:2]))==0, ]
  edgelist <- edgelist[edgelist$Source!="", ]
  edgelist <- edgelist[edgelist$Target!="", ] 
  edgelist <- edgelist[!is.na(edgelist$weight), ]
  # edgelist <- arrange(edgelist,Source,Target)
  edgelist <- edgelist %>% group_by(Source, Target) %>% summarize_at('weight', sum, na.rm=T) # Firm,  
  G <- graph.data.frame(edgelist,directed=FALSE)
  EL.DF <- data.frame(get.edgelist(G), weight=round(E(G)$weight, 3))
  names(EL.DF)<-c("Source", "Target", "Weight")
  
  return(EL.DF)
} 
rm(InputDF,from,to,tech.list,max.len,corrected.list,M1,idx,edgelist, EL.DF, G)

undirected.graph.summary<-function(Graph.object){
  Net<-Graph.object
  # 1. no. nodes
  no.node=length(V(Net))
  # 2. no. edges
  no.edge=length(E(Net))
  # 3. Net density: the ratio of the number of edges and the number of possible edges.
  # net.density=edge_density(Net, loops = FALSE)
  net.density=sum(E(Net)$weight) / ((vcount(Net)*(vcount(Net)-1)))
  # 4. Network Diameter: the length of the longest shortcut.
  net.diameter=diameter(Net, directed = F, unconnected = TRUE, weights = NULL)
  # 5. Ave. path length: mean value in the length of all the shortest paths
  ave.path=mean_distance(Net, directed = F, unconnected = TRUE)
  # 6. Ave. CC: the probability that the adjacent vertices of a vertex are connected
  ave.cc=transitivity(Net, type="global")
  
  ##
  output<-data.frame(no.node=no.node, no.edge=no.edge, net.density=round(net.density,3), 
                     net.diameter=round(net.diameter,3), ave.path=round(ave.path,3), ave.cc=round(ave.cc,3))
  return(output)
}

### [FUNCTION]  Centrality function
get.centrality<-function(Graph.object){
  # Net<-G.eu.p[[t]]
  Net<-Graph.object
  output<-data.frame(
    Deg=igraph::degree(Net),
    w.Deg=igraph::strength(Net),
    Btw=igraph::betweenness(Net, normalized = T),
    Eig=igraph::eigen_centrality(Net)$vector,
    Close=igraph::closeness(Net)
    # hub=hub_score(Net)$vector,
    # authority=authority_score(g)$vector
    # PageRank=page_rank(Net, directed = F)$vector
  )
  setDT(output, keep.rownames = TRUE)[]
  colnames(output)[1]<-"Id"
  return(output)}

directed.graph.summary<-function(Graph.object){
  Net<-Graph.object
  # 1. no. nodes
  no.node=length(V(Net))
  # 2. no. edges
  no.edge=length(E(Net))
  # 3. Net density: the ratio of the number of edges and the number of possible edges.
  # net.density=edge_density(Net, loops = FALSE)
  net.density=sum(E(Net)$weight) / ((vcount(Net)*(vcount(Net)-1)))
  # 4. Network Diameter: the length of the longest shortcut.
  net.diameter=diameter(Net, directed = F, unconnected = TRUE, weights = NULL)
  # 5. Ave. path length: mean value in the length of all the shortest paths
  ave.path=mean_distance(Net, directed = F, unconnected = TRUE)
  # 6. Ave. CC: the probability that the adjacent vertices of a vertex are connected
  ave.cc=transitivity(Net, type="global")
  
  ##
  output<-data.frame(no.node=no.node, no.edge=no.edge, net.density=round(net.density,3), 
                     net.diameter=round(net.diameter,3), ave.path=round(ave.path,3), ave.cc=round(ave.cc,3))
  return(output)
}

###########################################################################
### Network Analysis
###########################################################################

load(file="R file/cpc.trend.RData")

for (i in 1:length(unique(cpc.trend$type))){
  
  w.cpc.table <- cpc.trend %>% 
    filter(type==unique(cpc.trend$type)[i]) %>%
    # mutate(cpc4=substr(cpc코드,1,4)) %>%
    group_by(출원번호) %>% mutate(count=length(unique(cpc코드))) %>%
    mutate(share=1/count) %>% ungroup %>%
    group_by(출원번호,cpc코드) %>%
    dplyr::summarise(weight=sum(share)) %>% ungroup %>%
    select(-c("weight"))
  save(w.cpc.table, 
       file=paste0("R file/w.cpc.table_",unique(cpc.trend$type)[i],".RData"))
  
  # generating ID (1,2,3,4...) by Application ID
  cpc.set <- getanID(w.cpc.table, id.vars = "출원번호")
  colnames(cpc.set)[colnames(cpc.set)==".id"] <- "compound"
  cpc.set$compound <- as.factor(cpc.set$compound)
  
  # create CPC matrix: each row shows individual patent's list of CPCs
  cpc.set.w <- spread(cpc.set, compound, cpc코드)
  save(cpc.set.w, 
       file=paste0("R file/cpc.set.w_",unique(cpc.trend$type)[i],".RData"))

  ALL.EL.total.all <- getting.edgelist.local(cpc.set.w)
  ALL.EL.total.all %<>% dplyr::rename(weight=Weight)
  save(ALL.EL.total.all, 
       file=paste0("R file/ALL.EL.total.all_",unique(cpc.trend$type)[i],".RData"))
  
  # non-weight version  
  ALL.G.p.all <- graph.data.frame(ALL.EL.total.all, directed=FALSE)
  save(ALL.G.p.all, 
       file=paste0("R file/ALL.G.p.all_",unique(cpc.trend$type)[i],".RData"))
  
  ALL.EL.p.all <- data.frame(get.edgelist(ALL.G.p.all),
                             weight=round(E(ALL.G.p.all)$weight, 3))
  names(ALL.EL.p.all) <- c("Source", "Target", "weight")
  
  ALL.EL.p.all <- ALL.EL.p.all %>% 
    group_by(Source, Target) %>%
    summarize_at("weight", sum)
  save(ALL.EL.p.all, 
       file=paste0("R file/ALL.EL.p.all_",unique(cpc.trend$type)[i],".RData"))

  ID <- sort(unique(c(ALL.EL.p.all$Source, ALL.EL.p.all$Target)))
  Node.table.all <- data.frame(Id=ID, Label=ID, tech=substr(ID, 0,1))
  
  # Network analysis
  summ.cen.all <- get.centrality(ALL.G.p.all) %>% 
    mutate(type = unique(cpc.trend$type)[i])
  save(summ.cen.all, 
       file=paste0("R file/summ.cen.all_",unique(cpc.trend$type)[i],".RData"))
  print(i)
}

i<-1
load(file=paste0("R file/ALL.EL.p.all_",unique(cpc.trend$type)[i],".RData"))
ALL.EL.p.all %>% arrange(desc(weight)) %>%
  head(10) %>%
  left_join()


deg.btw.fig <- list()
for (i in 1:length(unique(cpc.trend$type))){
  load(file=paste0("R file/summ.cen.all_",unique(cpc.trend$type)[i],".RData"))
  
  deg.btw.fig[[i]] <- summ.cen.all %>%  
    mutate(w.Deg=log(w.Deg+1)/max(log(w.Deg+1)), Btw=log(Btw+1)/max(log(Btw+1))) %>%
    left_join(cpc.trend.all %>% 
                # mutate(cpc4=substr(cpc코드,1,4)) %>%
                select(type, cpc코드) %>% unique %>%
                mutate(label='key') %>% rename(Id=cpc코드)) %>%
    mutate(label=ifelse(is.na(label),"others","key")) %>%
    ggplot(aes(x=w.Deg, y=Btw, label=Id, color = label)) +
    geom_hline(yintercept=0.5, col="red") + geom_vline(xintercept=0.5, col="red") +
    geom_point() +
    geom_text(check_overlap = TRUE) +
    xlab("Weighted Degree Centrality") +
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(limits = c(0, 1)) +
    ylab("Betweenness Centrality") + theme_bw() +
    ggtitle(unique(cpc.trend$type)[i])
  
}
i <- 4
deg.btw.fig[[i]]

load(file=paste0("R file/summ.cen.all_",unique(cpc.trend$type)[i],".RData"))

summ.cen.all %>%  
  mutate(w.Deg=log(w.Deg+1)/max(log(w.Deg+1)), Btw=log(Btw+1)/max(log(Btw+1))) %>%
  left_join(cpc.trend.all %>% 
              # mutate(cpc4=substr(cpc코드,1,4)) %>%
              select(type, cpc코드) %>% unique %>%
              mutate(label='key') %>% rename(Id=cpc코드)) %>%
  mutate(label=ifelse(is.na(label),"others","key")) %>%
  arrange(desc(w.Deg)) %>% head(10)

##################################

## Visualize network
res_node_info <- res.org.df %>%
  select(Id, display_name, final_grouped_organization, country) %>%
  unique() %>%
  mutate(color = ifelse(country %in% eu_country, "blue", "red"))

res_graph_data <- res.ALL.EL.total.all %>%
  left_join(res_node_info %>% select(Id, display_name, country, color),
            by = c("Source" = "Id")) %>%
  rename(source.researcher = display_name, source.country = country, source.color = color) %>%
  left_join(res_node_info %>% select(Id, display_name, country, color),
            by = c("Target" = "Id")) %>%
  rename(target.researcher = display_name, target.country = country, target.color = color) %>%
  filter(source.country %in% eu_country | target.country %in% eu_country) %>%
  arrange(desc(weight)) %>%
  filter(!is.na(source.researcher) & !is.na(target.researcher)) %>%
  head(100) 

net <- graph.data.frame(res_graph_data %>%
                          select(Source, Target),
                        directed = FALSE)

# 노드 정보 추가 (Id를 사용하여 라벨과 색상 설정)
V(net)$label <- 
  ifelse(V(net)$name %in% res_node_info$Id, 
         res_node_info$display_name[match(V(net)$name, res_node_info$Id)], 
         V(net)$name)
V(net)$color <- 
  ifelse(V(net)$name %in% res_node_info$Id, 
         res_node_info$color[match(V(net)$name, res_node_info$Id)], 
         "grey")

# 그래프 시각화
ggnet2(net, size = "degree", color = "color", label = FALSE, 
       label.size = 1.5, label.trim = TRUE, alpha = 0.5, 
       edge.color = "grey", edge.alpha = 0.5) +
  guides(size = FALSE)


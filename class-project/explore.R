setwd("./")
x<-read.table("GTEx-12WSJ-exome-intersect.recode.vcf")
x$region<-paste(x[,1],x[,2],sep=":")
z<-read.table("GTEX-12WSJ_calls.tsv",header=T,stringsAsFactors=F)


z<-z[-which(z$region %in% x$region),]
z$ratio<-(z$site_total_alt_count/(z$site_total_alt_count + z$site_total_ref_count))
z<-z[which(z$altcount>5 & z$ratio<0.001),]

#744 calls left after this; 252 are unique


#1097 369 unique
z<-read.table("GTEX-12KS4_calls.tsv",header=T,stringsAsFactors=F)
z<-z[-which(z$region %in% x$region),]
z$ratio<-(z$site_total_alt_count/(z$site_total_alt_count + z$site_total_ref_count))
z<-z[which(z$altcount>5 & z$ratio<0.001),]


#Interesting set of sites in POU6F2
#NO MULTI TISSUE HITS. TISSUES ARE PREDOMINANT ESOPHAGUS MUSCULARIS AND SKIN NOT SUN EXPOSED
pou<-which(z$chr==17 & z$pos>38975956 & z$pos<39740075)


igk<-which(z$chr==2 & z$pos>89156873 & z$pos<90458465)
igl<-which(z$chr==22 & z$pos>22385572 & z$pos<23265082)
igh<-which(z$chr==14 & z$pos>105994256 & z$pos<107283085)
tmp<-apply(z,MAR=1,FUN=function(x){z<-as.numeric(x[7:10]); sum(z>0)})
dups<-z$region[duplicated(z$region)]
z$sratio<-z$altcount/z$depth
lines(density(z$sratio[c(igk,igh,igl)]))
plot(density(z$sratio[z$region %in% dups]),col=2)

#INVESTIGATING ALT READ COUNT THRESHOLDS
z<-read.table("GTEX-12WSJ_exomesites_rnaseq_readcounts/full_list.txt",header=T)
x<-read.table("GTEX-12WSJ.recode.vcf.gz")
x$id<-paste(x[,1],x[,2])
z$id<-paste(z$chr,z$pos)
#PULL OUT HET AND HOM REF SITES SEPARATELY
hets<-x$id[grep("0/1",x[,10])]
homs<-x$id[grep("0/0",as.character(x[,10]))]
summary(z$altcount[z$id %in% hets])

sum(z$altcount[z$id %in% hets]<6)/sum(z$depth[z$id %in% hets]>20)
#15% FALSE NEGATIVE RATE USING 6 READ THRESHOLD

sum(z$altcount[z$id %in% homs]>5)/sum(z$depth[z$id %in% homs]>20)

##### INVESTIGATE DISTRIBUTION OF MUTATIONS IN FULL DATASET (AFTER SOME MODEST FILTERING) 

setwd("./")
x<-read.table("uniq_filt_nonig_ann.bed",header=F)
x$gene<-gsub('\\S+SYMBOL=(\\S+);SYMBOL_SOURCE\\S+','\\1',x[,14])
syn<-x[which(x[,7]=="synonymous_variant"),]
syn<-syn[!duplicated(syn[,1]),]

test<-sapply(syn[,8],FUN=function(x){eval(parse(text=as.character(x)))})
hist(t2,nclass=100)
mis<-x[which(x[,7]=="missense_variant"),]
mis<-mis[!duplicated(mis[,1]),]

t2<-sapply(mis[,8],FUN=function(x){eval(parse(text=as.character(x)))})
lof<-x[which(x[,7]=="stop_gained"),]

t3<-sapply(lof[,8],FUN=function(x){eval(parse(text=as.character(x)))})
t4<-sapply(lof[,10],FUN=function(x){eval(parse(text=as.character(x)))})

t4<-lof[lof[,4]=="ENSG00000115414",]
t5<-sapply(t4[,10],FUN=function(x){eval(parse(text=as.character(x)))})

t4<-lof[lof[,4]=="ENSG00000165795",]
t5<-sapply(t4[,10],FUN=function(x){eval(parse(text=as.character(x)))})

cg<-read.table("~/gtex/mut/cancer/gene_list_somatic_mut_GTEx.txt",header=T) #CANCER GENES
#DNDS
 t2<-table(mis$gene)
 t3<-table(syn$gene)
 t3<-t3[names(t3) %in% names(t2)]
 t2<-t2[names(t2) %in% names(t3)]
 
 
 #LOOK AT WHETHER BEING AFRICAN AMERICAN PROTECTS AGAINST SUN EXPOSED SKIN MUTATIONS
 
ann<-read.table("~/gtex/GTEx_Data_2014-06-13_Annotations_SubjectPhenotypesDS.txt",header=T,sep="\t",comment.char="",quote="") 
x<-read.table("/Users/dconrad/gtex/mut/avi/full/all_calls_sorted_filtered_11.tsv.gz")
tmp<-table(x[,23])
x<-x[!x[,23] %in% names(tmp)[tmp>300],]
z<-x[which(x[,20] %in% "Skin - Sun Exposed (Lower leg)"),]
tmp<-table(z[,19])
t2<-data.frame(tmp)
t3<-merge(t2,ann[,1:6],by.x=1,by.y=1)

tissue.count<-table(x[,20])
tissue.count<-tissue.count[tissue.count>100]
bmat<-matrix(nrow=length(tissue.count),ncol=8)
pdf("~/gtex/mut/avi/full/mut_cov.pdf")
par(mfcol=c(1,2))

for (i in 1:length(tissue.count)){
z<-x[which(x[,20] %in% names(tissue.count)[i]),]
tmp<-table(z[,19])
t2<-data.frame(tmp)
t3<-merge(t2,ann[,1:6],by.x=1,by.y=1)
t3<-t3[t3$RACE %in% c(2,3),]
t3<-t3[t3$Freq>0,]
if (dim(t3)[1]==0){next}
tlm<-glm.nb(Freq ~ AGE+GENDER+RACE,data=t3) #2 is female #1 is male
summary(tlm)
bmat[i,1]<-as.character(names(tissue.count)[i])
bmat[i,2:4]<-as.numeric(summary(tlm)$coeff[14:16])
bmat[i,5:7]<-as.numeric(summary(tlm)$coeff[10:12])
bmat[i,8]<-as.numeric(tissue.count[i])
plot(t3$AGE,t3$Freq,main=paste(names(tissue.count)[i],bmat[i,2],sep="\n"))
boxplot(Freq ~ RACE, data=t3)

}
dev.off()
colnames(bmat)<-c("tissue","pv1","pv2","pv3","e1","e2","e3","count")
write.table(bmat,"tmp.out",row.names=F,quote=F,sep="\t")
bmat<-read.table("tmp.out",sep="\t",stringsAsFactors=F,header=T)

ann<-read.table("~/gtex/ann/phs000424.v6.pht002743.v6.p1.c1.GTEx_Sample_Attributes.GRU.txt",header=T,sep="\t",quote="",fill=T)
sra<-read.table("~/gtex/ann/SRARunTable.txt",header=T,sep="\t")
ann<-merge(ann,sra[,c(9,11)],by.x=2,by.y=2)

x<-read.table("/Users/dconrad/gtex/mut/avi/sig/memberships_k5.tsv",header=T)

ann<-ann[which(ann$Run_s %in% x[,1]),]
x<-x[which(x[,1] %in% ann$Run_s),]
x<-x[order(x[,1]),]
ann<-ann[order(ann$Run_s),]
z<-as.matrix(x[,-1],nrow=7717,ncol=5)
ann$New<-0
ann$New[which(ann$SMGEBTCHD %in% c("04/02/2012","05/21/2012","06/02/2012","06/08/2012","08/31/2012","09/02/2012","09/11/2012","09/27/2012","10/09/2013"))]<-1

drop<-which(x[,4]>0.5)
x<-x[-drop,]
ann<-ann[-drop,]

#WHAT SIGNATURES ARE STRONGLY ASSOCIATED WITH BATCH?
#HOW DO WE DECIDE? SEEMS LIKE ALL HAVE AT LEAST ONE STRONG BATCH ASSOCIATION
#DO WE REQUIRE THAT THE LARGEST EFFECT SIZE BE DUE TO SAMPLE TYPE?
tmp<-lm(x[,2]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
anova(tmp)

tmp<-lm(x[,6]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT+x$n_mut)
anova(tmp)

tmp<-lm(x[,4]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
anova(tmp)

tmp<-lm(x[,5]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
anova(tmp)

tmp<-lm(x[,6]~ann$SMGEBTCH+ann$SMTS+ann$SMRIN+ann$SMCENTER+ann$SMATSSCR+ann$SMTSPAX+ann$SMNABTCHT)
anova(tmp)

nmut<-read.table("/Users/dconrad/gtex/mut/avi/sig/tissues_nmuts.tsv",header=T,sep="\t")
x<-merge(x,nmut,by.x=1,by.y=2)

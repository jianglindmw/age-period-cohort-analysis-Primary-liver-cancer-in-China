library(Epi)

###data import###
HKF =read.table("C:/Users/Jiang/Desktop/APC-data/HK-F.txt" ,fileEncoding = "UTF16")
HKM =read.table("C:/Users/Jiang/Desktop/APC-data/HK-M.txt",fileEncoding = "UTF16")
SHF=read.table("C:/Users/Jiang/Desktop/APC-data/SH-F.txt",fileEncoding = "UTF16")
SHM=read.table("C:/Users/Jiang/Desktop/APC-data/SH-M.txt",fileEncoding = "UTF16")
JSF=read.table("C:/Users/Jiang/Desktop/APC-data/JS-F.txt",fileEncoding = "UTF16")
JSM=read.table("C:/Users/Jiang/Desktop/APC-data/JS-M.txt",fileEncoding = "UTF16")
HRBF=read.table("C:/Users/Jiang/Desktop/APC-data/HRB-F.txt",fileEncoding = "UTF16")
HRBM=read.table("C:/Users/Jiang/Desktop/APC-data/HRB-M.txt",fileEncoding = "UTF16")
ZSF=read.table("C:/Users/Jiang/Desktop/APC-data/ZS-F.txt",fileEncoding = "UTF16")
ZSM=read.table("C:/Users/Jiang/Desktop/APC-data/ZS-M.txt",fileEncoding = "UTF16")

###constrained period effect to 0——ACP model###
z.hkm<-apc.fit(HKM, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=7,C=9))
z.hkf<-apc.fit(HKF, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=7,C=9))
z.shm<-apc.fit(SHM, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=6,C=9))
z.shf<-apc.fit(SHF, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=6,C=9))
z.jsm<-apc.fit(JSM, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=5,C=8))
z.jsf<-apc.fit(JSF, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=5,C=8))
z.hrbm<-apc.fit(HRBM, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=4,C=8))
z.hrbf<-apc.fit(HRBF, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=4,C=8))
z.zsm<-apc.fit(ZSM, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=4,C=8))
z.zsf<-apc.fit(ZSF, model="ns",parm="ACP",dr.extr="weighted",npar=c(A=6,P=4,C=8))

###Plot###
##1 Hong Kong##
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hk-m-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hkm,frame=res,col="black",ci=T)
dev.off() 

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hk-f-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hkf,frame=res,col="black",ci=T)
dev.off() 

#post hoc analysis：constrained the slope of the period effect βp=+0.01/-0.01#
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hk-m-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hkm,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.hkm,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.hkm,frame=res,drift=0.99,col="black",lty = 3)
dev.off() 

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hk-f-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hkf,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.hkf,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.hkf,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

##2 Shanghai##
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/sh-m-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.shm,frame=res,col="black",ci=T)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/sh-f-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.shf,frame=res,col="black",ci=T)
dev.off()

#post hoc analysis：constrained the slope of the period effect βp=+0.01/-0.01#
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/sh-m-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.shm,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.shm,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.shm,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/sh-f-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.shf,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.shf,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.shf,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

##3 Jiashan##
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/js-m-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.jsm,frame=res,col="black",ci=T)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/js-f-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.jsf,frame=res,col="black",ci=T)
dev.off()

#post hoc analysis：constrained the slope of the period effect βp=+0.01/-0.01#
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/js-m-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.jsm,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.jsm,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.jsm,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/js-f-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.jsf,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.jsf,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.jsf,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

##4 Harbin##
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hrb-m-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hrbm,frame=res,col="black",ci=T)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hrb-f-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hrbf,frame=res,col="black",ci=T)
dev.off()

#post hoc analysis：constrained the slope of the period effect βp=+0.01/-0.01#
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hrb-m-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hrbm,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.hrbm,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.hrbm,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/hrb-f-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hrbf,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.hrbf,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.hrbf,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

##5 Zhongshan##
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/zs-m-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.zsm,frame=res,col="black",ci=T)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/zs-f-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.zsf,frame=res,col="black",ci=T)
dev.off()

#post hoc analysis：constrained the slope of the period effect βp=+0.01/-0.01#
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/zs-m-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.zsm,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.zsm,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.zsm,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/zs-f-sa.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.zsf,frame=res,drift=1,col="black",lty = 1)
apc.lines(z.zsf,frame=res,drift=1.01,col="black",lty = 2)
apc.lines(z.zsf,frame=res,drift=0.99,col="black",lty = 3)
dev.off()

##6 five registries of China##
jpeg(file="C:/Users/Jiang/Desktop/APC-plot/5-m-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hrbm,frame=res,col="#6699FF")
apc.lines(z.hkm,frame=res,col="#CC99FF")
apc.lines(z.shm,frame=res,col="#99CC33")
apc.lines(z.jsm,frame=res,col="#FFCC33")
apc.lines(z.zsm,frame=res,col="#FF3333")
dev.off()

jpeg(file="C:/Users/Jiang/Desktop/APC-plot/5-f-p0.png",
     width =4000,  height=2200, units = "px", res=400)
par( mar=c(4,4,1,4) )
res<-apc.frame( a.lab=seq(30,90,10), 
                cp.lab=seq(1900,2020,20),
                r.lab=c(1,2,5,10,20,50,100,200,500,1000),
                a.tic=seq(30,90,10),cp.tic=seq(1900,2020,20), r.tic=c(1:10,1:5*10),
                a.txt = "Age",cp.txt = "Calendar time", r.txt = "Rate per 100,000 person-years",
                rr.txt = "Rate ratio",ref.line = TRUE,gap=10,sides = c(1,2,4))
apc.lines(z.hrbf,frame=res,col="#6699FF")
apc.lines(z.hkf,frame=res,col="#CC99FF")
apc.lines(z.shf,frame=res,col="#99CC33")
apc.lines(z.jsf,frame=res,col="#FFCC33")
apc.lines(z.zsf,frame=res,col="#FF3333")
dev.off()

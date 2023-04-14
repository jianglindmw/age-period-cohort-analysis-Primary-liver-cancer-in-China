library(Epi)
library(BAPC)
library(INLA)

###1 retrospective projections：2008-2012###
##data import##
counts.re.HKfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HK/N-Hongkong-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.HKmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HK/N-Hongkong-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.HRBfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HRB/N-Harbin-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.HRBmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HRB/N-Harbin-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.JSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/JS/N-Jiashan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.JSmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/JS/N-Jiashan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.SHfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/SH/N-Shanghai-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.SHmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/SH/N-Shanghai-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.ZSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/ZS/N-Zhongshan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.re.ZSmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/ZS/N-Zhongshan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.HKfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HK/P-Hongkong-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.HKmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HK/P-Hongkong-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.HRBfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HRB/P-Harbin-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.HRBmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/HRB/P-Harbin-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.JSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/JS/P-Jiashan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.JSmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/JS/P-Jiashan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.SHfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/SH/P-Shanghai-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.SHmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/SH/P-Shanghai-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.ZSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/ZS/P-Zhongshan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.re.ZSmale = read.table("C:/Users/Jiang/Desktop/BAPC-retro data/ZS/P-Zhongshan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)

##fit APC model##
agegroups = c("30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84")
HKfemale.re.APC = APCList(counts.re.HKfemale, pop.re.HKfemale, gf=5, agelab=agegroups)
HKmale.re.APC = APCList(counts.re.HKmale, pop.re.HKmale, gf=5, agelab=agegroups)
HRBfemale.re.APC = APCList(counts.re.HRBfemale, pop.re.HRBfemale, gf=5, agelab=agegroups)
HRBmale.re.APC = APCList(counts.re.HRBmale, pop.re.HRBmale, gf=5, agelab=agegroups)
JSfemale.re.APC = APCList(counts.re.JSfemale, pop.re.JSfemale, gf=5, agelab=agegroups)
JSmale.re.APC = APCList(counts.re.JSmale, pop.re.JSmale, gf=5, agelab=agegroups)
SHfemale.re.APC = APCList(counts.re.SHfemale, pop.re.SHfemale, gf=5, agelab=agegroups)
SHmale.re.APC = APCList(counts.re.SHmale, pop.re.SHmale, gf=5, agelab=agegroups)
ZSfemale.re.APC = APCList(counts.re.ZSfemale, pop.re.ZSfemale, gf=5, agelab=agegroups)
ZSmale.re.APC = APCList(counts.re.ZSmale, pop.re.ZSmale, gf=5, agelab=agegroups)

data(whostandard)

##fit BAPC model##
#period-RW1#
HKf.re.rw1= BAPC(HKfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

HKm.re.rw1= BAPC(HKmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

SHf.re.rw1= BAPC(SHfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)


SHm.re.rw1= BAPC(SHmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 stdweight=whostandard[7:17,2], verbose=FALSE)

JSf.re.rw1= BAPC(JSfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

JSm.re.rw1= BAPC(JSmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 stdweight=whostandard[7:17,2], verbose=FALSE)

HRBm.re.rw1= BAPC(HRBmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                  model=list(age=list(model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                             period=list(include=TRUE, model="rw1",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             cohort=list(include=TRUE, model="rw2",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             overdis=list(include=TRUE, model="iid",
                                          prior = "loggamma", param = c(1, 0.005))),
                  stdweight=whostandard[7:17,2], verbose=FALSE)

HRBf.re.rw1= BAPC(HRBfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                  model=list(age=list(model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                             period=list(include=TRUE, model="rw1",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             cohort=list(include=TRUE, model="rw2",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             overdis=list(include=TRUE, model="iid",
                                          prior = "loggamma", param = c(1, 0.005))),
                  secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

ZSm.re.rw1= BAPC(ZSmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 stdweight=whostandard[7:17,2], verbose=FALSE)

ZSf.re.rw1= BAPC(ZSfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw1",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)
#period-RW2#
HKf.re.rw2= BAPC(HKfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

HKm.re.rw2= BAPC(HKmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

SHf.re.rw2= BAPC(SHfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)


SHm.re.rw2= BAPC(SHmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 stdweight=whostandard[7:17,2], verbose=FALSE)

JSf.re.rw2= BAPC(JSfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

JSm.re.rw2= BAPC(JSmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 stdweight=whostandard[7:17,2], verbose=FALSE)

HRBm.re.rw2= BAPC(HRBmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                  model=list(age=list(model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                             period=list(include=TRUE, model="rw2",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             cohort=list(include=TRUE, model="rw2",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             overdis=list(include=TRUE, model="iid",
                                          prior = "loggamma", param = c(1, 0.005))),
                  stdweight=whostandard[7:17,2], verbose=FALSE)

HRBf.re.rw2= BAPC(HRBfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                  model=list(age=list(model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                             period=list(include=TRUE, model="rw2",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             cohort=list(include=TRUE, model="rw2",
                                         prior = "loggamma", param = c(1, 0.00005)),
                             overdis=list(include=TRUE, model="iid",
                                          prior = "loggamma", param = c(1, 0.005))),
                  secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

ZSm.re.rw2= BAPC(ZSmale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 stdweight=whostandard[7:17,2], verbose=FALSE)

ZSf.re.rw2= BAPC(ZSfemale.re.APC, predict=list(npredict=5, retro=TRUE),      
                 model=list(age=list(model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                            period=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            cohort=list(include=TRUE, model="rw2",
                                        prior = "loggamma", param = c(1, 0.00005)),
                            overdis=list(include=TRUE, model="iid",
                                         prior = "loggamma", param = c(1, 0.005))),
                 secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)


###2 Projections for 2013-2032###
##data import##
counts.HKfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HK/N-Hongkong-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.HKmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HK/N-Hongkong-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.HRBfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HRB/N-Harbin-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.HRBmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HRB/N-Harbin-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.JSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/JS/N-Jiashan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.JSmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/JS/N-Jiashan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.SHfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/SH/N-Shanghai-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.SHmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/SH/N-Shanghai-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.ZSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/ZS/N-Zhongshan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
counts.ZSmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/ZS/N-Zhongshan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.HKfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HK/P-Hongkong-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.HKmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HK/P-Hongkong-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.HRBfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HRB/P-Harbin-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.HRBmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/HRB/P-Harbin-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.JSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/JS/P-Jiashan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.JSmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/JS/P-Jiashan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.SHfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/SH/P-Shanghai-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.SHmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/SH/P-Shanghai-male.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.ZSfemale = read.table("C:/Users/Jiang/Desktop/BAPC-data/ZS/P-Zhongshan-female.txt",fileEncoding = "UTF16",row.names=1, header=F)
pop.ZSmale = read.table("C:/Users/Jiang/Desktop/BAPC-data/ZS/P-Zhongshan-male.txt",fileEncoding = "UTF16",row.names=1, header=F)

##fit APC model##
agegroups = c("30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84")
HKfemale.APC = APCList(counts.HKfemale, pop.HKfemale, gf=5, agelab=agegroups)
HKmale.APC = APCList(counts.HKmale, pop.HKmale, gf=5, agelab=agegroups)
HRBfemale.APC = APCList(counts.HRBfemale, pop.HRBfemale, gf=5, agelab=agegroups)
HRBmale.APC = APCList(counts.HRBmale, pop.HRBmale, gf=5, agelab=agegroups)
JSfemale.APC = APCList(counts.JSfemale, pop.JSfemale, gf=5, agelab=agegroups)
JSmale.APC = APCList(counts.JSmale, pop.JSmale, gf=5, agelab=agegroups)
SHfemale.APC = APCList(counts.SHfemale, pop.SHfemale, gf=5, agelab=agegroups)
SHmale.APC = APCList(counts.SHmale, pop.SHmale, gf=5, agelab=agegroups)
ZSfemale.APC = APCList(counts.ZSfemale, pop.ZSfemale, gf=5, agelab=agegroups)
ZSmale.APC = APCList(counts.ZSmale, pop.ZSmale, gf=5, agelab=agegroups)

data(whostandard)

##fit BAPC model##
HKf.rw2= BAPC(HKfemale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

HKm.rw2= BAPC(HKmale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

SHf.rw2= BAPC(SHfemale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)


SHm.rw2= BAPC(SHmale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              stdweight=whostandard[7:17,2], verbose=FALSE)

JSf.rw2= BAPC(JSfemale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

JSm.rw2= BAPC(JSmale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              stdweight=whostandard[7:17,2], verbose=FALSE)

HRBm.rw2= BAPC(HRBmale.APC, predict=list(npredict=20, retro=FALSE),      
               model=list(age=list(model="rw2",
                                   prior = "loggamma", param = c(1, 0.00005)),
                          period=list(include=TRUE, model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                          cohort=list(include=TRUE, model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                          overdis=list(include=TRUE, model="iid",
                                       prior = "loggamma", param = c(1, 0.005))),
               stdweight=whostandard[7:17,2], verbose=FALSE)

HRBf.rw2= BAPC(HRBfemale.APC, predict=list(npredict=20, retro=FALSE),      
               model=list(age=list(model="rw2",
                                   prior = "loggamma", param = c(1, 0.00005)),
                          period=list(include=TRUE, model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                          cohort=list(include=TRUE, model="rw2",
                                      prior = "loggamma", param = c(1, 0.00005)),
                          overdis=list(include=TRUE, model="iid",
                                       prior = "loggamma", param = c(1, 0.005))),
               secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

ZSm.rw1= BAPC(ZSmale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw1",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              stdweight=whostandard[7:17,2], verbose=FALSE)

ZSf.rw1= BAPC(ZSfemale.APC, predict=list(npredict=20, retro=FALSE),      
              model=list(age=list(model="rw2",
                                  prior = "loggamma", param = c(1, 0.00005)),
                         period=list(include=TRUE, model="rw1",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         cohort=list(include=TRUE, model="rw2",
                                     prior = "loggamma", param = c(1, 0.00005)),
                         overdis=list(include=TRUE, model="iid",
                                      prior = "loggamma", param = c(1, 0.005))),
              secondDiff = FALSE,stdweight=whostandard[7:17,2], verbose=FALSE)

##BAPC-plot##
#Hong Kong#
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/HKm2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(HKm.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/HKf2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(HKf.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 

#Shanghai#
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/SHm2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(SHm.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/SHf2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(SHf.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 

#Jiashan#
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/JSm2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(JSm.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/JSf2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(JSf.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 

#Harbin#
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/HRBm2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(HRBm.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/HRBf2.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(HRBf.rw2, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 

#Zhongshan#
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/ZSm1.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(ZSm.rw1, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 
jpeg(file="C:/Users/Jiang/Desktop/BAPC-plot/ZSf1.jpg",
     width =2000,  height=1800, units = "px", res=400)
plotBAPC(ZSf.rw1, scale=100000, type="ageStdRate",obs.lwd=0,obs.cex=0.8,
         probs = seq(0.05, 0.95, by=0.1))
dev.off() 


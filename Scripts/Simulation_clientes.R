# 100.000 personas
# Cuadro:
# X1= -71.1080, 4.7082
# X2= -74.05393, 4.65386
# Jueves:
# punto inicial! empiezan 6-9 y vuelven 5-8
# dia laboral de 8-12, 2-6 60%
# restaurantes 11-2 para el 100%
# Viernes:
# punto inicial! empiezan 6-9 
# dia laboral de 8-12, 2-6 60%
# restaurantes 11-2 para el 100%
# Vuelven a las casa 40%
# Bares y licores 60% 10-6
# Sabado:
# Punto inicial: 8-10 
# eventos 10-5
# Zonas comerciales 10-5
# Restaurantes 12-3 y 6-9
# bares y licores 9-6 
# Domingo:
# eventos 10-5
# Zonas comerciales 10-5
# Restaurantes 12-3 
# vuelven a 6-10

library(SiMRiv)
library(spatstat)
library(tibble)
library(dplyr)
#library(data.table)
#
# levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(0.005, 0.005)) 
# for(i in 1:(n*0.4)){
#   sim.lw <- simulate(levy.walker, 1320)
#   df = tibble(x=sim.lw$V1,)%>% simulate(levy.walker, 1320)add_row(x = 4, y = 0)
#   plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
# }
normalize <- function(x)
{
  a=max(abs(max(x)),abs(min(x)))
  return((x) /(a))
}

# To get a vector, use apply instead of lapply

# JUEVES
min_time_jueves <- as.POSIXct("2020-01-01 06:00:00")
sec_jueves <- seq(from = min_time_jueves, length.out = 10440, by = 30)
X1= -74.1080
Y1= 4.7082
X2= -74.05393
Y2= 4.65386
# base
n=1000
base_jueves = expand.grid(sec_jueves, 1:n)
colnames(base_jueves)=c("fecha","id")

# Simulación puntos iniciales (Home)
set.seed(14)
pp <- rpoispp(n)
plot(density(pp))
pi=tibble(id=1:n,x=pp$x[0:n],y=pp$y[0:n])
# pi2=pi %>%
#   mutate(x=(x*(X1-X2))+X2,
#          y=(y*(Y1-Y2))+Y2)
# trabajadores:
wk=sort(sample(1:n,n*0.6))
# LUGARES DE TRABAJO 
set.seed(12345)
pp <- rpoispp(n*0.6)
plot(density(pp))
wp=tibble(id= wk,x=pp$x[0:(n*0.6)],y=pp$y[0:(n*0.6)])
# wp2=wp %>%
#   mutate(x=(x*(X1-X2))+X2,
#          y=(y*(Y1-Y2))+Y2)
# fwrite(pi2,"home.csv")
# fwrite(wp2,"work.csv")
# home to work
hw = pi %>% 
  inner_join(wp, by="id") 
nn=rpois(1,80)
xx=seq(as.numeric(hw[1,2]),as.numeric(hw[1,4]),,nn)
yy=seq(as.numeric(hw[1,3]),as.numeric(hw[1,5]),,nn)
a=rpois(1,100)
c1=tibble(x=xx+rnorm(nn,0,0.001),y=yy+rnorm(nn,0,0.001),id=1,fecha=sec_jueves[a:(a+length(xx)-1)]) %>% 
  add_row(x=xx+rnorm(nn,0,0.001),y=yy+rnorm(nn,0,0.001),id=1,fecha=sec_jueves[(a+2880):(a+2880+length(xx)-1)])

for(i in 2:nrow(hw)){
  nn=rpois(1,80)
  xx=seq(as.numeric(hw[1,2]),as.numeric(hw[1,4]),,nn)
  yy=seq(as.numeric(hw[1,3]),as.numeric(hw[1,5]),,nn)
  a=rpois(1,100)
  c1= c1  %>% add_row(x = xx+rnorm(nn,0,0.001), y = yy+rnorm(nn,0,0.001), id =i,fecha=sec_jueves[a:(a+length(xx)-1)]) %>% 
    add_row(x=xx+rnorm(nn,0,0.001),y=yy+rnorm(nn,0,0.001),id=i,fecha=sec_jueves[(a+2880):(a+2880+length(xx)-1)])
}

base_jueves = base_jueves %>% 
  left_join(c1, by = c("id","fecha"))

#work to home
nn=rpois(1,80)
xx=seq(as.numeric(hw[1,4]),as.numeric(hw[1,2]),,nn)
yy=seq(as.numeric(hw[1,5]),as.numeric(hw[1,2]),,nn)
a=rpois(1,180)+1320
c2=tibble(x=xx+rnorm(nn,0,0.001),y=yy+rnorm(nn,0,0.001),id=1,fecha=sec_jueves[a:(a+length(xx)-1)]) %>% 
  add_row(x=xx+rnorm(nn,0,0.001),y=yy+rnorm(nn,0,0.001),id=1,fecha=sec_jueves[(a+2880):(a+2880+length(xx)-1)])

for(i in 2:nrow(hw)){
  nn=rpois(1,80)
  xx=seq(as.numeric(hw[1,4]),as.numeric(hw[1,2]),,nn)
  yy=seq(as.numeric(hw[1,5]),as.numeric(hw[1,3]),,nn)
  a=rpois(1,180)+1320
  c2= c2  %>% add_row(x = xx+rnorm(nn,0,0.001), y = yy+rnorm(nn,0,0.001), id =i,fecha=sec_jueves[a:(a+length(xx)-1)]) %>% 
    add_row(x=xx+rnorm(nn,0,0.001),y=yy+rnorm(nn,0,0.001),id=i,fecha=sec_jueves[(a+2880):(a+2880+length(xx)-1)])
}

base_jueves = base_jueves %>% 
  left_join(c2, by = c("id","fecha")) %>% 
  mutate(x=ifelse(is.na(x.x),x.y,x.x),
         y=ifelse(is.na(y.x),y.y,y.x)) %>% 
  select(c(x,y,id,fecha))

# almuerzo
levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(1, 0.08)) 
sim.lw <- simulate(levy.walker, rpois(1,50))
plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
sim.lw[,1]=normalize(sim.lw[,1])
sim.lw[,2]=normalize(sim.lw[,2])     
a=rpois(1,120)+600
lun = tibble(x=sim.lw[,1],y=sim.lw[,2],id=wk[1],fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)]) %>% 
  add_row(x=rev(sim.lw[,1]), y=rev(sim.lw[,2]),id=wk[1],fecha=sec_jueves[(a+170):(a+170+length(sim.lw[,1])-1)])
for(i in wk[-1]){
  sim.lw <- simulate(levy.walker, rpois(1,50))
  sim.lw[,1]=normalize(sim.lw[,1])
  sim.lw[,2]=normalize(sim.lw[,2])
  a=rpois(1,120)+600
  lun= lun  %>% add_row(x = sim.lw[,1], y = sim.lw[,2], id =i,fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])%>% 
    add_row(x=rev(sim.lw[,1]), y=rev(sim.lw[,2]),id=i,fecha=sec_jueves[(a+170):(a+170+length(sim.lw[,1])-1)])
  
  #plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
}

lun= lun %>% 
  left_join(wp,by="id") %>% 
  mutate(x=x.x+x.y,
         y=y.x+y.y
         ) %>% 
  select(c(x,y,id,fecha))

base_jueves = base_jueves %>% 
  left_join(lun, by = c("id","fecha")) %>% 
  mutate(x=ifelse(is.na(x.x),x.y,x.x),
         y=ifelse(is.na(y.x),y.y,y.x)) %>% 
  select(c(x,y,id,fecha))

# no trabajadores 
nwk=which(!(1:n %in% wk))
levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(0.005, 0.005)) 
sim.lw <- simulate(levy.walker, rpois(1,1100))
sim.lw[,1]=normalize(sim.lw[,1])
sim.lw[,2]=normalize(sim.lw[,2])
a=rpois(1,180)
df = tibble(x=sim.lw[,1],y=sim.lw[,2],id=nwk[1],fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
for(i in nwk[-1]){
  sim.lw <- simulate(levy.walker, rpois(1,1100))
  sim.lw[,1]=normalize(sim.lw[,1])
  sim.lw[,2]=normalize(sim.lw[,2])
  a=rpois(1,120)
  df= df  %>% add_row(x = sim.lw[,1], y = sim.lw[,2], id =i,fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
  #plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
}
df= df %>% 
  left_join(pi,by="id") %>% 
  mutate(x=x.x+x.y,
         y=y.x+y.y
  ) %>% 
  select(c(x,y,id,fecha))
base_jueves = base_jueves %>% 
  left_join(df, by = c("id","fecha")) %>% 
  mutate(x=ifelse(is.na(x.x),x.y,x.x),
         y=ifelse(is.na(y.x),y.y,y.x)) %>% 
  select(c(x,y,id,fecha))

# VIERNES

dan=sort(sample(1:n,n*0.3))
nodan=which(!(1:n %in% dan))

# no trabajadores 
levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(0.005, 0.005)) 
sim.lw <- simulate(levy.walker, rpois(1,600))
sim.lw[,1]=normalize(sim.lw[,1])
sim.lw[,2]=normalize(sim.lw[,2])
a=rpois(1,180)+4620
viedan = tibble(x=sim.lw[,1],y=sim.lw[,2],id=nwk[1],fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
for(i in nwk[-1]){
  sim.lw <- simulate(levy.walker, rpois(1,600))
  sim.lw[,1]=normalize(sim.lw[,1])
  sim.lw[,2]=normalize(sim.lw[,2])
  a=rpois(1,180)+4620
  viedan= viedan  %>% add_row(x = sim.lw[,1], y = sim.lw[,2], id =i,fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
  #plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
}
viedan= viedan %>% 
  left_join(pi,by="id") %>% 
  mutate(x=x.x+x.y,
         y=y.x+y.y
  ) %>% 
  select(c(x,y,id,fecha))
base_jueves = base_jueves %>% 
  left_join(viedan, by = c("id","fecha")) %>% 
  mutate(x=ifelse(is.na(x.x),x.y,x.x),
         y=ifelse(is.na(y.x),y.y,y.x)) %>% 
  select(c(x,y,id,fecha))
#SABADO

#bares


levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(0.01, 0.01)) 
sim.lw <- simulate(levy.walker, rpois(1,2160))
sim.lw[,1]=normalize(sim.lw[,1])
sim.lw[,2]=normalize(sim.lw[,2])
plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
a=rpois(1,180)+6000
sabdan = tibble(x=sim.lw[,1],y=sim.lw[,2],id=dan[1],fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
for(i in dan[-1]){
  sim.lw <- simulate(levy.walker, rpois(1,2160))
  sim.lw[,1]=normalize(sim.lw[,1])
  sim.lw[,2]=normalize(sim.lw[,2])
  a=rpois(1,180)+6000
  sabdan= sabdan  %>% add_row(x = sim.lw[,1], y = sim.lw[,2], id =i,fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
  #plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
}
sabdan= sabdan %>% 
  left_join(pi,by="id") %>% 
  mutate(x=x.x+x.y,
         y=y.x+y.y
  ) %>% 
  select(c(x,y,id,fecha))
base_jueves = base_jueves %>% 
  left_join(sabdan, by = c("id","fecha")) %>% 
  mutate(x=ifelse(is.na(x.x),x.y,x.x),
         y=ifelse(is.na(y.x),y.y,y.x)) %>% 
  select(c(x,y,id,fecha))

#no bares

levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(0.01, 0.01)) 
sim.lw <- simulate(levy.walker, rpois(1,1300))
sim.lw[,1]=normalize(sim.lw[,1])
sim.lw[,2]=normalize(sim.lw[,2])
plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
a=rpois(1,100)+6000
sabnodan = tibble(x=sim.lw[,1],y=sim.lw[,2],id=nodan[1],fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
for(i in nodan[-1]){
  sim.lw <- simulate(levy.walker, rpois(1,1300))
  sim.lw[,1]=normalize(sim.lw[,1])
  sim.lw[,2]=normalize(sim.lw[,2])
  a=rpois(1,100)+6000
  sabnodan= sabnodan  %>% add_row(x = sim.lw[,1], y = sim.lw[,2], id =i,fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
}
sabnodan= sabnodan %>% 
  left_join(pi,by="id") %>% 
  mutate(x=x.x+x.y,
         y=y.x+y.y
  ) %>% 
  select(c(x,y,id,fecha))
base_jueves = base_jueves %>% 
  left_join(sabnodan, by = c("id","fecha")) %>% 
  mutate(x=ifelse(is.na(x.x),x.y,x.x),
         y=ifelse(is.na(y.x),y.y,y.x)) %>% 
  select(c(x,y,id,fecha))
#DOMINGO  

levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(0.01, 0.01)) 
sim.lw <- simulate(levy.walker, rpois(1,1300))
sim.lw[,1]=normalize(sim.lw[,1])
sim.lw[,2]=normalize(sim.lw[,2])
plot(sim.lw, type = "l", asp = 1, main = "L ́evy-like walker")
a=rpois(1,180)+8780
dom = tibble(x=sim.lw[,1],y=sim.lw[,2],id=1,fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
for(i in 2:n){
  sim.lw <- simulate(levy.walker, rpois(1,1300))
  sim.lw[,1]=normalize(sim.lw[,1])
  sim.lw[,2]=normalize(sim.lw[,2])
  a=rpois(1,180)+8780
  dom= dom  %>% add_row(x = sim.lw[,1], y = sim.lw[,2], id =i,fecha=sec_jueves[a:(a+length(sim.lw[,1])-1)])
}

dom= dom %>% 
  left_join(pi,by="id") %>% 
  mutate(x=x.x+x.y,
         y=y.x+y.y
  ) %>% 
  select(c(x,y,id,fecha))

base_jueves = base_jueves %>% 
  left_join(lun, by = c("id","fecha")) %>% 
  mutate(x=ifelse(is.na(x.x),x.y,x.x),
         y=ifelse(is.na(y.x),y.y,y.x)) %>% 
  select(c(x,y,id,fecha)) 

base_jueves = base_jueves%>% 
  mutate(x=ifelse(()))



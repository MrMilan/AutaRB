
rm(list=ls()) #mazani promenych
cat("\014")  #mazani konzole
carsa <- read.csv("~/Scholla/___FEL/21rocnik/DVZ/AutaRB/cars.csv")

cars <- carsa

cars <- na.omit(cars) #odstraneni nulovych
cars.label <- cars

cars$Manufacturer <- as.numeric(cars$Manufacturer)
cars$Model <- as.numeric(cars$Model)
cars$Type <- as.numeric(cars$Type)
cars$Min.Price <- as.numeric(cars$Min.Price)
cars$Price <- as.numeric(cars$Price)
cars$Max.Price <- as.numeric(cars$Max.Price)
cars$MPG.city <- as.numeric(cars$MPG.city)
cars$MPG.highway <- as.numeric(cars$MPG.highway)
cars$AirBags <- as.numeric(cars$AirBags)
cars$DriveTrain <- as.numeric(cars$DriveTrain)
cars$Cylinders <- as.numeric(cars$Cylinders)
cars$EngineSize <- as.numeric(cars$EngineSize)
cars$Horsepower <- as.numeric(cars$Horsepower)
cars$RPM <- as.numeric(cars$RPM)
cars$Rev.per.mile <- as.numeric(cars$Rev.per.mile)
cars$Man.trans.avail <- as.numeric(cars$Man.trans.avail)
cars$Fuel.tank.capacity <- as.numeric(cars$Fuel.tank.capacity)
cars$Passengers <- as.numeric(cars$Passengers)
cars$Length <- as.numeric(cars$Length)
cars$Wheelbase <- as.numeric(cars$Wheelbase)
cars$Width <- as.numeric(cars$Width)
cars$Turn.circle <- as.numeric(cars$Turn.circle)
cars$Rear.seat.room <- as.numeric(cars$Rear.seat.room)
cars$Luggage.room <- as.numeric(cars$Luggage.room)
cars$Weight <- as.numeric(cars$Weight)
cars$Origin <- as.numeric(cars$Origin)




cars.use = cars[,-c(1,2,4,6)]
medians = apply(cars.use,2,median)
mads = apply(cars.use,2,mad)
cars.use1 <- cars.use
cars.use = scale(cars.use,center=medians,scale=mads)

cars.dist = dist(cars.use)
cars.dist[is.infinite(cars.dist)] <- 100 #odstraneni nulovych
cars.hclust = hclust(cars.dist)
plot(cars.hclust,labels=cars.label$Model,main='Complete from hclust')#vykresleni

cars.wardClust <- hclust(cars.dist,method = "ward.D")#dobrz pro blizke skupiny
plot(cars.wardClust,labels=cars.label$Model,main='Ward.D from hclust')

#deleni do skupin
cars.cutToGroup.ward <- cutree(cars.wardClust, 3)
cars.cutToGroup.complete <- cutree(cars.hclust, 4)

#prumerovani
a3 = aggregate(cars[,-c(1,2)],list(cars.cutToGroup.ward),median)
a4 = aggregate(cars[,-c(1,2)],list(cars.cutToGroup.complete),median)

#vloyeni do tabulkz a export
tab3<- data.frame(Cluster=a3[,1],Freq=as.vector(table(cars.cutToGroup.ward)),a3[,-1])
tab4<- data.frame(Cluster=a4[,1],Freq=as.vector(table(cars.cutToGroup.complete)),a4[,-1])

write.table(tab3, file = "~/Scholla/___FEL/21rocnik/DVZ/AutaRB/tab3.csv", sep = ";", col.names = NA,qmethod = "double")
write.table(tab4, file = "~/Scholla/___FEL/21rocnik/DVZ/AutaRB/tab4.csv", sep = ";", col.names = NA,qmethod = "double")

#vzrobci aut
aa3 = aggregate(cars,list(cars.cutToGroup.ward),median)
aa4 = aggregate(cars,list(cars.cutToGroup.complete),median)

taab3<- data.frame(Cluster=a3[,1],Freq=as.vector(table(cars.cutToGroup.ward)),a3[,-1])
taab4<- data.frame(Cluster=a4[,1],Freq=as.vector(table(cars.cutToGroup.complete)),a4[,-1])




cars <- scale(cars) #normalizace

#kmeans(cars, centers, iter.max = 10, nstart = 1)
kakm <- kmeans(cars,4)

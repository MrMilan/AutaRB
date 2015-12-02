
rm(list=ls()) #mazani promenych
cat("\014")  #mazani konzole
cars <- read.csv("~/Schule/FEL/21rocnik/DVZ/Auta2/cars.csv")

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


cars <- na.omit(cars) #odstraneni nulovych

cars <- scale(cars) #normalizace

#kmeans(cars, centers, iter.max = 10, nstart = 1)
kakm <- kmeans(cars,4)

#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
log.data <- read.csv("splatoon_log.csv")
weapon.data <- read.table("weapon.txt", header = T, sep = ",",na.strings = "NA", fileEncoding='Shift_JIS')

buttle <- unique(log.data[,2])
#buttle <- buttle[-length(buttle)]
regular <- 0; gatci <- 0; amiibo <- 0;
for(i in 1:(length(log.data[,1]))){
  if(log.data[i,2] == "reg"){
    regular <- rbind(regular, log.data[i,])
  }else if(log.data[i,2] == "gatci"){
    gatci <- rbind(gatci, log.data[i,])
  }else if(log.data[i,2] == "amibo"){
    amiibo <- rbind(amiibo, log.data[i,])
  }
}
regular <- regular[-1, ];gatci <- gatci[-1, ];amiibo <- amiibo[-1,]

dataUpdate <- function(){
  log.data <- read.csv("splatoon_log.csv")
  weapon.data <- read.table("weapon.txt", header = T, sep = ",",na.strings = "NA", fileEncoding='Shift_JIS')
}

datePlot <- function(logs = log.data){ 
  date <- unique(logs[,1])
  win.date <- numeric(length(date))
  names(win.date) <- date
  win.rate <- numeric(length(date))
  names(win.rate) <- date
  
  for(i in 1:(length(logs[,1]))){
    for(j in 1:length(date)){
      if(logs[i,1] == date[j]){
        if(!is.na(logs[i, 5]) )
          win.date[j] <- win.date[j] + (logs[i, 5]) 
          win.rate[j] <- win.rate[j] + 1
      }
    }
  }
  win.rate <- signif((win.date / win.rate), digits = 3)
  barplot(win.date)
#  text(1:length(win.rate), 1, win.rate)
  return (win.rate)
}

pointPlot <- function(logs = log.data){
  weapon <- NULL;
  w <- unique(logs[,10])
  wcol <- rainbow(length(w))
  for(i in 1:length(logs[,10])){
    for(j in 1:length(w)){
      if(logs[i,10] == w[j]) weapon <- c(weapon, wcol[j])
    }
  }
  weapon <- weapon[-1]
  print(w)
  plot(1:length(logs[,6]), na.omit(logs[,6]) ,xlab = "match num",ylab = "point", col = weapon, pch = 16)
}


killdeathPlot <- function(logs = log.data, num){
  library("cluster", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
  cluster <- kmeans(na.omit(logs[,8:9]), num)
  plot(x = logs[,9], y = logs[,8], col = cluster$cluster, xlab = "Death", ylab = "Kill")
  clusplot(na.omit(logs[,8:9]), cluster$cluster, xlab = "Kill", ylab = "Death", color=TRUE, shade=TRUE)
}

getSelectedResult <- function(logs = log.data, result = 1){
  ret <- 0
  if(result == 1){
    ret <- subset(logs, result > 0)
  } else {
    ret <- subset(logs, result == 0)
  }
  return (ret)
}

getSelectedMatch <- function(logs = log.data, selectedMatch = "reg"){
  ret <- subset(logs, match == selectedMatch)
  return (ret)
}

getSelectedStage <- function(logs = log.data, selectedStage = "dekarain"){
  ret <- subset(logs, stage == selectedStage)
  return (ret)
}

getSelectedWeapon <- function(logs = log.data, selectedWeapon = "wakaba"){
  ret <- subset(logs, weapon == selectedWeapon)
  return (ret)
}

getSelectedRank <- function(logs = log.data, selectedRank = 1){
  ret <- subset(logs, rank == selectedRank)
  return (ret)
}

getSelectedType <- function(logs = log.data, selectedType = NA){
  ret <- subset(logs, type == selectedType)
  return (ret)
}

getWeaponName <- function(w_id){
  w_id <- as.character(w_id)
#  weapon.data <- read.table("weapon.txt", header = T, sep = "\t",na.strings = "NA")
  weaponName <- subset(weapon.data, identifier == w_id, main_name)
  return (weaponName)
}

weaponResult <- function(log = log.data, id = NA, num){
  
  log <- getSelectedWeapon(log, id)
  print(log)
  library("cluster", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
  cluster <- kmeans(na.omit(log[,8:9]), num)
  clusplot(na.omit(log[,8:9]), cluster$cluster, xlab = "Kill", ylab = "Death", color=TRUE, shade=TRUE)
}
############# Installing packages ##############
install.packages('ggplot2')
############# Loading packages ############
library(readr)
library(lattice)
library(ggplot2)
############# Loading data #############
WV96 <- read.csv("C:/Users/u0122642/Box Sync/PhD/Yield tables/Data/W-VL resultaten_1996.csv", ";", header = TRUE, dec = ",")
WV18 <- read.csv("C:/Users/u0122642/Box Sync/PhD/Yield tables/Data/W-VL resultaten_2018.csv",";", header = TRUE, dec = ",")
Table18 <- read.csv("C:/Users/u0122642/Box Sync/PhD/Yield tables/Data/Copy of opbrengsttabellen NL tabblad per boom.csv",";", header = TRUE, dec = ",")
############# Analysis Sim4Tree output ############
summary(WV96)
summary(WV18)
Years <- seq(2010,2160, by =5)
Species <- unique(WV96$Soort, WV18$Soort)
Species1 <- subset(WV96, Species == Species[1])
WV96 <- WV96[order(WV96$Soort, WV96$Jaar),]
WV18 <- WV18[order(WV18$Soort, WV18$Jaar),]
WV96[,1] <- as.numeric(NA)
WV18[,1] <- as.numeric(NA)
for (i in 1:nlevels(Species)) {
  WV96[c((((i-1)*31)+1):(i*31)),1] <- Years
  WV18[c((((i-1)*31)+1):(i*31)),1] <- Years
  
  windows()
  plot(Years, WV96$Totale.oogstvolume..m³...cumul..[c((((i-1)*31)+1):(i*31))], main = Species[i], ylab = 'Total harvest (tonnes)', type = "l")
  points(Years, WV18$Totale.oogstvolume..m³...cumul..[c((((i-1)*31)+1):(i*31))], main = Species[i], ylab = 'Total harvest (tonnes)', col = 2, type = "l")
  legend("topleft", legend = c('Table 1996', 'Table 2018'), col = (1:2), lty = 1)
  windows()
  plot(Years, WV96$Staande.voorraad..m³.[c((((i-1)*31)+1):(i*31))], main = Species[i], ylab = 'Standing Stock', type = "l")
  points(Years, WV18$Staande.voorraad..m³.[c((((i-1)*31)+1):(i*31))], main = Species[i], ylab = 'Standing Stock', col = 2, type = "l")
  legend("topleft", legend = c('Table 1996', 'Table 2018'), col = (1:2), lty = 1)
}
################# Analysis Yield tables ###################
Table18 <- Table18[,c(1:27)]
Table18[,1] <- sapply(as.character(Table18[,1]), switch, "moderate thinning" = 1, "heavy thinning" = 2)
for (i in 1:ncol(Table18)) {
  Table18[which(is.na(Table18[,i])),i] <- 0
}
####### Analyse difference between thinning intensities 
Biomass <- matrix(nrow = nrow(Table18), ncol = 7)
colnames(Biomass) <- c('t','Thinning','Siteclass','StandingV','HarvestedV','HavervestCum','TotBiom')
Biomass[1,c(1:5)] <- as.matrix(Table18[1,c(4,1,2,22,17)])
Biomass[1,6] <- Biomass[1,5]
Biomass[1,7] <- Biomass[1,4]+Biomass[1,6]
for (i in 2:nrow(Biomass)) {
  Biomass[i,c(1:5)] <- as.matrix(Table18[i,c(4,1,2,22,17)])
  if (Biomass[i,2] == Biomass[(i-1),2] & Biomass[i,3] == Biomass[(i-1),3]) {
    Biomass[i,6] <- Biomass[(i-1),6] + Biomass[i,5]
    
  } else {
    Biomass[i,6] <- Biomass[i,5]
  }
  Biomass[i,7] <- Biomass[i,4]+Biomass[i,6]
}
Biomass <- as.data.frame(Biomass)
windows()
ggplot(Biomass, aes(Biomass$t, Biomass$TotBiom, group = interaction(Biomass$Siteclass,Biomass$Thinning), color = as.factor(Biomass$Siteclass), linetype = factor(Biomass$Thinning,labels = c("Heavy","Moderate")))) + 
  geom_line() +
  geom_point() + 
  labs(title = 'Total biomass Scots Pine', x = 'Time', y = 'Biomass (m3/ha)', color = "Site Index", linetype = "Thinning") +
  #theme(legend.text = element_text(labels = c("Heavy","moderate")))
#scale_fill_manual(name = c("Thinning intensity"), labels = c("Heavy","moderate"))

  theme(panel.background = element_rect(fill = "NA", colour = "black"),legend.position = "right",axis.title = element_text(size=16),axis.text.y = element_text(size=12),legend.background = element_rect(colour = NA),
       legend.key = element_rect(colour = "white", fill = NA)) 
ggplot(Biomass, aes(Biomass$t, Biomass$HavervestCum, group = interaction(Biomass$Siteclass,Biomass$Thinning), color = as.factor(Biomass$Siteclass), linetype = factor(Biomass$Thinning,labels = c("Heavy","Moderate")))) + 
  geom_line() +
  geom_point() + 
  labs(title = 'Total biomass Scots Pine', x = 'Time', y = 'Biomass (m3/ha)', color = "Site Index", linetype = "Thinning") +
  #theme(legend.text = element_text(labels = c("Heavy","moderate")))
  #scale_fill_manual(name = c("Thinning intensity"), labels = c("Heavy","moderate"))
  
  theme(panel.background = element_rect(fill = "NA", colour = "black"),legend.position = "right",axis.title = element_text(size=16),axis.text.y = element_text(size=12),legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA)) 

  
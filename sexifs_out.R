library(RColorBrewer)
library(reshape)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
wd_df <- read.delim("species_wd.txt")

# 0.0509 * p * D^2 * H
# Chave J, Andalo C, Brown S, et al (2005) Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145:87-99. https://doi.org/10.1007/s00442-005-0100-x
# unit: cm, m, g/cm3, agb in kg
agb <- function(d, h, wd) {
  0.0509 * wd * d^2 * h
}

sc1a <- read.delim("sc1_400t_2.txt")
sc2a <- read.delim("sc2_400t_1.txt")
sc3a <- read.delim("sc3_400t_1.txt")
sc4a <- read.delim("sc4_400t_2.txt")
# 
# sc1 <- read.delim("sc1_400t_mort.txt")
# sc2 <- read.delim("sc2_400t_mort.txt")
# sc3 <- read.delim("sc3_400t_mort.txt")
# sc4 <- read.delim("sc4_400t_mort.txt")

sc1b <- read.delim("sc1_1100.txt")
sc2b <- read.delim("sc2_1100.txt")
sc3b <- read.delim("sc3_1100.txt")
sc4b <- read.delim("sc4_1100.txt")


sc_list <- list(sc1a, sc2a, sc3a, sc4a, sc1b, sc2b, sc3b, sc4b)

tdf_list <- list()
for(i in 1:length(sc_list)) {
  tdf <- merge(sc_list[[i]], wd_df, by.x = "SPECIES", by.y = "Label")
  tdf$agb <- agb(tdf$DBH*100, tdf$HEIGHT, tdf$WoodDensity)
  tdf$c <- tdf$agb * 0.5 / 1000 
  tdf_list[[i]] <- tdf  
}


# require(xlsx)
library(openxlsx2)
sheets <- c("scenario_1_1100", "scenario_2_1100", "scenario_3_1100", "scenario_4_1100")
for(i in c(1:4)) {
  df <- as.data.frame(tdf_list[[i]])
    # write.xlsx(df, file = paste(sheets[i], "xlsx", sep = "."), append = F, sheetName = sheets[i], showNA = FALSE)
  write_xlsx(df, file = paste(sheets[i], "xlsx", sep = "."))
}



# cdf_list <-  list()
# for(i in 1:length(tdf_list)) {
#   c_df <- aggregate(tdf_list[[i]]$c, by=list(tdf_list[[i]]$SIM_AGE, tdf_list[[i]]$SPECIES), FUN=sum)
#   names(c_df) <- c("t", "sp", "c")
#   cdf_list[[i]] <- c_df  
# }
t5 <- seq(5,50,5)
cdft_list <-  list()
for(i in 1:length(tdf_list)) {
  tdf<- tdf_list[[i]][tdf_list[[i]]$SIM_AGE %in% t5,]
  c_df <- aggregate(tdf$c, by=list(tdf$SIM_AGE), FUN=sum)
  names(c_df) <- c("t", "c")
  cdft_list[[i]] <- c_df  
}

data <- data.frame(cdft_list[[1]]$c, cdft_list[[2]]$c, cdft_list[[3]]$c, cdft_list[[4]]$c)

par(mar = c(4,4,2,2) + 0.1)
sc_lab <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4")
matplot(x = t5, y = data, type = "b",pch=1,col = 1:4, lwd = 2, 
        xlab = "Simulation time (years)",
        ylab = "Total carbon stock (t/ha)")
legend("topleft", legend = sc_lab, col=1:4, pch=1, lwd = 2, bty = "n")


init <- rbind(0, data[-nrow(data),])
data_seq <- data - init
matplot(x = t5, y = data_seq, type = "b",pch=1,col = 1:4, lwd = 2, 
        xlab = "Simulation time (years)",
        ylab = "Carbon sequestration (t/ha.5yr)")
legend("topleft", legend = sc_lab, col=1:4, pch=1, lwd = 2, bty = "n")

#################################################

for(s in 1:4) {
  data <- data.frame(cdft_list[[s]]$c, cdft_list[[s+4]]$c)
  
  par(mar = c(4,4,2,2) + 0.1)
  sc_lab <- c(paste("Scenario", s, "- 400 trees/ha"), paste("Scenario", s, "- 1100 trees/ha"))
  matplot(x = t5, y = data, type = "b",pch=1,col = 1:4, lwd = 2, 
          xlab = "Simulation time (years)",
          ylab = "Total carbon stock (t/ha)")
  legend("topleft", legend = sc_lab, col=1:4, pch=1, lwd = 2, bty = "n")
  
  
  init <- rbind(0, data[-nrow(data),])
  data_seq <- data - init
  matplot(x = t5, y = data_seq, type = "b",pch=1,col = 1:4, lwd = 2, 
          xlab = "Simulation time (years)",
          ylab = "Carbon sequestration (t/ha.5yr)")
  legend("topleft", legend = sc_lab, col=1:4, pch=1, lwd = 2, bty = "n")
}






###########################
## n trees
#############
n_list <-  list()
for(i in 1:length(tdf_list)) {
  tdf<- tdf_list[[i]][tdf_list[[i]]$SIM_AGE %in% t5,]
  n_df <- aggregate(tdf$c, by=list(tdf$SIM_AGE), FUN=length)
  names(n_df) <- c("t", "n")
  n_list[[i]] <- n_df
}

data_n <- data.frame(n_list[[1]]$n, n_list[[2]]$n, n_list[[3]]$n, n_list[[4]]$n)

par(mar = c(4,4,2,2) + 0.1)
sc_lab <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4")
matplot(x = t5, y = data_n, type = "b",pch=1,col = 1:4, lwd = 2, 
        xlab = "Simulation time (years)",
        ylab = "Number of trees (n/ha)")
legend("topright", legend = sc_lab, col=1:4, pch=1, lwd = 2, bty = "n")


init <- rbind(400, data_n[-nrow(data_n),])
data_mort <-init-data_n
matplot(x = t5, y = data_mort, type = "b",pch=1,col = 1:4, lwd = 2, 
        xlab = "Simulation time (years)",
        ylab = "Mortality (n/ha.5yr)")
legend("topright", legend = sc_lab, col=1:4, pch=1, lwd = 2, bty = "n")





###########################################




t5 <- seq(5,50,5)
tdf_5 <- tdf[tdf$SIM_AGE %in% t5,]

c_df <- aggregate(tdf_5$c, by=list(tdf_5$SIM_AGE, tdf_5$SPECIES), FUN=sum)
names(c_df) <- c("t", "sp", "c")
c_bar <- cast(c_df, sp~t)

colors <- brewer.pal(length(c_bar$sp), "Set3")
colors <- colors[1:length(c_bar$sp)]

par(mar = c(4,4,2,2) + 0.1)

## Total carbon stock
v_bar <- apply(c_bar[-1], 2, rev)
y <- round(colSums(c_bar[-1]),2)
ytop <- max(y) + 20
x <- barplot(v_bar, ylab = "Total carbon stock (t/ha)",
             names.arg= colnames(c_bar[-1]), ylim = c(0, ytop), 
             col = rev(colors),
             xlab = "Time (years)")
legend("topleft", legend=c_bar$sp, inset = 0.01, bty = "n",
       title = "Species", fill = colors, cex=0.8)
text(x,y+8,labels=as.character(y))

## Sequestration
init <- cbind(0, v_bar[,-ncol(v_bar)])
v_bar2 <- v_bar-init
y2 <- round(colSums(v_bar2),2)
ytop2 <- max(y2) + 5
x2 <- barplot(v_bar2, ylab = "Carbon Sequestration (t/ha.5yr)",
             names.arg= colnames(c_bar[-1]), ylim = c(0, ytop2), 
             col = rev(colors),
             xlab = "Time (years)")
legend("topleft", legend=c_bar$sp, inset = 0.01, bty = "n",
       title = "Species", fill = colors, cex=0.8)
text(x2,y2+1,labels=as.character(y2))

## Number of individuals
n_df <- aggregate(tdf_5$c, by=list(tdf_5$SIM_AGE, tdf_5$SPECIES), FUN=length)
names(n_df) <- c("t", "sp", "n")
n_bar <- cast(n_df, sp~t)
vn_bar <- apply(n_bar[-1], 2, rev)
y <- round(colSums(n_bar[-1]),2)
ytop <- max(y) + 20
x <- barplot(vn_bar, ylab = "Number of trees",
             names.arg= colnames(c_bar[-1]), ylim = c(0, ytop), 
             col = rev(colors),
             xlab = "Time (years)")
legend("topright", legend=c_bar$sp, inset = 0.01, bty = "n",
       title = "Species", fill = colors, cex=0.8)
text(x,y+10,labels=as.character(y))


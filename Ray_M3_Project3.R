print("Matt Ray") # [MR] Print Name

# [MR] Set Directory 
getwd()
setwd("/Users/matthewray/Documents/RWorkingDirectory")
getwd()

# [MR] Install Packages
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("pacman")
library(pacman)
p_load(FSA, FSAdata, magrittr,
       dplyr, tidyr, plyr, tidyverse) # [MR] Part of pacman package - load all libraries at once 

# [MR] Import the inchBio.csv and name the table <bio>
bio <- read.csv("inchBio.csv")

# [MR] Display the head, tail and structure of <bio>
headtail(bio)
str(bio)

# [MR] Create an object, <counts>, that counts and lists all the species records
speciesonly <- count(bio,"species")
print(speciesonly)
            
# [MR] Display just the 8 levels (names) of the species
unique(bio[c("species")])

# [MR] Create a <tmp> object that displays the different species and the number of record of each species in the dataset.
tmp <- count(bio,"species")
print(tmp)
summary(tmp)

# [MR] Create a subset, <tmp2>, of just the species variable 
tmp2 <- subset(bio,
               select=c("species"))
head(tmp2,5) # display the first five records

# [MR] Create a table, <w>, of the species variable
w <- table(bio$species)
sapply(w,class) # Display the class of w

# [MR] Convert <w> to a data frame named <t> and display the results
t <- data.frame(w)

print(t)

# [MR] Extract and display the frequency values from the <t> data frame
t[,2]

# [MR] Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio> 
cSpec <- table(bio$species)
is.table(cSpec)
print(cSpec)
summary(cSpec)

# [MR] Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class
cSpecPct <- prop.table(cSpec)
is.table(cSpecPct)
print(cSpecPct)

d# [MR] Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame 
u <- data.frame(cSpecPct)
is.data.frame(u)
print(u)

# [MR] Create a barplot of <cSpec> with the following: titled Fish Count
barplot(cSpec, # create barplot of cSpec
        main="Fish Count", # name title Fish Count
        ylab = "COUNTS", # label y axis COUNTS
        ylim = c(0,250), # adjusted limit to fit data
        col = "#90EE90", # change color to light green
        las = 1, # rotate Y axis to be horizontal 
        cex.names = .60) # set x axis magnification to 60%
dev.off()

# [MR] Create a barplot of <cSpecPct>
barplot(cSpecPct,
        ylim = c(0,0.4), # set y limit form 0 - 4 changed to 0.4 
        main = "Fish Relative Frequency", # set title of chart to "Fish Relative Frequency"
        ylab = "Frequency (Count/Total)", # add Y Label
        col = "royal blue", # change color
        las = 1, # rotate Y axis to be horizontal
        cex.names = .67) # set x axis magnification
dev.off()

# [MR] Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d>
d <- arrange(u,desc(Freq))
print(d)     

# [MR] Rename the <d> columns Var 1 to Species, and Freq to RelFreq
names(d) [1] <- "Species"
names(d) [2] <- "RelFreq"
print(d)

# [MR] Add new variables to <d> and call them cumulativefreq, counts, and cumulativecounts
print(t)
names(t) [1] <- "Species"
print(t)
dNew <- merge(d,t,by="Species")
class(dNew)
print(dNew)
names(dNew) [3] <- "Counts"
print(dNew)
dNewDesc <- arrange(dNew,desc(RelFreq))
class(dNewDesc)
print(dNewDesc)
dNewcumcounts <- mutate(group_by(dNewDesc,Species), Cumulativecounts=cumsum(Counts))
dNewxColumns <- mutate(group_by(dNewcumcounts,Species), Cumulativefreq=cumsum(RelFreq))
is.data.frame(dNewxColumns)

# [MR] Create a parameter variable <def_par> to store parameter variables
def_par <- par()

# [MR] Create a barplot, <pc>
par(mar=c(8,5,2,5))
pc <- barplot(dNewxColumns$Counts,
        width = 1,
        space = .15,
        border = NA,
        axes = F,
        ylim = c(0,(3.05*max(dNewxColumns$Counts,na.rm = T))),
        ylab = "Cumulative Counts",
        names.arg = dNewxColumns$Species,
        cex.names = 0.70,
        main = "Species Pareto",
        las = 2)

# [MR] Add a cumulative counts line
lines(pc, dNewxColumns$Cumulativecounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

# [MR] Place a grey box around the pareto plot
box(col = "grey62")

axis(side = 2, 
     at = c(0, dNewxColumns$Cumulativecounts), 
     las = 1, 
     col.axis = "grey62", 
     col = "grey62", 
     cex.axis = 0.8)
axis(side = 4, 
     at = c(0, dNewxColumns$Cumulativecounts), 
     labels = paste(c(0, round(dNewxColumns$Cumulativefreq * 100)) ,
                    "%",
                    sep=""), 
     las = 1, 
     col.axis = "cyan4", 
     col = "cyan4", 
     cex.axis = 0.8)

par(def_par)
dev.off()














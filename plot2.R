a <- Sys.time(); NEI <- readRDS("summarySCC_PM25.rds"); b <- Sys.time(); b-a
#18-19 seconds
c <- Sys.time(); SCC <- readRDS("Source_Classification_Code.rds"); d <- Sys.time(); d-c
#<.2 seconds
names(NEI)
dim(NEI) #6497651x6
#[1] "fips"      "SCC"       "Pollutant" "Emissions" "type"      "year"
names(SCC)
dim(SCC) #11717x15
# [1] "SCC"                 "Data.Category"       "Short.Name"          "EI.Sector"          
# [5] "Option.Group"        "Option.Set"          "SCC.Level.One"       "SCC.Level.Two"      
# [9] "SCC.Level.Three"     "SCC.Level.Four"      "Map.To"              "Last.Inventory.Year"
# [13] "Created_Date"        "Revised_Date"        "Usage.Notes" 

# 2) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 
# 2008? Use the base plotting system to make a plot answering this question.
library(dplyr)
NEIT <- tibble(NEI)
sumByYearBCM <- NEI %>% 
        filter(fips == "24510") %>% 
        group_by(year) %>% 
        summarise(sum(Emissions))
sumByYearBCM
png(filename = "plot2.png",
    width = 480,
    height = 480)
plot(sumByYearBCM
     , pch = 19
     , xaxt="n"
     , yaxt = "n"
     , ylab = "Sum of P2.5 Emissions in Baltimore City, Maryland"
     , ylim = c(1500, 3500)
)
axis(side = 1
     , at = sapply(sumByYearBCM[,1], as.double)
     , las = 2)
#borrowed from 
#https://stackoverflow.com/questions/8918452/r-changing-format-of-scale-on-y-axis
pts <- pretty(sapply(sumByYearBCM[,2], as.double)/1000)
axis(side = 2
     , at = pts*1000
     , labels = pts*1000
     , las = 1)
dev.off()
#Total emissions in Balitmore City, Maryland have trended down overall between
#1999 and 2008, but went up significantly between 2002 and 2005
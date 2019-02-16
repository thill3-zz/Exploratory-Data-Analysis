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

# 1) Have total emissions from PM2.5 decreased in the United States from 1999 to 
# 2008? Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(dplyr)
NEIT <- tibble(NEI)
sumByYear <- group_by(NEI, year) %>% summarise(sum(Emissions))
sumByYear
png(filename = "plot1.png",
    width = 480,
    height = 480)
#could use a barplot here for better presentation
with(NEI
     , plot(sumByYear
            , pch = 19
            , xaxt="n"
            , yaxt = "n"
            , ylab = "Sum of P2.5 Emissions"
            , ylim = c(3000000, 8000000)
     )
)
axis(side = 1
     , at = sapply(sumByYear[,1], as.double)
     , las = 2)
#borrowed from 
#https://stackoverflow.com/questions/8918452/r-changing-format-of-scale-on-y-axis
pts <- pretty(sapply(sumByYear[,2], as.double)/1000000)
axis(side = 2
     , at = (min(pts):max(pts))*1000000
     , labels = paste(pts, "MM", sep = "")
     , las = 1)
abline(lm(sumByYear[,2]~sumByYear[,1],sumByYear))
dev.off()
#Looks like total emissions have decreased each time of measurement
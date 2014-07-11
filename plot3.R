## Coursera Exploratory Data Analysis (exdata-004) Course Project 2
## plot3.R: Creates an exploratory plot for Question 3: 

## 3. Of the four types of sources indicated by the type (point, nonpoint,
## onroad, nonroad) variable, which of these four sources have seen decreases in
## emissions from 1999–2008 for Baltimore City? Which have seen increases in
## emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
## answer this question.

## Of the four source types in the Baltimore City data, three of them (On-Road,
## Non-Road, and Nonpoint) have seen general decreasing trends over the period 
## 1999 to 2008.  The "Point" source has no clear trend over the entire time 
## frame, although it did increase from 1999 to 2005.


## Code to (a) unzip RDS files from original zip file; (b) read RDS files and 
## save their contents as RData files; (c) read RData objects into the 
## workspace.  This code will do the least work necessary to get the data.frames
## into the workspace.  This code is repeated in all R scripts for this
## assignment because we've been asked to keep the R scripts self-contained; in
## a real-world application, I would either write this code into a separate file
## that is source()d by this one, or create all the plots in a single R script.

zip.filename    <- "exdata-data-NEI_data.zip"
rds.filenames   <- c( NEI = "summarySCC_PM25.rds", SCC = "Source_Classification_Code.rds" )
rdata.filenames <- gsub( 'rds$', 'rdata', rds.filenames )
invis <- sapply( names( rds.filenames ), function( nom ) {  ## Loops over the *name* of the vector of rds.filenames
  if( !( rdata.filenames[ nom ] %in% dir() ) ) {  ## Rdata file does not exist; create it from RDS
    if( !( rds.filenames[ nom ] %in% dir() ) ) {  ## RDS file does not exist; extract it from zip
      message( sprintf( "Unzipping %s from %s...", rds.filenames[ nom ], zip.filename ) )
      unzip( zip.filename, rds.filenames[ nom ] )
    }
    message( sprintf( "Reading %s from disk...", rds.filenames[ nom ] ) )
    myData <- readRDS( rds.filenames[ nom ] )
    assign( nom, myData )
    message( sprintf( "Saving %s data as rdata object...", nom ) )
    save( list=nom, file=rdata.filenames[ nom ] )
  }
  message( sprintf( "Loading %s into workspace...", rdata.filenames[ nom ] ) )
  load( rdata.filenames[ nom ], .GlobalEnv )
} )

## Data loaded; now manipulate the data and create the plot.

library( ggplot2 )

message( 'Subsetting for Baltimore City...' )
NEI.BAL <- subset( NEI, fips == '24510')

message( 'Summing up by the interaction of year and type...' )
SumByYearType.BAL <- with( NEI.BAL, aggregate( Emissions, by=list( year, type ), FUN = sum ) )
colnames( SumByYearType.BAL ) <- c( 'Year', 'Type', 'Total.PM25' )

message( 'Order the labels for source type...' )
SumByYearType.BAL$Type <- factor( SumByYearType.BAL$Type, levels=c( 'ON-ROAD', 'NON-ROAD', 'POINT', 'NONPOINT' ) )

message( 'Creating the plot...' )
png( 'plot3.png', width=960, height=480 )
qplot( Year, Total.PM25, data=SumByYearType.BAL, facets=.~Type, geom='line',
       main=expression( 'Total ' * PM[2.5] * 'by Year and Type for Balitmore City, 1999-2008' ),
       ylab = 'Total.PM25 (tons)')
dev.off()

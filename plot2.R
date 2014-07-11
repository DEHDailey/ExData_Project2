## Coursera Exploratory Data Analysis (exdata-004) Course Project 2
## plot2.R: Creates an exploratory plot for Question 2: 

## 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
## (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
## plot answering this question.

## Based on the plot produced by the following code, yes, total emissions of
## PM2.5 in Baltimore City decreased over the period from 1999 to 2008.

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

message( 'Subsetting for Baltimore City...' )
NEI.BAL <- subset( NEI, fips == '24510')

message( 'Summing up emissions by year...' )
SumByYear.BAL <- with( NEI.BAL, aggregate( Emissions, by=list( year ), FUN = sum ) )
colnames( SumByYear.BAL ) <- c( 'Year', 'Total.PM25' )

message( 'Creating the plot...' )
png( 'plot2.png' )  # Default size is 480 by 480 pixels
  ## Yes, this is "only" an exploratory graph, but we can spend a little
  ## extra effort to make it easier to read
  with( SumByYear.BAL, plot( Year, Total.PM25/1e3, type='o', pch=16, cex=2, lwd=3, ylim=range( c( 0, Total.PM25/1e3 ) ),
                         xaxt='n', ylab=expression( 'Total ' * PM[2.5] * ' (thousands of tons)' ) ) )
  axis( side = 1, at= SumByYear$Year )
  title( main=expression( "Total " * PM[2.5] * " by Year for Baltimore City, 1999-2008" ) )
dev.off()


## Coursera Exploratory Data Analysis (exdata-004) Course Project 2
## plot6.R: Creates an exploratory plot for Question 6: 

## 6. Compare emissions from motor vehicle sources in Baltimore City with
## emissions from motor vehicle sources in Los Angeles County, California (fips
## == "06037"). Which city has seen greater changes over time in motor vehicle
## emissions?

## Over the period 1999 to 2008, Baltimore City has seen greater reductions than
## Los Angeles in PM25 emissions from motor vehicle sources. Reductions were
## considered on a percentage basis because the actual values differ between the
## two cities by an order of magnitude.

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

message( 'Subsetting for Baltimore City and Los Angeles...' )
NEI.Sub <- subset( NEI, fips %in% c( '24510', '06037' ) )

message( 'Identifying and subsetting for motor-vehicle sources...' )
vehicleSCCs <- as.character( with( SCC, SCC[ grep( 'Vehicle', EI.Sector ) ] ) )
NEI.Sub.Vehicles <- subset( NEI.Sub, SCC %in% vehicleSCCs )

message( 'Summing up emissions by year and city...' )
SumByYear <- with( NEI.Sub.Vehicles, aggregate( Emissions, by=list( year, fips ), FUN = sum ) )
colnames( SumByYear ) <- c( 'Year', 'FIPS', 'Total.PM25' )
SumByYear$Total.PM25.K <- with( SumByYear, Total.PM25/1e3 )
SumByYear$Total.PM25.M <- with( SumByYear, Total.PM25/1e6 )
SumByYear$City <- with( SumByYear, ifelse( FIPS == '06037', "Los Angeles",
                                           ifelse( FIPS == '24510', "Baltimore City", "Bad FIPS" ) ) )

## Total.PM25 values are different by an order of magnitude between the cities. 
## It will be better to compare percentage change from the baseline value of
## 1999.

## Add baseline and %change values
SumByYear <- do.call( rbind, lapply( split( SumByYear, SumByYear$FIPS ), function( ss ) {
  ss$Baseline <- ss$Total.PM25[ ss$Year == 1999 ]
  return( ss )
} ) )
SumByYear$Difference <- with( SumByYear, Total.PM25 - Baseline )
SumByYear$PctChange  <- with( SumByYear, Difference / Baseline ) * 100



library( lattice )
png( 'plot6.png', width=640, height=480 )
xyplot( PctChange ~ Year | City, data=SumByYear , type='l',
        panel = function( x, y, ... ) {
          panel.xyplot( x, y, ... )
          panel.abline( h = 0, lty='dashed', col='darkgray' )
        },
        main=expression( PM[2.5] * " Emissions from Motor-Vehicle Sources, Percent Change from 1999 Baseline")
      )
dev.off()


# Create a png with 4 plots (2 x 2):
# globalActivePower
# voltage
# energy
# globalReactivePower
plot4<-function(){
  # load data
  data<-loadUsedData();
  
  # add datetime (created from Date and Time columns) column to datadrame
  data <- within(data, datetime <- as.POSIXct(strptime(paste(Date, Time, sep=" "),"%d/%m/%Y %H:%M:%S")))
  
  # set png device
  png(width=480, height=480, file="plot4.png")
  
  par(mfrow=c(2,2))
  
  globalActivePowerSubPlot(data)
  voltageSubPlot(data)  
  energySubPlot(data)
  globalReactivePowerSubPlot(data)
  
  # close device
  dev.off()
  print("done")
}

# draw globalReactivePower plot
globalReactivePowerSubPlot<-function(data){
  
  with(data, plot(Global_reactive_power~datetime, type="l",ylab="Global_reactive_power",xlab="datetime"))
  
  # boxing
  box()
}

# draw globalActivePower plot
globalActivePowerSubPlot<-function(data){
  
  with(data,plot(Global_active_power~datetime, type="l", ylab="Global Active Power", xlab=""))
  
  # boxing
  box()
}

# draw voltage plot
voltageSubPlot<-function(data){
  # draw plot
  with(data, plot(Voltage~datetime, type="l", ylab="Voltage", xlab="datetime"))
  
  # boxing
  box()
}

# draw Energy plot
energySubPlot<-function(data){
  # draw plot
  plot(data$Sub_metering_1~data$datetime, type="l", ylab="Energy sub metering", xlab="")
  lines(data$Sub_metering_2~data$datetime, col="red", type="l")
  lines(data$Sub_metering_3~data$datetime, col="blue", type="l")
  
  # add legend
  legend("topright", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, box.lwd=0 )
  
  # boxing
  box()  
}

# Load input data:
# download file from internet
# filter out irrelevant data
loadUsedData<-function(){
  # Create directory if missing
  if(!file.exists("data")){
    dir.create("data")
  }
  
  extractedData<-"./data/household_power_consumption.txt"  
  
  # download and extract the file if missing
  if(!file.exists(extractedData)){  
    fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipFile<-"./data/exdata_data_household_power_consumption.zip"
    download.file(fileUrl, destfile=zipFile, method = "curl")
    
    # extract data    
    unzip(zipFile,exdir="./data")
  }
  
  # read the first 10 lines to determine the classes of the rows - classe will be use to increase reading speed later
  d<-read.table(extractedData, sep=";", header=TRUE, nrows=10)
  classes <- as.list(sapply(d, class))
  
  # use internaly awk to filter relevant rows - increase processing spead
  awkCommad<-"awk -F\";\" \' NR==1 || $1 == \"1/2/2007\" || $1 == \"2/2/2007\"  {  print $0}\'"
  filterCommand<-paste(awkCommad,extractedData)
  
  # read relevant data
  filteredData <- read.table(pipe(filterCommand), sep=";", na.strings="?", colClasses=classes, header=TRUE)
  filteredData
}

plot4()

# Create a plot with Energy sub metering 1-3 with legend
plot3<-function(){
  # load data
  data<-loadUsedData();
  
  # add datetime (created from Date and Time columns) column to datadrame
  data <- within(data, datetime <- as.POSIXct(strptime(paste(Date, Time, sep=" "),"%d/%m/%Y %H:%M:%S")))
  
  # set png device
  png(width=480, height=480, file="plot3.png")
  
  # draw plot
  plot(data$Sub_metering_1~data$datetime, type="l", ylab="Energy sub metering", xlab="")
  lines(data$Sub_metering_2~data$datetime, col="red", type="l")
  lines(data$Sub_metering_3~data$datetime, col="blue", type="l")
  
  # boxing
  box()
  
  # add legend
  legend("topright", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1 )
  
  # close device
  dev.off()
  print("done")
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

plot3()
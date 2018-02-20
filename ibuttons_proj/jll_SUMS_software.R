# THIS PROGRAM COUNTS THE NUMBER AND LENGTH OF COOKING EVENTS

uganda_dir <- "/Users/javierlascurain-leon1/Dropbox/World Bank Uganda_ willingness to pay"

sums_dir <- file.path(uganda_dir, "Data/SUMS")
software_dir <- file.path(uganda_dir, "SUMS software")
desktop_dir <- "/Users/javierlascurain-leon1/Desktop"

# Import data
h <- read.csv(sums_dir %>% file.path(
    "Follow Up Post WTP_WTA SUMs data", "Banda", "001-BAN INTERVENTION STOVE STOVE.csv"), 
    nrows=30)
h1 <- which(h == "Date/Time")


# Create a database of all file names to import
sums.files = function () {
  
  e <- c("Mid-term SUMs data", "Follow up SUMs data", "Follow Up Post WTP_WTA SUMs data")
  a <- lapply(e, . %>% file.path(sums_dir, .) %>% list.files())
  
  d <- data.frame()
  k <- 1
  for (i in 1:3) {
    y = file.path(sums_dir, e[i], a[[i]])
    
    for (j in seq_along(y)) {
      # Get file names of directory
      x = list.files(y[j])
      for (m in seq_along(x)) {
        ifelse(i == 1, 
              d[k,1] <- "Mid-term SUMs data",
          ifelse(i == 2, 
              d[k,1] <- "Follow up SUMs data", 
              d[k,1] <- "Follow Up Post WTP_WTA SUMs data"))
        d[k,2] <- a[[i]][j]
        d[k,3] <- x[m]
        k <- k+1
      }
    }
  } 
  return(d)
}

# Generate a data frame with the file names and household names
d_nonames <- sums.files()
write.csv(d_nonames, desktop_dir %>% file.path("sums files Oct 14 2015.csv"))

d <- d_nonames %>% 
  set_names(c("sums.collection", "parish", "file.name")) %>% 
  mutate(file.code = file.path(sums.collection, file.name))

# View(d)
write.csv(d, file.path(software_dir, "all sums files.csv"))

j1 = read.csv(file.path(software_dir, "file names and hh codes.csv"))
# View(j1)

j1$file.name = as.character(j1$file.name)
d1 = merge(d, j1, by.x="file.code", by.y="file.name")
d$file.name[1] == j1$file.name[2]
# class(j1$file.name)

# THE RESULTS OF THE ABOVE CODE LINES ARE STORED IN "file names and hh codes.csv"

# Import data with HH.codes in proper format
files.sums = read.csv(software_dir %>% file.path("file names and hh codes.csv"))
View(files.sums)
# Create full file name
files.sums$file.name = sums_dir %>% file.path( 
  files.sums$sums.collection, files.sums$parish, files.sums$file.name)
#files.sums=files.sums[,-c(1,2,4)]

# Function to define starting row for data import
start.row = function (file.name) {
  h  <- read.csv(file.name, nrows = 30)
  h1 <- which(h == "Date/Time")
  return(h1)
}

files.sums$start.row = sapply(files.sums$file.name, start.row)
# View(files.sums)

# Reduce dataset to only intervention or traditional stoves
# files.sums = files.sums[files.sums$sums.type == "int",]
# files.sums = files.sums[files.sums$sums.type == "trad",]
# files.sums = files.sums[files.sums$sums.type == "trad"|files.sums$sums.type == "int",]
# files.sums=files.sums[c(1:45),]
# View(files.sums)

# Function to import data AND generate databases of cooking episodes
get.data <- function (file.name, start.row) {
  sums <- read.csv(file.name, skip=start.row)
  SUMS(sums)
}

# Test get.data function

X <- get.data(files.sums$file.name[1], files.sums$start.row[1])

# Function to aggregate the data into data frames
X <- data.frame()
Y <- data.frame()
for (i in 1:nrow(files.sums)) {
  X <- get.data(files.sums$file.name[i], files.sums$start.row[i])
  X$HH.code    <- files.sums$HH.code[i]
  X$stove.type <- files.sums$sums.type[i]
  X$collection <- files.sums$sums.collection[i]
  Y <- rbind(Y,X)
  X <- NULL
}

write.csv(Y, file.path(software_dir, "cooking episode data 4.csv"))

A <- data.frame()
# Function to calculate cooking episodes
SUMS <- function (sums, ces=6, s=1.145691, min.cooking.time=10, reading.frequency=10) {
  sums$slope <- 0
  sums$slope[c(2:nrow(sums))] <- sums$Value[c(2:nrow(sums))] - sums$Value[c(1:nrow(sums)-1)] 
  # Create time variable
  sums$time <- strptime(structure(sums$Date.Time), format="%m/%d/%y %I:%M:%S %p")
  # Estimate which records have temperature slope values above threshold
  d <- which(sums$slope >= s)
  d[length(d)+1] <- nrow(sums) # Add last row to data so software can estimate final time of last
  # cooking episode in the series
  # Calculate difference in time between records with slope above threshold
  e <- d[c(2:length(d))] - d[c(1:length(d)-1)] #Note: if the difference between the last record and the 
  # last record with eligible slope is less than 6 datapoints, this cooking episode is not counted
  # Identify points in d which mark end of cooking episodes
  a <- which(e >= ces)
  # Identify points in d that mark start of cooking episodes
  b <- c(1, a+1)
  # Remove last starting point because it has no ending time of cooking episode and may not be
  # start
  b <- b[-which.max(b)] 
  # Note that if difference between b[length(b)-1] and end point is less than 6 and more than one 
  # (or minimum cooking time), we may be truncating this cooking episode
  # Obtain maximum temperature
  H <- cbind(a, b)
  H <- H[which(a-b > 0), ]
  max.temp  <- NULL
  min.slope <- NULL
  for (i in 1:length(a)) {
    max.temp[i] <- max(sums$Value[c(d[b[i]]:d[a[i]])])
    a1 <- d[a[i]]
    a2 <- a1 + 4
    min.slope[i] <- min(sums$slope[c(a1:a2)])
  }
  # Obtain start time of cookign episode
  start.time <- sums$time[d[b]] - reading.frequency*60 
  # Subtract reading.frequency*60 becuase
  # we mark the start of the cooking episode when the eligible slope begins
  # Obtain end time of cooking episode
  end.time <- sums$time[d[a]]
  # Calculate cooking times of all episodes
  cooking.time <- end.time - start.time
  # Create day of month
  day <- paste0(end.time$year - 100, "-", end.time$mon+1, "-", end.time$mday)
  # day = end.time %$% sprintf("%i-%i-%i", year, mon + 1, mday)
  # day = strptime(structure(day), format= "%y-%m-%d")
  
  G <- unique(day)
  
  # Create data frame with cooking episodes
  A <- data.frame(
      start.time   = start.time, 
      end.time     = end.time, 
      cooking.time = cooking.time, 
      day          = day, 
      max.temp     = max.temp,
      min.slope    = min.slope) %>% 
    filter(cooking.time > min.cooking.time)
  # Eliminate cooking episodes of less than the minimum cooking time
  # A <- A[cooking.time > min.cooking.time, ]
  return(A)
}


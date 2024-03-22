This README.txt file was generated on 2024-Mar-19 by Jackson Powell

GENERAL INFORMATION

1. Title of Dataset: How modularity and heterotrophy complicate the understanding of causes of thermal performance curves: the case of feeding rate in a filter feeding animal

2. Author Information
	A. Principal Investigator Contact Information
		Name: Scott Burgess
		Institution: Florida State University
		Address: 319 Stadium Drive, Tallahassee, FL, USA 32306
		Email: sburgess@bio.fsu.edu

	B. Lead Author Contact Information
		Name: Jackson Powell
		Institution: Florida State University
		Address: 319 Stadium Drive, Tallahassee, FL, USA 32306
		Email: jacksonpowell129@gmail.com


3. Data of data collection (single date, range, approximate date): 2023-May-17 - 2023-Jun-20
4. Geographic location of data collection: Dog Island, Florida, USA

5. Information about funding sources that supported the collected of the data: National Science Foundation (NSF; OCE-1948788) and the Florida State University Council on Research and Creativity

DATA & FILE OVERVIEW

1. File List:
Figure 1.R
Figure 2.R
Figure 3.R
Figure 4.R
Calculated realized temperatures.R
Clearance Rate Data.csv
Phenotypic Data.csv
Temperature logger data.csv
2. Relationship between files:
Figure 1.R uses Clearance Rate Data.csv
Figure 2.R uses Clearance Rate Data.csv and Phenotypic Data.csv
Figure 3.R uses Clearance Rate Data.csv and Phenotypic Data.csv
Calculate realized temperatures.R uses Temperature logger data.csv
3. Metadata

Clearance Rate data.csv
bowl.name: Unique identifier for each bowl 
bugula.present: Y = Bowl with a Bugula neritina colony present, N = bowl absent of B. neritina colonies
batch: 1 = First experimental run using 22-day old B. neritina colonies; 2 = second experimental run using 25-day B. neritina old colonies
tankID: Unique identifier for the tank that was used as a water bath for bowls of algae. Also indicates the tanks’ target temperatures
target.temp: Temperature (°C) that tanks were assigned to reach
realized.temp: Median actualized temperature (°C) recorded by a temperature logger
time: Timestep of data collection each in days
cells.per.ul: Cell concentration per microliter from samples collected
ssc: Median side scatter area; proxy for median cell internal complexity/granularity
fsc: Median forward scatter area; proxy for median cell size


Phenotypic data.csv
name: Unique identifier for each B. neritina colony. Note that B. neritina colonies share the name as the bowls that they were placed into
batch: 1 = First experimental run using 22-day old B. neritina colonies; 2 = second experimental run using 25-day old B. neritina colonies
tankID: Unique identifier for the tank that was used as a water bath for bowls of algae. Also indicates the tanks’ target temperatures
target.temp: Temperature (°C) that tanks were assigned to reach
realized.temp: Median actualized temperature (°C) recorded by a temperature logger
time: Timestep of data collection in days
feeding.zooids: Number of zooids possessing a visible gut and lophophore tentacles inside the cystid
regressed.zooids: Number of zooids with visible signs of regression (brown bodies or tissue lacking lophophore tentacles)
dead.zooids: Cystids entirely lacking polypide tissue (visibly empty)
Temperature logger data.csv

log: The nth recording made by a temperature loggertime: Date and time (Eastern Daylight Time) when temperature was recorded by a loggertemp: Actualized temperature (°C) recorded by a temperature loggertarget.temp: Temperature (°C) that tanks were assigned to reachbatch: 1 = First experimental run using 22-day old B. neritina colonies; 2 = second experimental run using 25-day old B. neritina colonieslogger: Unique identifier for each temperature logger


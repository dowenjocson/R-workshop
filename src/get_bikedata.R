##

#Get bike data from the workshop folder and save it in the data folder

download.file (
  url= "https://drive.google.com/drive/folders/1As1_9xVlmDcuz2JAoC-t5jfHIH7d3g51?usp=sharing",
  destfile="data/daily_bike_data.csv"
)


#destfile is the name of the destination file

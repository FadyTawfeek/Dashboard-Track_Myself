# A dashboard application made in R shiny web framework, it uses data collected from Track Myself mobile app that is made for Parkinson's disease patients to help assess their symptoms level and medication adherence.
A dashboard to view charts presenting the patient's data over time, data viewed on Y-axis are game scores, daily surveys, medication adherence (calculated by the difference in
time between the medication's optimal time and the time when the patient took this medication), to be viewed by the patient and the doctors to evaluate the changes in 
symptoms and decide if a different tratment plan is needed.


It can be accessed from: https://fadytawfeek.shinyapps.io/Track-Myself/ on any browser or directly from the app itself using the device ID. (Please open the screenshots for better quality).


![alt text](https://github.com/FadyTawfeek/Dashboard-Track_Myself/blob/main/daily%20dashboard.png)


A 15-minute time difference red horizontal line is drawn in the medication adherence chart to serve as a break point of good and poor medication adherence,
and a blue trendline is drawn in all charts to represent the trend of the all time data for the patient (for example if the game scores are increasing,
the trendline will have a positive slope. X-axis represents date, and its format varies according to the date range selected (day, week, month):


![alt text](https://github.com/FadyTawfeek/Dashboard-Track_Myself/blob/main/weekly%20dashboard.png)


![alt text](https://github.com/FadyTawfeek/Dashboard-Track_Myself/blob/main/monthly%20dashboard.png)


For extended view of the data, 3 side tabs are used so that data is shown for every day even if the user chooses a long date range (and the date orientation is tilted when date range is too long).


![alt text](https://github.com/FadyTawfeek/Dashboard-Track_Myself/blob/main/med%20side.png)


![alt text](https://github.com/FadyTawfeek/Dashboard-Track_Myself/blob/main/sym%20side.png)

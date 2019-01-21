# ASAShootingApp

Large raw files are stored in an ignored directory called "IgnoreList". This allows collaborators to update the app with new data, but you can still run the app even if you don't have access to ASA raw data. Matthias keeps the master copy of the IgnoreList directory.

The sequence of data update scripts can be found in an ignored directory called "MasterUpdate". 00_MasterUpdate.R sources multiple scripts which read in new data from Dropbox, clean and prepare the data for the app, produce the HTML tables, and push the app up to the shinyapps.io server. This allows collaborators to update the app with new data, but you can still run the app even if you don't have access to ASA raw data

The paths to Dropbox are currently written for Matthias' computers, and are not robust to other computers. Other collaborators will need to review each file sourced in 00_MasterUpdate.R and change file paths to his or her local Dropbox folder. 

The app code is contained in the ui.R, server.R, and global.R files. Summarized data files required to run the app can be found in "AppData", and functions required to run the app can be found in "Functions".

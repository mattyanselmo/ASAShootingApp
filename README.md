# ASAShootingApp

Large raw files are stored in an ignored directory called "IgnoreList". You should upload this directory to your R shiny project.
This directory can be found here: ~\Dropbox\ASA Blog Data\ShootingShinyApp\IgnoreList\

00_MasterUpdate.R sources multiple scripts which read in new data from Dropbox, clean and prepare the data for the app, produce the HTML tables, and push the app up to the shinyapps.io server. 

The paths to Dropbox are currently written for Matthias' computers, and are not robust to other computers. Other contributors will need to review each file sourced in 00_MasterUpdate.R and change file paths to his or her local Dropbox folder. 

The app is contained in the ui.R, server.R, and global.R files. 

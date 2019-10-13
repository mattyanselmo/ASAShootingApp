# Notes and Planning

## Plan
Each tab/sub-tab has a slightly different set of filters in the side bar
and slightly different visualization(s). Because of this, a module needs 
to be made for each sub-tab's filters and for the visualization. For 
example the xGoals>Players tab will have it's own module for the 
sidebar filters and a module for the visualization.

There may be some opportunities for modularizing smaller pieces such as
title of the tab, download csv tab, or if in fact there is overlap
in sidebar filters or visualizations. Below is an outline of each sub-tab
to identify overlaps.

## Using the server
It should be noted that with the digital ocean server coming online, the filtering on the server side for all these modules will need to be translated to SQL queries 

## xGoals
### Players
The side bar filters elements:
* Refresh filters button
* numeric input Minimum minutes
* numeric input Minimum shots
* numeric input Minimum key passes
* radiobutton for season or date
* seasons/date dropdown
* teams dropdown
* positions dropdown
* shot patterns dropdown
* radiobutton for split
The visualization consists of:
* Title
* csv download button
* three tab panel of totals table, per 96 table, and scatter plot

### Teams
The side bar filters elements:
* radiobutton for basic or advanced stats
* radiobutton for standard v home-adjusted
* checkbox for view
* checkbox for split
* radiobutton season/date
* season/date dropdown
* pattern of play dropdown
* checkbox for gamestate
* checkbox for home/away
The visualization consists of:
* title
* csv download button
* four tab panel for totals, per game, scatter, and scatter split season

### Game by Game xG
The side bar filters elements are:
* radiobutton season/date
* season/date dropdown
The visualization consists of:
* Title
* csv download
* table

### Keepers 
The side bar filters elements:
* Refresh filters button
* numeric input Minimum minutes
* numeric input Minimum shots faced
* radiobutton for season or date
* seasons/date dropdown
* teams dropdown
* shot patterns dropdown
* radiobutton for split
This visualization is the same as the players tab and thus might be able to share the module, but filters are different
The visualization consists of:
* Title
* csv download button
* three tab panel of totals table, per 96 table, and scatter plot

### Other tabs
There are two additional tabs that have been prepped in the RoryDev branch. Both use
custom sidebar and visualizations

## xPasses
### Players
The side bar filters elements:
* Refresh filters button
* numeric input Minimum minutes
* numeric input Minimum passes
* radiobutton for season or date
* seasons/date dropdown
* teams dropdown
* positions dropdown
* radiobutton for area of the pitch
* radiobutton for split
This visualization is the same as the players tab and thus might be able to share the module, but filters are different
The visualization consists of:
* Title
* csv download button
* three tab panel of totals table, per 96 table, and scatter plot

### Teams
The side bar filters elements are:
* radiobutton season/date
* season/date dropdown
* dropdown area of pitch
* checkbox for split
The visualization is similar to the xGoals teams tab, though without the scatter split season tab. Might be able to share
The visualization consists of:
* title
* csv download button
* tab panel for totals, per game, scatter

## Possession chains
### Players
The side bar filters elements:
* Refresh filters button
* numeric input Minimum minutes
* radiobutton for season or date
* seasons/date dropdown
* teams dropdown
* positions dropdown
* radiobutton for split
This visualization is the same as the players tab and thus might be able to share the module, but filters are different
The visualization consists of:
* Title
* csv download button
* two tab panel of totals table and per 96 table

## Other tabs
There is not an obvious need to modularize
the predictions, salaries, glossary, or App info tabs at this time because they are simpler and take up less room.
# CS-424_Project-02

## Project Description

In this project I build a ShinyApps.io dashboard (https://mzimin2.shinyapps.io/CS-424_Project-02/) to display the data from https://www.epa.gov/egrid/download-data. More specifically these files: egrid2018_data_v2.xlsx, egrid2010_data.xls, and egrid2000_plant.xls.

### Project Objectives:
1) Show the plant generation data for the state of Illinois. Allow the user to pick and choose which energy sources they want to view.
2) Split the screen in half to have a Left/Right pane. Each pane needs to allow the user to be able to select a state and year to view. And there should be an option to be able to sync the check boxes between the 2 panes.
3) The user should be able to see the whole continential US, zoomed respectivly, and be able to use the check boxes from part 1/2 and an additional sliders for the power generation ranges.

---

## Install RStudio (Using Anaconda)

Goto https://www.anaconda.com/products/individual. Select the correct download for you local machine's Operating System (i.e., Linux, macOS, Windows, etc.) and follow the installation turorial to successfully install Anaconda.

To use RStudio, open the Anaconda GUI Navigator and click on RStudio. RStudio should then open.

## Cloing repository (paste command in your terminal)

***HEADS UP:*** Before pasting the command navigate to the directory you want to save the repository in

Option 1, if ssh is properly set up:
```
git clone git@github.com:mziminski/CS-424_Project-02.git
```
Option 2, if ssh is NOT properly set up:
```
git clone https://github.com/mziminski/CS-424_Project-02.git
```
option 3, if using GitHub CLI:
```
gh repo clone mziminski/CS-424_Project-02
```

## Opening Program

Go back to RStudio and in the file navigation pane (bottom right) navigate to the location your cloned Reop. Navigate inside this folder and open the app.R file. You should be able to run it without errors, but if errors are present use the following commands:

Do this first before trying the conda option, by adding the following line above the library(...) line of code.
```
library(purrr)
library(dplyr)
library(shiny)
library(stringr)
library(shinydashboard)
library(shinythemes)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(DT)
```

If the above doesn't work then install the conflicting package(s) usign the below code block:

```
install.packages("Package_Name")
```

## Running Program

If you didn't have any errors or you solved the errors you had before, then you can run the app.R file by pressing the green run button near the top-right of the left half of the RStudio window.

## You're Done
And there you have it, this repo should be running locally on your machine and a Shiny App should be popping up in a new window after RStudio compiles it!





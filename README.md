# CS-424_Project-02

TODO: Write Project Description

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
<br>install.packages("ggplot2")
<br>install.packages("rgeos")
<br>install.packages("ggmap")
<br>install.packages("maps")
<br>install.packages("mapdata")
<br>install.packages("maptools")
<br>install.packages("ggthemes")
<br>install.packages("sp")
<br>install.packages("stringr")
<br>install.packages("plyr")
```

If the above doesn't work then use your local machine's terminal and type the following commands:

```
<br>conda install -c conda-forge r-rgeos
<br>conda install -c conda-forge r-ggmap
<br>conda install -c conda-forge r-mapdata
<br>conda install -c conda-forge r-maptools
<br>conda install -c conda-forge r-ggthemes
<br>conda install -c r r-maps
```

## Running Program

If you didn't have any errors or you solved the errors you had before, then you can run the app.R file by pressing the green run button near the top-right of the left half of the RStudio window.

## You're Done
And there you have it, this repo should be running locally on your machine and a Shiny App should be popping up in a new window after RStudio compiles it!





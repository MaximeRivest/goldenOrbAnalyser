# goldenOrbAnalyser
Golden Orb Analyser is an R project that import and analyse bibliometric files from Scopus and Web of Science. It`s main goal is to provide a quick characterization of a scientific field.
## How it works ?

For now you can make it work in a few simple steps:
* Search for your scientific field of interest in Web of Science Core Collection.
* Download all the records obtained from the above mentionned search.

  To download the files :
  * click on the arrow next to **Save to EndNote online*.*
  * click on **Save to Other File Formats** in the menu that just appeared.
  * enter the required information. Note that you will be able to download only 500 records at the time. Therefore, you will have to repeat that step a few times.
  * in the record content field choose **Full Record and Cited References**
  * in the file format field choose **Tab-delimited (Win/Mac, UTF-8)**
  
* Now for windows, Linux and Mac users alike, you can put all the downloaded files in a folder which you should name `WOS_files/`. This folder should be in the `goldenOrbAnalyser/` directory.
* Now you can open R console or Rstudio or anything else you use.
* In the console you should run `setwd('path to goldenOrbAnalyser/')` where you replace path to goldenOrAnalyser with the path on your computer.
* Then you can finally open the app.R script and run all lines. Note that you should install the required packages before hand. A list of those is provided at the top of the app.R script. Note that the `make_cocitation_table_new()` function takes a while and that if you have more then approximatly 7000 records in your `WOS_files/` in a computer with 4.00 GB of ram it will abruptly(are not so abruptly because it will lag much) stop the analysis.

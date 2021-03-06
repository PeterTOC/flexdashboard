# Job Board Project

> The code files of interest would be;
>  - The scraping script labeled "table.R" 
>  - The code for the dashboard layout labelled "index.Rmd"


A great resource on setting up the project locally and on github can be found [here](https://www.r-bloggers.com/2020/09/deploying-flexdashboard-on-github-pages/)


## About the ETL project,



Extracting: 

- Web scraping a job board's website  and loading its HTML to my local environment.



Transforming: 

- parsing the content and grabbing elements of interest 🎣 

- skimming and filtering depending on preset personal preference 

- adding metrics that matter to me

- ranking and tabulating the data



Loading:

- Reformating the data into a dashboard displaying metrics of interest, published from Github.

- features included; clickable links to the original job posts, a downloadable data table, and most importantly, the developer's option to automate the process.

![dashboard](https://github.com/PeterTOC/flexdashboard/blob/master/docs/dashboard-20220512110924-1920x941.png)


Summary of layout;

- our table of interest

- Passion Hits; how many jobs hit my passion interests (in this case R programming)

- Hit to Miss Ratio; how well my interests are doing in the job market

- word cloud displaying the buzzwords mined from the job descriptions loaded. 🤭 space filler 🤫.

Tech Stack: R, Git, Github

Here is a link to the [published dashboard!](https://petertoc.github.io/flexdashboard/)

NOTE: Web scraping should always be done as a last resort, APIs and databases, when available, is always the way to go.

![wordcloud](https://github.com/PeterTOC/flexdashboard/blob/master/docs/webshot.png)

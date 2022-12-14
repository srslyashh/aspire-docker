# aspire-docker

### Useful documentations: 
- talks about buildpack that heroku uses: https://github.com/virtualstaticvoid/heroku-buildpack-r
- example of docker with heroku: https://github.com/virtualstaticvoid/heroku-docker-r-shiny-app
- rgdal package example with docker: https://github.com/virtualstaticvoid/heroku-docker-r-rgdal-example
    > issues i had : 
           - could not use Aptfile for some reason (i tried to use the buildpack heroku-community/apt on heroku)
           - docker gave these errors: "No sandbox user '_apt' on the system, can not drop priveleges.

### Files and what they contain
- all_census.RDS : data we are using for the app.R file
- app.R : contains .css code, ui code and server code of the application
- heroku.yml & Dockerfile: needed for the Docker to function according to the example above
- init.r : where all the packages and dependencies are downloaded 
- webpages.r : contains the code for webpages for the ui side
- run.R : telling heroku where to run R shiny

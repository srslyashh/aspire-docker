library(shiny)
library(shiny.router)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(leaflet) #interactive map
library(htmlwidgets) #labels on interactive maps
library(sf)         # spatial data - need
library(tigris)     # geojoin - need
library(raster)
library(plyr)
library(dplyr)
library(plotly)
library(fresh) # fresh is used to create a custom bootstrap application with customized font

source('webpages.r')

router <- make_router(
  route("/", home_page),
  route("censusdata", censusdata),
  route("schooldistricts", schooldistricts)
)
all_census = readRDS("all_census.RDS")

# Generates linebreaks when it is called. 
# N here refers to the amount of linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

# POVERTY DATA 
povertydata = c("Tot_pov_U_5_18", 
                "Tot_pov_U_5_17_18",
                "Tot_pov_U_18",
                "Tot_pov_RUC_18s")

poverty_legendT = c("Children in poverty </br> under 5 years",
                    "Children in poverty </br> from 5 to 17 years",
                    "Children in poverty </br> under 18 years",
                    "Related children of householder </br> in poverty under 18 years")

poverty_histT = c("Children in poverty under 5 years", 
                  "Children in poverty from 5 to 17 years",
                  "Children in poverty under 18 years",
                  "Related children of householder in poverty under 18 years")

poverty_meanT = c("Average total of children \n in poverty under 5 years", 
                  "Average total of children \n in poverty from 5 to 17 years",
                  "Average total of children \n in poverty under 18 years",
                  "Average total of related children \n of householder in poverty under 18 years")

#HEALTH COVERAGE DATA

hcdata = c("M_U_6__W_HIC", 
           "M_U_6__N_HIC",
           "M_16_18__W_HIC",
           "M_16_18__N_HIC")

healthC_legendT = c("Male under 6 years </br> with health insurance coverage", 
                    "Male under 6 years </br> without health insurance coverage",
                    "Male under 6 to 18 years </br> with health insurance coverage",
                    "Male under 6 to 18 years </br> without health insurance coverage")

healthC_histT = c("Male under 6 years with health insurance coverage", 
                  "Male under 6 years without health insurance coverage",
                  "Male under 6 to 18 years with health insurance coverage",
                  "Male under 6 to 18 years without health insurance coverage")

healthC_meanT = c("Average total of male under 6 years \n with health insurance coverage", 
                  "Average total of male under 6 years \n without health insurance coverage",
                  "Average total of male under 6 to 18 \n years with health insurance coverage",
                  "Average total of male under 6 to 18 years \n without health insurance coverage")

#HOUSEHOLD DATA

hhdata = c("Tot_hld_U_3", 
           "Tot_hld_3_4",
           "Tot_hld_5.x",
           "Tot_hld_6_8", 
           "Tot_hld_9_11",
           "Tot_hld_12_14",
           "Tot_hld_15_17")

household_legendT =c("Children in households \n under 3 years old", 
                     "Children in households \n that are 3 to 4 years old",
                     "Children in households \n that are 5 years old",
                     "Children in households \n that are 6 to 8 years old", 
                     "Children in households \n that are 9 to 11 years old",
                     "Children in households \n that are 12 to 14 years old",
                     "Children in households \n that are 15 to 17 years old")

household_histT = c("Children in households under 3 years old", 
                    "Children in households that are 3 to 4 years old",
                    "Children in households that are 5 years old",
                    "Children in households that are 6 to 8 years old", 
                    "Children in households that are 9 to 11 years old",
                    "Children in households that are 12 to 14 years old",
                    "Children in households that are 15 to 17 years old")

household_meanT = c("Average number of children \nin households under 3 years old", 
                    "Average number of children in  \nhouseholds that are 3 to 4 years old",
                    "Average number of children in  \nhouseholds that are 5 years old",
                    "Average number of children in  \nhouseholds that are 6 to 8 years old", 
                    "Average number of children in  \nhouseholds that are 9 to 11 years old",
                    "Average number of children in  \nhouseholds that are 12 to 14 years old",
                    "Average number of children in  \nhouseholds that are 15 to 17 years old")

#RESOURCE DATA

resourcedata = c("ALAND.x",
                 "AWATER.x")

resource_legendT = c( "Water resource in certain areas",
                      "Land resource in certain areas")

resource_meanT = c("Average water resource",
                   "Average land resource")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$script(src="https://kit.fontawesome.com/169495e0b5.js"),
  tags$head(
    tags$style(HTML(
      "
       /* Importing the fonts used from googlefont */
       @import url('https://fonts.googleapis.com/css2?family=El+Messiri&family=Orelega+One&family=Roboto+Slab:wght@400;500;600&family=Work+Sans:wght@600&display=swap');
        
        /* 
            Responsive css for mobile device 
        */
        
        
        /*
            Downloading font for the website.
        */
        
          @font-face {
              font-family: 'Stratum2WebBold';
              src: url('./stratumfontfiles/Stratum2WebBold.woff2') format('woff2'),
                url('./stratumfontfiles/Stratum2WebBold.woff') format('woff');
              font-display: swap;
          }
        
         sup.title{
            margin-right: -5px;
            margin-left: -5px;
         }
         
         .aspfooter-title
         {
            font-size: 20px;
         }
         
         *{
              margin: 0px;
              padding: 0px;
         }
         
         body
         {
            font-size: 1.2rem;
         }
            
            .flexbox-item{
              height: 200px;
              width: 350px;
              margin: 10px;
              box-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25);
              border-radius: 15px;
              background-color: rgba(217,217,217, 0.25);
            }
            
            .source-list{
              margin-top: 50px;
              width: 80%;
              margin-left: 10%;
              margin-right: 10%;
              margin-bottom: 2%;
            }
            
            .census-item{
              width: 400px;
              margin: 2% 2% 0px;
              box-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25);
              border-radius: 15px;
              background-color: rgba(217,217,217, 0.25);
            }
            
            .census-item-1{
              flex-shrink: 0;
            }
            
            .census-item-2{
              background-color: #F1CA9C;
              flex-shrink: 0;
            }
            
            .flexbox-item-1{
              flex-shrink: 0;
            }
            
            .flexbox-item-2{
              flex-shrink: 0;
            }
            
            .flexbox-item-3{
              flex-shrink: 0;
            }
            
            .flexbox-item-4{
              flex-shrink: 0;
            }
            
            .photo-flexbox-1{
              background-image: url(./census_householdage_left.jpg);
              background-position: 50% 20%;
            	background-position: no-repeat;
            }
            
            .photo-flexbox-2{
              background-image: url(./school_grades_left.jpg);
              background-position: 50% 55%;
            	background-position: no-repeat;
            }
            
            .photo-flexbox-3{
              background-image: url(./school_race_left.jpg);
              background-position: 50% 35%;
            	background-position: no-repeat;
            }
            
            .photo-flexbox-4{
              background-image: url(./census_resources_left.jpg);
              background-position: 50% 20%;
            	background-position: no-repeat;
            }
            
            .flexbox-title{
              font-weight: 600;
              font-family: 'Stratum2WebBold';
              color: #D73F09;
              margin: 40px 0px 0px 160px;
              font-size: 21px;
              position: relative;
              top: -225px;
            }
            
            a.flexbox-title-link{
              color: #D73F09;
              text-decoration: none;
            }
            
            a.flexbox-title-link:hover{
              color: #D73F09;
              text-decoration: underline;
            }
            
            .flex-link{
              margin: 10px 10px 0px 210px;
              font-size: 12px;
              position: relative;
              text-decoration: underline;
              top: -190px;
            }
            
            .flex-link-2{
              margin: 10px 10px 0px 210px;
              font-size: 12px;
              position: relative;
              text-decoration: underline;
              top: -210px;
            }
            
            
            .flexbox-p{
              margin: 10px 10px 0px 165px;
              font-size: 12px;
              position: relative;
              top: -230px;
              color: #242424;
            }
            
            .instruction-box{
              width: 80%;
              margin-right: 10%;
              margin-left: 10%;
              margin-top: 20px;
              background-color: #F1CA9C;
              border-radius: 8px;
              box-shadow: 2px 2px 2px grey;
            }
            
            p.instruction-p{
              padding: 5px 0px 5px 0px;
              text-align: center;
            }
            
            .img-topic{
              width: 100%;
              border-radius: 5px 5px 0px 0px;
            }
            
            html, body{
              height: 100%;
            }
            
            #container{
              min-height: 100%;
            }
            
            .center-plotly{
              margin-right: 10%;
              margin-left: 10%;
              width: 80%;
            }
            
            .data-content{
              width: 80%;
              margin-right: 10%;
              margin-left: 10%;
            }
    
            .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover 
            {
                font-weight: 500;
                color: #d73f09;
                cursor: default;
                background-color: #fff;
                border: 2px solid #b0b0ac;
                border-bottom-color: transparent;
            }
            
            .nav-tabs>li>a{
              border: 1px solid #e3e3e1;
              border-radius: 4px 4px 0 0;
              border-bottom-color: transparent;
              color: #a7aba8;
            }
            
            #p-title{
              color: black;
              padding: 0px 8px 0px 8px;
              font-size: 16px;
              font-weight: 500;
            }
            
            #header{
              font-weight: 600;
              color: white;
              font-family: 'El Messiri', sans-serif;
              background: black;
              padding: 5px 0px 5px 20px;
              font-size: 15px;
              /*position: absolute;*/
              top: 78px;
              min-width: 100%;
            }
            
            .mandatory_star{
              color: red; 
            }
            .optional{
              color: grey;
            }
            
            hr{
              margin-right: 10%;
              margin-left: 10%;
            }
            
            h3{
              font-weight: 600;
              font-family: 'Stratum2WebBold';
              color: #D73F09;
              font-size: 21px   ;
            }
            
            h3.center{
              text-align: center;
              font-size: 28px;
            }
            
            h2{
              margin-top: 5px;
              margin-bottom: 5px;
            }
            
            .text-box{
              padding: 50px 0px 50px 0px;
            }
            
            a{
              color: white;
              text-decoration: underline;
            }
            
            a.email-link{
              color: #597ea2;
              font-size: 13px;
            }
            
            a.blue-link{
              color: #597ea2;
              font-size: 12px;
              text-decoration: none;
            }
            
            a.blue-link:hover{
              color: #597ea2;
              text-decoration: underline;
            }
            
            a:hover{
              color: white;
            }
            
            a.aspire-link{
              color: #006a8e;
              text-decoration: underline;
            }
            
            a.social-link:hover{
              color: white;
              background-color: #d73f09;
            }
            
            a.social-link{
              color: #252525;
              font-family: 'Gudea', Arial, Verdana, sans-serif;
              font-size: 16px;
              font-weight: 600;
              border-bottom: 1px solid #d73f09;
              text-decoration: none;
              margin-right: 8px;
            }
            
            p.socials{
              color: black;
              font-size: 15px;
              font-family: 'Gudea', Arial, Verdana, sans-serif;
              font-weight: 500;
            }
            
            p.aspire-address{
              color: black;
              font-size: 15px; 
              font-weight: 400;
              margin: 0 0 0 0;
            }
            
            hr.source{
              background-color: #242424;
              height: 0.5px;
              margin-left: 0%;
              margin-right: 0%;
              margin-top: 0px;
              margin-bottom: 1px;
            }
            
            p.normal-p{
              padding-bottom: 20px;
              font-size: 2.2vh;
            }
            
            p.s-list{
              margin: 0px;
              color: #242424;
              font-size: 12px;
            }
            
            .eightyp-content{
              width: 80%;
              margin-top: 30px;
              margin-right: 10%;
              margin-left: 10%;
            }
            
            .mobile-header{
              min-height: 2px;
              max-height: 100%;
              background-color: #242424;
            }
            
            .mobile-holder{
              margin-top: 10px;
              min-height: 2px;
              max-height: 100%;
              background-color: #242424;
            }
            
            
            
            a:not(.upper-link):not(.lower-link):not(.previouslink):not(.currentlink):not(.image):not(.aspire-link):not(.button):not(.social-link):not(.menu-link):not(.blue-link)
            :not(.flexbox-title-link):not(.mobile-link):hover{
              color: white; 
              border: 2px solid rgba(237, 233, 232, 0.6);
              padding: 3px 3px 3px 3px;
              border-radius: 5px;
              background: rgba(237, 233, 232, 0.6);
            }
            
            ul.main-li{
              padding: 10px 0px 3px 0px; 
              margin: 0px;
              text-align: center;
            }
            
            a.menu-link{
              color:black;
              font-size: 16px;
              text-decoration: none;
              margin-right: 1rem;
              margin-top: 20px;
              padding: 5px 5px 5px 5px;
            }
            
            a.mobile-link{
              color: white;
              font-size: 1.8vh;
              text-decoration: none;
              margin-right: 1rem;
              margin-top: 30px;
              padding: 0px 5px 2px 5px;
            }
            
            a.menu-link:hover{
              color:white;
              background-color: black;
              padding:5px 5px 5px 5px;
              text-decoration: none;
            }
            
            p{
              color: black;
            }
            
            p.center{
              font-size: 13px;
              text-align: center;
              color: white;
            }
            
            .well{
              min-height: 0px;
              margin-bottom: 0px;
              background-color: none;
              border: 0px;
              border-radius: 0px;
              -webkit-box-shadow: 0;
              box-shadow: 0;
            }
            
            .land-ack{
              font-size: 15px;
              text-align: center;
            }
            
            ul.inline{
             list-style:none;
             font-size: 13px;
              text-align: center;
            }
            
            ul:not(.main-li){
              padding: 0px;
              margin: 0 0 11px 0px;
            }
            
            li{
              display: inline-block;
              padding: 0px 5px;
            }
            
            .number{
              color: white;
            }
            
            .content{
              width: 50%;
              margin-top: 80px;
              margin-right: 25%;
              margin-left: 25%;
              margin-bottom: 60px;
            }
            
            /*
              Elements of the orange header
            */
            .osu-header
            {
              background: #d73f09;
              height: 100px;
              min-width: 100%;
              padding: 0px 0px 0px 0px;
            }
            
            .osu-logo{
              background-image: url(./OSU_horizontal_2C_O_over_B.png);
            	background-size: contain;
            	background-position: center;
            	background-position: no-repeat;
              min-width: 100%;
              min-height: 100%;
            }
            
            /* 
              Oregon State University text in the header
            */
            .oregon-state-1{
              position: absolute;
              left: 130px;
              top: 20px;
            }
            
            .oregon-state-2{
              position: absolute;
              left: 130px;
              top: 45px;
            }
            
            a.upper-link{
              font-weight:600;
              font-size: 28px;
              text-decoration: none;
              font-family: 'Roboto Slab', serif;
            }
            
            a.lower-link{
              font-weight:500;
              font-size: 28px;
              text-decoration: none;
              color: black;
              font-family: 'Roboto Slab', serif;
            }
            
            a.upper-link:hover{
              border: none;
            }
            
            .img-padding{
              padding: 5px 0px 5px 10px;
              margin: 2px 0px 0px 20px;
            }
            
            /*
              Black header css
            */
            .header-links{
              padding: 10px;
              background-color: black;
              min-width: 100%;
            }
            
            .previouslink{
              font-family: 'Open sans',sans-serif;
              line-height: 16px;
              text-decoration: none;
              font-weight: 500;
            }
            
            .currentlink{
              font-weight: 600;
              font-family: 'Open sans',sans-serif;
              line-height: 16px;
              text-decoration: none;
            }
            
            .separator{
              color: white;
              line-height: 16px;
              font-family: 'Open sans',sans-serif;
            }
            
            #title{
              font-size: 30px;
              padding: 7px 10px 7px 10px;
              font-weight: 500;
              font-family: 'Stratum2WebBold';
              color: #d73f09;
            }
            
            .form-well{
              min-height: 605px;
              position: relative;
            }
            
            h3.title{
              font-size: 28px;
            }
            
            /*
              footer and footer background 
            */
            .footer{
              min-width: 100%;
              height: 300px;
              background-color: #252525;
              border-top: solid #d73f09;
              margin-top: 20px;
            }
            
            .footer-background{
              background-image: url(./repeatable-treeline.png);
            	background-image: contain;
            	background-position: center;
            	background-position: no-repeat;
              min-width: 100%;
              height: 300px;
            }
            
            h5{
              font-family: 'Orelega One', cursive;
              color: #d73f09;
              font-size: 18px;
            }
            
            .img-center{
               display: block;
               margin-left: auto;
               margin-right: auto;
            }
            
            h6.work-title{
              font-size: 16px;
              font-weight: 500;
            }
            
            .help-block{
              color: black;
              line-height: 1.6;
              font-family: 'Open sans',sans-serif;
            } 
            
            .flexbox-container{
              margin-right: 20%;
              margin-left: 20%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              justify-content: center;
            }
            
            .title-box{
              background-image: url(https://now.symassets.com/content/dam/norton/global/images/non-product/misc/tlc/three-generation-family-using-tablet-hero.jpg);
            	background-image: contain;
              background-position: 50% 20%;
            	background-position: no-repeat;
              min-width: 100%;
              height: 400px;
              display: flex;
              justify-content: center;
              align-items: center;
              margin-top: 10px;
            }
            
            .census-box{
              background-image: url(https://cf.ltkcdn.net/family/images/orig/200821-2121x1414-family.jpg);
            	background-image: contain;
              background-position: 50% 30%;
            	background-position: no-repeat;
              min-width: 100%;
              height: 400px;
              display: flex;
              justify-content: center;
              align-items: center;
              margin-top: 10px;
            }
            
            .information-box{
              border: 1px solid rgb(0,0,0,0.1);
              border-radius: 5px;
              text-align: center;
              background-color: rgb(252,252,252,0.9);
              width: 400px;
              margin-left: auto;
              margin-right: auto;
            }
            
            .container-fluid{
              padding-right: 0px;
              padding-left: 0px;
            }
            
            .content{
              width: 50%;
              margin-top: 80px;
              margin-right: 25%;
              margin-left: 25%;
              margin-bottom: 60px;
            }
            
            h3.content-title{
              font-weight: 500;
              font-size: 27px;
            }
            
            h4#title-desc{
              font-size: 17px;
              font-weight: 600;
            }
            
            .aspire-footer{
              text-align: center;
              margin: 10% 0px 0px;
            }
            
            .center-plotly{
              margin-right: 10%;
              margin-left: 10%;
              width: 80%;
            }
            
            .button-box{
              border: 1px solid #d73f09;
              background-color: #d73f09;
              margin-left: auto;
              margin-right: auto;
              width: 79px;
              height: 28px;
            }
            
            .button-box:hover{
               background-color: #252525;
              color: white;
              border: 1px solid #252525;
            }
            
            a.button{
              text-decoration: none;
              color: white;
              padding: 5px 10px 5px 10px;
              font-family: 'Open sans',sans-serif;
              margin-top: 50px;
            }
            
            #main-menu{
              width: 100%;
              padding: 5px 0px 5px 0px;
              background-color: white;
            }
            
            .povertyhist{
              width: 80%;
              margin-left: 10%;
              margin-right: 10%;
            }
            
            .textbox-topic{
              width: 55%;
              margin-left: 40%;
              background-color: rgba(217, 217, 217, 0.25);
              padding: 0px 0px 5px 0px;
              border-radius: 5px;
            }
            
            .inside-textbox{
              padding: 3px 30px 10px 30px;
            }
            
            .textbox-variable{
              width: 55%;
              margin-right: 30%;
              background-color: #F1CA9C;
              padding: 5px 20px 5px 20px;
              border-radius: 5px;
            }
            
            .fa-bars{
              color: white;
            }
            
            li.li-stack{
              list-style: none;
              display: list-item;
              margin-top: 5px;
            }
            
        @media only screen and (max-width: 610px) {
            text.legendtitletext{
              font-size: 1.2rem;
            }
            
            p.normal-p
            {
              font-size: 1.4rem;
            }
            
            #dropdownmenu{
              margin-top: 10px;
              background-color: #242424;
              padding: 7px 0px 7px 12px;
              transform: translateY(-10px);
              transition: opacity 150ms ease-in-out, transform 150ms ease-in-out;
            }
            
            a.mobile-link:hover{
              border-bottom: 2px solid white; 
            }
            
            #mmbutton{
              margin-left:102%;
              border: none;
              background-color: #242424;
            }
            
            .btn{
              border: none;
              background-color: #242424;
              margin-left: 99%;
              padding: 0px;
            }
            
            .btn.mmbutton{
              margin-left: 100%;
            }
        
            .mobhead-container{ 
                width: 90%;
            }
            
            .census-item{
              width: 100%;
            }
            
            .mobile-header{
              display: block;
              padding: 10px 5px 10px 5px;
              background-color: #242424;
            }
            
            .topic-p{
              font-size: 1.7vh;
            }
            
            button.link{
              margin-left: 100%;
              background-color: #242424;
              border: none;
              color: white;
              padding-right: 1rem;
              font-size: 1.7vh;
            }
            
            .title-box{
              margin-top: 0px;
            }
            
            .census-box{
              margin-top: 0px;
            }
        
            #main-menu{
              display: none;
            }
            
            
              
            p.normal-p{
              font-size: 1.7vh;
            }
            
            .flex-link-2{
              top: -15px;
              margin: 10px 5px 0px 14px;
            }
            
            .select-box{
            width: 80%;
            display: flex;
            justify-content: center;
            margin-left: 10%;
            margin-center: 10%;
          }
            .center-select{
              margin-top: 4%;
              width: 80%;
            }
            
            .input-middle{
              width: 80%;
            }
            
            .col-sm-6{
              padding-left: 0px;
              padding-right: 0px;
            }
            
            .census-flex-container{
              margin-right: 10%;
              margin-left: 10%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              justify-content: center;
            }
            
            .flexbox-container{
              margin-right: 10%;
              margin-left: 10%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              justify-content: center;
            }
            
            .flexbox-item{
              height: 100%;
              width: 100%;
              margin: 10px;
              box-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25);
              border-radius: 15px;
              background-color: rgba(217,217,217, 0.25);
            }
            
            .flexbox-title{
              margin: 40px 10px 0px 13px;
              top: -20px;
            }
            
            .flexbox-p{
              margin: 10px 10px 0px 16px;
              top: -20px;
            }
            
            .flex-link{
              top: -15px;
              margin: 10px 5px 0px 14px;
            }
            
            .photo-flexbox{
              height: 150px;
              border-radius: 15px 15px 0px 0px;
            }
            
            .census-item-2{
              margin-top: 15px;
            }
        }
        
        // Responsive css for desktop
        @media (min-width: 370px){
            #main-menu{
              width: 100%;
              padding: 5px 0px 5px 0px;
              display: none;
              background-color: white;
            }
            
            .center-select{
              margin-left: 20%;
              width: 60%;
              margin-top: 4%;
            }
            
            .input-middle{
               margin-left: 20%;
               width: 60%;
            }
            
            .col-sm-6{
              padding-left: 0px;
              padding-right: 0px;
            }
            
            .census-flex-container{
              margin-right: 20%;
              margin-left: 20%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              width: 60%;
              justify-content: center;
            }
            
            .flexbox-container{
              margin-right: 10%;
              margin-left: 10%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              justify-content: center;
            }
            
            .photo-flexbox{
              height: 200px;
              border-radius: 15px 0px 0px 15px;
              width: 45%;
            }
            
            .flexbox-item{
              height: 200px;
              margin: 10px;
              box-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25);
              border-radius: 15px;
              background-color: rgba(217,217,217, 0.25);
              width: 100%;
            }
            
            .flexbox-title{
              font-weight: 600;
              font-family: 'Stratum2WebBold';
              color: #D73F09;
              margin: 40px 0px 0px 120px;
              font-size: 21px;
              position: relative;
              top: -225px;
            }
            
            .flexbox-p{
              margin: 10px 10px 0px 130px;
              font-size: 12px;
              position: relative;
              top: -230px;
              color: #242424;
            }
            
            .title-box{
              background-image: url(https://now.symassets.com/content/dam/norton/global/images/non-product/misc/tlc/three-generation-family-using-tablet-hero.jpg);
            	background-image: contain;
              background-position: 50% 20%;
            	background-position: no-repeat;
              min-width: 100%;
              height: 400px;
              display: flex;
              justify-content: center;
              align-items: center;
              margin-top: 0px;
            }
        }
        
        @media (min-width: 610px){
          a.menu-link
          {
            font-size: 14px;
          }
          
          .census-item-2{
              margin-top: 15px;
          }
            
          .select-box{
            width: 80%;
            margin-left: 10%;
            margin-right: 10%;
            display: flex;
            justify-content: center;
          }
          
          .mobile-header{
            display: none;
          }
            .center-select{
              margin-top: 4%;
            }
            
            .input-middle{
            }
            
            .col-sm-6{
              padding-left: 0px;
              padding-right: 0px;
            }
            
            .census-flex-container{
              margin-right: 20%;
              margin-left: 20%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              width: 60%;
              justify-content: center;
            }
            
            .flexbox-container{
              margin-right: 20%;
              margin-left: 20%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              justify-content: center;
            }
            
            .photo-flexbox{
              height: 200px;
              border-radius: 15px 0px 0px 15px;
              width: 150px;
            }
            
        }
        
         @media (min-width: 959px)
         {
              body
              {
                font-size: 1.3rem;
              }
              a.menu-link
              {
                font-size: 16px;
              }
              
              p.normal-p
              {
                font-size: 1.5rem;
              }
              
              .mobile-header
              {
                display: none;
              }
              
             .select-box{
                width: 50%;
                display: flex;
                justify-content: center;
             }
             .center-select{
              margin-top: 4%;
            }
            
            .input-middle{
               margin-left: 130%;
               margin-top: -79px;
            }
            
            .census-flex-container{
              margin-right: 10%;
              margin-left: 10%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              width: 80%;
              justify-content: center;
            }
            
            .flexbox-container{
              margin-right: 10%;
              margin-left: 10%;
              margin-bottom: 5%;
              display: flex;
              flex-wrap: wrap;
              justify-content: center;
            }
            
            .photo-flexbox{
              height: 200px;
              border-radius: 15px 0px 0px 15px;
              width: 150px;
            }
            
            .flexbox-item{
              width: 350px;
              height: 200px;
              margin: 10px;
              box-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25);
              border-radius: 15px;
              background-color: rgba(217,217,217, 0.25);
            }
         }
         
         @media (min-width:1300px)
        {
          body
          {
            font-size: 1.3rem;
          }
           .mobile-header{
              display: none;
           }
           .center-select{
              margin-top: 4%;
            }
            
            .input-middle{
               margin-left: 110%;
               margin-top: -79px;
            }
            
            .photo-flexbox{
              height: 200px;
              border-radius: 15px 0px 0px 15px;
              width: 150px;
            }
            
            .flexbox-item{
              width: 400px;
              height: 200px;
              margin: 10px;
              box-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25);
              border-radius: 15px;
              background-color: rgba(217,217,217, 0.25);
            }
            
            p.flexbox-p
            {
              font-size: 1.4rem;
            }
            
            .flex-link-2
            {
              margin: 0px 10px 0px 250px;
            }
            
            .flex-link
            {
              margin: 0px 10px 0px 249px;
              top: -210px;
            }
            
            .aspire-footer
            {
              margin: 5% 0px 0px 0px;
            }
            
        }
        "
    ))
  ),
  tags$div(id="container",
           tags$div(id="main",
                    tags$div(class="osu-header",
                             tags$a(
                               class= "image",
                               href="https://oregonstate.edu", 
                               tags$img(src="osu.svg", 
                                        width="240",
                                        height="95",
                                        class="img-padding")
                             )),
                    tags$div(class="header-links",
                             tags$a(class="previouslink", href="https://health.oregonstate.edu", "College of Public Health and Sciences"),
                             tags$span(class="separator", "»"),
                             tags$a(class="previouslink", href="https://health.oregonstate.edu/hallie-ford", "Hallie E. Ford Center"),
                             tags$span(class="separator", "»"),
                             tags$a(class="currentlink", href="https://health.oregonstate.edu/asp3ire", "ASP3RE Center")
                    ),
                    tags$div(id="main-menu",
                             tags$ul(class="main-li",
                                     tags$li(a(class="menu-link", href = route_link("/"), "HOME")),
                                     tags$li(a(class="menu-link", href = "https://health.oregonstate.edu/asp3ire/about", "ABOUT")),
                                     tags$li(a(class="menu-link", href = "https://health.oregonstate.edu/asp3ire/team", "TEAM MEMBERS")),
                                     tags$li(a(class="menu-link", href = route_link("censusdata"), "CENSUS DATA")),
                                     tags$li(a(class="menu-link", href = route_link("schooldistricts"), "SCHOOL DISTRICTS"))
                             )),
                    tags$div(class="mobile-header",
                             tags$div(class="mobhead-container",
                                      actionButton("mmbutton",
                                                   tags$i(class="fa-solid fa-bars fa-2x"))),
                             shinyjs::hidden(
                               tags$div(id="dropdownmenu",
                                        tags$li(class="li-stack", a(class="mobile-link", href = route_link("/"), "HOME")),
                                        tags$li(class="li-stack", a(class="mobile-link", href = "https://health.oregonstate.edu/asp3ire/about", "ABOUT")),
                                        tags$li(class="li-stack", a(class="mobile-link", href = "https://health.oregonstate.edu/asp3ire/team", "TEAM MEMBERS")),
                                        tags$li(class="li-stack", a(class="mobile-link", href = route_link("censusdata"), "CENSUS DATA")),
                                        tags$li(class="li-stack", a(class="mobile-link", href = route_link("schooldistricts"), "SCHOOL DISTRICTS"))
                               )
                             )))),
  router$ui,
  tags$div(class="aspire-footer",
           HTML("
                <hr>
                  <h3 class='aspfooter-title'>
                    ASP<sup>3</sup>IRE Children's Environmental Health Center
                  </h3>
                  <div class='socials-list'>
                    <p class='socials'>
                      <i class='fa-solid fa-envelope far fa-1x'> </i>
                      <a class='social-link' href='mailto:dixie.jackson@oregonstate.edu' title='Email the ASP3IRE Center'>Email</a>
                      <i class='fa-brands fa-square-facebook'> </i>
                      <a class='social-link' href='https://www.facebook.com/ASP3IREOSU' title='ASP3IRE Facebook'>Facebook</a>
                      <i class='fa-brands fa-square-twitter'> </i>
                      <a class='social-link' href='https://twitter.com/AspireOsu' title='ASP3IRE Twitter'>Twitter</a>
                      <i class='fa-solid fa-square-phone-flip'> </i>
                      541-737-1387
                    </p>
                  </div>
                  <p class='aspire-address'>
                     Hallie E. Ford Center for Healthy Children & Families
                  </p>
                  <p class='aspire-address'>
                    2631 SW Campus Way, Corvallis, OR 97331
                  </p>
                ")),
  tags$footer(HTML("
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div class='footer-background'>
                              <div class='text-box'>
                                <div class='footer-copyright text-center py-3'>
                                 <p class='center'> 
                                          College of Public Health and Human Sciences <br> 
                                          160 SW 26th St, Corvallis, OR 97331</p>
                                 <p class= 'land-ack'>
                                    <a href= 'https://oregonstate.edu/land-acknowledgement'> Land Acknowledgement </a>
                                 </p>
                                 <ul class= 'inline'>
                                   <li>
                                        <a href = 'https://oregonstate.edu/digital-millennium-copyright-act'> © 2022 Oregon State University</a>
                                   </li>
                                   <li>
                                        <a href='https://health.oregonstate.edu/contact'>  Contact  </a>
                                   </li>
                                    <li>
                                        <p class= 'number'>541.737.3220</p>
                                   </li>
                                   <li>
                                        <a href='https://health.oregonstate.edu/user/326/contact'> Website Issue?</a>
                                   </li>
                                  </ul>
                                   <ul class= 'inline'>
                                   <li>
                                       <a href='https://oregonstate.edu/official-web-disclaimer'> Privacy Disclaimer and Accessibility Information</a>
                                   </li>
                                  </ul>
                                  <ul class= 'inline'>
                                   <li>
                                        <p><a href='https://clery.oregonstate.edu/annual-reports'> Annual Security and Fire Safety Reports </a></p>
                                   </li>
                                  </ul>
                               </div>
                              </div>
                           </div>
                           <!-- Copyright -->

                           </footer>
                     <!-- Footer -->"), class="footer")
)

server <- function(input, output, session) 
{
  router$server(input, output, session)
  
  font = list(
    color= "white"
  )
  
  observeEvent(input$mmbutton, {
    shinyjs::toggle(id="dropdownmenu")
  })
  
  output$povertyhist = renderPlotly({
    population = switch(input$age, 
                        "Under 5 years" = all_census$Tot_pov_U_5_18, 
                        "5 to 17 years" = all_census$Tot_pov_U_5_17_18,
                        "Under 18 years" = all_census$Tot_pov_U_18,
                        "Related children of householder under 18 years" = all_census$Tot_pov_RUC_18)
    
    histTitle = switch(input$age, 
                       "Under 5 years" =  poverty_histT[1], 
                       "5 to 17 years" = poverty_histT[2],
                       "Under 18 years" = poverty_histT[3],
                       "Related children of householder under 18 years" = poverty_histT[4])
    
    meanTitle = switch(input$age, 
                       "Under 5 years" =  poverty_meanT[1], 
                       "5 to 17 years" = poverty_meanT[2],
                       "Under 18 years" = poverty_meanT[3],
                       "Related children of householder under 18 years" = poverty_meanT[4])
    
    mean = switch(input$age, 
                  "Under 5 years" = mean(all_census$Tot_pov_U_5_18), 
                  "5 to 17 years" = mean(all_census$Tot_pov_U_5_17_18),
                  "Under 18 years" = mean(all_census$Tot_pov_U_18),
                  "Related children of householder under 18 years" = mean(all_census$Tot_pov_RUC_18))
    
    p = all_census %>% 
      ggplot(aes(x=population)) + 
      geom_histogram(binwidth=100, fill='#edce95', color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated total population", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=12, face= "bold", hjust=0.5),
            legend.title=element_text(size=9), 
            legend.text=element_text(size=8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  output$healthcovhist = renderPlotly({
    population = switch(input$hc_age, 
                        "Male under 6 years with health insurance coverage" = all_census$M_U_6__W_HIC, 
                        "Male under 6 years without health insurance coverage"= all_census$M_U_6__N_HIC,
                        "Male under 6 to 18 years with health insurance coverage" = all_census$M_16_18__W_HIC,
                        "Male under 6 to 18 years without health insurance coverage"= all_census$M_16_18__N_HIC)
    
    histTitle = switch(input$hc_age, 
                       "Male under 6 years with health insurance coverage" = healthC_histT[1], 
                       "Male under 6 years without health insurance coverage"= healthC_histT[2],
                       "Male under 6 to 18 years with health insurance coverage"= healthC_histT[3],
                       "Male under 6 to 18 years without health insurance coverage"= healthC_histT[4])
    
    meanTitle = switch(input$hc_age, 
                       "Male under 6 years with health insurance coverage" = healthC_meanT[1], 
                       "Male under 6 years without health insurance coverage"= healthC_meanT[2],
                       "Male under 6 to 18 years with health insurance coverage"= healthC_meanT[3],
                       "Male under 6 to 18 years without health insurance coverage"= healthC_meanT[4])
    
    mean = switch(input$hc_age, 
                  "Male under 6 years with health insurance coverage" = mean(all_census$M_U_6__W_HIC), 
                  "Male under 6 years without health insurance coverage"= mean(all_census$M_U_6__N_HIC),
                  "Male under 6 to 18 years with health insurance coverage" = mean(all_census$M_16_18__W_HIC),
                  "Male under 6 to 18 years without health insurance coverage"= mean(all_census$M_16_18__N_HIC))
    
    p = all_census %>% 
      ggplot(aes(x=population)) + 
      geom_histogram(binwidth=100, fill="#edce95", color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated total population", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=14, face= "bold", hjust=0.5),
            legend.title=element_text(size=10), 
            legend.text=element_text(size=9),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  output$hhist = renderPlotly({
    population = switch(input$hh_age, 
                        "Children in households under 3 years old" = all_census$Tot_hld_U_3, 
                        "Children in households that are 3 to 4 years old" = all_census$Tot_hld_3_4,
                        "Children in households that are 5 years old" = all_census$Tot_hld_5.x,
                        "Children in households that are 6 to 8 years old"= all_census$Tot_hld_6_8, 
                        "Children in households that are 9 to 11 years old"= all_census$Tot_hld_9_11,
                        "Children in households that are 12 to 14 years old"= all_census$Tot_hld_12_14,
                        "Children in households that are 15 to 17 years old"= all_census$Tot_hld_15_17)
    
    histTitle = switch(input$hh_age, 
                       "Children in households under 3 years old" = household_histT[1], 
                       "Children in households that are 3 to 4 years old"= household_histT[2],
                       "Children in households that are 5 years old" = household_histT[3],
                       "Children in households that are 6 to 8 years old"= household_histT[4], 
                       "Children in households that are 9 to 11 years old" = household_histT[5],
                       "Children in households that are 12 to 14 years old" = household_histT[6],
                       "Children in households that are 15 to 17 years old" = household_histT[7])
    
    meanTitle = switch(input$hh_age, 
                       "Children in households under 3 years old" = household_meanT[1], 
                       "Children in households that are 3 to 4 years old"= household_meanT[2],
                       "Children in households that are 5 years old" = household_meanT[3],
                       "Children in households that are 6 to 8 years old"= household_meanT[4], 
                       "Children in households that are 9 to 11 years old" = household_meanT[5],
                       "Children in households that are 12 to 14 years old" = household_meanT[6],
                       "Children in households that are 15 to 17 years old" = household_meanT[7])
    
    mean = switch(input$hh_age, 
                  "Children in households under 3 years old" = mean(all_census$Tot_hld_U_3), 
                  "Children in households that are 3 to 4 years old" = mean(all_census$Tot_hld_3_4),
                  "Children in households that are 5 years old" = mean(all_census$Tot_hld_5.x),
                  "Children in households that are 6 to 8 years old"= mean(all_census$Tot_hld_6_8), 
                  "Children in households that are 9 to 11 years old"= mean(all_census$Tot_hld_9_11),
                  "Children in households that are 12 to 14 years old"= mean(all_census$Tot_hld_12_14),
                  "Children in households that are 15 to 17 years old"= mean(all_census$Tot_hld_15_17))
    
    p = all_census %>% 
      ggplot(aes(x=population)) + 
      geom_histogram(binwidth=100, fill="#edce95", color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated total population", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=14, face= "bold", hjust=0.5),
            legend.title=element_text(size=10), 
            legend.text=element_text(size=9),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  output$resourcehist = renderPlotly({
    resource = switch(input$resource_type, 
                      "Water"= all_census$ALAND.x,
                      "Land" = all_census$AWATER.x)
    
    histTitle = switch(input$resource_type, 
                       "Water"= resource_legendT[1],
                       "Land" = resource_legendT[2])
    
    meanTitle = switch(input$resource_type, 
                       "Water" = resource_meanT[1],
                       "Land" = resource_meanT[2])
    
    mean = switch(input$resource_type, 
                  "Water"= mean(all_census$ALAND.x),
                  "Land" = mean(all_census$AWATER.x))
    
    p = all_census %>% 
      ggplot(aes(x=resource)) + 
      geom_histogram(bins=5 , fill="#edce95", color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated resource", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=14, face= "bold", hjust=0.5),
            legend.title=element_text(size=10), 
            legend.text=element_text(size=9),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  
  observeEvent(input$age,{
    click_ages <- reactiveValues( ids = vector() )
    observeEvent({input$poverty_shape_click},{
      click = input$poverty_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      povertydata = switch(input$age, 
                           "Under 5 years" = povertydata[1], 
                           "5 to 17 years" = povertydata[2],
                           "Under 18 years" = povertydata[3],
                           "Related children of householder under 18 years" = povertydata[4])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      print("here1")
      holder = all_census[, c(povertydata, "GEOID")]
      view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      view(data)
      print("here2")
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$age, 
                         "Under 5 years" = holder$Tot_pov_U_5_18, 
                         "5 to 17 years" = holder$Tot_pov_U_5_17_18,
                         "Under 18 years" = holder$Tot_pov_U_18,
                         "Related children of householder under 18 years" = holder$Tot_pov_RUC_18)
      
      overalldata = switch(input$age, 
                           "Under 5 years" = data$Tot_pov_U_5_18, 
                           "5 to 17 years" = data$Tot_pov_U_5_17_18,
                           "Under 18 years" = data$Tot_pov_U_18,
                           "Related children of householder under 18 years" = data$Tot_pov_RUC_18)
      print(holder)
      print(variable)
      min = min(variable)
      max  = max(variable)
      print(min)
      print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, 100, f = ceiling)
      print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, 100)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      print(bin)
      
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_ages$ids) == 0)
      {
        # If length of click_ages$ids is zero, then we must
        # append the clicked id of the map.
        click_ages$ids = c(click_ages$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_ages$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_ages$ids)
        {
          # Set the first element in the click_ages$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          print("oh no :(")
          print(click_ages$ids)
          click_ages$ids = NULL
          print(click_ages$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          print("oldie with the newbie")
          # Set the first element in the click_ages$ids to 0, as we have a new
          # element to keep a track of.
          click_ages$ids = NULL
          click_ages$ids = c(click_ages$ids, click$id)
          print(click_ages$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      print(color_list)
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("povertyhist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))
    })
  })
  observeEvent(input$hc_age,{
    click_hcage = reactiveValues(ids = vector())
    observeEvent({input$healthcoverage_shape_click},{
      click = input$healthcoverage_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      hcdata = switch(input$hc_age, 
                      "Male under 6 years with health insurance coverage" = hcdata[1], 
                      "Male under 6 years without health insurance coverage"= hcdata[2],
                      "Male under 6 to 18 years with health insurance coverage" = hcdata[3],
                      "Male under 6 to 18 years without health insurance coverage"= hcdata[4])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      holder = all_census[, c(hcdata, "GEOID")]
      view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$hc_age, 
                         "Male under 6 years with health insurance coverage" = holder$M_U_6__W_HIC, 
                         "Male under 6 years without health insurance coverage"= holder$M_U_6__N_HIC,
                         "Male under 6 to 18 years with health insurance coverage" = holder$M_16_18__W_HIC,
                         "Male under 6 to 18 years without health insurance coverage"= holder$M_16_18__N_HIC)
      
      overalldata = switch(input$hc_age, 
                           "Male under 6 years with health insurance coverage" = data$M_U_6__W_HIC, 
                           "Male under 6 years without health insurance coverage"= data$M_U_6__N_HIC,
                           "Male under 6 to 18 years with health insurance coverage" = data$M_16_18__W_HIC,
                           "Male under 6 to 18 years without health insurance coverage"= data$M_16_18__N_HIC)
      print(holder)
      print(variable)
      min = min(variable)
      max  = max(variable)
      print(min)
      print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, range)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      print(bin)
      
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_hcage$ids) == 0)
      {
        # If length of click_hcage$ids is zero, then we must
        # append the clicked id of the map.
        click_hcage$ids = c(click_hcage$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_hcage$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_hcage$ids)
        {
          # Set the first element in the click_hcage$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          print("oh no :(")
          print(click_hcage$ids)
          click_hcage$ids = NULL
          print(click_hcage$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          print("oldie with the newbie")
          # Set the first element in the click_hcage$ids to 0, as we have a new
          # element to keep a track of.
          click_hcage$ids = NULL
          click_hcage$ids = c(click_hcage$ids, click$id)
          print(click_hcage$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("healthcovhist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))
    })
  })
  observeEvent(input$hh_age, {
    click_hhage <- reactiveValues( ids = vector() )
    observeEvent({input$householdage_shape_click},{
      click = input$householdage_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      hhdata = switch(input$hh_age, 
                      "Children in households under 3 years old" = hhdata[1], 
                      "Children in households that are 3 to 4 years old" = hhdata[2],
                      "Children in households that are 5 years old" = hhdata[3],
                      "Children in households that are 6 to 8 years old"= hhdata[4], 
                      "Children in households that are 9 to 11 years old"= hhdata[5],
                      "Children in households that are 12 to 14 years old"= hhdata[6],
                      "Children in households that are 15 to 17 years old"= hhdata[7])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      holder = all_census[, c(hhdata, "GEOID")]
      view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$hh_age, 
                         "Children in households under 3 years old" = holder$Tot_hld_U_3, 
                         "Children in households that are 3 to 4 years old" = holder$Tot_hld_3_4,
                         "Children in households that are 5 years old" = holder$Tot_hld_5.x,
                         "Children in households that are 6 to 8 years old"= holder$Tot_hld_6_8, 
                         "Children in households that are 9 to 11 years old"= holder$Tot_hld_9_11,
                         "Children in households that are 12 to 14 years old"= holder$Tot_hld_12_14,
                         "Children in households that are 15 to 17 years old"= holder$Tot_hld_15_17)
      
      overalldata = switch(input$hh_age, 
                           "Children in households under 3 years old" = data$Tot_hld_U_3, 
                           "Children in households that are 3 to 4 years old" = data$Tot_hld_3_4,
                           "Children in households that are 5 years old" = data$Tot_hld_5.x,
                           "Children in households that are 6 to 8 years old"= data$Tot_hld_6_8, 
                           "Children in households that are 9 to 11 years old"= data$Tot_hld_9_11,
                           "Children in households that are 12 to 14 years old"= data$Tot_hld_12_14,
                           "Children in households that are 15 to 17 years old"= data$Tot_hld_15_17)
      print(holder)
      print(variable)
      min = min(variable)
      max  = max(variable)
      print(min)
      print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, range)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      print(bin)
      
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_hhage$ids) == 0)
      {
        # If length of click_hhage$ids is zero, then we must
        # append the clicked id of the map.
        click_hhage$ids = c(click_hhage$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_hhage$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_hhage$ids)
        {
          # Set the first element in the click_hhage$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          print("oh no :(")
          print(click_hhage$ids)
          click_hhage$ids = NULL
          print(click_hhage$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          print("oldie with the newbie")
          # Set the first element in the click_hhage$ids to 0, as we have a new
          # element to keep a track of.
          click_hhage$ids = NULL
          click_hhage$ids = c(click_hhage$ids, click$id)
          print(click_hhage$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("hhist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))            
    })
  })
  observeEvent(input$resource_type, {
    click_resource <- reactiveValues( ids = vector() )
    observeEvent({input$resources_shape_click},{
      click = input$resources_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      resourcesdata =  switch(input$resource_type, 
                              "Water"= resourcedata[1],
                              "Land" = resourcedata[2])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      holder = all_census[, c(resourcesdata, "GEOID")]
      #view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      #view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$resource_type, 
                         "Water"= holder$ALAND.x,
                         "Land" = holder$AWATER.x)
      
      overalldata = switch(input$resource_type, 
                           "Water"= data$ALAND.x,
                           "Land" = data$AWATER.x)
      #print(holder)
      #print(variable)
      min = min(variable)
      max  = max(variable)
      #print(min)
      #print(max)
      # set the range between them
      range = 5000000000
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      #print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      #print("max bin")
      #print(max_bin)
      # have to round up the data 
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_resource$ids) == 0)
      {
        # If length of click_resource$ids is zero, then we must
        # append the clicked id of the map.
        click_resource$ids = c(click_resource$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_resource$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_resource$ids)
        {
          # Set the first element in the click_resource$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          #print("oh no :(")
          #print(click_resource$ids)
          click_resource$ids = NULL
          #print(click_resource$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          #print("oldie with the newbie")
          # Set the first element in the click_resource$ids to 0, as we have a new
          # element to keep a track of.
          click_resource$ids = NULL
          click_resource$ids = c(click_resource$ids, click$id)
          #print(click_resource$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("resourcehist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))            
    })
  })
  
  #================================
  # if statement for the textOuput!
  #================================
  
  #------------ poverty data --------------
  output$poverty_variableTitle = renderText({
    out = ""
    if(input$age == "Under 5 years")
    {
      out = paste("About under 5 years")
    }
    if(input$age == "5 to 17 years")
    {
      out = paste("About 5 to 17 years")
    }
    if(input$age == "Under 18 years")
    {
      out = paste("About under 18 years")
    }
    if(input$age == "Related children of householder under 18 years")
    {
      out = paste("About related children of householder under 18 years")
    }
    out
  })
  output$poverty_variable = renderText({
    out = ""
    if(input$age == "Under 5 years")
    {
      out = paste("Explanation 1")
    }
    if(input$age == "5 to 17 years")
    {
      out = paste("Explanation 2")
    }
    if(input$age == "Under 18 years")
    {
      out = paste("Explanation 3")
    }
    if(input$age == "Related children of householder under 18 years")
    {
      out = paste("Explanation 4")
    }
    out
  })
  
  #------------ health cov hist --------------
  output$hc_title = renderText({
    out = ""
    if(input$hc_age == "Male under 6 years with health insurance coverage")
    {
      out = paste("About Variable 1")
    }
    if(input$hc_age == "Male under 6 years without health insurance coverage")
    {
      out = paste("About Variable 2")
    }
    if(input$hc_age == "Male under 6 to 18 years with health insurance coverage")
    {
      out = paste("About Variable 3")
    }
    if(input$hc_age == "Male under 6 to 18 years without health insurance coverage")
    {
      out = paste("About Variable 4")
    }
    out
  })
  output$hc_variable = renderText({
    out = ""
    if(input$hc_age == "Male under 6 years with health insurance coverage")
    {
      out = paste("Explanation 1 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$age == "Male under 6 years without health insurance coverage")
    {
      out = paste("Explanation 2 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$age == "Male under 6 to 18 years with health insurance coverage")
    {
      out = paste("Explanation 3 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$age == "Male under 6 to 18 years without health insurance coverage")
    {
      out = paste("Explanation 4 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    out
  })
  
  #------------ population in household by age --------------
  output$hhage_title = renderText({
    if(input$hh_age == "Children in households under 3 years old")
    {
      out = paste("About under 3 years")
    }
    if(input$hh_age == "Children in households that are 3 to 4 years old")
    {
      out = paste("About 3 to 4 years")
    }
    if(input$hh_age == "Children in households that are 5 years old")
    {
      out = paste("About 5 year olds")
    }
    if(input$hh_age =="Children in households that are 6 to 8 years old")
    {
      out = paste("About 6 to 8 years")
    }
    if(input$hh_age == "Children in households that are 12 to 14 years old")
    {
      out = paste("About under 14 years")
    }
    if(input$hh_age == "Children in households that are 15 to 17 years old")
    {
      out = paste("About 15 to 17 years")
    }
    out
  })
  output$hhage_variable = renderText({
    if(input$hh_age == "Under 5 years")
    {
      out = paste("Explanation 1 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$hh_age == "5 to 17 years")
    {
      out = paste("Explanation 2 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$hh_age == "Under 18 years")
    {
      out = paste("Explanation 3 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$hh_age == "Related children of householder under 18 years")
    {
      out = paste("Explanation 4 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    out
  })
  
  #------------ resources --------------
  output$resources_title = renderText({
    if(input$resource_type == "Water")
    {
      out = paste("About Water")
    }
    if(input$resource_type == "Land")
    {
      out = paste("About Land")
    }
    out
  })
  output$resources_variable = renderText({
    if(input$resource_type == "Water")
    {
      out = paste("About Water - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$resource_type == "Land")
    {
      out = paste("About Land - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    out
  })
}

shinyApp(ui = ui, server = server)

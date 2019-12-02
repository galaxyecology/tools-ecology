
Welcome to the Galaxy tool repo of Galaxyecology community!

# create your own Shiny app Galaxy Interactive Tool
## Dockerize you Shiny app using as a basis a shiny-server docker image
Create a Github repository named *your github name*/*shinyappname*-docker
You can use as an example, the github repo created for Geoexplorer Shiny app https://github.com/yvanlebras/geoexplorer-docker
  - copy/paste all files from the geoexplorer-docker repo
  - replace "SIG" folder by a folder named "*shinyappname*" and containing all files coming for your original Shiny app repository (so all files necessary to launch the Shiny app)
  - edit the `Dockerfile` with your favorite text editor
    - Specify each package your Shiny app need as made here https://github.com/yvanlebras/geoexplorer-docker/blob/master/Dockerfile between lines 8 and 17 (10 R packages are mandatory as dependencies for GeoExplorer)
	- Modify `SIG` mention by the folder name previously created on line 20 (COPY *shinyappname* /srv/shiny-server/sample-apps/*shinyappname*/)
Go to Dockerhub
  - create a Dockerhub repository named *your name* / *shinyappname*-docker specifying on the `Build settings` section that you want to use a Github repository to create the Docker image, so clicking on the cat ;)
  - select github organization so *your name* and then the Github repository *shinyappname*-docker
  - finally click on create and build

## Create Shiny app related Galaxy tool

Add a Galaxy xml file into https://github.com/usegalaxy-eu/galaxy/tree/release_19.09_europe/tools/interactive repository creating a pull request
- name of your xml file can be `interactivetool_*shinyappname*.xml`
  - in your `interactivetool_*shinyappname*.xml`, taking this as an example https://github.com/yvanlebras/galaxy/blob/patch-7/tools/interactive/interactivetool_geoexplorer.xml 
    - modify `geoexplorer` by your *shinyappname* in the first line
    - add a synthetic description of what your Shiny app is doing line 2
    - change the name of the *shinyappname* docker image on Dockerhub (best is to use *your name* / *shinyappname*-docker line 4
	- if needed, change the PATH where your Shiny app is reachable on your Docker image (where you copy/paste your Shiny app code on the related Dockerfile) like /sample-apps/*shinyappname* line 9
	- if needed, change the type of data the user can upload on the Shiny app. For now we only give access to one file in a "simple" Galaxy dockerized Shiny app, and by default (here for geoexplorer) we are using tabular or csv file line 25
	- Add a description of what the Shiny app does between line 36 and 56 and if possible the type of input data the app is taking
	- Add a citation to the original Shiny-app like here the original github repository where the Shiny app (not dockerized) is reachable line 64 and modify author (line 61) and title (line 62) in consequence
  - now you just have to create a Pull Request so tha usegalaxy.eu administrators can look at it, propose modification / ask questions and validate it. The tool will be reachable on usegalaxy.eu, in the **interactive tools** section of the tool panel the next day.


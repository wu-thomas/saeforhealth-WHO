# User instructions for starting and using the shiny app

Install Docker https://docs.docker.com/get-docker/

### Docker desktop app

1. Open the Docker desktop app

2. Download the image:
    - This is only required the first time you use the app or for downloading a new version/tag.
    - In the search bar, search for 'chacalle333/surveyprevrshinywho:test'.
    - Hover cursor over relevant result and click 'Pull'.

3. Run a container with the downloaded image:
    - From sidebar, open 'Images' -> 'Local'. Should now have a row for 'Name'='chacalle333/surveyprevrshinywho' and 'Tag'='test'. 
    - Under actions click 'Run' (the play button).
    - Expand 'optional settings'.
      - Under 'Ports': Fill 'Host port' with '3838'.
    - Click 'Run'

4. Open the app in your web browser by navigating to 'http://localhost:3838/'.

5. When done, close the webpage and:
    - From sidebar, open 'Containers'.
    - Look for rows with 'Image'='chacalle333/surveyprevrshinywho' & Status = 'Running'.
    - Under 'Actions' click 'Stop'

### command line (with internet access)

1. Open the command line.

2. Download the image: `docker pull chacalle333/surveyprevrshinywho:test`

3. Run a container with the downloaded image:　`docker run --rm -p 3838:3838 chacalle333/surveyprevrshinywho:test`

4. Open the app in your web browser by navigating to 'http://localhost:3838/'.

5. When done, close the webpage and:
    - From sidebar, open 'Containers'.
    - Look for rows with 'Image'='chacalle333/surveyprevrshinywho' & Status = 'Running'.
    - Under 'Actions' click 'Stop'

### command line (with file copy of image)

1. Open the command line.

2. Obtain the docker image in tar format from another source. Load into docker with: `docker load --input file_path.tar`

3. Run a container with the downloaded image:　`docker run --rm -p 3838:3838 chacalle333/surveyprevrshinywho:test`

4. Open the app in your web browser by navigating to 'http://localhost:3838/'.

5. When done, close the webpage and:
    - From sidebar, open 'Containers'.
    - Look for rows with 'Image'='chacalle333/surveyprevrshinywho' & Status = 'Running'.
    - Under 'Actions' click 'Stop'

# Development instructions for building and maintaining the docker image

The `Dockerfile` contains instructions for how to build the image.
The current file builds off of the [rocker images](https://rocker-project.org/) which already include important/common dependencies like shiny and geospatial analysis dependencies.

### Building the docker image and pushing to the docker hub registry

1. Build the image using the Dockerfile instructions: `docker build --platform linux/amd64 --build-arg CACHEBUST=(date +%s) -t chacalle333/surveyprevrshinywho:test .` or in Windows docker build --platform linux/amd64 --build-arg CACHEBUST=$(Get-Date -UFormat %s) -t yunhanwu/saeforhealth::v1.0.0 .


2. Can test using the image with the directions above.

3. Push the built image to the docker registry so that others can pull it down to their machine: `docker push chacalle333/surveyprevrshinywho:test`

### save the image to disk for offline transfers

After building/pulling a docker image to your machine it is possible to save the image to disk on computer 1, transfer to another computer, and load the image into docker on computer 2.

This is potentially useful if we have spotty internet access at the workshop and need to distribute the image to attendees.

1. Open the command line.

2. Save the image to disk with gzip: `docker save chacalle333/surveyprevrshinywho:test | gzip > surveyprevrshinywho_test.tar.gz`.

3. Transfer the zipped tar file to another computer (via external hard drive etc.).

4. Load the zipped tar file into docker: `docker load < surveyprevrshinywho_test.tar.gz`

5. Follow the user-directions above to run and open the shiny app.

# io-visualization
A set of tools that allow for interactive visualization of input-output tables.

The purpose of this project is to provide an interactive web visualization for input-output tables using R and PostgreSQL. As of right now there are two components, an R Shiny web application and a PostgreSQL Database (more specifically a postgis one). The network of those two components can be created with one single docker-compose command.

**Disclaimer:** This app is work in progress and has not been completed. This means that while you can still use certain parts, the app as a whole will not work.

## Usage
1. [Dependencies](#dependencies)
2. [Get the app](#get-the-app)
3. [Setup](#setup)
4. [How to run](#how-to-run)
5. [How to restart/stop](#how-to-restart-or-stop-the-app)

### Dependencies
In order to run the app, you need the following tools installed:

#### Docker and Docker Compose
To install Docker and Docker Compose (comes already installed with Docker Desktop) for you system follow the links below:

- Mac: [Install Docker Desktop for Mac](https://docs.docker.com/docker-for-mac/install/) to get Docker and Docker Compose installed
- Windows: [Install Docker Desktop for Windows](https://docs.docker.com/docker-for-windows/install/) to get Docker and Docker Compose installed
- Ubuntu:
    - [Docker](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
    - [Docker Compose](https://docs.docker.com/compose/install/)
- Other systems:
    - [Docker](https://docs.docker.com/install/)
    - [Docker Compose](https://docs.docker.com/compose/install/)

#### (git)
You only need git installed if you want to contribute to the repository or clone it without having to download it manually.

#### (R and RStudio)
You only need R (version 3.5 or higher) and RStudio installed in case you would like to make your own changes to the app and try them out before you create the docker containers.

### Get the app
To get the app, you can either

- download the source using the "Clone or download"-button above
- use `git clone https://github.com/fineprint-global/io-visualization.git`

### Setup
There are a few settings that you have to make before you can run the app.

#### `.env` file in the root directory
You have to set the following variables, here is an example `.env` file:

```
POSTGRES_PASSWORD=secret
POSTGRES_PORT=5454
SHINY_PORT=80
```

#### `.Renviron` file in the `/app` directory
In the `.Renviron` file, make sure the `POSTGRES_PORT` in the `.env` file and the `db_port` match. Here is an example file that needs to be created inside the `/app` directory.

```
db_host=ioviz_db
db_port=5454
db_name=fabio
db_user=app
db_password=secret
```

### How to run
There are two ways to run this app.

1. You can run it as is, with FABIO pre-loaded into the database.
2. You can use your own input-output table, which requires you to adapt the scripts in the [input-output-to-db](https://github.com/fineprint-global/io-visualization/tree/master/db/input-output-to-db) directory.

#### 1. Run as is

1. Make sure all necessary dependencies are installed.
2. Make sure Docker (Desktop) is up and running.
3. Make sure you completed the steps in [setup](#setup)
4. Navigate to the root directory (io-visualization) with a shell of your choice and run the following command:
`docker-compose up -d`

Now both, the ioviz_app (RShiny app) and the ioviz_db (postgis database) should be running on ports specified in the `docker-compose.yml`.

#### 2. Use your own input-output table
More detailed instructions on this will come soon, but you will have to adjust the main.R located in the [input-output-to-db](https://github.com/fineprint-global/io-visualization/tree/master/db/input-output-to-db) directory to load your own input-output table and adjust it to the proper database format.

### How to restart or stop the app
- To restart the containers, run `docker-compose restart`
- To stop the containers, move to the `io-visualization` directory and run `docker-compose stop`
- To stop containers and to remove containers, networks, volumes, and images created by `docker-compose up`, run `docker-compose down`

## Troubleshooting
*This section is still to come.*

## Acknowledgement
This project gratefully acknowledges financial support from the ERC as part of the [FINEPRINT](https://www.fineprint.global/) project.
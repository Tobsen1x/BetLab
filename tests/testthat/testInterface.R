context("Interface")
source(file = paste(Sys.getenv('PROJECT_PATH'), 'dataProvider/tuningProfileDao.R', sep = ''), 
       echo = FALSE, encoding = 'UTF-8')
source(file = paste(Sys.getenv('PROJECT_PATH'), 'interface/interfaceService.R', sep = ''), 
       echo = FALSE, encoding = 'UTF-8')

findTuningProfile(1)

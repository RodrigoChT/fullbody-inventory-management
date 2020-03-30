print(Sys.time())
require(shiny)
pandoc.folder <- Sys.getenv("RSTUDIO_PANDOC")
Sys.setenv(RSTUDIO_PANDOC = pandoc.folder)
folder_address <- 'C:/Users/FullBody/Google Drive/FullBodyInventorySystem'
runApp(folder_address, launch.browser = T)

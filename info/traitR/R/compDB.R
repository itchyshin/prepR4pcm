# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:       'Ctrl + Shift + T'
#' @import reticulate
library(reticulate)
#use_python('C:/Users/Finrodpoo/AppData/Local/Programs/Python/Python312/', required = TRUE)
setPythonEnv <- function() {

  py_require('pandas,openpyxl,tqdm,requests,jinja2,biopython,chardet')
}


#source_python('inst/python/speciesLoader.py')
#source_python('inst/python/datasetLoader.py')
#source_python('inst/python/matchTree.py')
#source_python('inst/python/obtainData.py')
#source_python('inst/python/speciesVerifier.py')

#' Initialize comparative database.
#'
#' Setups the initial database that will be the foundation of the system.
#'
#' The function loads AviList-v2025-11Jun-extended.xlsx file (read below for details on where to download it), it will use Pytaxon to standardize the names and create
#' data/species_subspeciesOnly.csv which is the master list for the database.
#'
#' Please ensure that your execution path has a data, temp and out folders.
#' Please ensure you have copied the config.json (required by Pytaxon) and AviList-v2025-11Jun-extended.xlsx files, found in the github of the project, to the
#' root of your execution path.
#'
#' Example, if using RStudio and the project is at /home/user/RProject1 the following should exist:
#'
#'  # /home/user/RProject1/data/
#'  # /home/user/RProject1/temp/ **
#'  * /home/user/RProject1/out/
#'  * /home/user/RProject1/AviList-v2025-11Jun-extended.xlsx
#'  * /home/user/RProject1/config.json
#'
#' IMPORTANT: Run this only for initial setup, if you run it again it will overwrite the main database file, which will cause the system to break.
#' @examples
#' createDatabase()
#' @export
createDatabase <- function() {
  setPythonEnv()
  speciesLoader <- import_from_path("speciesLoader", path = system.file("python", package = "traitR", mustWork = TRUE))
  speciesLoader$loadSpecies("AviList-v2025-11Jun-extended.xlsx","Scientific_name","Taxon_rank")
}

#' Add a new dataset to the comparative database.
#'
#' Adds the provided dataset as a new data file that can be queried by the comparative database.
#'
#' The function loads the provided dataset file (either a CSV or Excel file) and creates a new CSV file with the datasetName in the data folder.
#'
#' The function also runs the species names in the dataset through Pytaxon and then looks them up in the system's master list. If the species is found it registers
#' that it has information of that species in the new dataset, if it is not found it adds the species to the verification list so that a user can later match it to
#' the appropriate species in the master list.
#'
#' The new file contains the original data plus additional data added by the system so it can be queried.
#'
#' @param datasetFile String. Relative path, from execution path, of the file containing the dataset you wish to add.
#' @param datasetFileType String. The format of the dataset file. Accepts: excel, csv.
#' @param datasetName String. The name to assign to the dataset. This will be the name the rest of the system will indetify this dataset as.
#' @param speciesColumn String. The name of the column that specifies the scientific names of the species in the dataset.
#' @examples
#' In this example we will add the dataset "Chia_NestTrait.csv" to the database. In this dataset the column containing the species name is "Scientific_name", the file is located in the folder
#' "origin" and we want to save it as "Nest data". To add it we would run the following instruction:
#'
#' addDataset("origin/Chia_NestTrait.csv","csv","Nest data","Scientific_name")
#' @export
addDataset <- function(datasetFile,datasetFileType,datasetName,speciesColumn) {
  setPythonEnv()
  datasetLoader <- import_from_path("datasetLoader", path = system.file("python", package = "traitR", mustWork = TRUE))
  datasetLoader$addDataset(datasetFile,datasetFileType,datasetName,speciesColumn)
}

#' Add a new phylogenetic tree to the comparative database.
#'
#' Adds the provided phylogenetic tree to the comparative database and regiters the species it contains.
#'
#' The function loads the provided tree file (see below for accepted format) register it in the trees.csv file and registers which species are in the tree.
#'
#' The function also runs the species names in the tree through Pytaxon and then looks them up in the system's master list. If the species is found it registers
#' that the species can be found in this tree, if it is not found it adds the species to the verification list so that a user can later match it to
#' the appropriate species in the master list.
#'
#' The tree formats accepted are: newick, nexus, nexml, phyloxml and cdao
#'
#' @param treeFile String. Relative path, from execution path, of the file containing the tree you wish to add.
#' @param treeFileFormat String. The format of the dataset file. Accepts: newick, nexus, nexml, phyloxml, cdao.
#' @examples
#' In this example we will add the tree "Stage2_Hackett_MCC_no_neg.tre" to the database. This tree is in newick format, the file is located in the folder
#' "origin". To add it we would run the following instruction:
#'
#' addTree("origin/Stage2_Hackett_MCC_no_neg.tre","newick")
#' @export
addTree <- function(treeFile,treeFileFormat) {
  setPythonEnv()
  treeLoader <- import_from_path("matchTree", path = system.file("python", package = "traitR", mustWork = TRUE))
  treeLoader$add_tree(treeFile,treeFileFormat)
}

#' Obtain requested trait data for specified species from specified datasets.
#'
#'
#'
#' @param speciesName String. Scientific name of the species you are interested in obtaining data for.
#' @param traits String. Comma separated list of traits you want the data for.
#' @param datasets String. Comma separated list of datasets containing the desired traits.
#' @return Data frame with the desired trait data for the specified species. Function will also save results to out/testObtain.csv.
#' @examples
#'
#' traitSearch(speciesHacketString,traitsNestString,"Nest data")
#'
#' @export
traitSearch <- function(speciesName,traits,datasets) {
  setPythonEnv()
  defaultColumns <- "Sequence,Scientific_name"
  traits <- paste(defaultColumns,traits,sep=",")
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$obtainData(as.list(strsplit(speciesName,",")[[1]]),as.list(strsplit(traits,",")[[1]]),as.list(strsplit(datasets,",")[[1]]))
}

#' Obtain the datasets that contain the provided species.
#'
#' Function returns a Data frame that lists all datasets that contain the species provided as input.
#'
#' The function looks for each species in the master list and builds a list of unique dataset name that contain at least one of the species requested.
#'
#' The result is a list of all datasets that contain at least one of the species requested.
#'
#' @param speciesName String. Comma separated list of scientific name for species to find.
#' @return Data frame with all datasets in database that contain at least one of the species provided.
#' @examples
#'
#' speciesHacket <- speciesInTrees("origin/Stage2_Hackett_MCC_no_neg.tre")
#' speciesHacketString <- paste(speciesHacket,collapse=",")
#' datasetsIncludeSpecies(speciesHacketString)
#'
#' @export
datasetsIncludeSpecies <- function(speciesName) {
  setPythonEnv()
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$datasetsWithSpecies(as.list(strsplit(speciesName,",")[[1]]))
}

#' Returns a Data frame with the names of the columns (traits) included in the provided list of datasets.
#'
#' @param datasetName String. Comma separated list of dataset names.
#' @return A data frame with the traits included in the provided datasets.
#' @export
traitsInDatasets <- function(datasetNames) {
  setPythonEnv()
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$traitsInDatasets(as.list(strsplit(datasetNames,",")[[1]]))
}

#' Obtain the trees that contain the provided species.
#'
#' Function returns a Data frame that lists all trees that contain the species provided as input.
#'
#' The function looks for each species in the master list and builds a list of unique tree name that contain at least one of the species requested.
#'
#' The result is a list of all trees that contain at least one of the species requested.
#'
#' @param speciesNames String. Comma separated list of scientific name for species to find.
#' @return A data frame with the trees that contain at least one of the specified species.
#' @examples
#'
#' speciesHacket <- speciesInTrees("origin/Stage2_Hackett_MCC_no_neg.tre")
#' speciesHacketString <- paste(speciesHacket,collapse=",")
#' treesContainSpecies(speciesHacketString)
#'
#' @export
treesContainSpecies <- function(speciesNames) {
  setPythonEnv()
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$treesWithSpecies(as.list(strsplit(speciesNames,",")[[1]]))
}

#' Returns a Data frame with the species names included in the provided list of trees.
#' @param treeNames String. Comma separated list of trees to use.
#' @return A data frame with the species included in the provided trees.
#' @export
speciesInTrees <- function(treeNames) {
  setPythonEnv()
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$speciesInTrees(as.list(strsplit(treeNames,",")[[1]]))
}

#' Returns a Data frame where each entry is ";" separated tupple containing: first the name of the species as it appears in the tree the species names as are in the tree and second the name
#' of the species used in the database.
#' @param treeName String. Tree to obtain species names from.
#' @return A data frame where each entry is ";" separated tupple containing: first the name of the species as it appears in the tree the species names as are in the tree and second the name
#' of the species used in the database.
#' @export
speciesTreeName <- function(treeName) {
  setPythonEnv()
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$speciesNameInTree(treeName)
}

#' @export
getTreeList <- function() {
  setPythonEnv()
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$listTrees()
}

#' @export
getDatasetList <- function() {
  setPythonEnv()
  dataSearch <- import_from_path("obtainData", path = system.file("python", package = "traitR", mustWork = TRUE))
  dataSearch$listDatasets()
}

#' Returns a Data frame with the species names curated by Pytaxon. Pytaxon results are saved in the temp folder of your project.
#'
#' @param speciesNames String. Comma separated list of scientific name for species to analyze through Pytaxon.
#' @return A data frame with the names of the species returned by Pytaxon. Entries are in the same order as the input list.
#' @export
getPytaxonSpeciesName <- function(speciesNames) {
  setPythonEnv()
  pytaxonUtils <- import_from_path("pytaxonUtils", path = system.file("python", package = "traitR", mustWork = TRUE))
  pytaxonUtils$getPytaxonSpeciesName(as.list(strsplit(speciesNames,",")[[1]]))
}
folderPath <- 'test'

if (!dir.exists(folderPath)) {
  dir.create(folderPath)
}

folderPath <- 'test'

if (!dir.exists(folderPath)) {
  dir.create(folderPath)
}

#createDatabase()
#addDataset("testAtlantic1comma.csv","csv","EdTest","Binomial")
#addDataset("Beak shape.xlsx","excel","Beak_shape","PhyloName")
#addTree("BBtreeC2022.tre")

#obtainData("Rhea pennata",tuple("Sequence","Scientific_name","Beak_shape_Mass","Beak_shape_width","Beak_shape_depth"),tuple("Beak_shape"))
#datasetsIncludeSpecies("Rhea pennata,Accipiter albogularis,Hemispingus auricularis,Guira guira")
#traitsInDatasets("Beak_shape")
#treesContainSpecies("Tinamotis pentlandii,Nothoprocta ornata")
#speciesInTrees("BBtreeC2022.tre")

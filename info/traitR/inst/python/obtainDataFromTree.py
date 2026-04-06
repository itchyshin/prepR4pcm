import pandas as pd
import pytaxon
import math
from obtainData import obtainData

def obtainDataFromTree(speciesToFind,dataToFind,datasets):       
    
    #Load species file
    #speciesFile = "species_subspeciesOnly.xlsx"
    speciesFile = "data/species_subspeciesOnly.csv"
    
    #speciesList = pd.read_excel(speciesFile,header=0)
    speciesList = pd.read_csv(speciesFile,header=0)
    compiledData = speciesList
    index = 0
    
    #Load dataset list
    #datasetFile = "datasetDB.xlsx"
    datasetFile = "data/datasetDB.csv"
    
    #datasetList = pd.read_excel(datasetFile,header=0)
    datasetList = pd.read_csv(datasetFile,header=0)
    
    #Load each dataset and add it to compiledData
    for element in datasets:
        
        #Find dataset file name in list
        print(f"Dataset is: {element}")
        rowsDataset = datasetList[datasetList["datasetName"] == element]
        #datasetData = pd.read_excel(rowsDataset.at[rowsDataset.index[0],"datasetFile"])
        datasetData = pd.read_csv(f"data/{rowsDataset.at[rowsDataset.index[0],"datasetFile"]}")
        
        if index == 0:
            compiledData = pd.merge(compiledData, datasetData, left_on="Sequence", right_on="SequenceSpecies", suffixes=("_species",f"_dataset{index}"))
        else:
            compiledData = pd.merge(compiledData, datasetData, left_on="Sequence", right_on="SequenceSpecies", suffixes=(None,f"_dataset{index}"))
            
        index = index + 1
            
    #Find the species data
    rowsWithElementSpecies = compiledData[compiledData["Scientific_name"] == speciesToFind]
    
    #Find the specific data columns
    dataSearched = rowsWithElementSpecies.loc[:, dataToFind]
    
    #rowsWithElementSpecies.to_excel(f"testObtain.xlsx", index=False)
    #dataSearched.to_excel(f"testObtain.xlsx", index=False)
    dataSearched.to_csv(f"testObtain.csv", index=False)
    
def main():
    
    treeFileName = "trees.csv"
    recordedTreesDF = pd.read_csv(treeFileName,header=0)
    
    print(f"These are the trees available{recordedTreesDF["treeName"]}")
    
    treeName = input("Please input the tree you want to use: ")
    
    treeData = recordedTreesDF[recordedTreesDF["treeName"] == treeName]
    
    print(f"The species included in this tree are: {treeData["speciesIncluded"]}")
    
    treeSpecies = treeData["speciesIncluded"].tolist()
    treeSpeciesList = treeSpecies[0].split(',')
    
    speciesFile = "species_subspeciesOnly.csv"
    
    speciesList = pd.read_csv(speciesFile,header=0)
    
    nanMatrix = speciesList.isna()
    
    for species in treeSpeciesList:
        #Look for species in species list and obtain datasets the species is located
        rowsWithSpecies = speciesList[speciesList["Scientific_name"] == species]
        
        if len(rowsWithSpecies.index) == 0:
            print(f"Species {species} not found.")
            continue
        
        speciesDatasets = speciesList.at[rowsWithSpecies.index[0],"Dataset"]
        print(f"Species {species} found in datasets: {speciesDatasets}")         
    
    datasetsToSearch = input("Please input the datasets you wish to use (format: dataset1,dataset2): ")
    
    #Look for columnns for each dataset and print them
    #datasetFile = "datasetDB.xlsx"
    datasetFile = "datasetDB.csv"
    #datasetList = pd.read_excel(datasetFile, header=0)
    datasetList = pd.read_csv(datasetFile, header=0)
    
    datasetListToSearch = datasetsToSearch.split(",")
    
    for datasetName in datasetListToSearch:
        datasetRows = datasetList[datasetList["datasetName"] == datasetName]
        print(f"Dataset: {datasetName} has columns: {datasetList.at[datasetRows.index[0],"datasetColumns"]}")
    
    columnsToSearch = input("Please input columns you wish to use (format: column1,column2): ")
    columnsToSearch = "Sequence,Scientific_name," + columnsToSearch
    
    allData = None
    first = True
    
    for species in treeSpeciesList:
        #obtainData("Rhea pennata",["Sequence","Scientific_name","Beak_shape_Mass","Beak_shape_width","Beak_shape_depth"],["Beak_shape"])
        rowsWithSpecies = speciesList[speciesList["Scientific_name"] == species]
        
        if len(rowsWithSpecies.index) == 0:
            print(f"Species {species} not found.")
            continue
        
        speciesDatasets = speciesList.at[rowsWithSpecies.index[0],"Dataset"]        
        
        if not nanMatrix.at[rowsWithSpecies.index[0],"Dataset"]:
            speciesData = obtainData(species,columnsToSearch.split(","),datasetsToSearch.split(","))
            
            if first:
                allData = speciesData
                first = False
            else:
                allData = pd.concat([allData, speciesData], ignore_index=True)
            
    allData.to_csv(f"treeObtain.csv", index=False)
    
#main()
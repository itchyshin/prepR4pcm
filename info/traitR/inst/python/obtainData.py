import pandas as pd
import pytaxon

def obtainData(speciesToFind,dataToFind,datasets):       
    
    allData = None
    first = True    
    
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
    
    print("Setting datasets")
    
    #Load each dataset and add it to compiledData
    for element in datasets:
        
        #Find dataset file name in list
        #print(f"Dataset is: {element}")
        rowsDataset = datasetList[datasetList["datasetName"] == element]
        #datasetData = pd.read_excel(rowsDataset.at[rowsDataset.index[0],"datasetFile"])
        datasetData = pd.read_csv(f"data/{rowsDataset.at[rowsDataset.index[0],"datasetFile"]}")
        
        if index == 0:
            compiledData = pd.merge(compiledData, datasetData, how="left", left_on="Sequence", right_on="SequenceSpecies", suffixes=("_species",f"_dataset{index}"))
        else:
            compiledData = pd.merge(compiledData, datasetData, how="left", left_on="Sequence", right_on="SequenceSpecies", suffixes=(None,f"_dataset{index}"))
            
        index = index + 1
            
    #Find the species data
    #rowsWithElementSpecies = compiledData[compiledData["Scientific_name"] == speciesToFind]
    
    #Find the specific data columns
    #dataSearched = rowsWithElementSpecies.loc[:, dataToFind]
    
    print("Gathering information for species (this might take some time)")
    
    for species in speciesToFind:
        #obtainData("Rhea pennata",["Sequence","Scientific_name","Beak_shape_Mass","Beak_shape_width","Beak_shape_depth"],["Beak_shape"])
        #rowsWithSpecies = speciesList[speciesList["Scientific_name"] == species]
        
        #speciesData = obtainData(species,dataToFind,dataset)
        
        #print(f"I am gathering data for species: {species}")
        
        #Find the species data
        rowsWithElementSpecies = compiledData[compiledData["Scientific_name"] == species]
        
        #Find the specific data columns
        speciesData = rowsWithElementSpecies.loc[:, dataToFind]        
            
        if first:
            allData = speciesData
            first = False
        else:
            allData = pd.concat([allData, speciesData], ignore_index=True)
        
    allData.to_csv(f"out/multipleObtain.csv", index=False)
    
    return allData    
    
    #rowsWithElementSpecies.to_excel(f"testObtain.xlsx", index=False)
    #dataSearched.to_excel(f"testObtain.xlsx", index=False)
    #dataSearched.to_csv(f"out/testObtain.csv", index=False)
    
    #return dataSearched

def obtainDataMultipleSpecies(speciesToFind,dataToFind,dataset):
    allData = None
    first = True
    
    for species in speciesToFind:
        #obtainData("Rhea pennata",["Sequence","Scientific_name","Beak_shape_Mass","Beak_shape_width","Beak_shape_depth"],["Beak_shape"])
        #rowsWithSpecies = speciesList[speciesList["Scientific_name"] == species]
        
        speciesData = obtainData(species,dataToFind,dataset)
            
        if first:
            allData = speciesData
            first = False
        else:
            allData = pd.concat([allData, speciesData], ignore_index=True)
        
    allData.to_csv(f"out/multipleObtain.csv", index=False)
    
    return allData

def datasetsWithSpecies(speciesInterestList):
    #Load species list
    speciesFile = "data/species_subspeciesOnly.csv"
    
    #speciesDf = pd.read_excel(speciesFile,header=0)
    speciesList = pd.read_csv(speciesFile,header=0)
    speciesList = speciesList.fillna("")
    
    datasets = []
    
    for species in speciesInterestList:
        #Look for species in species list and obtain datasets the species is located
        rowsWithSpecies = speciesList[speciesList["Scientific_name"] == species]
        rowsWithSpeciesProtonym = speciesList[speciesList["Synonyms"] == species]
        
        if len(rowsWithSpecies.index) == 0 and len(rowsWithSpeciesProtonym.index) == 0:
            print(f"Species {species} not found.")
            continue
        
        if len(rowsWithSpecies.index) > 0:
            speciesDatasets = speciesList.at[rowsWithSpecies.index[0],"Dataset"]
        else:
            speciesDatasets = speciesList.at[rowsWithSpeciesProtonym.index[0],"Dataset"]
        
        #print(f"Species {species} found in datasets: {speciesDatasets}")
        if speciesDatasets == "":
            print(f"Species {species} is in no known dataset.")
            continue
            
        speciesDatasetsList = speciesDatasets.split(",")
        
        for dataset in speciesDatasetsList:
            
            #print(f"Found dataset {dataset}")
            if dataset not in datasets:
                #print(f"Adding dataset {dataset}")
                datasets.append(dataset)
    
    return datasets

def traitsInDatasets(datasetInterestList):
    
    datasetFile = "data/datasetDB.csv"
    #datasetList = pd.read_excel(datasetFile, header=0)
    datasetList = pd.read_csv(datasetFile, header=0)
    
    traitsFound = ""
    
    for datasetName in datasetInterestList:
        datasetRows = datasetList[datasetList["datasetName"] == datasetName]
        #print(f"Dataset: {datasetName} has columns: {datasetList.at[datasetRows.index[0],"datasetColumns"]}")
        
        if traitsFound == "":
            traitsFound = datasetList.at[datasetRows.index[0],"datasetColumns"]
        else:
            traitsFound = traitsFound + "," + datasetList.at[datasetRows.index[0],"datasetColumns"]
    
    return traitsFound.split(",")

def treesWithSpecies(speciesInterestList):
    treeFileName = "data/trees.csv"
    recordedTreesDF = pd.read_csv(treeFileName,header=0)
    
    speciesFile = "data/species_subspeciesOnly.csv"
    
    speciesList = pd.read_csv(speciesFile,header=0)    
    
    treesFound = ''
    
    nanMatrix = speciesList.isna()
    
    for species in speciesInterestList:
        rowsWithSpecies = speciesList[speciesList["Scientific_name"] == species]
        rowsWithSpeciesProtonym = speciesList[speciesList["Synonyms"] == species]
        
        if len(rowsWithSpecies.index) == 0 and len(rowsWithSpeciesProtonym) == 0:
            print(f"Species {species} not found.")
            continue
        
        if len(rowsWithSpecies.index) > 0:
            speciesTrees = speciesList.at[rowsWithSpecies.index[0],"Tree"]
            
            if nanMatrix.at[rowsWithSpecies.index[0],"Tree"]:
                print(f"Species {species} has no trees associated with it.")
                continue
        else:
            speciesTrees = speciesList.at[rowsWithSpeciesProtonym.index[0],"Tree"]
            
            if nanMatrix.at[rowsWithSpeciesProtonym.index[0],"Tree"]:
                print(f"Species {species} has no trees associated with it.")
                continue            
        
        treesList = speciesTrees.split(',')
        
        for tree in treesList:
            if treesFound == '':
                treesFound = tree
            else:
                if tree not in treesFound:
                    treesFound = treesFound + "," + tree
    
    return treesFound.split(",")  

def speciesInTrees(treesList):
    treeFileName = "data/trees.csv"
    recordedTreesDF = pd.read_csv(treeFileName,header=0)
    
    speciesFound = []
    
    for tree in treesList:
        treeData = recordedTreesDF[recordedTreesDF["treeName"] == tree]
        treeSpecies = treeData["speciesIncluded"].tolist()
        treeSpeciesList = treeSpecies[0].split(",")        
        
        for species in treeSpeciesList:
            if species not in speciesFound:
                speciesFound.append(species)
        
    return speciesFound

def speciesNameInTree(tree):
    treeFileName = "data/trees.csv"
    recordedTreesDF = pd.read_csv(treeFileName,header=0)
    
    speciesFound = []
    
    treeData = recordedTreesDF[recordedTreesDF["treeName"] == tree]
    treeSpecies = treeData["speciesIncludedWithOriginal"].tolist()
    #print(treeSpecies)
    treeSpeciesList = treeSpecies[0].split(",")        
    #print(treeSpeciesList)
        
    for species in treeSpeciesList:
        
        species = species.replace("(","")
        species = species.replace(")","")
        #speciesEntryElements = speciesEntry.split(";")
        #species = speciesEntryElements[0]
        
        speciesFound.append(species)
        
    return speciesFound

def listTrees():
    treeFileName = "data/trees.csv"
    
    recordedTreesDF = pd.read_csv(treeFileName,header=0)
    
    treeData = recordedTreesDF["treeName"]
    
    return treeData

def listDatasets():
    datasetFileName = "data/datasetDB.csv"
    
    recordedDatasetsDF = pd.read_csv(datasetFileName,header=0)
    
    datasetData = recordedDatasetsDF["datasetName"]
    
    return datasetData

#listTree()
#datasetsWithSpecies(["Rhea pennata","Accipiter albogularis","Hemispingus auricularis","Guira guira"])    
#def main():
    #speciesName = input("Please input species you want to look for: ")
    
    ##Get name from Pytaxon ASSUMING 1 SPECIES FOR NOW
    ##Create dataframe with species info to create temp file for Pytaxon
    #speciesToAdd = []
    #speciesRow = [None for i in range(2)]
    #speciesRow[0] = speciesName
    #speciesRow[1] = speciesName
    #speciesToAdd.append(speciesRow)
    
    #speciesPytaxonDF = pd.DataFrame(speciesToAdd, columns=["species","scientificName"])
    
    #speciesPytaxonDF.to_excel("pytaxonTemp.xlsx", index=False)
    
    ##Run Pytaxon
    #pt = pytaxon.Pytaxon(11)
    #pt.read_spreadshet("pytaxonTemp.xlsx")
    #pt.read_columns(f"x,x,x,x,x,x,species,scientificName")
    #pt.check_species_and_lineage()  
    
    ##Obtain new name to use
    #ptData = pt._incorrect_data
    #numEntries = ptData['Wrong Name'].count(speciesName)
    
    #if numEntries >= 2:
        #pytaxonName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesName)]
        
        #if pytaxonName != '':
            ##speciesOfInterestName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
            #speciesName = pytaxonName     
    
    ##Load species file
    ##speciesFile = "species_subspeciesOnly.xlsx"
    #speciesFile = "species_subspeciesOnly.csv"
    
    ##speciesList = pd.read_excel(speciesFile,header=0)
    #speciesList = pd.read_csv(speciesFile,header=0)
    
    ##Look for species in species list and obtain datasets the species is located
    #rowsWithSpecies = speciesList[speciesList["Scientific_name"] == speciesName]
    
    #if len(rowsWithSpecies.index) == 0:
        #print(f"Species {speciesName} not found.")
        #return
    
    #speciesDatasets = speciesList.at[rowsWithSpecies.index[0],"Dataset"]
    #print(f"Species found in datasets: {speciesDatasets}")
    
    #datasetsToSearch = input("Please input the datasets you wish to use (format: dataset1,dataset2): ")
    
    ##Look for columnns for each dataset and print them
    ##datasetFile = "datasetDB.xlsx"
    #datasetFile = "datasetDB.csv"
    ##datasetList = pd.read_excel(datasetFile, header=0)
    #datasetList = pd.read_csv(datasetFile, header=0)
    
    #datasetListToSearch = datasetsToSearch.split(",")
    
    #for datasetName in datasetListToSearch:
        #datasetRows = datasetList[datasetList["datasetName"] == datasetName]
        #print(f"Dataset: {datasetName} has columns: {datasetList.at[datasetRows.index[0],"datasetColumns"]}")
    
    #columnsToSearch = input("Please input columns you wish to use (format: column1,column2): ")
    
    ##obtainData("Rhea pennata",["Sequence","Scientific_name","Beak_shape_Mass","Beak_shape_width","Beak_shape_depth"],["Beak_shape"])
    #obtainData(speciesName,columnsToSearch.split(","),datasetsToSearch.split(","))
    
#main()
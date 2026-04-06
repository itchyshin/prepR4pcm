import pandas as pd

def verifySpecies(speciesName, speciesAvilistName):
    #speciesToVerifyFile = "newSpecies.xlsx"
    speciesToVerifyFile = "newSpecies.csv"
    #speciesToVerifyList = pd.read_excel(speciesToVerifyFile, header=0)
    speciesToVerifyList = pd.read_csv(speciesToVerifyFile, header=0)
    
    #speciesListFile = "species_subspeciesOnly.xlsx"
    speciesListFile = "species_subspeciesOnly.csv"
    #speciesList = pd.read_excel(speciesListFile, header=0)
    speciesList = pd.read_csv(speciesListFile, header=0)
    
    rowsWithSpeciesVerify = speciesToVerifyList[speciesToVerifyList["Scientific_name"] == speciesName]
    
    if len(rowsWithSpeciesVerify.index) == 0:
        print(f"Species {speciesName} not found in list of species to verify.")
        return
    
    rowsWithSpecies = speciesList[speciesList["Scientific_name"] == speciesAvilistName]
    
    if len(rowsWithSpecies.index) == 0:
        print(f"Species {speciesAvilistName} not found in species list.")
        return
    
    indexToVerify = 0
    for i in range(len(rowsWithSpeciesVerify.index)):
        verifyDatasets = rowsWithSpeciesVerify.at[rowsWithSpeciesVerify.index[indexToVerify],"Dataset"]
        speciesDatasetName = rowsWithSpeciesVerify.at[rowsWithSpeciesVerify.index[indexToVerify],"speciesDatasetName"]
        
        #update datasets in species list to include datasets of the species being verified
        speciesList.at[rowsWithSpecies.index[0],"Dataset"] = f"{speciesList.at[rowsWithSpecies.index[0],"Dataset"]},{verifyDatasets}"
        
        #Add name to verify as a synonym in master list
        speciesList.at[rowsWithSpecies.index[0],"Synonyms"] = f"{speciesList.at[rowsWithSpecies.index[0],"Synonyms"]},{speciesName}"
        
        #update each dataset entry with the ID from the verified species
        #datasetFile = "datasetDB.xlsx"
        datasetFile = "datasetDB.csv"
        #datasetList = pd.read_excel(datasetFile, header=0)
        datasetList = pd.read_csv(datasetFile, header=0)
        
        verifyDatasetList = verifyDatasets.split(",")
        
        for datasetName in verifyDatasetList:
            rowsDatasetName = datasetList[datasetList["datasetName"] == datasetName]
            verifyDatasetFile = datasetList.at[rowsDatasetName.index[0],"datasetFile"]
            verifySpeciesColumn = datasetList.at[rowsDatasetName.index[0],"speciesName"]
            
            #verifyDataset = pd.read_excel(verifyDatasetFile, header=0)
            verifyDataset = pd.read_csv(verifyDatasetFile, header=0)
            
            #rowsVerifyDatasetWithSpeciesVerify = verifyDataset[verifyDataset[verifySpeciesColumn] == speciesName]
            rowsVerifyDatasetWithSpeciesVerify = verifyDataset[verifyDataset[verifySpeciesColumn] == speciesDatasetName]
            
            #print(rowsVerifyDatasetWithSpeciesVerify.index[1])
            
            index = 0
            for j in range(len(rowsVerifyDatasetWithSpeciesVerify.index)):
                verifyDataset.at[rowsVerifyDatasetWithSpeciesVerify.index[index],"SequenceSpecies"] = speciesList.at[rowsWithSpecies.index[0],"Sequence"]
                index = index + 1
                #verifyDataset.at[entry.index[0],"SequenceSpecies"] = speciesList.at[rowsWithSpecies.index[0],"Sequence"]
            
            #verifyDataset.to_excel(verifyDatasetFile, index=False)
            verifyDataset.to_csv(verifyDatasetFile, index=False)
            
        #Drop species from verify list
        #print(rowsWithSpeciesVerify.index[indexToVerify])
        speciesToVerifyList = speciesToVerifyList.drop(rowsWithSpeciesVerify.index[indexToVerify])
        indexToVerify = indexToVerify + 1
    
    #speciesToVerifyList.to_excel(speciesToVerifyFile, index=False)
    speciesToVerifyList.to_csv(speciesToVerifyFile, index=False)
    #speciesList.to_excel(speciesListFile, index=False)
    speciesList.to_csv(speciesListFile, index=False)
    
    print("Completed")
    

def main():
    speciesToVerifyFile = "newSpecies.csv"
    speciesToVerifyList = pd.read_csv(speciesToVerifyFile, header=0)
    
    speciesToVerifyNames = speciesToVerifyList["Scientific_name"]
    
    for species in speciesToVerifyNames:
        print(f"{species}")
        
    speciesToVerify = input("Please type the species to verify: ")
    
    
        
#main()
#verifySpecies("A","Nothura maculosa")
#verifySpecies("Nothura chacoensis","B")
#verifySpecies("Nothura chacoensis","Nothura maculosa")
#verifySpecies("Nothura chacoensis","Nothura maculosa")
#verifySpecies("Euphonia cyanocephala","Nothura maculosa")
#verifySpecies("Phyllomyias burmeisteri","Nothura maculosa")
#verifySpecies("Percnohierax leucorrhous","Nothura maculosa")
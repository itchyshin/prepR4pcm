from speciesLoader import loadSpecies
from datasetLoader import addDataset
from matchTree import addTree
from obtainData import obtainData
from speciesVerifier import verifySpecies
import pandas as pd
import pytaxon

def main():
    logFilename = "log.txt"
    
    while(True):
    #Main menu
        print("Select the option you'd like to do:")
        print("1. Set species master list.")
        print("2. Add dataset.")
        print("3. Add tree.")
        print("4. Search for a species.")
        print("5. Search based on a phylogenetic tree")
        print("6. Verify species.")
        print("7. Exit")
        
        optionSelected = input("Selection: ")
        
        if optionSelected == "1":
            loadSpecies("AviList-v2025-11Jun-extended.xlsx","Scientific_name","Taxon_rank")
            
            with open(logFilename,"a") as f:
                f.write("loadSpecies(\"AviList-v2025-11Jun-extended.xlsx\",\"Scientific_name\",\"Taxon_rank\")\n")
                
        elif optionSelected == "2":
            #Add dataset information
            useGenus = False
            datasetSpeciesGenus = ""
            datasetSheets = [1]
            
            datasetFileName = input("Please provide the name of the dataset file you want to add: ")
            datasetFileType = input("Please provide type of file: ")
            
            if datasetFileType == "excel":
                datasetSheetsString = input("Please enter the sheets to import, separated by a comma without spaces: ")
                datasetSheets = list(map(int, datasetSheetsString.split(",")))
                
            datasetName = input("Please enter name of the dataset: ")
            datasetSpeciesColumn = input("Please enter name of the column that contains species names: ")
            datasetSpeciesUseGenus = input("Is the genus needed for species name [Y/N]: ")
            
            if datasetSpeciesUseGenus == "Y":
                useGenus = True
                datasetSpeciesGenus = input("Please enter name of the column that contains the genus: ")
                
            addDataset(datasetFileName, datasetFileType, datasetName, datasetSpeciesColumn, datasetSheets, useGenus, datasetSpeciesGenus)
            
            with open(logFilename,"a") as f:
                f.write(f"datasetFileName={datasetFileName}\n")
                f.write(f"datasetFileType={datasetFileType}\n")
                f.write(f"datasetName={datasetName}\n")
                f.write(f"datasetSpeciesColumn={datasetSpeciesColumn}\n")
                f.write(f"datasetSheets={datasetSheets}\n")
                f.write(f"useGenus={useGenus}\n")
                f.write(f"datasetSpeciesGenus={datasetSpeciesGenus}\n")
                f.write("addDataset(datasetFileName, datasetFileType, datasetName, datasetSpeciesColumn, datasetSheets, useGenus, datasetSpeciesGenus)\n")
                
        elif optionSelected == "3":
            #Add tree information
            treeFileName = input("Please provide the name of the tree file you want to add: ")
            treeFileFormat = input("Please provide the format of the tree: ")
            
            #addTree(treeFileName, "species_subspeciesOnly.xlsx", treeFileFormat)
            addTree(treeFileName, "species_subspeciesOnly.csv", treeFileFormat)
            
            with open(logFilename,"a") as f:
                f.write(f"treeFileName={treeFileName}\n")
                f.write(f"treeFileFormat={treeFileFormat}\n")
                f.write("addTree(treeFileName, \"species_subspeciesOnly.xlsx\", treeFileFormat)\n")
                
        elif optionSelected == "4":
            
            #Ask for the species to look for
            speciesName = input("Please provide the name of the species you want to search: ")
            
            #Get name from Pytaxon ASSUMING 1 SPECIES FOR NOW
            #Create dataframe with species info to create temp file for Pytaxon
            speciesToAdd = []
            speciesRow = [None for i in range(2)]
            speciesRow[0] = speciesName
            speciesRow[1] = speciesName
            speciesToAdd.append(speciesRow)
            
            speciesPytaxonDF = pd.DataFrame(speciesToAdd, columns=["species","scientificName"])
            
            speciesPytaxonDF.to_excel("pytaxonTemp.xlsx", index=False)
            
            #Run Pytaxon
            pt = pytaxon.Pytaxon(11)
            pt.read_spreadshet("pytaxonTemp.xlsx")
            pt.read_columns(f"x,x,x,x,x,x,species,scientificName")
            pt.check_species_and_lineage()  
            
            #Obtain new name to use
            ptData = pt._incorrect_data
            numEntries = ptData['Wrong Name'].count(speciesName)
            
            if numEntries >= 2:
                pytaxonName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesName)]
                
                if pytaxonName != '':
                    #speciesOfInterestName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
                    speciesName = pytaxonName            
            
            #Load species file
            #speciesFile = "species_subspeciesOnly.xlsx"
            speciesFile = "species_subspeciesOnly.csv"
            
            #speciesList = pd.read_excel(speciesFile,header=0)
            speciesList = pd.read_csv(speciesFile,header=0)
            
            #Look for species in species list and obtain datasets the species is located
            rowsWithSpecies = speciesList[speciesList["Scientific_name"] == speciesName]
            datasetContainSpecies = ""
            
            if len(rowsWithSpecies.index) > 0:
                #print(f"Species {speciesName} not found.")
            
                datasetContainSpecies = speciesList.at[rowsWithSpecies.index[0],"Dataset"]
                #print(f"Species found in datasets: {speciesDatasets}")
                #if datasetContainSpecies == "":
                #    datasetContainSpecies = speciesDatasets
                #    continue
                
                #datasetContainSpecies = datasetContainSpecies + "," + speciesDatasets
                    
                print("This are the datasets that contain the specified species: ")
                print(f"{datasetContainSpecies}")
                
                datasetsToSearchString = input("Enter a comma separated list of datasets you want to look at: ")
                datasetsToSearchList = datasetsToSearchString.split(",")
                
                #datasetFile = "datasetDB.xlsx"
                datasetFile = "datasetDB.csv"
                #datasetList = pd.read_excel(datasetFile, header=0)
                datasetList = pd.read_csv(datasetFile, header=0)
                
                for datasetName in datasetsToSearchList:
                    datasetRows = datasetList[datasetList["datasetName"] == datasetName]
                    print(f"Dataset: {datasetName} has columns: {datasetList.at[datasetRows.index[0],"datasetColumns"]}")
                    
                columnsToSearch = input("Enter a comma separated list of the columns you wish to obtain: ")
                
                obtainData(speciesName,columnsToSearch.split(","),datasetsToSearchList)
                
                with open(logFilename,"a") as f:
                    f.write(f"speciesName={speciesName}\n")
                    f.write(f"columnsToSearch={columnsToSearch}\n")
                    f.write(f"datasetsToSearchList={datasetsToSearchList}\n")
                    f.write("obtainData(speciesName,columnsToSearch.split(\",\"),datasetsToSearchList)\n")
                
            else:
                print("No matching species was found.")
        
        elif optionSelected == "5":        
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
            
        elif optionSelected == "6":
            #Load species that need verification
            #speciesVerifyFile = "newSpecies.xlsx"
            speciesVerifyFile = "newSpecies.csv"
            
            #speciesVerifyDF = pd.read_excel(speciesVerifyFile,header=0)    
            speciesVerifyDF = pd.read_csv(speciesVerifyFile,header=0)
            
            speciesVerifyNameList = speciesVerifyDF["Scientific_name"]
            
            print("Species that need verification: ")
            
            speciesNameString = ""
            
            for speciesName in speciesVerifyNameList:
                if speciesNameString == "":
                    speciesNameString = speciesName
                    continue
                speciesNameString = speciesNameString + ", " + speciesName
                
            print(f"{speciesNameString}")
            
            speciesToVerify = input("Please enter species you wish to verify: ")
            
            #Load species file
            #speciesFile = "species_subspeciesOnly.xlsx"
            
            #speciesDF = pd.read_excel(speciesFile,header=0)
            #speciesNameList = speciesDF["Scientific_name"]
            
            #print("Species in master list: ")
            
            #speciesNameString = ""
            
            #for speciesName in speciesNameList:
            #    if speciesNameString == "":
            #        speciesNameString = speciesName
            #        continue
            #    speciesNameString = speciesNameString + ", " + speciesName
                
            #print(f"{speciesNameString}")
            
            matchedSpecies = input("Please enter species that should match the verified species: ")
            verifySpecies(speciesToVerify,matchedSpecies)
            
            with open(logFilename,"a") as f:
                f.write(f"speciesToVerify={speciesToVerify}\n")
                f.write(f"matchedSpecies={matchedSpecies}\n")
                f.write("verifySpecies(speciesToVerify,matchedSpecies)\n")
            
        elif optionSelected == "7":
            break
    
main()
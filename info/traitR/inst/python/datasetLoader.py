import pandas as pd
import pytaxon
import chardet

def detectFileEncoding(filePath):
    # Open the file in binary mode ('rb')
    with open(filePath, 'rb') as f:
        # Read the entire file content as bytes for accurate detection
        rawData = f.read()

    # Use chardet.detect() to analyze the data
    result = chardet.detect(rawData)

    # Extract the encoding and confidence
    encoding = result['encoding']
    confidence = result['confidence']

    return encoding

def addDataset(datasetFile,fileType,newName,speciesColumn,sheetList=[1],genusNeeded=False,genusColumn=""):
    
    try:
        #datasetList = pd.read_excel("datasetDB.xlsx", header=0)
        datasetList = pd.read_csv("data/datasetDB.csv", header=0)
        
        rowsWithDataset = datasetList[datasetList["datasetName"] == newName]
        
        if len(rowsWithDataset.index) > 0:
            print("A dataset with the chosen name is already recorded.")
            return
    except FileNotFoundError:
        temp = 0
    
    pt = pytaxon.Pytaxon(11)
    pt.read_spreadshet(datasetFile)
    #pt.read_columns(f"Family,Family,Family,Family,Family,Family,{speciesColumn},Family")
    #pt.read_columns(f"{speciesColumn},{speciesColumn},{speciesColumn},{speciesColumn},{speciesColumn},{speciesColumn},{speciesColumn},{speciesColumn}")
    #pt.read_columns(f"{speciesColumn},{speciesColumn},{speciesColumn},{speciesColumn},Family,{speciesColumn},{speciesColumn},{speciesColumn}")
    pt.read_columns(f"x,x,x,x,x,x,{speciesColumn},{speciesColumn}")
    pt.check_species_and_lineage()
    #pt.create_to_correct_spreadsheet('test')
    #Read dataset file to load dataFrames or dict of dataFrames
    if fileType == "excel":
        datasetToAddDf = pd.read_excel(datasetFile, header=0, sheet_name=None)
    elif fileType == "csv":
        
        try:
            detectedEncoding = detectFileEncoding(datasetFile)
            print(f"Encoding is: {detectedEncoding}")
            #result = chardet.detect(datasetFile)
            #datasetToAddDf = pd.read_csv(datasetFile, header=0, encoding='latin1') 
            datasetToAddDf = pd.read_csv(datasetFile, header=0, encoding=detectedEncoding)
            
        except FileNotFoundError:
            print(f"The file '{datasetFile}' was not found")
        
    #If it has several sheets process the dictionary
    if isinstance(datasetToAddDf, dict):
        dataFrameList = datasetToAddDf.values()
        
        index = 1
        #Process each element
        for dataFrameValue in dataFrameList:
            
            if index in sheetList:
                if index == 1:
                    processDataFrame(dataFrameValue,newName,speciesColumn,genusNeeded,genusColumn,pt)
                else:
                    processDataFrame(dataFrameValue,f"{newName}_{index}",speciesColumn,genusNeeded,genusColumn,pt)
            index = index + 1
        
    else:
        processDataFrame(datasetToAddDf,newName,speciesColumn,genusNeeded,genusColumn,pt)

def processDataFrame(datasetToAddDf,newName,speciesColumn,genusNeeded,genusColumn,pt):
    
    pt.create_to_correct_spreadsheet('test')
    #Load species list
    speciesFile = "data/species_subspeciesOnly.csv"
    
    #speciesDf = pd.read_excel(speciesFile,header=0)
    speciesDf = pd.read_csv(speciesFile,header=0)
    
    #Record dataset in dataset list
    #datasetRow = [newName,f"{newName}.xlsx","",f"{newName}_{speciesColumn}",f"{newName}_{genusNeeded}",f"{newName}_{genusColumn}"]
    datasetRow = [newName,f"{newName}.csv","",f"{newName}_{speciesColumn}",f"{newName}_{genusNeeded}",f"{newName}_{genusColumn}"]
    
    try:
        #datasetList = pd.read_excel("datasetDB.xlsx", header=0)
        datasetList = pd.read_csv("data/datasetDB.csv", header=0)
        
        rowsWithDataset = datasetList[datasetList["datasetName"] == newName]
        
        if len(rowsWithDataset.index) > 0:
            print("A dataset with the chosen name is already recorded.")
            return
        
        datasetList = pd.concat([datasetList,pd.DataFrame([datasetRow], columns=["datasetName","datasetFile","datasetColumns","speciesName","genusNeeded","genusName"])], ignore_index=True)
        
    except FileNotFoundError:
        datasetList = pd.DataFrame([datasetRow], columns=["datasetName","datasetFile","datasetColumns","speciesName","genusNeeded","genusName"])    
        
    #if fileType == "excel":
    #    datasetToAddDf = pd.read_excel(datasetFile, header=0)
    #elif fileType == "csv":
    #    datasetToAddDf = pd.read_csv(datasetFile, header=0, encoding='latin1')
        
    #Append dataset name to column names
    datasetColumns = ""
    #for index in range(len(datasetToAddDf.columns)):
    for columnName in datasetToAddDf.columns:
        #print(f"Column: {columnName}")
        datasetToAddDf.rename(columns={columnName: f"{newName}_{columnName}"}, inplace=True)
        
        if datasetColumns == "":
            datasetColumns = f"{newName}_{columnName}"
        else:
            datasetColumns = f"{datasetColumns},{newName}_{columnName}"
            
    datasetList.at[datasetList[datasetList["datasetName"] == newName].index[0],"datasetColumns"] = datasetColumns
        
    #Update speciesColumn name to new name
    speciesColumn = f"{newName}_{speciesColumn}"
    
    datasetToAddDf.insert(len(datasetToAddDf.columns),"SequenceSpecies",[0] * len(datasetToAddDf.index))
    #print(f"Columns: {datasetToAddDf.columns}")
    
    #load file with species that need to be verified
    #verificationFile = "newSpecies.xlsx"
    verificationFile = "data/newSpecies.csv"
    
    try:
        #speciesToVerifyList = pd.read_excel(verificationFile, header=0)
        speciesToVerifyList = pd.read_csv(verificationFile, header=0)
    
    except FileNotFoundError:
        speciesToVerifyList = None
    
    totalNotFound = 0
    speciesToAdd = []
    latestSequence = speciesDf.at[len(speciesDf.index) - 1,"Sequence"]
    
    #Check if species list has a dataset column, if not create it
    if "Dataset" not in speciesDf.columns:
        speciesDf.insert(len(speciesDf.columns),"Dataset",[""] * len(speciesDf.index))
        
    nanMatrix = speciesDf.isna()
    
    #Find sequence ID in species list to add for each species in dataset
    for element in datasetToAddDf.index:
        #(f"Finding data for Struthio molybdophanes: {df[df["Scientific_name"] == "Struthio molybdophanes"]}")
        #Look for species on species list based on Scientific name or protonym
        speciesOfInterestName = datasetToAddDf.at[element,speciesColumn].replace("_"," ")
        
        #If needed add genus to name for scientfic name
        if genusNeeded:
            genusOfInterest = datasetToAddDf.at[element,genusColumn].replace("_"," ")
            speciesOfInterestName = f"{genusOfInterest} {speciesOfInterestName}"
        
        originalName = speciesOfInterestName   
        #Search for name using Pytaxon name
        ptData = pt._incorrect_data
        numEntries = ptData['Wrong Name'].count(speciesOfInterestName)
        #print(f"{speciesOfInterestName} found {numEntries} times")
        
        if numEntries >= 2 and ('(' not in ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)] and ',' not in ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]):
            pytaxonName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
            
            if pytaxonName != '':
                #speciesOfInterestName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
                speciesOfInterestName = pytaxonName
        
        rowsWithElementSpecies = speciesDf[speciesDf["Scientific_name"] == speciesOfInterestName]
        rowsWithElementProtonym = speciesDf[speciesDf["Protonym"] == speciesOfInterestName]
        
        #print(f"Found elements: {rowsWithElementSpecies}")
        
        #Update sequence ID based on where species was found
        if len(rowsWithElementSpecies.index) > 0:
            datasetToAddDf.at[element,"SequenceSpecies"] = rowsWithElementSpecies.at[rowsWithElementSpecies.index[0],"Sequence"]
            
            #Add dataset to species list
            if speciesDf.at[rowsWithElementSpecies.index[0],"Dataset"] == "" or nanMatrix.at[rowsWithElementSpecies.index[0],"Dataset"]:
                speciesDf = speciesDf.fillna("")
                speciesDf.at[rowsWithElementSpecies.index[0],"Dataset"] = newName
            else:
                speciesDf.at[rowsWithElementSpecies.index[0],"Dataset"] = f"{speciesDf.at[rowsWithElementSpecies.index[0],"Dataset"]},{newName}"
            continue
        elif len(rowsWithElementProtonym.index) > 0:
            datasetToAddDf.at[element,"SequenceSpecies"] = rowsWithElementProtonym.at[rowsWithElementProtonym.index[0],"Sequence"]
            
            #Add dataset to species list
            if speciesDf.at[rowsWithElementProtonym.index[0],"Dataset"] == "" or nanMatrix.at[rowsWithElementProtonym.index[0],"Dataset"]:
                speciesDf = speciesDf.fillna("")
                speciesDf.at[rowsWithElementProtonym.index[0],"Dataset"] = newName
            else:
                speciesDf.at[rowsWithElementProtonym.index[0],"Dataset"] = f"{speciesDf.at[rowsWithElementProtonym.index[0],"Dataset"]},{newName}"            
            continue
        
        #Species was not on the list so it needs to be added to list of species to be verified
        print(f"{datasetToAddDf.at[element,speciesColumn]} as {speciesOfInterestName} not found in master list. \n")
        totalNotFound = totalNotFound + 1
        
        if speciesToVerifyList is None:
            latestSequence = latestSequence + 1
            speciesRow = [None for i in range(len(speciesDf.columns) + 1)]
            #speciesRow[0] = latestSequence
            speciesRow[0] = -1
            speciesRow[5] = speciesOfInterestName
            speciesRow[27] = newName
            speciesRow[29] = originalName
            
            speciesToAdd.append(speciesRow)
        
        else:
            rowsVerifySpecies = speciesToVerifyList[speciesToVerifyList["Scientific_name"] == speciesOfInterestName]
            
            if len(rowsVerifySpecies.index) > 0:
                speciesToVerifyList.at[rowsVerifySpecies.index[0],"Dataset"] = f"{speciesToVerifyList.at[rowsVerifySpecies.index[0],"Dataset"]},{newName}"
                
            else:
                latestSequence = latestSequence + 1
                speciesRow = [None for i in range(len(speciesDf.columns) + 1)]
                #speciesRow[0] = latestSequence
                speciesRow[0] = -1
                speciesRow[5] = speciesOfInterestName
                speciesRow[27] = newName
                speciesRow[29] = originalName
                
                speciesToAdd.append(speciesRow)                
        
        #datasetToAddDf.at[element,"SequenceSpecies"] = latestSequence
        datasetToAddDf.at[element,"SequenceSpecies"] = -1
        
    print(f"Total entries not found: {totalNotFound}")
    #print(speciesDf.columns.tolist())
    frameColumns = speciesDf.columns.tolist()
    frameColumns.append("speciesDatasetName")
    #print(frameColumns)
    
    if speciesToVerifyList is not None:
        #speciesToVerifyList = pd.concat([speciesToVerifyList, pd.DataFrame(speciesToAdd, columns=speciesToVerifyList.columns.tolist().append("speciesDatasetName"))], ignore_index=True)
        speciesToVerifyList = pd.concat([speciesToVerifyList, pd.DataFrame(speciesToAdd, columns=frameColumns)], ignore_index=True)
    
    else:
        #speciesToVerifyList = pd.DataFrame(speciesToAdd, columns=speciesDf.columns.tolist().append("speciesDatasetName"))
        speciesToVerifyList = pd.DataFrame(speciesToAdd, columns=frameColumns)
        
    #speciesToVerifyList.to_excel(verificationFile, index=False)
    speciesToVerifyList.to_csv(verificationFile, index=False)
    
    #speciesDf = pd.concat([speciesDf,pd.DataFrame(speciesToAdd, columns=speciesDf.columns)], ignore_index=True)
    #speciesDf.to_excel(speciesFile, index=False)
    speciesDf.to_csv(speciesFile, index=False)
    
    #datasetToAddDf.to_excel("textBeak.xlsx", index=False)
    #datasetToAddDf.to_excel(f"{newName}.xlsx", index=False)
    datasetToAddDf.to_csv(f"data/{newName}.csv", index=False)
    #datasetDf.to_excel("bigDataCompilation.xlsx", index=False)
        
    #datasetList.to_excel("datasetDB.xlsx", index=False)
    datasetList.to_csv("data/datasetDB.csv", index=False)

#def main():
    
    #filesToLoad = [["Beak shape.xlsx","excel","Beak_shape","PhyloName"],
    #               ["Alternative ecological strategies.csv","csv","Alternative_ecological_strategies","Alt_Species"],
    #               ["AVONET Supplementary dataset 1.xlsx","excel","AVONET_Supplementary dataset 1.xlsx","Species1"]]
    #addDataset("Beak shape.xlsx","excel","Beak_shape","PhyloName")
    #addDataset("Alternative ecological strategies.csv","csv","Alternative_ecological_strategies","Alt_Species")
    #addDataset("Beak shape.xlsx","excel","Beak_shape","PhyloName")
    #addDataset("ATLANTIC_BIRD_TRAITS_completed_2025_10_d16TEST.csv","csv","EdTest","Binomial")
    #addDataset("ATLANTIC_BIRD_TRAITS_completed_2025_10_d16TEST2.csv","csv","EdTest","Binomial")
    #addDataset("ATLANTIC_BIRD_TRAITS_completed_2025_10_d16.csv","csv","EdTest","Binomial")
    #addDataset("clutch_dataset1.csv","csv","Clutch_dataset_1","binomial")
#main()
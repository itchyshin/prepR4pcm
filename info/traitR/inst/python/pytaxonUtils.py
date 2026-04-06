import pandas as pd
import pytaxon
from pathlib import Path

def getPytaxonSpeciesName(speciesList):
    speciesForPytaxon = []
    pytaxonNames = []
    
    tempDir = Path("temp")
    tempDir.mkdir(parents=True, exist_ok=True)
    
    for species in speciesList:
        speciesOfInterestName = species
        
        #Get name from Pytaxon ASSUMING 1 SPECIES FOR NOW
        #Create dataframe with species info to create temp file for Pytaxon
        speciesRow = [None for i in range(2)]
        speciesRow[0] = speciesOfInterestName
        speciesRow[1] = speciesOfInterestName
        speciesForPytaxon.append(speciesRow)
    
    #print(f"Number of columns in speciesToAdd {len(speciesToAdd[0])}")
    speciesPytaxonDF = pd.DataFrame(speciesForPytaxon, columns=["species","scientificName"])
        
    speciesPytaxonDF.to_excel("temp/pytaxonTemp.xlsx", index=False)
        
    #Run Pytaxon
    pt = pytaxon.Pytaxon(11)
    pt.read_spreadshet("temp/pytaxonTemp.xlsx")
    pt.read_columns(f"x,x,x,x,x,x,species,scientificName")
    pt.check_species_and_lineage()     
    
    for species in speciesList:
        speciesOfInterestName = species
        
        originalName = speciesOfInterestName
        #Obtain new name to use
        ptData = pt._incorrect_data
        numEntries = ptData['Wrong Name'].count(speciesOfInterestName)
        
        if numEntries >= 2 and ('(' not in ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)] and ',' not in ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]):
            pytaxonName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
            
            if pytaxonName != '':
                #speciesOfInterestName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
                speciesOfInterestName = pytaxonName    
                
        pytaxonNames.append(speciesOfInterestName)
    
    return pytaxonNames

#names = getPytaxonSpeciesName(["Rhea pennata","Accipiter albogularis","Hemispingus auricularis","Guira guira"])
#print(f"{names}")
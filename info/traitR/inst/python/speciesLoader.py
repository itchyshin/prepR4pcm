import pandas as pd

def loadSpecies(filename,column_name,column_category):
    
    species = "species"
    subspecies = "subspecies"
    
    df = pd.read_excel(filename,header=0)
    
    #print(f"Data example: {df.at[1,"Taxon_rank"]}")
    
    #(f"Finding data for Struthio molybdophanes: {df[df["Scientific_name"] == "Struthio molybdophanes"]}")
    
    df.insert(len(df.columns),"Synonyms",[""] * len(df.index))
    
    #Check if species list has a dataset column, if not create it
    if "Dataset" not in df.columns:
        df.insert(len(df.columns),"Dataset",[""] * len(df.index))    
        
    #Check if species list has a tree column, if not create it
    if "Tree" not in df.columns:
        df.insert(len(df.columns),"Tree",[""] * len(df.index))        
    
    total = 0
    
    for element in df.index:
        
        total = element
        df.at[element,"Synonyms"] = df.at[element,"Protonym"]
        
        if df.at[element,column_category] != species and df.at[element,column_category] != subspecies:
            df = df.drop(element)
            #print(f"Dropping {element}")
    
    #df.to_excel("species_subspeciesOnly.xlsx", index=False)
    
    dfMammals = pd.read_csv("mdd.csv", header=0, encoding='latin1') 
    
    mammalSpeciesName = dfMammals['sciName'].tolist()
    mammalSpeciesName = [species.replace("_", " ") for species in mammalSpeciesName]
    
    total = total + 1
    
    for species in mammalSpeciesName:
        #speciesOfInterestName = leaf.name.replace("_"," ")
        total = total + 1
        
        originalName = species
        
        #latestSequence = latestSequence + 1
        speciesRow = [None for i in range(len(df.columns))]
        #speciesRow[0] = latestSequence
        speciesRow[0] = total
        speciesRow[5] = species
            
        #speciesToAdd.append(speciesRow)
        df = pd.concat([df,pd.DataFrame([speciesRow], columns=["Sequence","Taxon_rank","Order","Family","Family_English_name","Scientific_name","Authority","Bibliographic_details","English_name_AviList","English_name_Clements_v2024","English_name_BirdLife_v9","Proposal_number","Decision_summary","Range","Extinct_or_possibly_extinct","IUCN_Red_List_Category","BirdLife_DataZone_URL","Species_code_Cornell_Lab","Birds_of_the_World_URL","AvibaseID","Gender_of_genus","Type_species_of_genus","Type_locality","Title_of_original_description","Original_description_URL","Protonym","Synonyms","Dataset","Tree"])], ignore_index=True)
    
    df.to_csv("data/species_subspeciesOnly.csv", index=False)
    
#def main():
    
    #filename = input("Enter filename of species dataset: ")
    
    #column_name = input("Enter the header name of the column containing the species name: ")
    
    #column_category = input("Enter the header name of the column containing the category, if no column exist enter NONE: ")
    
    #loadSpecies(filename,column_name,column_category)

#loadSpecies("AviList-v2025-11Jun-extended.xlsx","Scientific_name","Taxon_rank")    
#main()
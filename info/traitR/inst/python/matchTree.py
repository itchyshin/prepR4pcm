from Bio import Phylo
import pandas as pd
import pytaxon
from pathlib import Path
import json 
import numpy as np 


def get_species_in_db(species, db):
    sn = db[db['Scientific_name'].isin(species)]['Scientific_name']
    pn = db[db['Protonym'].isin(species)]['Protonym']
    sy = db[db['Synonyms'].str.contains('|'.join(species))]['Synonyms']
    
    indices = []
    indices += sn.index.tolist()
    indices += pn.index.tolist()
    indices += sy.index.tolist()

    species_in_db = []
    species_in_db += sn.values.tolist()
    species_in_db += pn.values.tolist()
    sy_species = sy.apply(lambda x: x.split(',')).explode().tolist()
    sy_species = list(set(sy_species) & set(species))
    species_in_db += sy_species

    return (indices, species_in_db)


def get_pytaxon_names(species):
    """
    Gets pytaxon names.
    
    Args: 
        species (list): list of names to look-up

    Returns: 
        Dataframe of (orig_name, new_name) pairs
        where orig_name are names from species and
        new_name are names from pytaxon.

        Pyxaton returns 9 types of output; 
        this function accepts 2 types (the 'clean' kind):
        1. None [ignore]
        2. Genus species [accept]
        3. Genus species (LastName, Year) [ignore]
        4. Genus species LastName, Year [ignore]
        5. Genus species subspecies [accept]
        6. Genus species subspecies (LastName, Year) [ignore]
        7. Genus species subspecies LastName, Year [ignore]
        8. Genus (LastName, Year) [ignore]
        9. Genus LastName, Year [ignore]
    """

    # Prepare data
    data = [[name, name] for name in species]
    data = pd.DataFrame(data, columns=["species", "scientificName"])
    data.to_csv("temp/pytaxonTemp.csv", index=False)
    
    # Pytaxon look-up
    pt = pytaxon.Pytaxon(11)
    pt.read_spreadshet("temp/pytaxonTemp.csv")
    pt.read_columns(f"x,x,x,x,x,x,species,scientificName")
    pt.check_species_and_lineage()
    
    # Format result    
    df = pd.DataFrame(data = {
        "orig_name": pt._incorrect_data['Wrong Name'], 
        "new_name": pt._incorrect_data['Suggested Name']
    })

    for index in df.index:
        if df.loc[index, 'orig_name'] in df.loc[index, 'new_name']:
            df.loc[index, 'new_name'] = df.loc[index, 'orig_name']
        
        if any(sub in df.loc[index, 'new_name'] for sub in "(,)"):
            df.loc[index, 'new_name'] = ""
    
    df = df[df['new_name'] != '']
    df = df.reset_index(drop=True)
    df.to_csv("temp/pytaxonNames.csv", index=False)

    return df


def append_new_value(current_values, new_value):
    if new_value not in current_values:
        if current_values:
            return f"{current_values},{new_value}"
        else:
            return new_value
    else:
        return current_values


def update_db_trees(db, indices, tree_path):
    print("Updating 'data/species_subspeciesOnly.csv' -> column 'Tree'")

    n = len(indices)
    for i, index in enumerate(indices):
        print(f"Add tree to db {i+1}/{n}: '{db.loc[index, 'Scientific_name']}'")
        db.loc[index, 'Tree'] = append_new_value(db.loc[index, 'Tree'], tree_path)

    print(f"Successfully linked tree to {n} species.")

    return db


def update_db_synonyms(db, indices_pt, pt):
    print("Updating 'data/species_subspeciesOnly.csv' -> column 'Synonyms'")

    new_names = pt['new_name'].tolist()
    n = len(new_names)

    filtered_db = db.loc[indices_pt]

    for i, new_name in enumerate(new_names):
        orig_name = pt[pt['new_name']==new_name]['orig_name'].values[0]
            
        print(f"Add synonym to db {i+1}/{n}: orig='{orig_name}' syn='{new_name}'")

        indices = filtered_db[filtered_db['Scientific_name']==new_name].index.tolist()
        indices += filtered_db[filtered_db['Protonym']==new_name].index.tolist()
        indices += filtered_db[filtered_db['Synonyms'].str.contains(new_name, regex=False)].index.tolist()
        indices = list(set(indices))

        for index in indices:
            db.loc[index, 'Synonyms'] = append_new_value(db.loc[index, 'Synonyms'], orig_name)

    print(f"Successfully added {n} synonyms.")

    return db


def update_verify(verify, species, pt, tree_path):
    print("Updating 'data/newSpecies.csv'")

    # Case 1: species already in verify
    indices, species_in_verify = get_species_in_db(species=species, db=verify)
    verify = update_db_trees(db=verify, indices=indices, tree_path=tree_path)

    # Case 2: species not in verify
    species_not_in_verify = list(set(species) - set(species_in_verify))
    species_to_add = pd.DataFrame(columns=verify.columns)
    
    n = len(species_not_in_verify)
    for i, name in enumerate(species_not_in_verify):
        print(f"Add species to verify {i+1}/{n}: {name}'")

        species_to_add.loc[i, 'Sequence'] = -1
        species_to_add.loc[i, 'Scientific_name'] = name
        species_to_add.loc[i, 'Tree'] = tree_path
        species_to_add.loc[i, 'speciesDatasetName'] = pt[pt['new_name']==name]['orig_name'].values[0]

    verify = pd.concat([verify, species_to_add], ignore_index=True)

    print(f"Successfully added {n} species to the verify list.")

    return verify


def update_tree_registry(tree_registry, species, pt, tree_path):
    def lookup_pt(orig_name):
        pt_name = pt[pt['orig_name']==orig_name]['new_name']
        return pt_name.values[0] if not pt_name.empty else orig_name

    print("Updating 'data/trees.csv'")

    output = pd.DataFrame()
    output['orig_name'] = species
    output['new_name'] = output['orig_name'].apply(lambda x: lookup_pt(x))

    species_included = ",".join(output['orig_name'].tolist())
    species_included_with_original = []

    n = len(species)

    for i, row in output.iterrows():
        orig_name = row['orig_name'].strip().replace(' ', '_')
        new_name = row['new_name']

        print(f"Add species to tree registry {i+1}/{n}: orig='{orig_name}' new='{new_name}'")

        species_included_with_original.append(f"({orig_name};{new_name})")
    species_included_with_original = ",".join(species_included_with_original)

    row_to_add = pd.DataFrame({
        'treeName': [tree_path], 
        'treeLocation': [''], 
        'speciesIncluded': [species_included], 
        'speciesIncludedWithOriginal': [species_included_with_original]
    })
    
    tree_registry = pd.concat([tree_registry, row_to_add], ignore_index=True)    

    print(f"Successfully added {n} species to the tree registry list.")

    return tree_registry


def add_tree(tree_path, tree_format="newick"):
    """
    Adds tree to the database. 

    Args:
        tree_path (str): Path to the tree file 
        tree_format (str): Format of the tree 

    Returns: 
        None
    """
    # Read tree
    tree = Phylo.read(tree_path, tree_format)
    leaves = tree.get_terminals()
    species = [leaf.name.replace("_", " ") for leaf in leaves]

    # Read db
    db_file = "data/species_subspeciesOnly.csv"
    db = pd.read_csv(db_file, header=0)
    db = db.fillna("")

    # Species in db
    indices, species_in_db = get_species_in_db(species, db)
    
    # Species not in db
    species_not_in_db = list(set(species) - set(species_in_db))

    # pytaxon
    pt = get_pytaxon_names(species_not_in_db)
    # pt = get_pytaxon_names(species_not_in_db[:10])
    
    # Species in db (pytaxon)
    new_names = pt['new_name'].tolist()
    indices_pt, species_in_db_pt = get_species_in_db(new_names, db)
    species_in_db = list(set(species_in_db) | set(species_in_db_pt))

    # Species not in db (pytaxon)
    species_not_in_db_pt = list(set(new_names) - set(species_in_db_pt))

    # Update db
    indices = list(set(indices) | set(indices_pt))
    db = update_db_trees(db, indices, tree_path)
    db = update_db_synonyms(db, indices_pt, pt)
    db.to_csv(db_file, index=False)

    # Update verify
    verify_file = "data/newSpecies.csv"
    if Path(verify_file).is_file(): verify = pd.read_csv(verify_file).fillna("")
    else: verify = pd.DataFrame(columns=db.columns.tolist() + ['speciesDatasetName'])
    verify = update_verify(verify, species_not_in_db_pt, pt, tree_path)
    verify.to_csv(verify_file, index=False)

    # Update tree file
    tree_registry_file = "data/trees.csv"
    if Path(tree_registry_file).is_file(): tree_registry = pd.read_csv(tree_registry_file, header=0)
    else: tree_registry = pd.DataFrame(columns=["treeName", "treeLocation", "speciesIncluded", "speciesIncludedWithOriginal"])
    tree_registry = update_tree_registry(tree_registry, species, pt, tree_path)
    tree_registry.to_csv(tree_registry_file, index=False)

    return None


def addTree(treeFilename, treeFormat = "newick"):
    """
    Adds a new tree to the database. 

    Args:
        treeFilename (str): Path to the tree.
        treeFormat (str): Format of the tree: "newick", etc.

    Returns:
        None
    """

    # 1) List of Species 
    speciesFilename = "data/species_subspeciesOnly.csv"
    #speciesDf = pd.read_excel(speciesFilename,header=0)
    speciesDf = pd.read_csv(speciesFilename,header=0)

    # 2) New Tree
    tree = Phylo.read(treeFilename, treeFormat)
    
    # 3) Species "to verify" i.e. not on Pytaxon / List of Species (step 1)
    #verificationFile = "newSpecies.xlsx"
    verificationFile = "data/newSpecies.csv"
    #treeDatasetFile = "trees.xlsx"
    treeDatasetFile = "data/trees.csv"
    
    try:
        #speciesToVerifyList = pd.read_excel(verificationFile, header=0)
        speciesToVerifyList = pd.read_csv(verificationFile, header=0)
    
    except FileNotFoundError:
        speciesToVerifyList = None
    
    # 4) List of Species in the New Tree
    leafs = tree.get_terminals()
    speciesInTreeList = ""
    
    totalNotFound = 0
    speciesToAdd = []    
    
    # 5) Boolean Matrix: Data Missingness
    nanMatrix = speciesDf.isna()
    leafsInTree = 0
    
    # 6) Create a list of [species, species] pairs from the tree's leaves
    speciesForPytaxon = []
    for leaf in leafs:
        speciesOfInterestName = leaf.name.replace("_"," ")
        
        #Get name from Pytaxon ASSUMING 1 SPECIES FOR NOW
        #Create dataframe with species info to create temp file for Pytaxon
        speciesRow = [None for i in range(2)]
        speciesRow[0] = speciesOfInterestName
        speciesRow[1] = speciesOfInterestName
        speciesForPytaxon.append(speciesRow)
    
    # 7) Create a DataFrame with columns: ["species", "scientificName"]
    #print(f"Number of columns in speciesToAdd {len(speciesToAdd[0])}")
    speciesPytaxonDF = pd.DataFrame(speciesForPytaxon, columns=["species","scientificName"])
    
    # 8) Save the DataFrame (step 7) as a temporary Excel file
    speciesPytaxonDF.to_excel("temp/pytaxonTemp.xlsx", index=False)
        
    # 9) Run Pytaxon - source_id=11
    pt = pytaxon.Pytaxon(11)
    pt.read_spreadshet("temp/pytaxonTemp.xlsx") # Reads the spreadsheet (see Step 7)
    pt.read_columns(f"x,x,x,x,x,x,species,scientificName") # Prints "Columns choosed." when successful.
    pt.check_species_and_lineage() # QUESTION: Performs the query
    
    # 10) For all species in the new tree:
    for leaf in leafs:
        speciesOfInterestName = leaf.name.replace("_"," ")
        
        originalName = speciesOfInterestName
        #Obtain new name to use
        ptData = pt._incorrect_data
        numEntries = ptData['Wrong Name'].count(speciesOfInterestName)
        
        if numEntries >= 2 and ('(' not in ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)] and ',' not in ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]):
            pytaxonName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
            
            if pytaxonName != '':
                #speciesOfInterestName = ptData['Suggested Name'][ptData['Wrong Name'].index(speciesOfInterestName)]
                speciesOfInterestName = pytaxonName            
        
        leafsInTree = leafsInTree + 1
        
        #Look for the species name in the master list as species or a protonym or a synonym
        rowsWithElementSpecies = speciesDf[speciesDf["Scientific_name"] == speciesOfInterestName]
        rowsWithElementProtonym = speciesDf[speciesDf["Protonym"] == speciesOfInterestName]
        rowsWithElementSynonym = speciesDf[speciesDf["Synonyms"].str.contains(speciesOfInterestName,na=False)]
        
        #Register tree with the species
        if len(rowsWithElementSpecies.index) > 0:
            #Add dataset to species list
            if speciesDf.at[rowsWithElementSpecies.index[0],"Tree"] == "" or nanMatrix.at[rowsWithElementSpecies.index[0],"Tree"]:
                speciesDf = speciesDf.fillna("")
                speciesDf.at[rowsWithElementSpecies.index[0],"Tree"] = treeFilename
            else:
                speciesDf.at[rowsWithElementSpecies.index[0],"Tree"] = f"""{speciesDf.at[rowsWithElementSpecies.index[0],"Tree"]},{treeFilename}"""
                
            if speciesInTreeList == "":
                speciesInTreeList = speciesOfInterestName
                # EDIT: speciesInTreeListWithOriginal
            else:
                speciesInTreeList = f"{speciesInTreeList},{speciesOfInterestName}"
                # EDIT: speciesInTreeListWithOriginal
                
            continue
        elif len(rowsWithElementProtonym.index) > 0:
            #Add dataset to species list
            if speciesDf.at[rowsWithElementProtonym.index[0],"Tree"] == "" or nanMatrix.at[rowsWithElementProtonym.index[0],"Tree"]:
                speciesDf = speciesDf.fillna("")
                speciesDf.at[rowsWithElementProtonym.index[0],"Tree"] = treeFilename
            else:
                speciesDf.at[rowsWithElementProtonym.index[0],"Tree"] = f"""{speciesDf.at[rowsWithElementProtonym.index[0],"Tree"]},{treeFilename}"""
                
            if speciesInTreeList == "":
                speciesInTreeList = speciesDf.at[rowsWithElementProtonym.index[0],"Scientific_name"]
                # EDIT: speciesInTreeListWithOriginal
            else:
                speciesInTreeList = f"""{speciesInTreeList},{speciesDf.at[rowsWithElementProtonym.index[0],"Scientific_name"]}"""
                # EDIT: speciesInTreeListWithOriginal
                
            continue
        elif len(rowsWithElementSynonym.index) > 0:
            #Add dataset to species list
            if speciesDf.at[rowsWithElementSynonym.index[0],"Tree"] == "" or nanMatrix.at[rowsWithElementSynonym.index[0],"Tree"]:
                speciesDf = speciesDf.fillna("")
                speciesDf.at[rowsWithElementSynonym.index[0],"Tree"] = treeFilename
            else:
                speciesDf.at[rowsWithElementSynonym.index[0],"Tree"] = f"""{speciesDf.at[rowsWithElementSynonym.index[0],"Tree"]},{treeFilename}"""
                
            if speciesInTreeList == "":
                speciesInTreeList = speciesDf.at[rowsWithElementSynonym.index[0],"Scientific_name"]
                # EDIT: speciesInTreeListWithOriginal
            else:
                speciesInTreeList = f"""{speciesInTreeList},{speciesDf.at[rowsWithElementSynonym.index[0],"Scientific_name"]}"""
                # EDIT: speciesInTreeListWithOriginal
                
            continue
        
        #Species no found so need to add it to verification list
        print(f"{speciesOfInterestName} not found in master list. \n")
        totalNotFound = totalNotFound + 1
        
        #print(f"Number of columns in speciesToAdd {len(speciesToAdd[-1])}")
        if speciesToVerifyList is None:
            #latestSequence = latestSequence + 1
            speciesRow = [None for i in range(len(speciesDf.columns) + 1)]
            #speciesRow[0] = latestSequence
            speciesRow[0] = -1
            speciesRow[5] = speciesOfInterestName
            speciesRow[28] = treeFilename
            speciesRow[29] = originalName
            
            speciesToAdd.append(speciesRow)
        
        else:
            nanMatrixVerifySpecies = speciesToVerifyList.isna()
            rowsVerifySpecies = speciesToVerifyList[speciesToVerifyList["Scientific_name"] == speciesOfInterestName]
            
            if len(rowsVerifySpecies.index) > 0:
                if speciesToVerifyList.at[rowsVerifySpecies.index[0],"Tree"] == '' or nanMatrixVerifySpecies.at[rowsVerifySpecies.index[0],"Tree"]:
                    speciesToVerifyList = speciesToVerifyList.fillna("")
                    speciesToVerifyList.at[rowsVerifySpecies.index[0],"Tree"] = treeFilename
                else:
                    speciesToVerifyList.at[rowsVerifySpecies.index[0],"Tree"] = f"""{speciesToVerifyList.at[rowsVerifySpecies.index[0],"Tree"]},{treeFilename}"""
                
            else:
                #latestSequence = latestSequence + 1
                speciesRow = [None for i in range(len(speciesDf.columns) + 1)]
                #speciesRow[0] = latestSequence
                speciesRow[0] = -1
                speciesRow[5] = speciesOfInterestName
                speciesRow[28] = treeFilename
                speciesRow[29] = originalName
                
                speciesToAdd.append(speciesRow)
                
    print(f"Total species not found: {totalNotFound}")
    print(f"Total species in tree: {leafsInTree}")
    #Add tree to tree dataset
    treeRow = [treeFilename,"",speciesInTreeList]
    
    # EDIT: treeRow = [treeFilename,"",speciesInTreeList, speciesInTreeListWithOriginal]

    try:
        #treesList = pd.read_excel(treeDatasetFile, header=0)
        treesList = pd.read_csv(treeDatasetFile, header=0)
        
        treesList = pd.concat([treesList,pd.DataFrame([treeRow], columns=["treeName","treeLocation","speciesIncluded"])], ignore_index=True)
        # EDIT: speciesIncludedWithOriginal
        
    except FileNotFoundError:
        treesList = pd.DataFrame([treeRow], columns=["treeName","treeLocation","speciesIncluded"])    
        # EDIT: speciesIncludedWithOriginal
        
    #treesList.to_excel(treeDatasetFile, index=False)
    treesList.to_csv(treeDatasetFile, index=False)
    
    frameColumns = speciesDf.columns.tolist()
    frameColumns.append("speciesDatasetName")    
    
    #print(f"Number of columns in speciesToAdd {len(speciesToAdd[-1])}")
    if speciesToVerifyList is not None:
        speciesToVerifyList = pd.concat([speciesToVerifyList, pd.DataFrame(speciesToAdd, columns=speciesToVerifyList.columns)], ignore_index=True)
    
    else:
        speciesToVerifyList = pd.DataFrame(speciesToAdd, columns=frameColumns)
        
    #speciesToVerifyList.to_excel(verificationFile, index=False)
    speciesToVerifyList.to_csv(verificationFile, index=False)
    
    #speciesDf.to_excel(speciesFilename, index=False)
    speciesDf.to_csv(speciesFilename, index=False)

#def main():
    #addTree("BBtreeC2022.tre","species_subspeciesOnly.csv")
    #addTree("new_tree.nwk","species_subspeciesOnly.csv")
        
#tree = Phylo.read("BBtreeC2022.tre","newick")
#print(tree)
#terminals = tree.get_terminals()
#print("Rhea_americana" == terminals[1].name)
#test = terminals[1].name
#test = test + "a"
#print(test)
#main()
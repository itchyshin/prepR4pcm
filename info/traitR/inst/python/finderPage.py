import streamlit as st
import pandas as pd
import io
from openpyxl import load_workbook
from difflib import get_close_matches
import os
import re
#from datasetLoader import addDataset
from obtainData import obtainData

# --- UI ---
st.title("🦜 Finder")

# --- Form ---
with st.form("speciesForm"):
    speciesInput = st.text_input("Enter species name:", "")
    submitSpecies = st.form_submit_button("Enter species")
    
    if submitSpecies:
        st.session_state["speciesSelected"] = speciesInput
        
with st.form("datasetForm"):
    
    #Load species file
    speciesFile = "species_subspeciesOnly.xlsx"
    
    speciesList = pd.read_excel(speciesFile,header=0)
    
    #Look for species in species list and obtain datasets the species is located
    rowsWithSpecies = speciesList[speciesList["Scientific_name"] == st.session_state.get("speciesSelected", "")]
    
    if len(rowsWithSpecies.index) > 0:
        #print(f"Species {speciesName} not found.")
    
        speciesDatasets = speciesList.at[rowsWithSpecies.index[0],"Dataset"]
        #print(f"Species found in datasets: {speciesDatasets}")
        selectedDatasets = st.multiselect(
            "Select datasets to include:",
            options=speciesDatasets.split(","),
            key="selectedDatasets"
        )
            
        submitDatasets = st.form_submit_button("Enter datasets")
    
        if submitDatasets:
            st.session_state["datasetsSelected"] = selectedDatasets
    else:
        st.write("No valid species has been selected.")
        
with st.form("ColumnForm"):
    #Look for columnns for each dataset and print them
    datasetFile = "datasetDB.xlsx"
    datasetList = pd.read_excel(datasetFile, header=0)
    #datasetListToSearch = datasetsToSearch.split(",")
    
    datasetListToSearch = st.session_state.get("datasetsSelected", "")
    
    print(f"This is what the dataset list looks like: {datasetListToSearch}")
    
    for datasetName in datasetListToSearch:
        datasetRows = datasetList[datasetList["datasetName"] == datasetName]
        st.write(f"Dataset: {datasetName} has columns: {datasetList.at[datasetRows.index[0],"datasetColumns"]}")
    
    #columnsToSearch = input("Please input columns you wish to use (format: column1,column2): ")
    columnsToSearch = st.text_input("Please input columns you wish to use (format: column1,column2): ", "")
    
    #obtainData("Rhea pennata",["Sequence","Scientific_name","Beak_shape_Mass","Beak_shape_width","Beak_shape_depth"],["Beak_shape"])    
    
    submitted = st.form_submit_button("Search and Download Excel")

# --- On Submit ---
if submitted:
    
    obtainData(st.session_state.get("speciesSelected",""),columnsToSearch.split(","),st.session_state.get("datasetsSelected",""))
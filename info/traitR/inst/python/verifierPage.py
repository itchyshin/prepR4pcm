import streamlit as st
import pandas as pd
from speciesVerifier import verifySpecies

# --- UI ---
st.title("🦜 Verifier")

with st.form("verifyForm"):
    #Load species that need verification
    speciesVerifyFile = "newSpecies.xlsx"
    
    speciesVerifyList = pd.read_excel(speciesVerifyFile,header=0)    
    
    selectedSpeciesVerify = st.selectbox(
        "Select species to verify:",
        options=speciesVerifyList["Scientific_name"],
        key="selectedSpeciesVerify"
    )
    
    #Load species file
    speciesFile = "species_subspeciesOnly.xlsx"
    
    speciesList = pd.read_excel(speciesFile,header=0)    
    
    selectedSpecies = st.selectbox(
        "Select species:",
        options=speciesList["Scientific_name"],
        key="selectedSpecies"
    )
    
    submitSpecies = st.form_submit_button("Verify species")
    
    if submitSpecies:
        verifySpecies(selectedSpeciesVerify,selectedSpecies)
        st.rerun()
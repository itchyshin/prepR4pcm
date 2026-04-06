import streamlit as st
import pandas as pd
from matchTree import addTree

# --- UI ---
st.title("🦜 Add Tree")

with st.form("addTreeForm"):
    #Tree to be uploaded
    uploaded_tree = st.file_uploader("Choose a tree to upload")
    treeType = st.text_input("Enter type of file:","")
    
    submitTree = st.form_submit_button("Add tree")
    
    if submitTree and uploaded_tree is not None:
        addTree(uploaded_tree,"species_subspeciesOnly.xlsx",treeType)
        st.rerun()
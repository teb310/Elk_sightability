# SightabilityModels
Elk sightability factor - South Coast Region

This folder holds the information you'll need to run a Sightability Model on annual elk survey data. First, read the "How to Run Sightability Model...docx" to understand what data you need and how to format it.

Example input data and associated outputs are available in their respective folders. The scripts were based on Feiberg (2013) data and scripts, which are available for reference in the 'Feiberg' folder.

Please be sure to copy this folder onto your C:/ drive before attempting to work with the data and scripts.

### Differences between this repo and SightabilityModel repo
  1. For the app, you need to include an "EPU_list" sheet in your excel input file. In SightabilityModel, the EPU list is created within the name_fixer function.
  2. The app runs the Bayesian model only, while the SightabilityModel repo stores scripts for both the Bayesian model and the modified Horvitz-Thompson (mHT) model.
  3. The app will throw customized warnings/errors when common issues occur. SightabilityModel scripts will either throw generic warnings/errors or problems will manifest while running the model or in the output (i.e., a fuller understanding of R is recommended when opting for the SightabilityModel scripts).
  4. The app stratifies the data by sex/age class while the SightabilityModel scripts do not. 

Please email teb310@gmail.com if you have questions about this repo.

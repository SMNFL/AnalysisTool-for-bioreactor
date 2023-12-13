# AnalysisTool-for-bioreactor

Intruduction for console application of AdrianasAnalysisTool (Drag and drop the folder on Desktop)

You have to change <currentUser> to your username

Also you have to install .NET 8.0 from 
https://dotnet.microsoft.com/en-us/download/dotnet/8.0

paste data table in the AndianasAnalysisTool "InsertTableHere" folder
	/Users/<currentUser>/Desktop/AdrianaAnalysisTool/InsertTableHere

(there is also a output folder for the data analysis tables):
	/Users/<currentUser>/Desktop/AdrianaAnalysisTool/Output



Step by Step introduction:

open terminal on IOS (or Windows console on Windows) and paste the following text and press enter:

	cd /Users/<currentUser>/Desktop/AdrianaAnalysisTool/Analysis/src/App

	e.g. cd /Users/simon/Desktop/AdrianaAnalysisTool/Analysis/src/App


Next paste the following text and press enter again:
	dotnet run


Now the application runs and you can read the Introduction to use it!


TIPS!:
Give the Input like:
	"202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 3" 
Or 
	"202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 all"


[
	filename (filename of the .txt data table needs all 8 columns for Light- ,OD- and pumpdata),
	upper OD Threshold (needs to be a number with dot e.g. 3.0),
	lower OD Threshold (needs to be a number with dot e.g. 3.0),
	cylinder (choose "1" - "8" as single number or "all" as word)
]



If it fails and you don't know why please contact foelling@rhrk.de









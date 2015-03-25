# Useful funcitons for data analysis of NetLogo
# models in directory 
#"/Users/Santi/Desktop/ABM/RNetlogoResources"


#SEEDING FUNCTIONS 
#Can only be run once a NetLogo object is instatiated via NLStart

# getSeeds(NumSeeds, )
# generates and returns vector of NumSeeds number of seeds for 
# NetLogo random number generator
getSeeds <- function(NumSeeds, FileName="seeds.rda", NewSeeds = FALSE)
{
	#set filename in directory
	FileInDir <- paste(dirname(model.path), FileName, sep = "/")
	#if new seeds is TRUE (default) generates new NumSeeds
	#and saves to disc in model.path
	if (isTRUE(NewSeeds))
	{
		#generate new seeds
		Seeds<-vector(length = NumSeeds)
		for(i in 1:length(Seeds))
		{ 
			Seeds[i]<-NLReport("new-seed")
			
		}

		#save new seeds in directory of model.path
		save(Seeds, file = FileInDir)
		#return vector of seeds
		return(Seeds)
	}
	#load new seeds into R under the vector name seeds
	#load(paste(dirname(model.path), FileName, sep = "/"))
	else
	{
		#if file exists
		if (isTRUE(file.exists(FileInDir)))
		{
			load(file = FileInDir)
			return(Seeds)
		}
		else 
		{
			print("ERROR SEED FILE DOESN'T EXIST")	
		}

	}
}

# saveSeeds(Seeds, path)
# saves Seeds vector to a rda file to the path
# default path is model.path
saveSeeds <- function(Seeds, FileName="seeds.rda")
{
	FileInDir <- paste(dirname(model.path), FileName, sep = "/")
	save(Seeds, file = FileInDir)
}

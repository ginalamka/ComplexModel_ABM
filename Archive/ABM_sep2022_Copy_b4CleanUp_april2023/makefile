# Makefile
# GFL edits 5/25/2022 

# Original created:
# Date: 4/2/2015 - Created by Mark Christie
# set paths, directories, and filenames

# JW update:14 Oct 2015; AH update for Easley: 24 May 2022
# GL update for 20 directories: 8 June 2022

## Set project name
USER = glamka
PROJ = run_a

## Set dir names
HOMEDIR = /home/gfl0003/ABM_sep2022
SCRATCHDIR = /scratch/${USER}/${PROJ}
OUTDIR = /home/gfl0003/datahold

## Each directory will only contain the scripts used to process that particular
## set of model replicates
DIRECTORY1 = 01
DIRECTORY2 = 02
DIRECTORY3 = 03
DIRECTORY4 = 04
DIRECTORY5 = 05
DIRECTORY6 = 06
DIRECTORY7 = 07
DIRECTORY8 = 08
DIRECTORY9 = 09
DIRECTORY10 = 10
DIRECTORY11 = 11
DIRECTORY12 = 12
DIRECTORY13 = 13
DIRECTORY14 = 14
DIRECTORY15 = 15
DIRECTORY16 = 16
DIRECTORY17 = 17
DIRECTORY18 = 18
DIRECTORY19 = 19
DIRECTORY20 = 20

## Script
FILE1 = run_R_model.sh
FILE2 = Cover.R
FILE3 = FunctionSourcer.R
FILE4 = "parameters_${PROJ}_01.csv" #note doesn't change, so only need one

FILE4.1.1 = "summary_${PROJ}_01.csv"
FILE4.1.2 = "summary_${PROJ}_02.csv"
FILE4.1.3 = "summary_${PROJ}_03.csv"
FILE4.1.4 = "summary_${PROJ}_04.csv"
FILE4.1.5 = "summary_${PROJ}_05.csv"
FILE4.1.6 = "summary_${PROJ}_06.csv"
FILE4.1.7 = "summary_${PROJ}_07.csv"
FILE4.1.8 = "summary_${PROJ}_08.csv"
FILE4.1.9 = "summary_${PROJ}_09.csv"
FILE4.1.10 = "summary_${PROJ}_10.csv"
FILE4.1.11 = "summary_${PROJ}_11.csv"
FILE4.1.12 = "summary_${PROJ}_12.csv"
FILE4.1.13 = "summary_${PROJ}_13.csv"
FILE4.1.14 = "summary_${PROJ}_14.csv"
FILE4.1.15 = "summary_${PROJ}_15.csv"
FILE4.1.16 = "summary_${PROJ}_16.csv"
FILE4.1.17 = "summary_${PROJ}_17.csv"
FILE4.1.18 = "summary_${PROJ}_18.csv"
FILE4.1.19 = "summary_${PROJ}_19.csv"
FILE4.1.20 = "summary_${PROJ}_20.csv"

FILE4.2.1 = "dead.csv"  #"POP_${PROJ}_01.csv"
FILE4.2.2 = "dead.csv"  #"POP_${PROJ}_02.csv"
FILE4.2.3 = "dead.csv"  #"POP_${PROJ}_03.csv"
FILE4.2.4 = "dead.csv"  #"POP_${PROJ}_04.csv"
FILE4.2.5 = "dead.csv"  #"POP_${PROJ}_05.csv"
FILE4.2.6 = "dead.csv"  #"POP_${PROJ}_06.csv"
FILE4.2.7 = "dead.csv"  #"POP_${PROJ}_07.csv"
FILE4.2.8 = "dead.csv"  #"POP_${PROJ}_08.csv"
FILE4.2.9 = "dead.csv"  #"POP_${PROJ}_09.csv"
FILE4.2.10 = "dead.csv"  #"POP_${PROJ}_10.csv"
FILE4.2.11 = "dead.csv"  #"POP_${PROJ}_11.csv"
FILE4.2.12 = "dead.csv"  #"POP_${PROJ}_12.csv"
FILE4.2.13 = "dead.csv"  #"POP_${PROJ}_13.csv"
FILE4.2.14 = "dead.csv"  #"POP_${PROJ}_14.csv"
FILE4.2.15 = "dead.csv"  #"POP_${PROJ}_15.csv"
FILE4.2.16 = "dead.csv"  #"POP_${PROJ}_16.csv"
FILE4.2.17 = "dead.csv"  #"POP_${PROJ}_17.csv"
FILE4.2.18 = "dead.csv"  #"POP_${PROJ}_18.csv"
FILE4.2.19 = "dead.csv"  #"POP_${PROJ}_19.csv"
FILE4.2.20 = "dead.csv"  #"POP_${PROJ}_20.csv"

FILE4.3.1 = "rep_summary_${PROJ}_01.csv"
FILE4.3.2 = "rep_summary_${PROJ}_02.csv"
FILE4.3.3 = "rep_summary_${PROJ}_03.csv"
FILE4.3.4 = "rep_summary_${PROJ}_04.csv"
FILE4.3.5 = "rep_summary_${PROJ}_05.csv"
FILE4.3.6 = "rep_summary_${PROJ}_06.csv"
FILE4.3.7 = "rep_summary_${PROJ}_07.csv"
FILE4.3.8 = "rep_summary_${PROJ}_08.csv"
FILE4.3.9 = "rep_summary_${PROJ}_09.csv"
FILE4.3.10 = "rep_summary_${PROJ}_10.csv"
FILE4.3.11 = "rep_summary_${PROJ}_11.csv"
FILE4.3.12 = "rep_summary_${PROJ}_12.csv"
FILE4.3.13 = "rep_summary_${PROJ}_13.csv"
FILE4.3.14 = "rep_summary_${PROJ}_14.csv"
FILE4.3.15 = "rep_summary_${PROJ}_15.csv"
FILE4.3.16 = "rep_summary_${PROJ}_16.csv"
FILE4.3.17 = "rep_summary_${PROJ}_17.csv"
FILE4.3.18 = "rep_summary_${PROJ}_18.csv"
FILE4.3.19 = "rep_summary_${PROJ}_19.csv"
FILE4.3.20 = "rep_summary_${PROJ}_20.csv"

all:
	@echo "Please type....."
	@echo "    make directories"
	@echo "    make scripts"
	@echo "    make setgroups"
	@echo "    make submit"
	@echo "    make filemerge"
	@echo "    make output"
	
directories:
	@echo "Here we go directories"
	mkdir $(SCRATCHDIR)
	mkdir $(SCRATCHDIR)/$(DIRECTORY1)
	mkdir $(SCRATCHDIR)/$(DIRECTORY2)
	mkdir $(SCRATCHDIR)/$(DIRECTORY3)
	mkdir $(SCRATCHDIR)/$(DIRECTORY4)
	mkdir $(SCRATCHDIR)/$(DIRECTORY5)
	mkdir $(SCRATCHDIR)/$(DIRECTORY6)
	mkdir $(SCRATCHDIR)/$(DIRECTORY7)
	mkdir $(SCRATCHDIR)/$(DIRECTORY8)
	mkdir $(SCRATCHDIR)/$(DIRECTORY9)
	mkdir $(SCRATCHDIR)/$(DIRECTORY10)
	mkdir $(SCRATCHDIR)/$(DIRECTORY11)
	mkdir $(SCRATCHDIR)/$(DIRECTORY12)
	mkdir $(SCRATCHDIR)/$(DIRECTORY13)
	mkdir $(SCRATCHDIR)/$(DIRECTORY14)
	mkdir $(SCRATCHDIR)/$(DIRECTORY15)
	mkdir $(SCRATCHDIR)/$(DIRECTORY16)
	mkdir $(SCRATCHDIR)/$(DIRECTORY17)
	mkdir $(SCRATCHDIR)/$(DIRECTORY18)
	mkdir $(SCRATCHDIR)/$(DIRECTORY19)
	mkdir $(SCRATCHDIR)/$(DIRECTORY20)

	
scripts:
	@echo "Here we go write scripts"	
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY1)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY2)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY3)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY4)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY5)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY6)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY7)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY8)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY9)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY10)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY11)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY12)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY13)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY14)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY15)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY16)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY17)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY18)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY19)/
	cp -r $(HOMEDIR)/* $(SCRATCHDIR)/$(DIRECTORY20)/


setgroups:
	@echo "Here we go set groups"
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY1)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY2)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY3)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY4)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY5)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY6)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY7)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY8)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY9)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY10)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY11)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY12)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY13)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY14)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY15)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY16)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY17)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY18)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY19)/$(FILE2)
	sed -i 's/_proj_/$(PROJ)/g' $(SCRATCHDIR)/$(DIRECTORY20)/$(FILE2)

	sed -i 's/_group_/$(DIRECTORY1)/g' $(SCRATCHDIR)/$(DIRECTORY1)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY2)/g' $(SCRATCHDIR)/$(DIRECTORY2)/Source/$(FILE3)	
	sed -i 's/_group_/$(DIRECTORY3)/g' $(SCRATCHDIR)/$(DIRECTORY3)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY4)/g' $(SCRATCHDIR)/$(DIRECTORY4)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY5)/g' $(SCRATCHDIR)/$(DIRECTORY5)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY6)/g' $(SCRATCHDIR)/$(DIRECTORY6)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY7)/g' $(SCRATCHDIR)/$(DIRECTORY7)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY8)/g' $(SCRATCHDIR)/$(DIRECTORY8)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY9)/g' $(SCRATCHDIR)/$(DIRECTORY9)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY10)/g' $(SCRATCHDIR)/$(DIRECTORY10)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY11)/g' $(SCRATCHDIR)/$(DIRECTORY11)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY12)/g' $(SCRATCHDIR)/$(DIRECTORY12)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY13)/g' $(SCRATCHDIR)/$(DIRECTORY13)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY14)/g' $(SCRATCHDIR)/$(DIRECTORY14)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY15)/g' $(SCRATCHDIR)/$(DIRECTORY15)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY16)/g' $(SCRATCHDIR)/$(DIRECTORY16)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY17)/g' $(SCRATCHDIR)/$(DIRECTORY17)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY18)/g' $(SCRATCHDIR)/$(DIRECTORY18)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY19)/g' $(SCRATCHDIR)/$(DIRECTORY19)/Source/$(FILE3)
	sed -i 's/_group_/$(DIRECTORY20)/g' $(SCRATCHDIR)/$(DIRECTORY20)/Source/$(FILE3)
	
	sed -i 's/_group_/$(DIRECTORY1)/g' $(SCRATCHDIR)/$(DIRECTORY1)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY2)/g' $(SCRATCHDIR)/$(DIRECTORY2)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY3)/g' $(SCRATCHDIR)/$(DIRECTORY3)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY4)/g' $(SCRATCHDIR)/$(DIRECTORY4)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY5)/g' $(SCRATCHDIR)/$(DIRECTORY5)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY6)/g' $(SCRATCHDIR)/$(DIRECTORY6)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY7)/g' $(SCRATCHDIR)/$(DIRECTORY7)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY8)/g' $(SCRATCHDIR)/$(DIRECTORY8)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY9)/g' $(SCRATCHDIR)/$(DIRECTORY9)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY10)/g' $(SCRATCHDIR)/$(DIRECTORY10)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY11)/g' $(SCRATCHDIR)/$(DIRECTORY11)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY12)/g' $(SCRATCHDIR)/$(DIRECTORY12)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY13)/g' $(SCRATCHDIR)/$(DIRECTORY13)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY14)/g' $(SCRATCHDIR)/$(DIRECTORY14)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY15)/g' $(SCRATCHDIR)/$(DIRECTORY15)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY16)/g' $(SCRATCHDIR)/$(DIRECTORY16)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY17)/g' $(SCRATCHDIR)/$(DIRECTORY17)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY18)/g' $(SCRATCHDIR)/$(DIRECTORY18)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY19)/g' $(SCRATCHDIR)/$(DIRECTORY19)/$(FILE2)
	sed -i 's/_group_/$(DIRECTORY20)/g' $(SCRATCHDIR)/$(DIRECTORY20)/$(FILE2)

	sed -i 's/_group_/$(DIRECTORY1)/g' $(SCRATCHDIR)/$(DIRECTORY1)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY2)/g' $(SCRATCHDIR)/$(DIRECTORY2)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY3)/g' $(SCRATCHDIR)/$(DIRECTORY3)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY4)/g' $(SCRATCHDIR)/$(DIRECTORY4)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY5)/g' $(SCRATCHDIR)/$(DIRECTORY5)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY6)/g' $(SCRATCHDIR)/$(DIRECTORY6)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY7)/g' $(SCRATCHDIR)/$(DIRECTORY7)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY8)/g' $(SCRATCHDIR)/$(DIRECTORY8)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY9)/g' $(SCRATCHDIR)/$(DIRECTORY9)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY10)/g' $(SCRATCHDIR)/$(DIRECTORY10)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY11)/g' $(SCRATCHDIR)/$(DIRECTORY11)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY12)/g' $(SCRATCHDIR)/$(DIRECTORY12)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY13)/g' $(SCRATCHDIR)/$(DIRECTORY13)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY14)/g' $(SCRATCHDIR)/$(DIRECTORY14)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY15)/g' $(SCRATCHDIR)/$(DIRECTORY15)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY16)/g' $(SCRATCHDIR)/$(DIRECTORY16)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY17)/g' $(SCRATCHDIR)/$(DIRECTORY17)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY18)/g' $(SCRATCHDIR)/$(DIRECTORY18)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY19)/g' $(SCRATCHDIR)/$(DIRECTORY19)/$(FILE1)
	sed -i 's/_group_/$(DIRECTORY20)/g' $(SCRATCHDIR)/$(DIRECTORY20)/$(FILE1)

	
submit:
	@echo "Here we go prep submit"
	chmod +x $(SCRATCHDIR)/$(DIRECTORY1)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY2)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY3)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY4)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY5)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY6)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY7)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY8)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY9)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY10)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY11)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY12)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY13)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY14)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY15)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY16)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY17)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY18)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY19)/$(FILE1)
	chmod +x $(SCRATCHDIR)/$(DIRECTORY20)/$(FILE1)

	sbatch $(SCRATCHDIR)/$(DIRECTORY1)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY2)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY3)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY4)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY5)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY6)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY7)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY8)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY9)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY10)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY11)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY12)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY13)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY14)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY15)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY16)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY17)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY18)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY19)/$(FILE1)
	sbatch $(SCRATCHDIR)/$(DIRECTORY20)/$(FILE1)


filemerge:
	@echo "Here we go filemerge"
	#cd $(OUTDIR)
	#mkdir $(PROJ)
	#cd $(SCRATCHDIR)
	cat $(SCRATCHDIR)/$(DIRECTORY1)/Output/$(FILE4.1.1) $(SCRATCHDIR)/$(DIRECTORY2)/Output/$(FILE4.1.2) $(SCRATCHDIR)/$(DIRECTORY3)/Output/$(FILE4.1.3) $(SCRATCHDIR)/$(DIRECTORY4)/Output/$(FILE4.1.4) $(SCRATCHDIR)/$(DIRECTORY5)/Output/$(FILE4.1.5) $(SCRATCHDIR)/$(DIRECTORY6)/Output/$(FILE4.1.6) $(SCRATCHDIR)/$(DIRECTORY7)/Output/$(FILE4.1.7) $(SCRATCHDIR)/$(DIRECTORY8)/Output/$(FILE4.1.8) $(SCRATCHDIR)/$(DIRECTORY9)/Output/$(FILE4.1.9) $(SCRATCHDIR)/$(DIRECTORY10)/Output/$(FILE4.1.10) $(SCRATCHDIR)/$(DIRECTORY11)/Output/$(FILE4.1.11) $(SCRATCHDIR)/$(DIRECTORY12)/Output/$(FILE4.1.12) $(SCRATCHDIR)/$(DIRECTORY13)/Output/$(FILE4.1.13) $(SCRATCHDIR)/$(DIRECTORY14)/Output/$(FILE4.1.14) $(SCRATCHDIR)/$(DIRECTORY15)/Output/$(FILE4.1.15) $(SCRATCHDIR)/$(DIRECTORY16)/Output/$(FILE4.1.16) $(SCRATCHDIR)/$(DIRECTORY17)/Output/$(FILE4.1.17) $(SCRATCHDIR)/$(DIRECTORY18)/Output/$(FILE4.1.18) $(SCRATCHDIR)/$(DIRECTORY19)/Output/$(FILE4.1.19) $(SCRATCHDIR)/$(DIRECTORY20)/Output/$(FILE4.1.20) > $(OUTDIR)/$(PROJ)_all_summary.csv
	cat $(SCRATCHDIR)/$(DIRECTORY1)/Output/$(FILE4.2.1) $(SCRATCHDIR)/$(DIRECTORY2)/Output/$(FILE4.2.2) $(SCRATCHDIR)/$(DIRECTORY3)/Output/$(FILE4.2.3) $(SCRATCHDIR)/$(DIRECTORY4)/Output/$(FILE4.1.4) $(SCRATCHDIR)/$(DIRECTORY5)/Output/$(FILE4.2.5) $(SCRATCHDIR)/$(DIRECTORY6)/Output/$(FILE4.2.6) $(SCRATCHDIR)/$(DIRECTORY7)/Output/$(FILE4.2.7) $(SCRATCHDIR)/$(DIRECTORY8)/Output/$(FILE4.2.8) $(SCRATCHDIR)/$(DIRECTORY9)/Output/$(FILE4.2.9) $(SCRATCHDIR)/$(DIRECTORY10)/Output/$(FILE4.2.10) $(SCRATCHDIR)/$(DIRECTORY11)/Output/$(FILE4.2.11) $(SCRATCHDIR)/$(DIRECTORY12)/Output/$(FILE4.2.12) $(SCRATCHDIR)/$(DIRECTORY13)/Output/$(FILE4.2.13) $(SCRATCHDIR)/$(DIRECTORY14)/Output/$(FILE4.2.14) $(SCRATCHDIR)/$(DIRECTORY15)/Output/$(FILE4.2.15) $(SCRATCHDIR)/$(DIRECTORY16)/Output/$(FILE4.2.16) $(SCRATCHDIR)/$(DIRECTORY17)/Output/$(FILE4.2.17) $(SCRATCHDIR)/$(DIRECTORY18)/Output/$(FILE4.2.18) $(SCRATCHDIR)/$(DIRECTORY19)/Output/$(FILE4.2.19) $(SCRATCHDIR)/$(DIRECTORY20)/Output/$(FILE4.2.20) > $(OUTDIR)/$(PROJ)_all_dead.csv
	cat $(SCRATCHDIR)/$(DIRECTORY1)/Output/$(FILE4.3.1) $(SCRATCHDIR)/$(DIRECTORY2)/Output/$(FILE4.3.2) $(SCRATCHDIR)/$(DIRECTORY3)/Output/$(FILE4.3.3) $(SCRATCHDIR)/$(DIRECTORY4)/Output/$(FILE4.3.4) $(SCRATCHDIR)/$(DIRECTORY5)/Output/$(FILE4.3.5) $(SCRATCHDIR)/$(DIRECTORY6)/Output/$(FILE4.3.6) $(SCRATCHDIR)/$(DIRECTORY7)/Output/$(FILE4.3.7) $(SCRATCHDIR)/$(DIRECTORY8)/Output/$(FILE4.3.8) $(SCRATCHDIR)/$(DIRECTORY9)/Output/$(FILE4.3.9) $(SCRATCHDIR)/$(DIRECTORY10)/Output/$(FILE4.3.10) $(SCRATCHDIR)/$(DIRECTORY11)/Output/$(FILE4.3.11) $(SCRATCHDIR)/$(DIRECTORY12)/Output/$(FILE4.3.12) $(SCRATCHDIR)/$(DIRECTORY13)/Output/$(FILE4.3.13) $(SCRATCHDIR)/$(DIRECTORY14)/Output/$(FILE4.3.14) $(SCRATCHDIR)/$(DIRECTORY15)/Output/$(FILE4.3.15) $(SCRATCHDIR)/$(DIRECTORY16)/Output/$(FILE4.3.16) $(SCRATCHDIR)/$(DIRECTORY17)/Output/$(FILE4.3.17) $(SCRATCHDIR)/$(DIRECTORY18)/Output/$(FILE4.3.18) $(SCRATCHDIR)/$(DIRECTORY19)/Output/$(FILE4.3.19) $(SCRATCHDIR)/$(DIRECTORY20)/Output/$(FILE4.3.20) > $(OUTDIR)/$(PROJ)_all_repsuc.csv
	cp $(SCRATCHDIR)/$(DIRECTORY1)/Output/$(FILE4) $(OUTDIR)/$(PROJ)_parameters.csv #only copied once cuz same for all runs

	@echo "Don't forget to remove headers in the merged files!"

#output:
#	@echo "Here we go output"
#	cp -r $(SCRATCHDIR)/ ./temporary
#	cp -r $(SCRATCHDIR)/makefile ./temporary
#	cp -r $(SCRATCHDIR)/all_summary.csv ./temporary/all_summary.csv
#	cp -r $(SCRATCHDIR)/all_pop.csv ./temporary/all_pop.csv
#	mv temporary output`date +%m.%d.%y_%H.%M`

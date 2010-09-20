CREATION DATE: 16 Sept. 2005
MODIFICATION DATE: 28 Jul. 2010


COMPILATION ON vayu
-------------------
module load intel-fc
module load nag
make -f ICA.mak


PROBLEM WITH THIS VERSION
-------------------------
 1. no ica.log file is created

LIST OF CORRECTIONS to the original code TO COMPILE UNDER LINUX
---------------------------------------------------------------

ICA NAG mark 22
 1. include command were case sensitive, I had to change them to the correct case
 2. replaced E04FDF (01 Dec. 2004), G05EAF and G05EZF.

USAGE
-----

ON LINUX 
--------

 - make nag library available
   module load nag

	from the directory where ICA was compiled
	-----------------------------------------

	ICA.exe < Data/ICAinputDataSettings.txt
	ICA.exe < Data/Simulated/1/ICAinputDataSettings.txt


	Using another data directory (because the name of input files might be too long for ICA to cope with)
	----------------------------

	- Add the location of ICA.exe into you PATH
	  (with tcsh) set path = ( $path /home/599/mak599/Fortran/ICA/ICA_NAG_Mark22 )

	- Go to a directory containing ICA input data
	cd /home/599/mak599/Fortran/ICA/ICA_NAG_Mark22/Data/Simulated/Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/1

	- Check which ICA.exe you are using
	which ICA.exe

	- Execute ICA with defined setting
	ICA.exe < ICAsettings.txt



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
 2. replace E04FDF (01 Dec. 2004), G05EAF and G05EZF.

USAGE
-----

ON LINUX, from the current directory
------------------------------------

ICA.exe < Data/ICAinputDataSettings.txt
ICA.exe < Data/Simulated/1/ICAinputDataSettings.txt

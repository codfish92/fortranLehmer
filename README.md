fortranLehmer
=============

lehmer generator in fortran for csci 260

# To compile  
#### on windows  
g95 -c mod_lehmer.f95  
g95 stats.f95 mod_lehmer.o  
### on linux
f95 -c mod_lehmer.f95  
f95 stats.f95 mod_lehmer.o   

To include in a file, the syntax 'use Lehmer' is required. Put this line above the 'implicit none' line.  
You must run the 'g95 -c mod_lehmer.f95' to generate the object code. You then run 'g95 yourFile.f95 mod_lehmer.o'  


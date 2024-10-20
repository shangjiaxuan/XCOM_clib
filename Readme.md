# Readme for C port

This C port aims to make the database easy to use in modern
programming use. The library is based on the Fortran
procedures, but have a few differences to note. Currently
no table printing function is implemented yet.

However, there are a few key differences

* All calculations are done in double precision
* Data files should be put in `XCOM` subdirectory
* Data points on both sides of K edge is used for 
non-phoelectric cross sections. (This may case jitter,
but is not investegated yet.).


Sample usage in `sample.cpp`
```
#include "XCOM.h"

#include <stdio.h>
int main()
{
	int error = 0;
	XCOM_material_info mat1;
	FILE* normal_mat1_output = 0;
	{
		XCOM_material_info info_resin = {0};
		if (!XCOM_parse_formula(&info_resin, "C6H7O2")) {
			error = -1;
			goto CLEANUP;
		}
		XCOM_material_info info_carbon = {0};
		info_carbon.element_count = 1;
		info_carbon.Z[0] = 6;
		info_carbon.weight_portion[0] = 1;

		const double rho_carbon_fiber = 1.80;
		const double rho_resin = 1.10;

		mat1 = info_resin;

		const double mat1_carbon_part = (1.4 - rho_resin) / (rho_carbon_fiber - rho_resin);

		if (!XCOM_material_mix_into(&mat1, mat1_carbon_part, &info_carbon)) {
			error = -1;
			goto CLEANUP;
		}
	}
	if (!XCOM_load_material(&mat1)) {
		error = -1;
		goto CLEANUP;
	}
	normal_mat1_output = fopen("mat1.csv", "w");
	if (!normal_mat1_output) {
		error = -1;
		goto CLEANUP;
	}
	for(int i = 1; i <= 200; ++i) {
		double energy = i *1000;
		double mat1_coherent = XCOM_material_coherent_scatter_attenuation(&mat1, energy);
		double mat1_incoherent = XCOM_material_incoherent_scatter_attenuation(&mat1, energy);
		double mat1_phot = XCOM_material_photoelectric_attenuation(&mat1, energy);
		double mat1_np = XCOM_material_nucleus_pair_production_attenuation(&mat1, energy);
		double mat1_sp = XCOM_material_shell_pair_production_attenuation(&mat1, energy);

		fprintf(normal_mat1_output, "%.6E,%.6E,%.6E,%.6E,%.6E,%.6E\n",
			energy, mat1_coherent, mat1_incoherent, mat1_phot, mat1_np, mat1_sp);
	}
CLEANUP:
	if(normal_mat1_output)
		fclose(normal_mat1_output);
	return error;
}
```

This prints the data for one material in csv format between 1keV to 200 keV

# Original Readme

XCOM Documentation

M. J. Berger and J. H. Hubbell

A computer program and data base are presented which can
be used to calculate, with a personal computer, photon
cross sections for scattering, photoelectric
absorbtion and pair production, as well as total
attenuation coefficients, in any element, compound, or
mixture, at energies from 1 keV to 100 GeV.
____________________________________________________
Table of Contents:

I.	Disclaimer
II.	Why you should use the Web
III.	Version History
IV.	Files for XCOM
V.	Setup
VI.	Running XCOM
VII.	Program Notes
____________________________________________________
I. Disclaimer:

The National Institute of Standards and Technology (NIST)
uses its best efforts to deliver a high quality copy of
the Database and to verify that the data contained therein
have been selected on the basis of sound scientific
judgement. However, NIST makes no warranties to that
effect, and NIST shall not be liable for any damage that
may result from errors or omissions in the Database.
____________________________________________________
II. Why you should use the Web:

	Simple and convenient user interface
	Graphing
	Updated regularly

Check out the online version at:

http://physics.nist.gov/XCOM
____________________________________________________
III. Version History:

	Description
	Version Number
	Date
	Programmer(s)

	Programming Update
	Version 3.1
	June 1999
	Martin J. Berger and Stephen M. Seltzer
	
	NIST Standard Reference Database 8
	Version 2.0
	June 1990
	Martin J. Berger and John H. Hubbell

	Original Release
	Version 1.2
	July 1987
	Martin J. Berger and John H. Hubbell


For more comprehensive version history information see the
online documentation:

http://physics.nist.gov/PhysRefData/Xcom/Text/version.html
____________________________________________________
IV. Files for XCOM:

XCOM.f		Fortan Source
ATWTS.DAT	Atomic Weights Data File
ENB.DAT		Data File
HASH1.DAT	Data File
HASH2.DAT	Data File
INDEX.DAT	Data File
MDATX3.xxx	Cross Sections Data for each element
		where "xxx" is the atomic number

for PC only

XCOM.exe	Photon Cross Sections Program
makedisk.bat	Application that can be used to copy the
		XCOM program to a floppy disk without
		including any extraneous files
____________________________________________________
V. Setup:

1. Make a directory for XCOM
2. Unzip the XCOM zip into that directory

Note: PC users can skip step 3

3. Compile (and link) XCOM.f

To run the program, run XCOM and follow command line
instructions
____________________________________________________
VI. Running XCOM:

Follow comand line instructions

Note on: Format for entry from prepared input file
	- First, the number of energies in the list
	- Then, energies in MeV
	- All items separated by blank spaces
____________________________________________________
VII. Program Notes:

For calculation information such as the method used
see the online documentation:

http://physics.nist.gov/PhysRefData/Xcom/Text/intro.html


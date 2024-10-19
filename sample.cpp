#define XCOM_STATIC
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
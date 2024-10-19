#pragma once

#ifndef _XCOM_H_
#define _XCOM_H_

// dll export support
#ifdef _WIN32
#ifndef XCOM_STATIC
#ifdef XCOM_BUILD_DLL
#define XCOM_API __declspec(dllexport)
#else
#define XCOM_API __declspec(dllimport)
#endif
#else
#define XCOM_API
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif
/*
	Units:
		energy			: eV
		cross_section	: b/atom
		attenuation		: cm2/g
*/

typedef struct XCOM_element_info XCOM_element_info;

XCOM_API int XCOM_load_data(XCOM_element_info* elem, const char* filename);
XCOM_API const XCOM_element_info*  XCOM_get_element(int Z);

XCOM_API double XCOM_coherent_scatter_cross_section(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_incoherent_scatter_cross_section(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_photoelectric_cross_section(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_nucleus_pair_production_cross_section(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_shell_pair_production_cross_section(const XCOM_element_info* elem, double E);

XCOM_API double XCOM_scatter_cross_section(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_total_cross_section(const XCOM_element_info* elem, double E);

XCOM_API double XCOM_incoherent_scatter_attenuation(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_coherent_scatter_attenuation(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_photoelectric_attenuation(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_nucleus_pair_production_attenuation(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_shell_pair_production_attenuation(const XCOM_element_info* elem, double E);

XCOM_API double XCOM_scatter_attenuation(const XCOM_element_info* elem, double E);
XCOM_API double XCOM_total_attenuation(const XCOM_element_info* elem, double E);

typedef struct XCOM_material_info XCOM_material_info;

XCOM_API int XCOM_parse_formula(XCOM_material_info* mat, const char* formula);
XCOM_API int XCOM_material_mix_into(XCOM_material_info* mat, double portion_2, const XCOM_material_info* mat2);
XCOM_API int XCOM_material_mix(XCOM_material_info* mat, size_t portion_count, const XCOM_material_info* portions, double* weight_factor, int normalize);

XCOM_API int XCOM_load_material(const XCOM_material_info* mat);

XCOM_API double XCOM_material_coherent_scatter_attenuation(const XCOM_material_info* mat, double E);
XCOM_API double XCOM_material_incoherent_scatter_attenuation(const XCOM_material_info* mat, double E);
XCOM_API double XCOM_material_photoelectric_attenuation(const XCOM_material_info* mat, double E);
XCOM_API double XCOM_material_nucleus_pair_production_attenuation(const XCOM_material_info* mat, double E);
XCOM_API double XCOM_material_shell_pair_production_attenuation(const XCOM_material_info* mat, double E);

XCOM_API double XCOM_material_total_attenuation(const XCOM_material_info* mat, double E);
XCOM_API double XCOM_material_scatter_attenuation(const XCOM_material_info* mat, double E);

#define XCOM_BASE_ENERGY_POINTS 80
#define XCOM_MAX_ENERGY_POINTS XCOM_BASE_ENERGY_POINTS + 2*XCOM_MAX_SHELL_COUNT
#define XCOM_MAX_SHELL_COUNT 14
#define XCOM_MAX_SHELL_POINTS 35
#define XCOM_PAIR_NUCLEUS_POINTS 54
#define XCOM_PAIR_SHELL_POINTS 50

// should be read only most of the time
typedef struct XCOM_element_info
{
	double atomic_weight;
	int Z;
	int energy_count;
	int shell_count;
	int shell_known_count;

	double energies[XCOM_MAX_ENERGY_POINTS];
	double log_energies[XCOM_MAX_ENERGY_POINTS];

	double coherent_scatter[XCOM_MAX_ENERGY_POINTS];
	double coherent_scatter_spline_A[XCOM_MAX_ENERGY_POINTS];
	double coherent_scatter_spline_B[XCOM_MAX_ENERGY_POINTS];
	double coherent_scatter_spline_C[XCOM_MAX_ENERGY_POINTS];
	double coherent_scatter_spline_D[XCOM_MAX_ENERGY_POINTS];

	double incoherent_scatter[XCOM_MAX_ENERGY_POINTS];
	double incoherent_scatter_spline_A[XCOM_MAX_ENERGY_POINTS];
	double incoherent_scatter_spline_B[XCOM_MAX_ENERGY_POINTS];
	double incoherent_scatter_spline_C[XCOM_MAX_ENERGY_POINTS];
	double incoherent_scatter_spline_D[XCOM_MAX_ENERGY_POINTS];

	double photoelectric[XCOM_MAX_ENERGY_POINTS];
	double photoelectric_spline_A[XCOM_MAX_ENERGY_POINTS];
	double photoelectric_spline_B[XCOM_MAX_ENERGY_POINTS];
	double photoelectric_spline_C[XCOM_MAX_ENERGY_POINTS];
	double photoelectric_spline_D[XCOM_MAX_ENERGY_POINTS];

	double nucleus_pair_production[XCOM_PAIR_NUCLEUS_POINTS + 1];
	double nucleus_pair_spline_A[XCOM_PAIR_NUCLEUS_POINTS + 1];
	double nucleus_pair_spline_B[XCOM_PAIR_NUCLEUS_POINTS + 1];
	double nucleus_pair_spline_C[XCOM_PAIR_NUCLEUS_POINTS + 1];
	double nucleus_pair_spline_D[XCOM_PAIR_NUCLEUS_POINTS + 1];

	double shell_pair_production[XCOM_PAIR_SHELL_POINTS + 1];
	double shell_pair_spline_A[XCOM_PAIR_SHELL_POINTS + 1];
	double shell_pair_spline_B[XCOM_PAIR_SHELL_POINTS + 1];
	double shell_pair_spline_C[XCOM_PAIR_SHELL_POINTS + 1];
	double shell_pair_spline_D[XCOM_PAIR_SHELL_POINTS + 1];

	int shell_indexes[XCOM_MAX_SHELL_COUNT];
	double shell_energies[XCOM_MAX_SHELL_COUNT];
	double shell_peak_difference[XCOM_MAX_SHELL_COUNT];
	char shell_names[XCOM_MAX_SHELL_COUNT][3];

	int shell_energy_points[XCOM_MAX_SHELL_COUNT];
	double shell_energy_levels[XCOM_MAX_SHELL_COUNT][XCOM_MAX_SHELL_POINTS];
	double shell_photon_capture[XCOM_MAX_SHELL_COUNT][XCOM_MAX_SHELL_POINTS];

	double log_shell_energy_points[XCOM_MAX_SHELL_COUNT][XCOM_MAX_SHELL_POINTS];
	double adjusted_shell_photon_capture[XCOM_MAX_SHELL_COUNT][XCOM_MAX_SHELL_POINTS];
}XCOM_element_info;

#define XCOM_ELEMENT_COUNT 100

typedef struct XCOM_material_info
{
	double total_weight;			// valid only if just parsed from formula
	double weight_portion[XCOM_ELEMENT_COUNT];
	int Z[XCOM_ELEMENT_COUNT];
	int counts[XCOM_ELEMENT_COUNT];	// valid only if just parsed from formula
	size_t element_count;
} XCOM_material_info;

#ifdef __cplusplus
}
#endif

#endif

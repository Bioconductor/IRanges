#include "IRanges_interface.h"

typedef CharAE (*new_CharAE_from_string_FUNTYPE)(const char *);
CharAE new_CharAE_from_string(const char *string)
{
	static new_CharAE_from_string_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (new_CharAE_from_string_FUNTYPE)
			R_GetCCallable("IRanges", "_new_CharAE_from_string");
	return fun(string);
}

typedef CharAEAE (*new_CharAEAE_FUNTYPE)(int, int);
CharAEAE new_CharAEAE(int buflength, int nelt)
{
	static new_CharAEAE_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (new_CharAEAE_FUNTYPE)
			R_GetCCallable("IRanges", "_new_CharAEAE");
	return fun(buflength, nelt);
}

typedef void (*append_string_to_CharAEAE_FUNTYPE)(CharAEAE *, const char *);
void append_string_to_CharAEAE(CharAEAE *cbbuf, const char *string)
{
	static append_string_to_CharAEAE_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (append_string_to_CharAEAE_FUNTYPE)
			R_GetCCallable("IRanges", "_append_string_to_CharAEAE");
	return fun(cbbuf, string);
}

typedef SEXP (*get_IRanges_start_FUNTYPE)(SEXP x);
SEXP get_IRanges_start(SEXP x)
{
	static get_IRanges_start_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (get_IRanges_start_FUNTYPE)
			R_GetCCallable("IRanges", "_get_IRanges_start");
	return fun(x);
}

typedef SEXP (*get_IRanges_width_FUNTYPE)(SEXP x);
SEXP get_IRanges_width(SEXP x)
{
	static get_IRanges_width_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (get_IRanges_width_FUNTYPE)
			R_GetCCallable("IRanges", "_get_IRanges_width");
	return fun(x);
}

typedef int (*get_IRanges_length_FUNTYPE)(SEXP x);
int get_IRanges_length(SEXP x)
{
	static get_IRanges_length_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (get_IRanges_length_FUNTYPE)
			R_GetCCallable("IRanges", "_get_IRanges_length");
	return fun(x);
}

typedef void (*set_IRanges_names_FUNTYPE)(SEXP x, SEXP names);
void set_IRanges_names(SEXP x, SEXP names)
{
	static set_IRanges_names_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (set_IRanges_names_FUNTYPE)
			R_GetCCallable("IRanges", "_set_IRanges_names");
	return fun(x, names);
}

typedef void (*copy_IRanges_slots_FUNTYPE)(SEXP x, SEXP x0);
void copy_IRanges_slots(SEXP x, SEXP x0)
{
	static copy_IRanges_slots_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (copy_IRanges_slots_FUNTYPE)
			R_GetCCallable("IRanges", "_copy_IRanges_slots");
	return fun(x, x0);
}

typedef SEXP (*new_IRanges_FUNTYPE)(const char *class, SEXP start, SEXP width, SEXP names);
SEXP new_IRanges(const char *class, SEXP start, SEXP width, SEXP names)
{
	static new_IRanges_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (new_IRanges_FUNTYPE)
			R_GetCCallable("IRanges", "_new_IRanges");
	return fun(class, start, width, names);
}

typedef SEXP (*alloc_IRanges_FUNTYPE)(const char *class, int length);
SEXP alloc_IRanges(const char *class, int length)
{
	static alloc_IRanges_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (alloc_IRanges_FUNTYPE)
			R_GetCCallable("IRanges", "_alloc_IRanges");
	return fun(class, length);
}


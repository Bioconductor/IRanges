#include "IRanges.h"


/* This function gets the length of vectors in a list. */
SEXP IRanges_length_vectors_in_list(SEXP list)
{
	int n = LENGTH(list);
	SEXP element_lengths;
	PROTECT(element_lengths = NEW_INTEGER(n));
	for (int i = 0; i < n; i++) {
		INTEGER(element_lengths)[i] = LENGTH(VECTOR_ELT(list, i));
	}
	UNPROTECT(1);
	return element_lengths;
}

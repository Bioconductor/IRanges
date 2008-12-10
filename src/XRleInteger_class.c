#include "IRanges.h"

static int add(int x, int y) {
	return x + y;
}

static int subtract(int x, int y) {
	return x - y;
}

static int multiply(int x, int y) {
	return x * y;
}

static int divide(int x, int y) {
	return x / y;
}

static int modulo(int x, int y) {
	return x % y;
}

static int exponentiate(int x, int y) {
	return pow(x, y);
}


SEXP XRleInteger_Arith(SEXP x, SEXP y, SEXP Generic)
{
	int (*fun)(int, int);
	int x_i, y_i, x_len, y_len, x_cumsum, y_cumsum, values_length;
	int prev_value, prev_index, curr_value;
	int *x_values_ptr, *x_lengths_ptr, *y_values_ptr, *y_lengths_ptr;
	SEXP x_values_tag, x_lengths_tag, y_values_tag, y_lengths_tag;
	SEXP x_values, x_lengths, x_vectorLength, y_values, y_lengths, y_vectorLength;
	SEXP ans, ans_lengths, ans_lengths_tag, ans_values, ans_values_tag;

	switch(CHAR(STRING_ELT(Generic, 0))[0]) {
		case '+':
			fun = &add;
			break;
		case '-':
			fun = &subtract;
			break;
		case '*':
			fun = &multiply;
			break;
		case '/':
			fun = &divide;
			break;
		case '%':
			fun = &modulo;
			break;
		case '^':
			fun = &exponentiate;
			break;
	}

	x_vectorLength = GET_SLOT(x, install("vectorLength"));
	y_vectorLength = GET_SLOT(y, install("vectorLength"));

	if (INTEGER(x_vectorLength)[0] != INTEGER(y_vectorLength)[0])
		error("cannot add vectors of unequal length");

	x_values = GET_SLOT(x, install("values"));
	x_lengths = GET_SLOT(x, install("lengths"));
	x_values_tag = _get_XSequence_tag(x_values);
	x_lengths_tag = _get_XSequence_tag(x_lengths);

	y_values = GET_SLOT(y, install("values"));
	y_lengths = GET_SLOT(y, install("lengths"));
	y_values_tag = _get_XSequence_tag(y_values);
	y_lengths_tag = _get_XSequence_tag(y_lengths);

	x_i = 0;
	y_i = 0;
	values_length = 0;
	x_len = LENGTH(x_values_tag);
	y_len = LENGTH(y_values_tag);
	x_values_ptr = INTEGER(x_values_tag);
	x_lengths_ptr = INTEGER(x_lengths_tag);
	y_values_ptr = INTEGER(y_values_tag);
	y_lengths_ptr = INTEGER(y_lengths_tag);
	x_cumsum = *x_lengths_ptr;
	y_cumsum = *y_lengths_ptr;
	prev_value = fun(*x_values_ptr, *y_values_ptr) - 1;
	while (x_i < x_len || y_i < y_len) {
		if (fun(*x_values_ptr, *y_values_ptr) != prev_value) {
			values_length++;
			prev_value = fun(*x_values_ptr, *y_values_ptr);
		}
		if (x_cumsum == y_cumsum) {
			x_i++;
			x_values_ptr++;
			x_lengths_ptr++;
			x_cumsum += *x_lengths_ptr;
			y_i++;
			y_values_ptr++;
			y_lengths_ptr++;
			y_cumsum += *y_lengths_ptr;
		} else if (x_cumsum < y_cumsum) {
			x_i++;
			x_values_ptr++;
			x_lengths_ptr++;
			x_cumsum += *x_lengths_ptr;
		} else {
			y_i++;
			y_values_ptr++;
			y_lengths_ptr++;
			y_cumsum += *y_lengths_ptr;
		}
	}

	PROTECT(ans_lengths_tag = NEW_INTEGER(values_length));
	PROTECT(ans_values_tag = NEW_INTEGER(values_length));
	int *ans_lengths_ptr = INTEGER(ans_lengths_tag);
	int *ans_values_ptr = INTEGER(ans_values_tag);
	memset(ans_lengths_ptr, 0, values_length * sizeof(int));
	memset(ans_values_ptr, 0, values_length * sizeof(int));

	x_i = 0;
	y_i = 0;
	x_values_ptr = INTEGER(x_values_tag);
	x_lengths_ptr = INTEGER(x_lengths_tag);
	y_values_ptr = INTEGER(y_values_tag);
	y_lengths_ptr = INTEGER(y_lengths_tag);
	x_cumsum = *x_lengths_ptr;
	y_cumsum = *y_lengths_ptr;
	prev_value = fun(*x_values_ptr, *y_values_ptr) - 1;
	prev_index = 0;
	ans_lengths_ptr--;
	ans_values_ptr--;
	while (x_i < x_len || y_i < y_len) {
		curr_value = fun(*x_values_ptr, *y_values_ptr);
		if (curr_value != prev_value) {
			ans_lengths_ptr++;
			ans_values_ptr++;
			*ans_values_ptr = curr_value;
		}
		if (x_cumsum == y_cumsum) {
			*ans_lengths_ptr += x_cumsum - prev_index;
			prev_index = x_cumsum;
			x_i++;
			x_values_ptr++;
			x_lengths_ptr++;
			x_cumsum += *x_lengths_ptr;
			y_i++;
			y_values_ptr++;
			y_lengths_ptr++;
			y_cumsum += *y_lengths_ptr;
		} else if (x_cumsum < y_cumsum) {
			*ans_lengths_ptr += x_cumsum - prev_index;
			prev_index = x_cumsum;
			x_i++;
			x_values_ptr++;
			x_lengths_ptr++;
			x_cumsum += *x_lengths_ptr;
		} else {
			*ans_lengths_ptr += y_cumsum - prev_index;
			prev_index = y_cumsum;
			y_i++;
			y_values_ptr++;
			y_lengths_ptr++;
			y_cumsum += *y_lengths_ptr;
		}
		if (*ans_values_ptr != prev_value) {
			prev_value = *ans_values_ptr;
		}
	}

	PROTECT(ans_lengths = _new_XInteger_from_tag(ans_lengths_tag));
	PROTECT(ans_values = _new_XInteger_from_tag(ans_values_tag));
	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("XRleInteger")));
	SET_SLOT(ans, mkChar("vectorLength"), ScalarInteger(INTEGER(x_vectorLength)[0]));
	SET_SLOT(ans, mkChar("lengths"), ans_lengths);
	SET_SLOT(ans, mkChar("values"), ans_values);
	UNPROTECT(5);
	return ans;
}

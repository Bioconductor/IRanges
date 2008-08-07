#include "IRanges_interface.h"

typedef CharBuf (*new_CharBuf_from_string_FUNTYPE)(const char *);
CharBuf new_CharBuf_from_string(const char *string)
{
	static new_CharBuf_from_string_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (new_CharBuf_from_string_FUNTYPE)
			R_GetCCallable("IRanges", "_new_CharBuf_from_string");
	return fun(string);
}

typedef CharBBuf (*new_CharBBuf_FUNTYPE)(int, int);
CharBBuf new_CharBBuf(int buflength, int nelt)
{
	static new_CharBBuf_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (new_CharBBuf_FUNTYPE)
			R_GetCCallable("IRanges", "_new_CharBBuf");
	return fun(buflength, nelt);
}

typedef void (*append_string_to_CharBBuf_FUNTYPE)(CharBBuf *, const char *);
void append_string_to_CharBBuf(CharBBuf *cbbuf, const char *string)
{
	static append_string_to_CharBBuf_FUNTYPE fun = NULL;

	if (fun == NULL)
		fun = (append_string_to_CharBBuf_FUNTYPE)
			R_GetCCallable("IRanges", "_append_string_to_CharBBuf");
	return fun(cbbuf, string);
}


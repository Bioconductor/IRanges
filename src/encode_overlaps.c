/****************************************************************************
 *                             Encode overlaps                              *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"


/*
 * A low-level helper for "superficial" checking of the 'space' vector
 * associated with a Ranges object.
 */
static const int *check_Ranges_space(SEXP space, int len, const char *what)
{
	if (space == R_NilValue)
		return NULL;
	if (!IS_INTEGER(space))
		error("'%s_space' must be an integer vector or NULL", what);
	if (LENGTH(space) != len)
		error("when not NULL, '%s_space' must have "
		      "the same length as 'start(%s)'", what, what);
	return INTEGER(space);
}

static void CharAE_append_char(CharAE *char_ae, char c, int times)
{
	int i;

	for (i = 0; i < times; i++)
		_CharAE_insert_at(char_ae, _CharAE_get_nelt(char_ae), c);
	return;
}

static void CharAE_append_int(CharAE *char_ae, int d)
{
	static char buf[12]; /* should be enough for 32-bit ints */
	int ret;

	ret = snprintf(buf, sizeof(buf), "%d", d);
	if (ret < 0) /* should never happen */
		error("IRanges internal error in CharAE_append_int(): "
		      "snprintf() returned value < 0");
	if (ret >= sizeof(buf)) /* could happen with ints > 32-bit */
		error("IRanges internal error in CharAE_append_int(): "
		      "output of snprintf() was truncated");
	_append_string_to_CharAE(char_ae, buf);
	return;
}

/*
 * q_start, q_width: int arrays of length q_len.
 * q_space: NULL or an int array of length q_len.
 * q_len: nb of ranges in the query.
 * Lq_len: 0 if all the ranges in the query are coming from the same segment
 *         (single-end read), or the nb of ranges in the query coming from the
 *         left segment if the query is made of ranges coming from 2 segments
 *         (paired-end read). If the latter, then Lq_len must be >= 1 and
 *         < q_len (this implies that q_len >= 2), and the first Lq_len ranges
 *         in the query are assumed to belong to the left segment.
 * s_start, s_width: int arrays of length s_len.
 * s_space: NULL or an int array of length s_len.
 * s_len: nb of ranges in the subject.
 * as_matrix, Loffset, Roffset: if as_matrix, then the full matrix of codes
 *         is returned and the returned values for Loffset and Roffset are
 *         undefined. Otherwise, the matrix is trimmed and the returned values
 *         for Loffset and Roffset are the number of cols removed on the left
 *         and right sides of the matrix, respectively.
 * out: character array containing the matrix of codes (possibly trimmed)
 *  
 * A special 1-letter code 'X' is used for ranges that are not on the same
 * space.
 */
static void one_overlap_encoding(
		const int *q_start, const int *q_width, const int *q_space,
		int q_len, int Lq_len,
		const int *s_start, const int *s_width, const int *s_space,
		int s_len,
		int as_matrix, int *Loffset, int *Roffset, CharAE *out)
{
	int out_nelt0, i, starti, widthi, spacei, j, startj, widthj, spacej,
	    j1, j2, nrow;
	char code;

	if (!as_matrix) {
		if (Lq_len) {
			CharAE_append_int(out, Lq_len);
			CharAE_append_char(out, '-', 2);
			CharAE_append_int(out, q_len - Lq_len);
		} else {
			CharAE_append_int(out, q_len);
		}
		CharAE_append_char(out, ':', 1);
		out_nelt0 = _CharAE_get_nelt(out);
	}
	/* j1: 0-based index of first (i.e. leftmost) OVM col with a non-"m",
	       or 's_len' if there is no such col.
	   j2: 0-based index of last (i.e. rightmost) OVM col with a non-"a",
	       or -1 if there is no such col. */
	j1 = s_len;
	j2 = -1;
	/* Walk col by col. */
	for (j = 0; j < s_len; j++) {
		startj = s_start[j];
		widthj = s_width[j];
		spacej = s_space == NULL ? 0 : s_space[j];
		for (i = 0; i < q_len; i++) {
			if (Lq_len && i == Lq_len)
				CharAE_append_char(out, '-', 2);
			starti = q_start[i];
			widthi = q_width[i];
			spacei = q_space == NULL ? 0 : q_space[i];
			if (spacei == -1 || spacej == -1 || spacei != spacej)
				code = 'X';
			else
				code = 'g' + _overlap_code(starti, widthi,
							   startj, widthj);
			CharAE_append_char(out, code, 1);
			if (j1 == s_len && code != 'm')
				j1 = j;
			if (code != 'a')
				j2 = j;
		}
	}
	if (!as_matrix) {
		/* By making 'j2' a 1-based index we will then have
		   0 <= j1 <= j2 <= s_len, which will simplify further
		   arithmetic/logic. */
		if (q_len == 0) {
			/* A 0-row OVM needs special treatment. */
			j2 = s_len;
		} else {
			j2++;
		}
		*Loffset = j1;
		*Roffset = s_len - j2;
		nrow = q_len;
		if (Lq_len)
			nrow += 2;
		/* Remove "a"-cols on the right. */
		_CharAE_set_nelt(out, out_nelt0 + j2 * nrow);
		/* Remove "m"-cols on the left. */
		_CharAE_delete_at(out, out_nelt0, j1 * nrow);
		/* Insert ":" at the end of each remaining col. */
		for (j = j2 - j1; j >= 1; j--)
			_CharAE_insert_at(out, out_nelt0 + j * nrow, ':');
	}
	return;
}

static void safe_one_overlap_encoding(
		SEXP query_start, SEXP query_width, SEXP query_space,
		int Lquery_len,
		SEXP subject_start, SEXP subject_width, SEXP subject_space,
		int as_matrix, int *Loffset, int *Roffset, CharAE *out)
{
	int q_len, s_len;
	const int *q_start, *q_width, *q_space, *s_start, *s_width, *s_space;

	q_len = _check_Ranges_start_width(query_start, query_width,
					  &q_start, &q_width, "query");
	if (Lquery_len != 0 && (Lquery_len < 1 || Lquery_len >= q_len))
		error("number of ranges in query coming from the left "
		      "segment must be >= 1 and < length(query)");
	q_space = check_Ranges_space(query_space, q_len, "query");
	s_len = _check_Ranges_start_width(subject_start, subject_width,
					  &s_start, &s_width, "subject");
	s_space = check_Ranges_space(subject_space, s_len, "subject");
	one_overlap_encoding(q_start, q_width, q_space, q_len, Lquery_len,
		s_start, s_width, s_space, s_len,
		as_matrix, Loffset, Roffset, out);
	return;
}

/* type: 0=CHARSXP, 1=STRSXP, 2=RAWSXP
   as_matrix: 0 or 1, ignored when type is 0 */
static SEXP make_encoding_from_CharAE(const CharAE *buf, int type,
				      int as_matrix,
				      int q_len, int Lq_len, int s_len)
{
	SEXP ans, ans_elt, ans_dim;
	int buf_nelt, i, nrow;

	buf_nelt = _CharAE_get_nelt(buf);
	if (type == 0 || (type == 1 && !as_matrix)) {
		PROTECT(ans = mkCharLen(buf->elts, buf_nelt));
		if (type == 1) {
			PROTECT(ans = ScalarString(ans));
			UNPROTECT(1);
		}
		UNPROTECT(1);
		return ans;
	}
	if (type == 1) {
		PROTECT(ans = NEW_CHARACTER(buf_nelt));
		for (i = 0; i < buf_nelt; i++) {
			PROTECT(ans_elt = mkCharLen(buf->elts + i, 1));
			SET_STRING_ELT(ans, i, ans_elt);
			UNPROTECT(1);
		}
	} else {
		PROTECT(ans = _new_RAW_from_CharAE(buf));
	}
	if (as_matrix) {
		nrow = q_len;
		if (Lq_len)
			nrow += 2;
		PROTECT(ans_dim	= NEW_INTEGER(2));
		INTEGER(ans_dim)[0] = nrow;
		INTEGER(ans_dim)[1] = s_len;
		SET_DIM(ans, ans_dim);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

static SEXP make_LIST_from_ovenc_parts(SEXP Loffset, SEXP Roffset,
				       SEXP encoding)
{
	SEXP ans, ans_names, ans_names_elt;

	PROTECT(ans = NEW_LIST(3));

	PROTECT(ans_names = NEW_CHARACTER(3));
	PROTECT(ans_names_elt = mkChar("Loffset"));
	SET_STRING_ELT(ans_names, 0, ans_names_elt);
	UNPROTECT(1);
	PROTECT(ans_names_elt = mkChar("Roffset"));
	SET_STRING_ELT(ans_names, 1, ans_names_elt);
	UNPROTECT(1);
	PROTECT(ans_names_elt = mkChar("encoding"));
	SET_STRING_ELT(ans_names, 2, ans_names_elt);
	UNPROTECT(1);
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);

	SET_VECTOR_ELT(ans, 0, Loffset);
	SET_VECTOR_ELT(ans, 1, Roffset);
	SET_VECTOR_ELT(ans, 2, encoding);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT ---
 * 'query_start', 'query_width', 'query_space': integer vectors of the same
 *     length M (or NULL for 'query_space').
 * 'Lquery_length': single integer.
 * 'subject_start', 'subject_width', 'subject_space': integer vectors of the
 *     same length N (or NULL for 'subject_space').
 * Integer vectors 'query_start', 'query_width', 'subject_start' and
 * 'subject_width' are assumed to be NA free. 'query_width' and 'subject_width'
 * are assumed to contain non-negative values. For efficiency reasons, those
 * assumptions are not checked.
 * Return the matrix of 1-letter codes (if 'as_matrix' is TRUE), otherwise a
 * named list with the 3 following components:
 *     1. Loffset: single integer;
 *     2. Roffset: single integer;
 *     3. encoding: the compact encoding (type II) as a single string (if
 *        'as_raw' is FALSE) or a raw vector (if 'as_raw' is TRUE).
 */
SEXP encode_overlaps1(SEXP query_start, SEXP query_width,
			SEXP query_space, SEXP Lquery_length,
		      SEXP subject_start, SEXP subject_width,
			SEXP subject_space,
		      SEXP as_matrix, SEXP as_raw)
{
	int Lquery_len, as_matrix0, as_raw0, Loffset, Roffset;
	CharAE buf;
	SEXP encoding, ans_Loffset, ans_Roffset, ans;

	Lquery_len = INTEGER(Lquery_length)[0];
	as_matrix0 = as_matrix != R_NilValue && LOGICAL(as_matrix)[0];
	as_raw0 = as_raw != R_NilValue && LOGICAL(as_raw)[0];
	buf = _new_CharAE(0);
	safe_one_overlap_encoding(
		query_start, query_width, query_space, Lquery_len,
		subject_start, subject_width, subject_space,
		as_matrix0, &Loffset, &Roffset, &buf);
	PROTECT(encoding = make_encoding_from_CharAE(&buf, as_raw0 ? 2 : 1,
						as_matrix0,
						LENGTH(query_start),
						Lquery_len,
						LENGTH(subject_start)));
	if (as_matrix0) {
		UNPROTECT(1);
		return encoding;
	}
	PROTECT(ans_Loffset = ScalarInteger(Loffset));
	PROTECT(ans_Roffset = ScalarInteger(Roffset));
	PROTECT(ans = make_LIST_from_ovenc_parts(ans_Loffset, ans_Roffset,
						 encoding));
	UNPROTECT(4);
	return ans;
}

/* --- .Call ENTRY POINT ---/
 * 'query_starts', 'query_widths', 'query_spaces': lists of integer vectors.
 * The 3 lists are assumed to have the same length (M) and shape.
 * 'Lquery_lengths': NULL or integer vector of length M.
 * 'subject_starts', 'subject_widths', 'subject_spaces': lists of integer
 * vectors. The 3 lists are assumed to have the same length (N) and shape.
 * Return a named list with the 3 following components (all of the same
 * length):
 *     1. Loffset: integer vector;
 *     2. Roffset: integer vector;
 *     3. encoding: character vector containing the compact encodings (type
 *        II).
 */
SEXP RangesList_encode_overlaps(SEXP query_starts, SEXP query_widths,
				SEXP query_spaces, SEXP Lquery_lengths,
				SEXP subject_starts, SEXP subject_widths,
				SEXP subject_spaces)
{
	int q_len, s_len, ans_len, Lquery_len, i, j, k;
	SEXP ans_Loffset, ans_Roffset, ans_encoding, ans_encoding_elt, ans,
	     query_start, query_width, query_space,
	     subject_start, subject_width, subject_space;
	CharAE buf;

	/* TODO: Add some basic checking of the input values. */
	q_len = LENGTH(query_starts);
	s_len = LENGTH(subject_starts);
	if (q_len == 0 || s_len == 0)
		ans_len = 0;
	else
		ans_len = q_len >= s_len ? q_len : s_len;
	PROTECT(ans_Loffset = NEW_INTEGER(ans_len));
	PROTECT(ans_Roffset = NEW_INTEGER(ans_len));
	PROTECT(ans_encoding = NEW_CHARACTER(ans_len));
	query_space = subject_space = R_NilValue;
	Lquery_len = 0;
	buf = _new_CharAE(0);
	for (i = j = k = 0; k < ans_len; i++, j++, k++) {
		if (i >= q_len)
			i = 0; /* recycle i */
		if (j >= s_len)
			j = 0; /* recycle j */
		query_start = VECTOR_ELT(query_starts, i);
		query_width = VECTOR_ELT(query_widths, i);
		if (query_spaces != R_NilValue)
			query_space = VECTOR_ELT(query_spaces, i);
		if (Lquery_lengths != R_NilValue)
			Lquery_len = INTEGER(Lquery_lengths)[i];
		subject_start = VECTOR_ELT(subject_starts, j);
		subject_width = VECTOR_ELT(subject_widths, j);
		if (subject_spaces != R_NilValue)
			subject_space = VECTOR_ELT(subject_spaces, j);
		safe_one_overlap_encoding(
			query_start, query_width, query_space, Lquery_len,
			subject_start, subject_width, subject_space,
			0,
			INTEGER(ans_Loffset) + k,
			INTEGER(ans_Roffset) + k,
			&buf);
		PROTECT(ans_encoding_elt = make_encoding_from_CharAE(&buf, 0,
						0, q_len, Lquery_len, s_len));
		SET_STRING_ELT(ans_encoding, k, ans_encoding_elt);
		UNPROTECT(1);
		_CharAE_set_nelt(&buf, 0);
	}
	if (ans_len != 0 && (i != q_len || j != s_len))
		warning("longer object length is not a multiple "
			"of shorter object length");
	PROTECT(ans = make_LIST_from_ovenc_parts(ans_Loffset, ans_Roffset,
						 ans_encoding));
	UNPROTECT(4);
	return ans;
}


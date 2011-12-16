#include "IRanges.h"

/*
 * Generalized comparison of ranges
 * ================================
 *
 * There are 13 different ways 2 ranges can be positioned with respect to each
 * other. They are described in the table below together with their associated
 * codes. The 1st range is called the "query" range (q) and the 2nd range the
 * "subject" range (s).
 *
 *                   numeric code & |                 numeric code &
 *                  1-letter code & |                1-letter code &
 *                        long code |                      long code
 *   -------------  --------------- | -------------  ---------------
 *   q: xxxx        -6   'a'  "qNs" | q:       xxxx   6   'm'  "sNq"
 *   s:       xxxx                  | s: xxxx      
 *   -------------  --------------- | -------------  ---------------
 *   q:  xxxx       -5   'b'  "qs"  | q:      xxxx    5   'l'  "sq"
 *   s:      xxxx                   | s:  xxxx     
 *   -------------  --------------- | -------------  ---------------
 *   q:   xxxx      -4   'c'  "q=s" | q:     xxxx     4   'k'  "s=q"
 *   s:     xxxx                    | s:   xxxx    
 *   -------------  --------------- | -------------  ---------------
 *   q:   xxxxxx    -3   'd'  "q="  | q:      xxx     3   'j'  "s="
 *   s:      xxx                    | s:   xxxxxx  
 *   -------------  --------------- | -------------  ---------------
 *   q:  xxxxxxxx   -2   'e'  "q=q" | q:    xxxx      2   'i'  "s=s"
 *   s:    xxxx                     | s:  xxxxxxxx
 *   -------------  --------------- | -------------  ---------------
 *   q:   xxx       -1   'f'  "=s"  | q:   xxxxxx     1   'h'  "=q"
 *   s:   xxxxxx                    | s:   xxx
 *   -------------  -------------------------------  ---------------
 *                \   q:   xxxxxx     0   'g'  "="    /
 *                 \  s:   xxxxxx                    /
 *                  \-------------------------------/
 * Notes:
 *   o This way of comparing ranges is a refinement over the standard ranges
 *     comparison defined by the <, >, <=, >=, == and != operators. In
 *     particular a numeric code that is < 0, = 0, or > 0 corresponds to
 *     q < s, q == s, or q > s, respectively.
 *   o In this file we use the term "overlap" in a loose way even when there
 *     is actually no overlap between the query and the subject. Real overlaps
 *     correspond to numeric codes >= -4 and <= 4, and to long codes that
 *     contain an equal ("=").
 *   o Long codes are designed to be user-friendly whereas numeric and
 *     1-letter codes are designed to be more compact and memory efficient.
 *     Typically the formers will be exposed to the end-user and translated
 *     internally into the latters.
 *   o Swapping q and s changes the sign of the corresponding numeric code and
 *     substitutes "q" by "s" and "s" by "q" in the corresponding long code.
 *   o Reflecting ranges q and s relative to an arbitrary position (i.e. doing
 *     a symetry with respect to a vertical axis) has the effect of reversing
 *     the associated long code e.g. "q=s" becomes "s=q".
 *
 * 'q_start', 'q_width', 's_start' and 's_width' are assumed to be non NA.
 * 'q_width' and 's_width' are assumed to be >= 0.
 */
static int overlap_code(int q_start, int q_width, int s_start, int s_width)
{
	int q_end, s_end;

	q_end = q_start + q_width; /* not the real 'q_end' */
	if (q_end < s_start)
		return -6;
	if (q_end == s_start)
		return -5;
	s_end = s_start + s_width; /* not the real 's_end' */
	if (s_end < q_start)
		return 6;
	if (s_end == q_start)
		return 5;
	q_end--; /* the real 'q_end' */
	s_end--; /* the real 's_end' */
	if (q_start < s_start) {
		if (q_end < s_end)
			return -4;
		if (q_end == s_end)
			return -3;
		return -2;
	}
	if (q_start == s_start) {
		if (q_end < s_end)
			return -1;
		if (q_end == s_end)
			return 0;
		return 1;
	}
	if (q_end < s_end)
		return 2;
	if (q_end == s_end)
		return 3;
	return 4;
}


/****************************************************************************
 * "Parallel" generalized comparison of 2 Ranges objects.
 */

void _ranges_pcompare(const int *x_start, const int *x_width, int x_len,
		      const int *y_start, const int *y_width, int y_len,
		      int *out, int out_len, int with_warning)
{
	int i, j, k;

	for (i = j = k = 0; k < out_len; i++, j++, k++) {
		if (i >= x_len)
			i = 0; /* recycle i */
		if (j >= y_len)
			j = 0; /* recycle j */
		out[k] = overlap_code(x_start[i], x_width[i],
				      y_start[j], y_width[j]);
	}
	/* Warning message appropriate only when 'out_len' is
           'max(x_len, y_len)' */
	if (with_warning && out_len != 0 && (i != x_len || j != y_len))
		warning("longer object length is not a multiple "
			"of shorter object length");
	return;
}

/* --- .Call ENTRY POINT ---
 * 'x_start' and 'x_width': integer vectors of the same length M.
 * 'y_start' and 'y_width': integer vectors of the same length N.
 * If M != N then the shorter object is recycled to the length of the longer
 * object, except if M or N is 0 in which case the object with length != 0 is
 * truncated to length 0.
 * The 4 integer vectors are assumed to be NA free and 'x_width' and
 * 'y_width' are assumed to contain non-negative values. For efficiency
 * reasons, those assumptions are not checked.
 */
SEXP Ranges_compare(SEXP x_start, SEXP x_width,
		    SEXP y_start, SEXP y_width)
{
	int m, n, ans_length;
	SEXP ans;

	if (!IS_INTEGER(x_start) || !IS_INTEGER(x_width))
		error("'start(x)' and 'width(x)' must be integer vectors");
	if (LENGTH(x_start) != LENGTH(x_width))
		error("'start(x)' and 'width(x)' must have the same length");
	if (!IS_INTEGER(y_start) || !IS_INTEGER(y_width))
		error("'start(y)' and 'width(y)' must be integer vectors");
	if (LENGTH(y_start) != LENGTH(y_width))
		error("'start(y)' and 'width(y)' must have the same length");
	m = LENGTH(x_start);
	n = LENGTH(y_start);
	if (m == 0 || n == 0)
		ans_length = 0;
	else
		ans_length = m >= n ? m : n;
	PROTECT(ans = NEW_INTEGER(ans_length));
	_ranges_pcompare(INTEGER(x_start), INTEGER(x_width), m,
			 INTEGER(y_start), INTEGER(y_width), n,
			 INTEGER(ans), ans_length, 1);
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Encode "all ranges against all ranges" overlaps between 2 Ranges objects.
 *
 * Comparing all the ranges in a gapped read with all the ranges in a
 * transcript produces a matrix of codes. For example, if the read contains
 * 2 gaps and the transcript has 7 exons, the matrix of 1-letter codes has
 * 3 rows and 7 columns and will typically look like:
 *
 *     A = mjaaaaa   B = mjaaaaa   C = mjaaaaa   D = mmmjaaa   E = mmmiaaa
 *         mmgaaaa       mmgaaaa       mmaaaaa       mmmmmga       mmmkiaa
 *         mmmfaaa       mmmgaaa       mmfaaaa       mmmmmmf       mmmmmmm
 *
 *   A, B: Compatible splicing.
 *   C: Compatible splicing modulo 1 inserted exon (splicing would be
 *      compatible if one exon was inserted between exons #2 and #3).
 *   D: Compatible splicing modulo 1 dropped exon (splicing would be compatible
 *      if exon #5 was dropped).
 *   E: Incompatible splicing. 1st range in the read is within exon #4, 2nd
 *      range overlaps with exon #4 and is within exon #5 (this implies that
 *      exon #4 and #5 overlap), and 3rd range is beyond the bounds of
 *      the transcript (on the downstream side),
 *
 * Note that we make no assumption that the exons in the transcript are
 * ordered from 5' to 3' or non overlapping. They only need to be ordered by
 * ascending *rank*, which most of the time means that they are ordered from
 * 5' to 3' and with non-empty gaps between them (introns). However, this is
 * not always the case. We've seen at least one exception in the transcript
 * annotations provided by UCSC where 2 consecutive exons are overlapping!
 *  
 * Sparse representation: for each row in the matrix of 1-letter codes, report
 * only the sequence between the "m" prefix and the "a" suffix and put the
 * 1-base column index of the first non-"m" letter in front of that. For
 * example, the sparse representation of row "mmmecaa" is "4ec". If there is
 * nothing between the "m" prefix and the "a" suffix, then report the first "a"
 * (or the last "m" if the row contains only "m"'s). Finally paste together the
 * results for all the rows using the ":" separator:
 *
 *     A = 2j:3g:4f  B = 2j:3g:4g  C = 2j:3a:3f  D = 4j:6g:7f  E = 4i:4ki:7m
 *
 * Sparse representation using relative shifts: the 1st row is represented as
 * previously but for each subsequent row the sequence to report is preceded
 * by an horizontal shift relative to the position of the last reported letter
 * plus one. A shift = 0 is represented by an empty string, a shift > 0 by one
 * or more ">" (suggesting a shift to the right), and a shift < 0 by one or
 * more "<" (suggesting a shift to the left):
 *
 *     A = 2j:g:f   B = 2j:g:g   C = 2j:a:<f   D = 4j:>g:f  E = 4i:<ki:>m
 *
 * Using relative shifts makes it easier to use regular expressions. For
 * example, reads with 1 gap and a splicing that is compatible with the
 * transcript can be filtered with regex "^[0-9]+(j|g):(g|f)$"
 */

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

/* Uses special 1-letter code 'X' for ranges that are not on the same space. */
void _enc_overlaps(const int *q_start, const int *q_width,
			const int *q_space, int q_len,
		   const int *s_start, const int *s_width,
			const int *s_space, int s_len,
		   int sparse_output, CharAE *out)
{
	int i, starti, widthi, spacei, j, startj, widthj, spacej,
	    j1, j2, shift;
	char code;

	if (sparse_output && (q_len == 0 || s_len == 0))
		error("sparse output not supported when "
		      "query or subject have length 0");
	j2 = 0;
	for (i = 0; i < q_len; i++) {
		if (sparse_output && i != 0)
			CharAE_append_char(out, ':', 1);
		/* j1: 1-base column index of the first letter of the sequence
		 * to report when in sparse mode. Concisely defined as the last
		 * position in the row that is preceded by "m"'s only.
		 * j2: 1-base column index of the last letter of the sequence
		 * to report when in sparse mode. Concisely defined as the
		 * first position in the row that is >= j1 and followed by
		 * "a"'s only.
		 * When we exit the for (j = 0; ...) loop below, we have the
		 * guarantee that 1 <= j1 <= j2 <= s_len.
		 */
		j1 = 0;
		starti = q_start[i];
		widthi = q_width[i];
		spacei = q_space == NULL ? 0 : q_space[i];
		for (j = 0; j < s_len; j++) {
			startj = s_start[j];
			widthj = s_width[j];
			spacej = s_space == NULL ? 0 : s_space[j];
			if (spacei == -1 || spacej == -1 || spacei != spacej)
				code = 'X';
			else
				code = 'g' + overlap_code(starti, widthi,
							  startj, widthj);
			if (sparse_output && j1 == 0) {
				if (code == 'm' && j + 1 < s_len)
					continue;
				j1 = j + 1;
				if (j2 == 0) {
					CharAE_append_int(out, j1);
				} else {
					shift = j1 - j2 - 1;
					if (shift >= 0)
						CharAE_append_char(out, '>',
								   shift);
					else
						CharAE_append_char(out, '<',
								   -shift);
				}
				j2 = j1;
			}
			CharAE_append_char(out, code, 1);
			if (sparse_output && code != 'a')
				j2 = j + 1;
		}
		if (sparse_output) {
			/* Remove trailing "a"'s */
			_CharAE_set_nelt(out, _CharAE_get_nelt(out) -
					      (s_len - j2));
		}
	}
	return;
}

SEXP _enc_overlaps1(SEXP query_start, SEXP query_width, SEXP query_space,
		    SEXP subject_start, SEXP subject_width, SEXP subject_space,
		    int sparse_output, int as_raw)
{
	int m, n, i;
	const int *q_space, *s_space;
	CharAE buf;
	SEXP ans, ans_elt, ans_dim;

	/* Check 'query_start', 'query_width' and 'query_space'. */
	if (!IS_INTEGER(query_start) || !IS_INTEGER(query_width))
		error("'start(query)' and 'width(query)' must be "
		      "integer vectors");
	if (LENGTH(query_start) != LENGTH(query_width))
		error("'start(query)' and 'width(query)' must have "
		      "the same length");
	m = LENGTH(query_start);
	if (query_space == R_NilValue) {
		q_space = NULL;
	} else if (!IS_INTEGER(query_space)) {
		error("'query_space' must be an integer vector or NULL");
	} else if (LENGTH(query_space) != m) {
		error("when not NULL, 'query_space' must have "
		      "the same length as 'start(query)'");
	} else {
		q_space = INTEGER(query_space);
	}

	/* Check 'subject_start', 'subject_width' and 'subject_space'. */
	if (!IS_INTEGER(subject_start) || !IS_INTEGER(subject_width))
		error("'start(subject)' and 'width(subject)' must be "
		      "integer vectors");
	if (LENGTH(subject_start) != LENGTH(subject_width))
		error("'start(subject)' and 'width(subject)' must have "
		      "the same length");
	n = LENGTH(subject_start);
	if (subject_space == R_NilValue) {
		s_space = NULL;
	} else if (!IS_INTEGER(subject_space)) {
		error("'subject_space' must be an integer vector or NULL");
	} else if (LENGTH(subject_space) != n) {
		error("when not NULL, 'subject_space' must have "
		      "the same length as 'start(subject)'");
	} else {
		s_space = INTEGER(subject_space);
	}

	if (sparse_output || as_raw) {
		if (!sparse_output) {
			/* FIXME: Risk of integer overflow! */
			buf = _new_CharAE(m * n);
		} else {
			buf = _new_CharAE(0);
		}
		_enc_overlaps(INTEGER(query_start), INTEGER(query_width),
				q_space, m,
			      INTEGER(subject_start), INTEGER(subject_width),
				s_space, n,
			      sparse_output, &buf);
		if (!as_raw)
			return mkCharLen(buf.elts, _CharAE_get_nelt(&buf));
		PROTECT(ans = _new_RAW_from_CharAE(&buf));
		if (!sparse_output) {
			PROTECT(ans_dim	= NEW_INTEGER(2));
			INTEGER(ans_dim)[0] = n;
			INTEGER(ans_dim)[1] = m;
			SET_DIM(ans, ans_dim);
			UNPROTECT(1);
		}
		UNPROTECT(1);
		return ans;
	}
	PROTECT(ans = NEW_CHARACTER(m));
	buf = _new_CharAE(n);
	for (i = 0; i < m; i++) {
		_enc_overlaps(
			INTEGER(query_start) + i, INTEGER(query_width) + i,
			  q_space, 1,
			INTEGER(subject_start), INTEGER(subject_width),
			  s_space, n,
			0, &buf);
		PROTECT(ans_elt = mkCharLen(buf.elts, _CharAE_get_nelt(&buf)));
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
		_CharAE_set_nelt(&buf, 0);
		if (q_space != NULL)
			q_space++;
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT ---
 * 'query_start', 'query_width', 'query_space': integer vectors of the same
 * length M (or NULL for 'query_space').
 * 'subject_start', 'subject_width', 'subject_space': integer vectors of the
 * same length N (or NULL for 'subject_space').
 * Integer vectors 'query_start', 'query_width', 'subject_start' and
 * 'subject_width' are assumed to be NA free. 'query_width' and 'subject_width'
 * are assumed to contain non-negative values. For efficiency reasons, those
 * assumptions are not checked.
 * Returns the matrix of 1-letter codes (if 'sparse_output' is FALSE), or the
 * sparse format with relative shifts (if 'sparse_output' is TRUE) in which
 * case both M and N must be != 0.
 */
SEXP encode_overlaps1(SEXP query_start, SEXP query_width,
			SEXP query_space,
		      SEXP subject_start, SEXP subject_width,
			SEXP subject_space,
		      SEXP sparse_output, SEXP as_raw)
{
	int sparse0, as_raw0;
	SEXP ans;

	sparse0 = sparse_output == R_NilValue || LOGICAL(sparse_output)[0];
	as_raw0 = as_raw != R_NilValue && LOGICAL(as_raw)[0];
	PROTECT(ans = _enc_overlaps1(query_start, query_width, query_space,
				subject_start, subject_width, subject_space,
				sparse0, as_raw0));
	if (sparse0 && !as_raw0)
		ans = ScalarString(ans);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT ---
 * 'query_starts', 'query_widths', 'query_spaces': lists of integer vectors
 * of the same length (M) and shape.
 * 'subject_starts', 'subject_widths', 'subject_spaces': lists of integer
 * vectors of the same length (N) and shape.
 */
SEXP RangesList_encode_overlaps(SEXP query_starts, SEXP query_widths,
				SEXP query_spaces,
				SEXP subject_starts, SEXP subject_widths,
				SEXP subject_spaces)
{
	int m, n, ans_length, i, j, k;
	SEXP query_start, query_width, query_space,
	     subject_start, subject_width, subject_space,
	     ans, ans_elt;

	/* TODO: Some basic check of the input values. */
	m = LENGTH(query_starts);
	n = LENGTH(subject_starts);
	if (m == 0 || n == 0)
		return NEW_CHARACTER(0);
	ans_length = m >= n ? m : n;
	PROTECT(ans = NEW_CHARACTER(ans_length));
	query_space = subject_space = R_NilValue;
	for (i = j = k = 0; k < ans_length; i++, j++, k++) {
		if (i >= m)
			i = 0; /* recycle i */
		if (j >= n)
			j = 0; /* recycle j */
		query_start = VECTOR_ELT(query_starts, i);
		query_width = VECTOR_ELT(query_widths, i);
		if (query_spaces != R_NilValue)
			query_space = VECTOR_ELT(query_spaces, i);
		subject_start = VECTOR_ELT(subject_starts, j);
		subject_width = VECTOR_ELT(subject_widths, j);
		if (subject_spaces != R_NilValue)
			subject_space = VECTOR_ELT(subject_spaces, j);
		PROTECT(ans_elt = _enc_overlaps1(
					query_start,
					query_width,
					query_space,
					subject_start,
					subject_width,
					subject_space,
					1, 0));
		SET_STRING_ELT(ans, k, ans_elt);
		UNPROTECT(1);
	}
	if (i != m || j != n)
		warning("longer object length is not a multiple "
			"of shorter object length");
	UNPROTECT(1);
	return ans;
}


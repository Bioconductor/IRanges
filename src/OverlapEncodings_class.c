#include "IRanges.h"


/****************************************************************************
 * Generalized comparison of 2 integer ranges.
 *
 * TODO: This should probably go somewhere else.
 *
 * There are 13 different ways 2 integer ranges x and y can be positioned
 * with respect to each other. They are summarized in the following table
 * together with the codes we assign them:
 *
 *                   numeric code & |                 numeric code &
 *                  1-letter code & |                1-letter code &
 *                        long code |                      long code
 *   -------------  --------------- | -------------  ---------------
 *   x: oooo        -6   'a'  "xNy" | x:       oooo   6   'm'  "yNx"
 *   y:       oooo                  | y: oooo      
 *   -------------  --------------- | -------------  ---------------
 *   x:  oooo       -5   'b'  "xy"  | x:      oooo    5   'l'  "yx"
 *   y:      oooo                   | y:  oooo     
 *   -------------  --------------- | -------------  ---------------
 *   x:   oooo      -4   'c'  "x=y" | x:     oooo     4   'k'  "y=x"
 *   y:     oooo                    | y:   oooo    
 *   -------------  --------------- | -------------  ---------------
 *   x:   oooooo    -3   'd'  "x="  | x:      ooo     3   'j'  "y="
 *   y:      ooo                    | y:   oooooo  
 *   -------------  --------------- | -------------  ---------------
 *   x:  oooooooo   -2   'e'  "x=x" | x:    oooo      2   'i'  "y=y"
 *   y:    oooo                     | y:  oooooooo
 *   -------------  --------------- | -------------  ---------------
 *   x:   ooo       -1   'f'  "=y"  | x:   oooooo     1   'h'  "=x"
 *   y:   oooooo                    | y:   ooo
 *   -------------  -------------------------------  ---------------
 *                \   x:   oooooo     0   'g'  "="    /
 *                 \  y:   oooooo                    /
 *                  \-------------------------------/
 * Notes:
 *   o This way of comparing ranges is a refinement over the standard ranges
 *     comparison defined by the <, >, <=, >=, == and != operators. In
 *     particular a numeric code that is < 0, = 0, or > 0 corresponds to
 *     x < y, x == y, or x > y, respectively.
 *   o In this file we use the term "overlap" in a loose way even when there
 *     is actually no overlap between ranges x and y. Real overlaps correspond
 *     to numeric codes >= -4 and <= 4, and to long codes that contain an
 *     equal ("=").
 *   o Long codes are designed to be user-friendly whereas numeric and
 *     1-letter codes are designed to be more compact and memory efficient.
 *     Typically the formers will be exposed to the end-user and translated
 *     internally into the latters.
 *   o Swapping x and y changes the sign of the corresponding numeric code and
 *     substitutes "x" by "y" and "y" by "x" in the corresponding long code.
 *   o Reflecting ranges x and y relative to an arbitrary position (i.e. doing
 *     a symetry with respect to a vertical axis) has the effect of reversing
 *     the associated long code e.g. "x=y" becomes "y=x".
 *
 * 'x_start', 'x_width', 'y_start' and 'y_width' are assumed to be non NA (not
 * checked). 'x_start' and 'y_start' must be 1-based. 'x_width' and 'y_width'
 * are assumed to be >= 0 (not checked).
 */
static int overlap_code(int x_start, int x_width, int y_start, int y_width)
{
	int x_end_plus1, y_end_plus1;

	x_end_plus1 = x_start + x_width;
	if (x_end_plus1 < y_start)
		return -6;
	if (x_end_plus1 == y_start)
		return -5;
	y_end_plus1 = y_start + y_width;
	if (y_end_plus1 < x_start)
		return 6;
	if (y_end_plus1 == x_start)
		return 5;
	if (x_start < y_start) {
		if (x_end_plus1 < y_end_plus1)
			return -4;
		if (x_end_plus1 == y_end_plus1)
			return -3;
		return -2;
	}
	if (x_start == y_start) {
		if (x_end_plus1 < y_end_plus1)
			return -1;
		if (x_end_plus1 == y_end_plus1)
			return 0;
		return 1;
	}
	if (x_end_plus1 < y_end_plus1)
		return 2;
	if (x_end_plus1 == y_end_plus1)
		return 3;
	return 4;
}

/* "Parallel" generalized comparison of 2 Ranges objects. */
static void _ranges_pcompare(
		const int *x_start, const int *x_width, int x_len,
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
 * Comparing the M ranges in a gapped read with the N ranges in a transcript
 * produces a M x N matrix of 1-letter codes. We call this matrix an OVM
 * ("overlaps matrix"). For example, if the read contains 2 gaps and the
 * transcript has 7 exons, the OVM has 3 rows and 7 columns and would
 * typically look like:
 *
 *      A = mjaaaaa   B = mjaaaaa   C = mjaaaaa   D = mmmjaaa   E = mmmiaaa
 *          mmgaaaa       mmgaaaa       mmaaaaa       mmmmmga       mmmkiaa
 *          mmmfaaa       mmmgaaa       mmfaaaa       mmmmmmf       mmmmmmm
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
 * 5' to 3' (or 3' to 5') and with non-empty gaps (introns) between them.
 * However, this is not always the case: we've seen at least one exception in
 * the transcript annotations provided by UCSC where 2 consecutive exons are
 * overlapping!
 * We call "normal" a transcript where the exons ordered by rank are also
 * ordered from 5' to 3' or 3' to 5' and with non-empty gaps between them.
 *
 * We need to encode OVMs in a way that is compact and more "regex friendly"
 * i.e. more suitable for detection of different read-to-transcript splicing
 * situations with regular expressions.
 * We are currently considering 2 types of encodings but this is still a
 * work in progress and it's not clear at the moment which encoding is best.
 * The 2 encodings share the same first step where we trim the OVM by removing
 * cols on its left that contain only "m"'s and cols on its right that contain
 * only "a"'s. We call Loffset ("left offset") and Roffset ("right offset")
 * the numbers of cols removed on the left and on the right, respectively:
 *
 *       Loffset   Roffset
 *   A         1         3
 *   B         1         3
 *   C         1         4
 *   D         3         0
 *   E         3         0
 *
 * Then we encode the trimmed OVM (M x n matrix). The 2 encodings described
 * below don't loose information i.e. the OVM can always be reconstructed
 * from Loffset, Roffset, and the encoding.
 *
 * Type II encoding
 * ----------------
 *
 *   1) The trimmed OVM is walked row by row from the top left to the bottom
 *      right.
 *   2) For each row, the sequence between the m-prefix and the a-suffix is
 *      reported with curly brackets ("{" and "}") placed around it.
 *   3) The 2 sequences S(i) and S(i+1) reported for 2 consecutive rows (rows
 *      i and i+1) are separated by a number of ">"'s or "<"'s indicating the
 *      horizontal gap between the end of S(i) and the start of S(i+1).
 *      A null, positive or negative gap is represented by an empty string,
 *      one or more ">"'s (suggesting a shift to the right), or one or more
 *      "<"'s (suggesting a shift to the left), respectively.
 *   4) The length of the m-prefix in the first row and the length of the
 *      a-suffix in the last row are called the initial and final gaps,
 *      respectively. They are reported at the beginning and end of the linear
 *      sequence, respectively.
 *
 *          Loffset   Roffset   Type II encoding
 *      A         1         3   "{j}{g}{f}"
 *      B         1         3   "{j}{g}{g}"
 *      C         1         4   "{j}{}{f}"
 *      D         3         0   "{j}>{g}{f}"
 *      E         3         0   "{i}<{ki}>>{}"
 *
 *   In the best case (no negative gaps), the length of this linear sequence
 *   is 2M + n. If there is at least one negative gap, then the sequence is
 *   longer. In the worst case (no m-prefix and no a-suffix in any of the
 *   rows), then its length is 2M + n*(2M-1).
 *
 *   Examples of regular expressions that can be used on this encoding:
 *
 *     a. For detecting reads with 1 gap and a splicing that is compatible
 *        with the transcript:
 *            ^\{[jg]\}\{[gf]\}$
 *        Note: replace single backslash by double backslash when putting this
 *        in a character string in R for use with grep().
 *
 *     b. For detecting reads (with or without gaps) with a splicing that is
 *        compatible with the transcript:
 *            ^(\{[i]\}|(\{j\})?(\{g\})*(\{f\})?)$
 *
 *     c. For detecting reads with a splicing that is not compatible with the
 *        transcript but that would be compatible if one exon was dropped:
 *            ^\{[jg]\}(\{g\})*>(\{g\})*\{[gf]\}$
 *
 *     Problem: those regex work only on OVMs that have at most 1 letter on
 *     each row between the m-prefix and the a-suffix. This is not guaranteed
 *     to be the case for a transcript that is not "normal".
 *
 * Type III encoding
 * -----------------
 *
 * Coming soon...
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

/* Type I encoding is obsolete. encodeI() only used with full_OVM=1 to
   generate the full OVM matrix. */
static void encodeI(
		const int *q_start, const int *q_width,
		const int *q_space, int q_len,
		const int *s_start, const int *s_width,
		const int *s_space, int s_len,
		int full_OVM, CharAE *out)
{
	int i, starti, widthi, spacei, j, startj, widthj, spacej,
	    j1, j2, shift;
	char code;

	if (!full_OVM && (q_len == 0 || s_len == 0))
		error("Type I encoding not supported when "
		      "query or subject have no ranges");
	j2 = 0;
	for (i = 0; i < q_len; i++) {
		if (!full_OVM && i != 0)
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
			if (!full_OVM && j1 == 0) {
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
			if (!full_OVM && code != 'a')
				j2 = j + 1;
		}
		if (!full_OVM) {
			/* Remove trailing "a"'s */
			_CharAE_set_nelt(out, _CharAE_get_nelt(out) -
					      (s_len - j2));
		}
	}
	return;
}

/* Uses special 1-letter code 'X' for ranges that are not on the same space. */
/*
static void encodeII(
		const int *q_start, const int *q_width,
		const int *q_space, int q_len,
		const int *s_start, const int *s_width,
		const int *s_space, int s_len,
		int full_OVM, int *Loffset, int *Roffset, CharAE *out)
{
	return;
}
*/

/* Uses special 1-letter code 'X' for ranges that are not on the same space. */
static void encodeIII(
		const int *q_start, const int *q_width,
		const int *q_space, int q_len,
		const int *s_start, const int *s_width,
		const int *s_space, int s_len,
		int *Loffset, int *Roffset, CharAE *out)
{
	int i, starti, widthi, spacei, j, startj, widthj, spacej, j1, j2;
	char code;

	if (q_len == 0 && s_len != 0)
		error("Type III encoding not supported when "
		      "query has no ranges but subject has ranges");
	j1 = j2 = -1;
	/* Walk col by col */
	for (j = 0; j < s_len; j++) {
		startj = s_start[j];
		widthj = s_width[j];
		spacej = s_space == NULL ? 0 : s_space[j];
		for (i = 0; i < q_len; i++) {
			starti = q_start[i];
			widthi = q_width[i];
			spacei = q_space == NULL ? 0 : q_space[i];
			if (spacei == -1 || spacej == -1 || spacei != spacej)
				code = 'X';
			else
				code = 'g' + overlap_code(starti, widthi,
							  startj, widthj);
			CharAE_append_char(out, code, 1);
			/* j1 = first col where a non-"m" is seen */
			if (j1 == -1 && code != 'm')
				j1 = j;
			/* j2 = last col where a non-"a" was seen */
			if (code != 'a')
				j2 = j;
		}
	}
	if (j1 == -1)
		j1 = s_len;
	j2++;
	/* Remove cols on the right. */
	_CharAE_set_nelt(out, q_len * j2);
	/* Remove cols on the left. */
	_CharAE_delete_at(out, 0, q_len * j1);
	*Loffset = j1;
	*Roffset = s_len - j2;
	/* Insert ":" between remaining cols and before the 1st and after the
	   last column. */
	for (j = j2 - j1; j >= 0; j--)
		_CharAE_insert_at(out, j * q_len, ':');
	return;
}

static SEXP _enc_overlaps1(
		SEXP query_start, SEXP query_width, SEXP query_space,
		SEXP subject_start, SEXP subject_width, SEXP subject_space,
		int sparse_output, int as_raw)
{
	int m, n, Loffset, Roffset, i;
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
		/* FIXME: Risk of integer overflow! */
		buf = _new_CharAE(m * n);
		if (sparse_output) {
			encodeIII(INTEGER(query_start), INTEGER(query_width),
				q_space, m,
				INTEGER(subject_start), INTEGER(subject_width),
				s_space, n,
				&Loffset, &Roffset, &buf);
		} else {
			encodeI(INTEGER(query_start), INTEGER(query_width),
				q_space, m,
				INTEGER(subject_start), INTEGER(subject_width),
				s_space, n,
				1, &buf);
		}
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
		encodeI(INTEGER(query_start) + i, INTEGER(query_width) + i,
			q_space, 1,
			INTEGER(subject_start), INTEGER(subject_width),
			s_space, n,
			1, &buf);
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


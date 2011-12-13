#include "IRanges.h"

/*
 * There are 13 different ways 2 ranges can be positioned with respect to each
 * other. They are described in the table below together with their associated
 * codes. The 1st range is called the "query" range (q) and the 2nd range the
 * "subject" range (s).
 *
 *                   numeric code & |                 numeric code &
 *                  1-letter code & |                1-letter code &
 *                        long code |                      long code
 *   -------------  --------------- | -------------  ---------------
 *   q: xxxx         0   'a'  "qNs" | q:       xxxx  12   'm'  "sNq"
 *   s:       xxxx                  | s: xxxx      
 *   -------------  --------------- | -------------  ---------------
 *   q:  xxxx        1   'b'  "qs"  | q:      xxxx   11   'l'  "sq"
 *   s:      xxxx                   | s:  xxxx     
 *   -------------  --------------- | -------------  ---------------
 *   q:   xxxx       2   'c'  "q=s" | q:     xxxx    10   'k'  "s=q"
 *   s:     xxxx                    | s:   xxxx    
 *   -------------  --------------- | -------------  ---------------
 *   q:   xxxxxx     3   'd'  "q="  | q:      xxx     9   'j'  "s="
 *   s:      xxx                    | s:   xxxxxx  
 *   -------------  --------------- | -------------  ---------------
 *   q:  xxxxxxxx    4   'e'  "q=q" | q:    xxxx      8   'i'  "s=s"
 *   s:    xxxx                     | s:  xxxxxxxx
 *   -------------  --------------- | -------------  ---------------
 *   q:   xxx        5   'f'  "=s"  | q:   xxxxxx     7   'h'  "=q"
 *   s:   xxxxxx                    | s:   xxx
 *   -------------  -------------------------------  ---------------
 *                \   q:   xxxxxx     6   'g'  "="    /
 *                 \  s:   xxxxxx                    /
 *                  \-------------------------------/
 * Notes:
 *   o Long codes are designed to be user-friendly whereas numeric and
 *     1-letter codes are designed to be more compact and memory efficient.
 *     Typically the formers will be exposed to the end-user and translated
 *     internally into the latters.
 *   o In this file we use the term "overlap" in a loose way even when there
 *     is actually no overlap between the query and the subject. Real overlaps
 *     correspond to numeric codes 2-10 and to long codes that contain an
 *     equal ("=").
 *   o Inverting the order of q and s has the effect to replace numeric code
 *     C by 12 - C and to substitute "q" by "s" and "s" by "q" in the long
 *     code.
 *   o Reflecting ranges q and s relative to an arbitrary position (i.e. doing
 *     a symetry with respect to a vertical axis) has the effect to reverse
 *     the associated long code.
 *
 * 'q_start', 'q_width', 's_start' and 's_width' are assumed to be non NA.
 * 'q_width' and 's_width' are assumed to be >= 0.
 */
static int overlap_code(int q_start, int q_width, int s_start, int s_width)
{
	int q_end, s_end;

	q_end = q_start + q_width;  /* not the real 'q_end' */
	if (q_end < s_start)
		return 0;
	if (q_end == s_start)
		return 1;
	s_end = s_start + s_width;  /* not the real 's_end' */
	if (s_end < q_start)
		return 12;
	if (s_end == q_start)
		return 11;
	q_end--;  /* the real 'q_end' */
	s_end--;  /* the real 's_end' */
	if (q_start < s_start) {
		if (q_end < s_end)
			return 2;
		if (q_end == s_end)
			return 3;
		return 4;
	}
	if (q_start == s_start) {
		if (q_end < s_end)
			return 5;
		if (q_end == s_end)
			return 6;
		return 7;
	}
	if (q_end < s_end)
		return 8;
	if (q_end == s_end)
		return 9;
	return 10;
}


/****************************************************************************
 * Encode "parallel" overlaps.
 */

void _enc_poverlaps(const int *q_start, const int *q_width, int q_len,
		    const int *s_start, const int *s_width, int s_len,
		    char *out)
{
	int i, out_len;

	out_len = q_len >= s_len ? q_len : s_len;
	for (i = 0; i < out_len; i++, out++) {
		*out = 'a' + overlap_code(*q_start, *q_width,
					  *s_start, *s_width);
		if (q_len != 1) {
			q_start++;
			q_width++;
		}
		if (s_len != 1) {
			s_start++;
			s_width++;
		}
	}
	return;	
}

/* --- .Call ENTRY POINT ---
 * 'query_start' and 'query_width': integer vectors of the same length M.
 * 'subject_start' and 'subject_width': integer vectors of the same length N.
 * If M != N then either M or N must be 1. If M is 1, then N must be > M and
 * 'query_start' and 'query_width' are recycled to length N. If N is 1, then
 * M must be > N and 'subject_start' and 'subject_width' are recycled to
 * length M.
 * The 4 integer vectors are assumed to be NA free and 'query_width' and
 * 'subject_width' are assumed to contain non-negative values. For efficiency
 * reasons, those assumptions are not checked.
 */
SEXP encode_poverlaps(SEXP query_start, SEXP query_width,
		      SEXP subject_start, SEXP subject_width)
{
	int m, n, ans_length;
	SEXP ans;

	if (!IS_INTEGER(query_start) || !IS_INTEGER(query_width)
	 || !IS_INTEGER(subject_start) || !IS_INTEGER(subject_width))
		error("'query_start', 'query_width', 'subject_start' "
		      "and 'subject_width' must be integer vectors");
	if (LENGTH(query_start) != LENGTH(query_width))
		error("'query_start' and 'query_width' must have "
		      "the same length");
	if (LENGTH(subject_start) != LENGTH(subject_width))
		error("'subject_start' and 'subject_width' must have "
		      "the same length");
	m = LENGTH(query_start);
	n = LENGTH(subject_start);
	ans_length = m;
	if (m != n) {
		if (m == 0 || n == 0)
			error("when query or subject has length 0, "
			      "both of them must have length 0");
		if (m == 1)
			ans_length = n;
		else if (n != 1)
			error("when lengths of query and subject are "
			      "different, one of them must have length 1");
	}
	PROTECT(ans = NEW_RAW(ans_length));
	_enc_poverlaps(INTEGER(query_start), INTEGER(query_width), m,
		       INTEGER(subject_start), INTEGER(subject_width), n,
		       (char *) RAW(ans));
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Encode "all against all" overlaps.
 *
 * Comparing all the ranges in a gapped read with all the ranges in a
 * transcript produces a matrix of codes. For example, if the read contains
 * 2 gaps and the transcript has 7 exons, the matrix of 1-letter codes has
 * 3 rows and 7 columns and will typically look like:
 *
 *   A = mjaaaaa    B = mjaaaaa    C = mjaaaaa    D = mmmjaaa    E = miaaaaa
 *       mmgaaaa        mmgaaaa        mmaaaaa        mmmmmga        miaaaaa
 *       mmmfaaa        mmmgaaa        mmfaaaa        mmmmmmf        mmmecaa
 *
 *    compatible     compatible       splicing       splicing      3rd range
 *      splicing       splicing       would be       would be    in the read
 *                                  compatible     compatible         covers
 *                                 if one exon     if exon #5    exon #4 and
 *                                was inserted    was dropped      partially
 *                               between exons                        covers
 *                                   #2 and #3                       exon #5
 *
 * Note that we make no assumption that the exons in the transcript are
 * ordered from 5' to 3' or non overlapping. They only need to be ordered by
 * ascending *rank*, which most of the time means that they are ordered from
 * 5' to 3' and with non-empty gaps between them (introns). However, this is
 * not always the case. We've seen at least one exception in the transcript
 * annotations provided by UCSC where 2 consecutive exons are overlapping!
 *  
 * Sparse representation: for each row, report only the sequence between the
 * "m" prefix and the "a" suffix. Then put the col nb of the first non-m
 * letter in front of that. For example, the sparse representation of row
 * "mmmecaa" is "4ec". If there is nothing between the "m" prefix and the "a"
 * suffix, then report the first "a". Finally paste together the results for
 * all the rows:
 *
 *   A = 2j3g4f     B = 2j3g4g     C = 2j3a3f     D = 4j6g7f     E = 2i2i4ec
 *
 * Sparse representation using Global Offset and Cumulative Shifts (a shift
 * can be either one or more "<", or "=", or one or more ">"):
 *
 *   A = 2:j<g<f    B = 2:j<g<g    C = 2:j<a=f    D = 4:j<<g<f   E = 2:i=i<<ec
 *
 * The advantage of the GOCS string over the non-GOCS string is that it's
 * easier to use regular expressions on the former. For example, reads with
 * 1 gap and a splicing that is compatible with the transcript can be filtered
 * with regex ":(j|g)<(g|f)$"
 */

int _enc_overlaps_as_GOCS(const int *q_start, const int *q_width, int q_len,
			  const int *s_start, const int *s_width, int s_len,
			  char *out)
{
	int out_len, i, j;
	char code;

	out_len = 0;
	/* Producing the full matrix instead of the GOCS string for now.
         * FIXME: Produce the GOCS string. */
	for (i = 0; i < q_len; i++) {
		for (j = 0; j < s_len; j++) {
			code = 'a' + overlap_code(q_start[i], q_width[i],
						  s_start[j], s_width[j]);
			out[out_len++] = code;
		}
	}
	return out_len;
}

/* --- .Call ENTRY POINT ---
 * 'query_start' and 'query_width': integer vectors of the same length M.
 * 'subject_start' and 'subject_width': integer vectors of the same length N
 * M or N cannot be 0.
 * The 4 integer vectors are assumed to be NA free and 'query_width' and
 * 'subject_width' are assumed to contain non-negative values. For efficiency
 * reasons, those assumptions are not checked.
 * Returns the corresponding GOCS string in a raw vector.
 */
SEXP overlaps_to_GOCS(SEXP query_start, SEXP query_width,
		      SEXP subject_start, SEXP subject_width)
{
	int m, n, ans_length;
	SEXP ans;

	if (!IS_INTEGER(query_start) || !IS_INTEGER(query_width)
	 || !IS_INTEGER(subject_start) || !IS_INTEGER(subject_width))
		error("'query_start', 'query_width', 'subject_start' "
		      "and 'subject_width' must be integer vectors");
	if (LENGTH(query_start) != LENGTH(query_width))
		error("'query_start' and 'query_width' must have "
		      "the same length");
	if (LENGTH(subject_start) != LENGTH(subject_width))
		error("'subject_start' and 'subject_width' must have "
		      "the same length");
	m = LENGTH(query_start);
	n = LENGTH(subject_start);
	if (m == 0 || n == 0)
		error("query or subject cannot have length 0");
	ans_length = m * n;
	PROTECT(ans = NEW_RAW(ans_length));
	_enc_overlaps_as_GOCS(INTEGER(query_start), INTEGER(query_width), m,
			      INTEGER(subject_start), INTEGER(subject_width), n,
			      (char *) RAW(ans));
	UNPROTECT(1);
	return ans;
}


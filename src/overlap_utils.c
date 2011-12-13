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
 *   ~~~~~~~~~~~~~  ~~~~~~~~~~~~~~~ | ~~~~~~~~~~~~~  ~~~~~~~~~~~~~~~
 *   q: xxxx         0   'a'  "qNs" | q:       xxxx  12   'm'  "sNq"
 *   s:       xxxx                  | s: xxxx      
 *                                  |
 *   q:  xxxx        1   'b'  "qs"  | q:      xxxx   11   'l'  "sq"
 *   s:      xxxx                   | s:  xxxx     
 *                                  |
 *   q:   xxxx       2   'c'  "q=s" | q:     xxxx    10   'k'  "s=q"
 *   s:     xxxx                    | s:   xxxx    
 *                                  |
 *   q:   xxxxxx     3   'd'  "q="  | q:      xxx     9   'j'  "s="
 *   s:      xxx                    | s:   xxxxxx  
 *                                  |
 *   q:  xxxxxxxx    4   'e'  "q=q" | q:    xxxx      8   'i'  "s=s"
 *   s:    xxxx                     | s:  xxxxxxxx
 *                                  |
 *   q:   xxx        5   'f'  "=s"  | q:   xxxxxx     7   'h'  "=q"
 *   s:   xxxxxx                    | s:   xxx
 *                                  |
 *   q:   xxxxxx     6   'g'  "="   |
 *   s:   xxxxxx                    |
 *
 * Notes:
 *   (a) Overlaps correspond to numeric codes 2-10 and to long codes that
 *       contain an equal ("=").
 *   (b) Inverting the order of q and s has the effect to replace numeric code
 *       C by 12 - C and to substitute "q" by "s" and "s" by "q" in the long
 *       code.
 *   (c) Reflecting ranges q and s relative to an arbitrary position (i.e.
 *       doing a symetry with respect to a vertical axis) has the effect to
 *       reverse the associated long code.
 */

/*
 * 'q_start', 'q_width', 's_start' and 's_width' are assumed to be non NA.
 * 'q_width' and 's_width' are assumed to be >= 0.
 */
static char enc_overlap(int q_start, int q_width, int s_start, int s_width)
{
	int q_end, s_end;

	q_end = q_start + q_width;  /* not the real q_end */
	if (q_end < s_start)
		return 'a';
	if (q_end == s_start)
		return 'b';
	s_end = s_start + s_width; /* not the real s_end */
	if (s_end < q_start)
		return 'm';
	if (s_end == q_start)
		return 'l';
	q_end--;  /* the real q_end */
	s_end--;  /* the real s_end */
	if (q_start < s_start) {
		if (q_end < s_end)
			return 'c';
		if (q_end == s_end)
			return 'd';
		return 'e';
	}
	if (q_start == s_start) {
		if (q_end < s_end)
			return 'f';
		if (q_end == s_end)
			return 'g';
		return 'h';
	}
	if (q_end < s_end)
		return 'i';
	if (q_end == s_end)
		return 'j';
	return 'k';
}

void _enc_overlaps(const int *q_start, const int *q_width, int q_len,
		   const int *s_start, const int *s_width, int s_len,
		   char *out)
{
	int i, out_len;

	out_len = q_len >= s_len ? q_len : s_len;
	for (i = 0; i < out_len; i++, out++) {
		*out = enc_overlap(*q_start, *q_width, *s_start, *s_width);
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
 */
SEXP encode_overlaps(SEXP query_start, SEXP query_width,
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
	if (m == n) {
		ans_length = m;
	} else if (m == 0 || n == 0) {
		error("when query or subject has length 0, both of them "
                      "must have length 0");
	} else if (m == 1) {
		ans_length = n;
	} else if (n == 1) {
		ans_length = m;
	} else {
		error("when lengths of query and subject are different, "
		      "one of them must have length 1");
	}
	PROTECT(ans = NEW_RAW(ans_length));
	_enc_overlaps(INTEGER(query_start), INTEGER(query_width), m,
		      INTEGER(subject_start), INTEGER(subject_width), n,
		      (char *) RAW(ans));
	UNPROTECT(1);
	return ans;
}


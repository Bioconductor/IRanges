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
 *     A = mjaaaaa   B = mjaaaaa   C = mjaaaaa   D = mmmjaaa   E = aaaaaaa
 *         mmgaaaa       mmgaaaa       mmaaaaa       mmmmmga       mmmiaaa
 *         mmmfaaa       mmmgaaa       mmfaaaa       mmmmmmf       mmmkiaa
 *
 *   A, B: Compatible splicing.
 *   C: Compatible splicing modulo 1 inserted exon (splicing would be
 *      compatible if one exon was inserted between exons #2 and #3).
 *   D: Compatible splicing modulo 1 dropped exon (splicing would be compatible
 *      if exon #5 was dropped).
 *   E: Incompatible splicing. 1st range in the read is beyond the bounds of
 *      the transcript (on the upstream side), 2nd range is within exon #4,
 *      and 3rd range overlaps with exon #4 and is within exon #5. This implies
 *      that exon #4 and #5 overlap.
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
 * col nb of the first non-m letter in front of that. For example, the sparse
 * representation of row "mmmecaa" is "4ec". If there is nothing between the
 * "m" prefix and the "a" suffix, then report the first "a" (or the last "m"
 * if the row contains only "m"'s). Finally paste together the results for all
 * the rows:
 *
 *     A = 2j3g4f    B = 2j3g4g    C = 2j3a3f    D = 4j6g7f    E = 1a4i4ki
 *
 * Sparse representation using One Global Offset and Cumulative Shifts (a shift
 * can be either one or more "<", or "=", or one or more ">"):
 *
 *     A = 2:j<g<f   B = 2:j<g<g   C = 2:j<a=f   D = 4:j<<g<f  E = 1:a<<<i=ki
 *
 * The advantage of the OGOCS string over the non-OGOCS string is that it's
 * easier to use regular expressions on the former. For example, reads with
 * 1 gap and a splicing that is compatible with the transcript can be filtered
 * with regex ":(j|g)<(g|f)$"
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
	static char buf[12];  /* should be enough for 32-bit ints */
	int ret;

	ret = snprintf(buf, sizeof(buf), "%d", d);
	if (ret < 0)  /* should never happen */
		error("IRanges internal error in CharAE_append_int(): "
		      "snprintf() returned value < 0");
	if (ret >= sizeof(buf))  /* could happen with ints > 32-bit */
		error("IRanges internal error in CharAE_append_int(): "
		      "output of snprintf() was truncated");
	_append_string_to_CharAE(char_ae, buf);
	return;
}

void _enc_overlaps_as_OGOCS(const int *q_start, const int *q_width, int q_len,
			    const int *s_start, const int *s_width, int s_len,
			    CharAE *out)
{
	int global_offset, offset, i, j, pos1;
	char code;

	if (q_len == 0 || s_len == 0)
		error("query or subject cannot have length 0");
	global_offset = 0;
	for (i = 0; i < q_len; i++) {
		offset = 0;
		for (j = 0; j < s_len; j++) {
			code = 'a' + overlap_code(q_start[i], q_width[i],
						  s_start[j], s_width[j]);
			if (offset == 0) {
				if (code == 'm' && j + 1 < s_len)
					continue;
				offset = j + 1;
				if (global_offset == 0) {
					global_offset = offset;
					CharAE_append_int(out, global_offset);
					CharAE_append_char(out, ':', 1);
				} else {
					if (offset < global_offset)
						CharAE_append_char(out, '>',
							global_offset - offset);
					else if (offset == global_offset)
						CharAE_append_char(out, '=', 1);
					else
						CharAE_append_char(out, '<',
							offset - global_offset);
					global_offset = offset;
				}
				pos1 = _CharAE_get_nelt(out) + 1;
			}
			CharAE_append_char(out, code, 1);
		}
		/* Remove trailing "a"'s */
		j = _CharAE_get_nelt(out);
		while (j > pos1) {
			if (out->elts[--j] != 'a')
				break;
		}
		_CharAE_set_nelt(out, j);
	}
	return;
}

/* --- .Call ENTRY POINT ---
 * 'query_start' and 'query_width': integer vectors of the same length M.
 * 'subject_start' and 'subject_width': integer vectors of the same length N
 * M or N cannot be 0.
 * The 4 integer vectors are assumed to be NA free and 'query_width' and
 * 'subject_width' are assumed to contain non-negative values. For efficiency
 * reasons, those assumptions are not checked.
 * Returns the corresponding OGOCS string in a raw vector.
 */
SEXP overlaps_to_OGOCS(SEXP query_start, SEXP query_width,
		       SEXP subject_start, SEXP subject_width)
{
	int m, n;
	CharAE buf;

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
	buf = _new_CharAE(0);
	_enc_overlaps_as_OGOCS(
			INTEGER(query_start), INTEGER(query_width), m,
			INTEGER(subject_start), INTEGER(subject_width), n,
			&buf);
	return _new_RAW_from_CharAE(&buf);
}


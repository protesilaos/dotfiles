#include <X11/Xft/Xft.h>
#include "st.h"
#include "boxdraw_data.h"

static void drawbox(XftDraw *, int, int, int, int, XftColor *, ushort);
static void drawboxlines(XftDraw *, int, int, int, int, XftColor *, ushort);

/* public API */

int
isboxdraw(const Glyph *g)
{
	return (g->u & ~0xff) == 0x2500 &&
	       boxdata[(uint8_t)g->u] != 0 &&
	       (g->mode & ATTR_ITALIC) == 0;
}

/* the "index" is actually the entire shape data encoded as ushort */
ushort
boxdrawindex(const Glyph *g)
{
	return boxdata[(uint8_t)g->u] | ((g->mode & ATTR_BOLD) ? BDB : 0);
}

void
drawboxes(XftDraw *xd, int x, int y, int cw, int ch, XftColor *fg,
          const XftGlyphFontSpec *specs, int len)
{
	for ( ; len-- > 0; x += cw, specs++)
		drawbox(xd, x, y, cw, ch, fg, (ushort)specs->glyph);
}

/* implementation */

void
drawbox(XftDraw *xd, int x, int y, int w, int h, XftColor *fg, ushort bd)
{
	if (bd & (BDL | BDA)) {
		/* lines (light/double/heavy/arcs) */
		drawboxlines(xd, x, y, w, h, fg, bd);

	} else if (bd & BBD) {
		/* lower (8-X)/8 block */
		int d = ((uint8_t)bd * h + 4) / 8;
		XftDrawRect(xd, fg, x, y + d, w, h - d);

	} else if (bd & BBU) {
		/* upper X/8 block */
		XftDrawRect(xd, fg, x, y, w, ((uint8_t)bd * h + 4) / 8);

	} else if (bd & BBL) {
		/* left X/8 block */
		XftDrawRect(xd, fg, x, y, ((uint8_t)bd * w + 4) / 8, h);

	} else if (bd & BBR) {
		/* right (8-X)/8 block */
		int d = ((uint8_t)bd * w + 4) / 8;
		XftDrawRect(xd, fg, x + d, y, w - d, h);

	} else if (bd & BBQ) {
		/* Quadrants */
		int w2 = (w + 1) / 2, h2 = (h + 1) / 2;
		if (bd & TL)
			XftDrawRect(xd, fg, x, y, w2, h2);
		if (bd & TR)
			XftDrawRect(xd, fg, x + w2, y, w - w2, h2);
		if (bd & BL)
			XftDrawRect(xd, fg, x, y + h2, w2, h - h2);
		if (bd & BR)
			XftDrawRect(xd, fg, x + w2, y + h2, w - w2, h - h2);
	}
}

void
drawboxlines(XftDraw *xd, int x, int y, int w, int h, XftColor *fg, ushort bd)
{
	/* s: stem thickness. width/8 roughly matches underscore thickness. */
	/* We draw bold as 1.5 * normal-stem and at least 1px thicker.      */
	/* doubles draw at least 3px, even when w or h < 3. bold needs 6px. */
	int mwh = MIN(w, h);
	int base_s = MAX(1, (mwh + 4) / 8);
	int bold = (bd & BDB) && mwh >= 6;  /* possibly ignore boldness */
	int s = bold ? MAX(base_s + 1, (3 * base_s + 1) / 2) : base_s;
	int w2 = (w - s + 1) / 2, h2 = (h - s + 1) / 2;
	/* the s-by-s square (x + w2, y + h2, s, s) is the center texel.    */
	/* The base length (per direction till edge) includes this square.  */

	int light = bd & (LL | LU | LR | LD);
	int double_ = bd & (DL | DU | DR | DD);

	if (light) {
		/* d: additional (negative) length to not-draw the center   */
		/* texel - at arcs and avoid drawing inside (some) doubles  */
		int arc = bd & BDA;
		int multi_light = light & (light - 1);
		int multi_double = double_ & (double_ - 1);
		/* light crosses double only at DH+LV, DV+LH (ref. shapes)  */
		int d = arc || (multi_double && !multi_light) ? -s : 0;

		if (bd & LL)
			XftDrawRect(xd, fg, x, y + h2, w2 + s + d, s);
		if (bd & LU)
			XftDrawRect(xd, fg, x + w2, y, s, h2 + s + d);
		if (bd & LR)
			XftDrawRect(xd, fg, x + w2 - d, y + h2, w - w2 + d, s);
		if (bd & LD)
			XftDrawRect(xd, fg, x + w2, y + h2 - d, s, h - h2 + d);
	}

	/* double lines - also align with light to form heavy when combined */
	if (double_) {
		/*
		* going clockwise, for each double-ray: p is additional length
		* to the single-ray nearer to the previous direction, and n to
		* the next. p and n adjust from the base length to lengths
		* which consider other doubles - shorter to avoid intersections
		* (p, n), or longer to draw the far-corner texel (n).
		*/
		int dl = bd & DL, du = bd & DU, dr = bd & DR, dd = bd & DD;
		if (dl) {
			int p = dd ? -s : 0, n = du ? -s : dd ? s : 0;
			XftDrawRect(xd, fg, x, y + h2 + s, w2 + s + p, s);
			XftDrawRect(xd, fg, x, y + h2 - s, w2 + s + n, s);
		}
		if (du) {
			int p = dl ? -s : 0, n = dr ? -s : dl ? s : 0;
			XftDrawRect(xd, fg, x + w2 - s, y, s, h2 + s + p);
			XftDrawRect(xd, fg, x + w2 + s, y, s, h2 + s + n);
		}
		if (dr) {
			int p = du ? -s : 0, n = dd ? -s : du ? s : 0;
			XftDrawRect(xd, fg, x + w2 - p, y + h2 - s, w - w2 + p, s);
			XftDrawRect(xd, fg, x + w2 - n, y + h2 + s, w - w2 + n, s);
		}
		if (dd) {
			int p = dr ? -s : 0, n = dl ? -s : dr ? s : 0;
			XftDrawRect(xd, fg, x + w2 + s, y + h2 - p, s, h - h2 + p);
			XftDrawRect(xd, fg, x + w2 - s, y + h2 - n, s, h - h2 + n);
		}
	}
}

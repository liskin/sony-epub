#!/bin/bash

# pdfcrop.sh -- non-invasive automatic PDF cropper
#
# Crops white margins of PDF pages. As opposed to other pdfcrop tools out
# there, this doesn't disassemble and reassemble the PDF file, nor does it use
# pdfTeX to reprocess the file, hence all bookmarks and hyperlinks stay intact,
# and it is much faster.  It uses ghostscript to calculate the bounding boxes,
# and then it updates the information in the original PDF file using pdfedit.
#
# Requirements: ghostscript, pdfedit
#
# Usage:
#  $ pdfcrop.sh file.pdf
#
# Known issue: Sometimes the resulting PDF file cannot be displayed on my Sony
# PRS-350, and this is easily fixed by filtering the file through pdfopt
# (that's a part of ghostscript).
#
# Author: Tomas Janousek <tomi@nomi.cz>
# License: BSD

f=$1; shift
if [ "$?" != "0" ]; then
    echo Usage: pdfcrop.sh file.pdf
    exit 1
fi

PREAMBLE='
margin = 5;

function setProp4( dict, p, a, b, c, d ) {
    if ( !dict.exist( p ) ) {
	var n = createArray();
	n.add( createReal( 0 ) );
	n.add( createReal( 0 ) );
	n.add( createReal( 0 ) );
	n.add( createReal( 0 ) );
	dict.add( p, n );
    }

    x = dict.property( p );
    x.property( 0 ).set( a );
    x.property( 1 ).set( b );
    x.property( 2 ).set( c );
    x.property( 3 ).set( d );
}

function setCrop( doc, pagenum, a, b, c, d ) {
    /* Ignore blank pages. */
    if ( a >= c || b >= d ) return;

    dict = doc.getPage( pagenum ).getDictionary();

    /* Adjust to bottom left corner of MediaBox. */
    media = dict.property( "MediaBox" );
    x = media.property( 0 ).value();
    y = media.property( 1 ).value();
    a += x; c += x;
    b += y; d += y;

    /* Add margin. */
    a -= margin; b -= margin; c += margin; d += margin;

    /* Set CropBox and TrimBox. */
    setProp4( dict, "CropBox", a, b, c, d );
    setProp4( dict, "TrimBox", a, b, c, d );
}

doc = loadPdf( takeParameter(), false );
'

PLSCRIPT='
BEGIN { $i = 1; }
if (/HiResBoundingBox: (\S+) (\S+) (\S+) (\S+)/) {
    print STDERR " $i";
    print "setCrop( doc, $i, $1, $2, $3, $4 );\n";
    $i++;
}
END { print STDERR "\n"; }
'

EPILOG='
doc.save( false );
exit( 0 );
'

echo -n "pdfcrop.sh: getting bounding boxes..."
{
    echo -n "$PREAMBLE"
    gs -dSAFER -dNOPAUSE -dBATCH -q -r72 -sDEVICE=bbox -f "$f" 2>&1 | perl -ne "$PLSCRIPT"
    echo -n "$EPILOG"
} >"$f".fix.qs

echo "pdfcrop.sh: cropping pages..."
pdfedit -console -run "$f".fix.qs "$f"

echo "pdfcrop.sh: done"

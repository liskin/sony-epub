#!/usr/bin/perl

# pdfcrop.pl -- non-invasive automatic PDF cropper
#
# Crops white margins of PDF pages. As opposed to other pdfcrop tools out
# there, this doesn't disassemble and reassemble the PDF file, nor does it use
# pdfTeX to reprocess the file, hence all bookmarks and hyperlinks stay intact.
# It uses ghostscript and bbox to calculate the bounding boxes, and then it
# updates the information in the original PDF file using pdfedit.
#
# Requirements: pdftk, ghostscript, bbox (ps2eps package in Debian), pdfedit
#
# Usage:
#  $ pdfcrop.pl file.pdf
#
# If you want pdfcrop.pl not to delete the pdfedit script so that you can tweak
# it (use different margins, etc.), use:
#  $ PDFCROP_DEBUG=1 pdfcrop.pl file.pdf
#
# Known issue: Sometimes the resulting PDF file cannot be displayed on my Sony
# PRS-350, and this is easily fixed by filtering the file through pdfopt
# (that's a part of ghostscript).
#
# Author: Tomas Janousek <tomi@nomi.cz>
# License: BSD

use strict;
use Cwd 'abs_path';
use File::Temp qw/:mktemp/;
use Data::Dumper;

sub usage {
    print "Usage: pdfcrop.pl file.pdf\n";
    exit;
}

my $f = shift or usage();
$f = abs_path($f);

my $debug = $ENV{'PDFCROP_DEBUG'};

my $dir = "/tmp/pdfcrop_XXXXXX";
die unless ($dir = mkdtemp($dir));

chdir $dir;
print "pdfcrop: Extracting pages.\n";
system "pdftk", $f, "burst", "output", "%d.pdf";

my %pages;
my @files = <*.pdf>;

for (@files) {
    my ($page) = (/(\d+)\.pdf/);
    print "pdfcrop: Calculating bounding box of page $_.\n";
    my $bbox = `gs -dSAFER -dNOPAUSE -q -r72 -sDEVICE=ppmraw -sOutputFile=- -f $_ -c showpage -c quit | bbox -r 72`;
    $pages{$page} = [($bbox =~ /HiResBoundingBox: (\S+) (\S+) (\S+) (\S+)/)];
}

open my $fix, "> fix.qs" or die $!;
print $fix <<END;
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
END

for my $p (sort (keys %pages)) {
    my $l = $pages{$p};
    print $fix "setCrop( doc, $p, $l->[0], $l->[1], $l->[2], $l->[3] );\n";
}

print $fix <<END;
doc.save( false );
exit( 0 );
END

print "pdfcrop: Applying page crops.\n";
system "pdfedit", "-console", "-run", "fix.qs", $f;

print "pdfcrop: Done.\n";

if ($debug == 1) {
    print "pdfcrop: Note: files left in $dir. You can tweak fix.qs and run `pdfedit -console -run fix.qs $f' to apply tweaked values.\n";
} else {
    system "rm", "-rf", $dir;
}

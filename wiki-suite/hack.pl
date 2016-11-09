if ( ! -d "ORIG"){
  system "git clone http://github.com/ku-fpg/blank-canvas.wiki ORIG";
}
if ( ! -d "DIFF"){
  system "mkdir -p DIFF/images";
}

foreach (<blank-canvas.wiki/images/*.png>) {
    /blank-canvas.wiki\/(.*)/;
    $nm = $1;
    print "$nm: ";
    system "compare -metric mae 'blank-canvas.wiki/$nm' 'ORIG/$nm' 'DIFF/$nm'";
    print "\n";
}

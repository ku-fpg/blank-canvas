open(F,"blank-canvas.wiki/Home.md");
open(G,">README.md");
$pre = "wiki/";
while(<F>) {
    # fix http links
    s/(http:\/\/\S+)(\s)/<$1>$2/g;
    # fix wiki links
    s/\[\[([^\]]+)\]\]/($a=$&)=~s%\s%\%20%g;$a/eg;
    s/\[\[([^\]]+)\]\]/<$pre$1>/g;


    print G "$_";
}
close(F);
close(G);

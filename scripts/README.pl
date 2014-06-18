open(F,"blank-canvas.wiki/Home.md");
open(G,">README.md");
$pre = "https://github.com/ku-fpg/blank-canvas/wiki";
while(<F>) {
    # fix http links
    s/(http:\/\/\S+)(\s)/<$1>$2/g;
    # fix wiki links
    s/\[\[([^\]\|]+)\]\]/\[\[$1\|$1\]\]/g;
    s/\[\[([^\|]+)\|([^\]]+)\]\]/\[$1\]($2)/g;
    s/\]\(([A-Z][^\)]+)\)/($a=$&)=~s%\s%\%20%g;$a/eg;
    s/\]\(([^h][^\)]+)\)/\]\($pre\/$1\)/g;
    s/(\[[^\]]*\]\([^\)]+\.png\))/\!$1/g;

#< <wiki/images/Red_Line.png>
#---
#> ![](https://github.com/ku-fpg/blank-canvas/wiki/images/Red_Line.png)


    print G "$_";
}
close(F);
close(G);

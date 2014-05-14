open(F,"Graphics/Blank/Canvas.hs");

$state = 0;
while(<F>) {
	if ($state <= 2) {
		$keep .= $_;
	}
	if ($state == 0) {
		if (/HTML5 Canvas assignments:\s+(.*)$/) {
			foreach (split(/,\s*/,$1)) {
				$isAssign{$_} = 1;
			}
			
		}

		next if (! /^data Command/);
		$state = 1;
		next;
	}
	if ($state == 1) {
		if (/^$/ || /Specials/) {
			$state = 2;
                        $show .= "  show (Specials sp) = show sp\n";
			next;
		}
                foreach(keys %dict) {
                        delete($dict{$_});
                }
                if (/forall\s+(\S+)\s+\.\s+(\S+)\s(\S+)\s\=\>/ && $1 eq $3) {
                        $_ = $` . $';
                        $dict{$1} = $2;
                        print "Adding $2 $1 as dictionary\n";
                }

                if (/[\|=]\s+(\S+)\s+(.*)$/) {
			print "($1)<$2>\n";

			$cmd = $1;
			$args = $2;
			$orig_args = $2;
			$args =~ s/\((.*)\)/$1/;
			@args = split(/[\(\),]/,$args);
			# name is the lower case version
			$name = $cmd;
			$name =~ s/(\w+)/\l$1/g;
			if ($args eq "") {
				$type = "() -> Canvas ()";
			} else {
				$type = "$orig_args -> Canvas ()";
			}
                        foreach(@args) {
                                if (defined($dict{$_})) {
                                        $type = "$dict{$_} $_ => " . $type;
                                }
                        }

			$header .= "        , $name\n";

#			$dsl .= "-- | '$name'\n";
			$dsl .= "\n";
			$dsl .= "$name :: $type\n";
			if ($args eq "") {
				$dsl .= "$name () = Command $cmd\n";
				$show .= "  show $cmd = \"c.$name()\"\n";
			} else {
				$dsl .= "$name = Command . $cmd\n";
				@ins = ();
				$n = 1;
				foreach (@args) {
					push(@ins,"a$n");
					$n++;
 				}
 				$show .= "  show ($cmd (" . join(',',@ins) . ")) = \"c.$name";
				if (defined $isAssign{$cmd}) {
				  $show .= " = ";
				} 
				$show .= "(\" ++ ";
				@outs = ();
				$n = 1;
				for $arg (@args) {
				     if ($arg eq "String" || $arg =~ /^\[.*\]$/) {
				     	push(@outs,"showJS a$n");
				     } else {
				     	push(@outs,"showJS a$n");
				     }
				     $n++;
				}
				$show .= join(" ++ \",\" ++ ",@outs);
				$show .= " ++ \")\"\n";
			}

		}
	}
	if ($state == 2) {
		if (/AUTO GENERATED AFTER HERE/) {
			$state = 3;
			next;
		}
	}
}


#print "-- DSL\n$dsl\n";
#print "-- Header\n$header\n";
#print "$keep";
#print "\n";
#print "instance Show Command where\n";
#print "$show\n";

open(G,">Graphics/Blank/Generated.hs");
print G "{-# OPTIONS_GHC -fno-warn-orphans #-}\n";
print G "module Graphics.Blank.Generated where\n";
print G "\n";
print G "import Graphics.Blank.Canvas\n";
print G "\n";
print G "instance Show Command where\n";
print G "$show\n";
print G "-- DSL\n$dsl\n";

close(G);